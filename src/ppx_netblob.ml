open Ast_convenience
open Ast_mapper
open Ast_helper
open Asttypes
open Cohttp
open Cohttp_lwt_unix
open Parsetree
open Location
open Longident

let deriver = "netblob"

let raise_errorf = Ppx_deriving.raise_errorf

(** parse the options passed to the direct invocation of the deriver
 * @returns a tuple of the target url of the request as a string and the method
 * of the request as a polymorphic variant
 * @TODO change the type of the target URL to an actual cohttp `Url.t` type or
 * something, add support for more than just `get` requests
 *)
let parse_options options =
  let (_, expr) = List.hd options in
  try
    let url =
      List.find (fun (name, _) -> name = "url") options
      |> snd
      |> Ast_convenience.get_str
      |> function
        | Some s ->
            s
        | None ->
            raise_errorf
              ~loc:expr.pexp_loc
              "%s option \"url\" accepts a string constant parameter"
              deriver
    in
    let meth =
      List.find (fun (name, _) -> name = "meth") options
      |> snd
      |> function
        | [%expr `Get] -> `Get
        | [%expr `Post] ->
            raise_errorf ~loc:expr.pexp_loc "%s does not yet support the POST method" deriver
        | _ ->
            raise_errorf ~loc:expr.pexp_loc "%s: invalid HTTP method" deriver
    in
    let format =
      List.find (fun (name, _) -> name = "format") options
      |> snd
      |> function
        | [%expr `Json] -> `Json None
        | [%expr (`Json [%e? func])] -> `Json (Some func)
        | [%expr `Xml] -> `Xml
        | [%expr `Text] -> `Text
        | _ -> raise_errorf ~loc:expr.pexp_loc "%s: invalid response format" deriver
    in
    url, meth, format
  with
    | Not_found ->
        raise_errorf ~loc:expr.pexp_loc "%s requires both a 'meth' option and a 'url' option" deriver

let attr_key attrs =
  Ppx_deriving.(attrs |> attr ~deriver "key" |> Arg.(get_attr ~deriver expr))

let attr_default attrs =
  Ppx_deriving.(attrs |> attr ~deriver "default" |> Arg.(get_attr ~deriver expr))

let attr_ispostparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "post" |> Arg.get_flag ~deriver)

let attr_isgetparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "get" |> Arg.get_flag ~deriver)

let attr_ispathparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "path" |> Arg.get_flag ~deriver)

let is_optional { pld_name = { txt = name }; pld_type; pld_attributes } =
  let attrs = pld_attributes @ pld_type.ptyp_attributes in
  match attr_default attrs with
  | Some _ -> true
  | None ->
      (match Ppx_deriving.remove_pervasives ~deriver pld_type with
        | [%type: [%t? _] option] -> true
        | _ -> false)

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let url, meth, format = parse_options options in
  let quoter = Ppx_deriving.create_quoter () in
  let creator =
    match type_decl.ptype_kind with
      | Ptype_record labels ->
          let fields =
            List.map
              (fun { pld_name = { txt = name; loc } } ->
                name, evar name)
              labels
          in
          let is_option = List.exists is_optional labels in
          (* [fn] is the actual HTTP calling function, so it's at the very
           * bottom of the recursive stack *)
          let fn =
            let formatter =
              match format with
                | `Json None ->
                    [%expr Yojson.Safe.from_string]
                | `Json (Some func) ->
                    [%expr
                      (fun s ->
                        let open Result in
                        let mime =
                          Cohttp.(
                            Header.get (Response.headers resp) "Content-Type")
                          |> function
                            | Some s -> s
                            | None -> ""
                        in
                        match String.trim (String.lowercase_ascii mime) with
                          | "application/json; charset=utf-8"
                          | "" ->
                              let json = Yojson.Safe.from_string s in
                              begin match [%e func] json with
                                | Ok _ as x ->
                                    x
                                | Error msg ->
                                    Error (
                                      "netblob: the following fragment does \
                                       not adhere to the expected schema (" ^
                                       msg ^ "):\n" ^
                                       Yojson.Safe.pretty_to_string json
                                       ^ "\n")
                              end
                          | s ->
                              Error (
                                Printf.sprintf
                                  "bad response Content-Type (%s):expected (%s)"
                                  mime
                                  "application/json; charset=utf-8"))]
                | `Xml -> [%expr (fun s -> Xmlm.make_input (`String (0, s)))]
                | `Text -> [%expr (fun s -> s)]
            in
            let payload =
              [%expr
                let headers = Cohttp.Header.init_with "User-Agent" "Mozilla/5.0" in
                Client.get ~headers uri
                >>= fun (resp, body) ->
                let rcode = Code.code_of_status (Response.status resp) in
                match rcode with
                  | 200 ->
                      Cohttp_lwt_body.to_string body
                      >>= fun s ->
                      Lwt.return ([%e formatter] s)
                  | 301 ->
                      let msg =
                        match Header.get (Cohttp_lwt_unix.Response.headers resp) "Location" with
                          | Some s -> s
                          | None -> "ERROR: no redirect target specified"
                      in
                      Lwt.fail_with (
                        Printf.sprintf
                          "Netblob received HTTP response code 301, meaning \
                          that the requested resource has been moved to %s"
                          msg)
                  | n ->
                      Lwt.fail_with (
                        Printf.sprintf
                          "Netblob received HTTP response code %d" n)]
            in
            match is_option with
              | true ->
                  Exp.fun_ Label.nolabel None (punit ()) payload
              | false ->
                  payload
          in
          List.fold_left (fun accum { pld_name = { txt = name }; pld_type; pld_attributes } ->
            let attrs = pld_attributes @ pld_type.ptyp_attributes in
            let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
            let evar_name = evar name in
            let ename = Exp.constant (Pconst_string (name, None)) in
            let key =
              match attr_key attrs with
                | Some key -> key
                | None -> ename
            in
            (** The function that will be used at runtime to marshal this
              * parameter into a string or (nonempty) list of strings *)
            let rec make_converter pld_type =
              let t =
                (** We need to start by extracting the base type
                  * @TODO figure out the desired semantics for [list option]s
                  *   and [option list]s *)
                match pld_type with
                  | [%type: [%t? t] list] -> t
                  | [%type: [%t? t] option] -> t
                  | [%type: [%t? t]] -> t
              in
              match t with
                | [%type: int] ->
                    [%expr (string_of_int)]
                | [%type: bool] ->
                    [%expr (string_of_bool)]
                | [%type: float] ->
                    [%expr (string_of_float)]
                | [%type: string] ->
                    [%expr ((fun x -> x)[@inlined])]
                | [%type: [%t? t1] * [%t? t2]] -> (* I'm so sorry *)
                    let c1, c2 = make_converter t1, make_converter t2 in
                    [%expr (fun (a, b) -> ([%e c1] a) ^ "," ^ ([%e c2] b))]
                | [%type: [%t? t1] * [%t? t2] * [%t? t3]] ->
                    (* I'll never use anything bigger than a 3-tuple, right? *)
                    let c1, c2, c3 = make_converter t1, make_converter t2, make_converter t3 in
                    [%expr (fun (a, b, c) -> ([%e c1] a) ^ "," ^ ([%e c2] b) ^ "," ([%e c3]))]
                | [%type: [%t? _]] ->
                    raise_errorf ~loc "%s doesn't know about this type" deriver
            in
            let converter = make_converter pld_type in
            (** The converter needs to get wrapped with [List.map] if t is a
              * list type *)
            let converter =
              match pld_type with
                | [%type: [%t? _] list] ->
                    [%expr
                      List.map ([%e converter])]
                | _ ->
                    [%expr (fun x -> [[%e converter] x])]
            in
            let add_to_uri_accum =
              [%expr
                let x = [%e converter] [%e evar_name] in
                let uri = Uri.add_query_param uri ([%e key], x) in
                [%e accum]]
            in
            let add_path_to_uri_accum =
              [%expr
                let [x] = [%e converter] [%e evar_name] in
                let path = Filename.concat (Uri.path uri) x in
                let uri = Uri.with_path uri path in
                [%e accum]]
            in
            let addparam_accum =
              match attr_ispathparam attrs with
                | true ->
                    add_path_to_uri_accum
                | false ->
                    add_to_uri_accum
            in
            match attr_default attrs with
              | Some default ->
                  let default = Some (Ppx_deriving.quote ~quoter default) in
                  Exp.fun_ (Label.optional name) default (pvar name) addparam_accum
              | None ->
                  begin match pld_type with
                    | [%type: [%t? _] option] ->
                        let accum' =
                          [%expr
                            let uri =
                              match [%e evar_name] with
                                | Some x ->
                                    let x = [%e converter] x in
                                    begin match x with
                                      | [] ->
                                          (* because fuck you that's why *)
                                          raise (Failure ("parameter is required"))
                                      | x ->
                                          Uri.add_query_param uri ([%e key], x)
                                    end
                                | None -> uri
                            in
                            [%e accum]]
                        in
                        Exp.fun_ (Label.optional name) None (pvar name) accum'
                    | _ ->
                        Exp.fun_ (Label.labelled name) None (pvar name) addparam_accum
                  end)
            fn
            labels
      | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver
  in
  let uri = Exp.constant (Pconst_string (url, None)) in
  let creator =
    [%expr
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let open Lwt in
      let uri = Uri.of_string [%e uri] in
      [%e creator]]
  in
  let prefix =
    match meth with
      | `Get -> "netblob_get"
      | `Post -> "netblob_post"
  in
  let name =
    match type_decl with
      | { ptype_name = { txt = "t" } } ->
          prefix
      | _ ->
          Ppx_deriving.mangle_type_decl (`Prefix prefix) type_decl
  in
  [Vb.mk (pvar name) (Ppx_deriving.sanitize ~quoter creator)]

let sig_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let url, meth, format = parse_options options in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
      | Ptype_record labels ->
        let has_option = List.exists is_optional labels in
        let typ =
          match has_option with
            | true -> Typ.arrow Label.nolabel (tconstr "unit" []) typ
            | false -> typ
        in
        List.fold_left (fun accum { pld_name = { txt = name; loc }; pld_type; pld_attributes } ->
          let attrs = pld_type.ptyp_attributes @ pld_attributes in
          let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
          match attr_default attrs with
            | Some _ -> Typ.arrow (Label.optional name) pld_type accum
            | None ->
                begin match pld_type with
                  | [%type: [%t? _] list] ->
                      Typ.arrow (Label.optional name) pld_type accum
                  | [%type: [%t? opt] option] ->
                      Typ.arrow (Label.optional name) opt accum
                  | _ ->
                      Typ.arrow (Label.labelled name) pld_type accum
                end)
          typ labels
      | _ -> raise_errorf ~loc "%s can only be derived for record types" deriver
  in
  let prefix =
    match meth with
      | `Get -> "netblob_get"
      | `Post -> "netblob_post"
  in
  let name =
    match type_decl with
      | { ptype_name = { txt = "t" } } ->
          prefix
      | _ ->
          Ppx_deriving.mangle_type_decl (`Prefix prefix) type_decl
  in
  [Sig.value (Val.mk (mknoloc name) typ)]

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       [Str.value Nonrecursive (List.concat (List.map (str_of_type ~options ~path) type_decls))])
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()))

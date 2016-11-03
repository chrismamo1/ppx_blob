(*  "https://goo.gl/nTD9Oc" *)
open Lwt

let () =
  let get_message = [%netblob { runtime = "https://goo.gl/nTD9Oc" }] in
  Lwt_main.run (
    get_message ()
    >>= fun s ->
    Lwt_io.printl s)

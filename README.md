ppx_netblob
========

OCaml ppx to include a binary blob from a URL as a string. Writing `[%netblob
"url"]` will replace the string with the result of sending an HTTP GET
request to `url` at compile time. This allows the inclusion of arbitary,
possibly compressed, data, without the need to respect OCaml's lexical
conventions. It should be noted that `ppx_netblob` will interpret HTTP 301
responses, following the URL given in the response's `Location` header, which
is a possible security vulnerability (and emitting a warning). I would advise
against using this in production code, since I haven't done a huge amount of
research into how well `cohttp` supports HTTPS, so I'm not sure if this is
subject to security downgrading attacks.

To build
--------

Requires OCaml 4.02 or above.

Run `make` in the top directory. Then run `make` in the `examples` directory.
Now run the `quine` executable.

To install
----------

Run `make install` in the top directory once `make` has been run.

To use
------

The basic (ill-advised) usage of `ppx_netblob` involves loading a network
resource into a string at compile-time, e.g.

    let () =
      print_endline [%netblob "https://goo.gl/nTD9Oc"]

is transformed into:

    let () =
      print_endline "Hello, World!"

It should be noted that this sort of usage presents a smorgasbord of potential
problems for both security and basic usability, although superficial precautions
have been taken to minimize such problems. For instance, compiling the example
above would produce the following warning:

    WARNING: received response code 301 MOVED PERMANENTLY to "https://gist.githubusercontent.com/chrismamo1/ca3210b8f503ecc3ec5b154ff39fb2b3/raw/0fb8245d996f93a0df1e20f94e7df6403c094f62/hello_world.txt" when requesting resource "https://goo.gl/nTD9Oc", this is probably a security vulnerability.

The more useful feature of `ppx_netblob` involves building custom HTTP request
functions at compile time, e.g.

    open Lwt

    let () =
      let get_message = [%netblob { runtime = "https://goo.gl/nTD9Oc" }] in
      Lwt_main.run (
        get_message ()
        >>= fun s ->
        Lwt_io.printl s)

in this example, `[%netblob { runtime = "https://goo.gl/nTD9Oc" }]` is expanded
into a decently performant function which handles a few problematic cases. This
feature is very incomplete, however, and users of this tool (when and if they
start to exist) should not expect it to retain a consistent interface over the
next few months.

TODO
----

 - Allow constraints to be placed on the parameters which will be accepted when
 using the runtime netblob ppx, e.g.
 
    [%netblob { runtime = "https://github.com/search" ; parameters = ["utf8"; "q"]}

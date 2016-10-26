(*  "https://goo.gl/nTD9Oc" *)

open OUnit2

let a =
  Lwt_main.run
    ([%netblob
      { runtime = "https://gist.githubusercontent.com/chrismamo1/ca3210b8f503ecc3ec5b154ff39fb2b3/raw/0fb8245d996f93a0df1e20f94e7df6403c094f62/hello_world.txt" }]
        ~get:[]
        ())

let b =
  Lwt_main.run
    ([%netblob { runtime = "https://goo.gl/nTD9Oc" }] ~get:[] ())

let suite =
  "ppx_netblob tests" >:::
    [ "compile-time tests" >:::
      [ "ppx_netblob" >::
        (fun _ ->
          assert_equal
            ~cmp:(fun a b -> if String.compare a b = 0 then true else false)
            "Hello, World!"
            [%netblob "https://gist.githubusercontent.com/chrismamo1/ca3210b8f503ecc3ec5b154ff39fb2b3/raw/0fb8245d996f93a0df1e20f94e7df6403c094f62/hello_world.txt"])
      ; "ppx_netblob with a URL shortener" >::
        (fun _ ->
          assert_equal
            ~cmp:(fun a b -> if String.compare a b = 0 then true else false)
            "Hello, World!"
            [%netblob "https://goo.gl/nTD9Oc"])
      ]
    ; "runtime tests" >:::
        [ "ppx_netblob" >::
          (fun _ ->
            assert_equal
              a
              "Hello, World!")
        ; "ppx_netblob with a URL shortener" >::
          (fun _ ->
            assert_equal
              b
              "Hello, World!")
        ]
    ]

let () =
  run_test_tt_main suite

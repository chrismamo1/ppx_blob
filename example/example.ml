open Lwt

type coords =
  { lat : float
  ; lon : float
  }
  [@@deriving yojson]

type weather_desc =
  { id : int
  ; main : string
  ; description : string
  ; icon : string
  }
  [@@deriving yojson]

type main_desc =
  { temp : float
  ; pressure : int
  ; humidity : int
  ; temp_min : float
  ; temp_max : float
  }
  [@@deriving yojson]

type wind_desc =
  { speed : float
  ; deg : int
  }
  [@@deriving yojson]

type clouds_desc =
  { all : int }
  [@@deriving yojson]

type sys_desc =
  { typ : int [@key "type"]
  ; id : int
  ; message : float
  ; country : string
  ; sunrise : int
  ; sunset : int
  }
  [@@deriving yojson]

type weather =
  { coord : coords
  ; weather : weather_desc list
  ; base : string
  ; main : main_desc
  ; visibility : int
  ; wind : wind_desc
  ; clouds : clouds_desc
  ; dt : int
  ; sys : sys_desc
  ; id : int
  ; name : string
  ; cod : int
  }
  [@@deriving yojson]

type weather_req =
  { q : string
  ; appid : string [@key "APPID"]
  }
[@@deriving
  netblob
  { url = "http://api.openweathermap.org/data/2.5/weather"
  ; meth = `Get
  ; format = `Json weather_of_yojson }]
(** netblob_get_weather_req : q:string -> appid:string -> (weather Result.result) Lwt.t *)

let () =
  let city = Sys.argv.(1) in
  Lwt_main.run (
    netblob_get_weather_req ~q:city ~appid:"f2fe6367e16e3e7ddd4b00cca425a084"
    >>= function
      | Result.Ok weather ->
          Lwt_io.printf
            "Current temperature in %s (%f, %f): %f"
            city
            weather.coord.lat weather.coord.lon
            weather.main.temp
      | Result.Error msg ->
          raise (Failure ("Problem parsing json: " ^ msg)))

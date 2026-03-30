open Value

type env = (string, value) Hashtbl.t
;;

let mk_env () = Hashtbl.create 20
;;

let lookup env name =
  if Hashtbl.mem env name then
    Hashtbl.find env name
  else
    raise (Runtime_Error ("undefined variable: " ^ name))
;;

let update env name value =
  Hashtbl.replace env name value
;;

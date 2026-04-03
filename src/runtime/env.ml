open Value

let mk_env () =
  { table = Hashtbl.create 20; parent = None }
;;

let mk_child_env parent =
  { table = Hashtbl.create 20; parent = Some parent }
;;

let define env name value =
  Hashtbl.replace env.table name value
;;

let rec lookup env name =
  if Hashtbl.mem env.table name then
    Hashtbl.find env.table name
  else
    match env.parent with
    | Some parent ->
        lookup parent name
    | None ->
        raise (Runtime_Error ("undefined variable: " ^ name))
;;

let rec update env name value =
  if Hashtbl.mem env.table name then
    Hashtbl.replace env.table name value
  else
    match env.parent with
    | Some parent ->
        update parent name value
    | None ->
        define env name value
;;

exception Runtime_Error of string

open Ast

type dict_key =
  | Int_Key of int
  | Float_Key of float
  | Bool_Key of bool
  | String_Key of string
  | Tuple_Key of dict_key list
;;

type env =
  { table : (string, value) Hashtbl.t
  ; parent : env option
  }

and func_value =
  { name : string option
  ; params : string list
  ; body : stmt list
  ; closure : env
  }

and value =
  | Int_Val of int
  | Float_Val of float
  | Bool_Val of bool
  | String_Val of string
  | List_Val of value list
  | Tuple_Val of value list
  | Dict_Val of (dict_key, value) Hashtbl.t
  | Function_Val of func_value
  | None_Val
;;

let quoted_string s =
  "\"" ^ String.escaped s ^ "\""
;;

let float_string x =
  let s = string_of_float x in
  if String.get s (String.length s - 1) = '.'
  then
    s ^ "0"
  else
    s
;;

let rec string_of_dict_key k =
  match k with
  | Int_Key n ->
      string_of_int n
  | Float_Key x ->
      float_string x
  | Bool_Key true ->
      "True"
  | Bool_Key false ->
      "False"
  | String_Key s ->
      quoted_string s
  | Tuple_Key [] ->
      "()"
  | Tuple_Key [k1] ->
      "(" ^ string_of_dict_key k1 ^ ",)"
  | Tuple_Key ks ->
      "(" ^ string_of_dict_keys ks ^ ")"

and
  string_of_dict_keys ks =
    match ks with
    | [] ->
        ""
    | [k1] ->
        string_of_dict_key k1
    | k1::rest ->
        string_of_dict_key k1 ^ ", " ^ string_of_dict_keys rest
;;

let rec string_of_value v =
  match v with
  | Int_Val n ->
      string_of_int n
  | Float_Val x ->
      float_string x
  | Bool_Val true ->
      "True"
  | Bool_Val false ->
      "False"
  | String_Val s ->
      quoted_string s
  | List_Val vs ->
      "[" ^ string_of_values vs ^ "]"
  | Tuple_Val [] ->
      "()"
  | Tuple_Val [v1] ->
      "(" ^ string_of_value v1 ^ ",)"
  | Tuple_Val vs ->
      "(" ^ string_of_values vs ^ ")"
  | Dict_Val d ->
      "{" ^ string_of_dict_entries d ^ "}"
  | Function_Val f ->
      (match f.name with
       | Some name -> "<function " ^ name ^ ">"
       | None -> "<function>")
  | None_Val ->
      "None"

and
  string_of_values vs =
    match vs with
    | [] ->
        ""
    | [v1] ->
        string_of_value v1
    | v1::rest ->
        string_of_value v1 ^ ", " ^ string_of_values rest

and
  string_of_dict_entries d =
    let entries =
      Hashtbl.fold
        (fun k v acc ->
           (string_of_dict_key k ^ ": " ^ string_of_value v)::acc)
        d
        []
    in
    String.concat ", " (List.sort String.compare entries)
;;

let rec dict_key_of_value v =
  match v with
  | Int_Val n ->
      Int_Key n
  | Float_Val x ->
      Float_Key x
  | Bool_Val b ->
      Bool_Key b
  | String_Val s ->
      String_Key s
  | Tuple_Val vs ->
      Tuple_Key (List.map dict_key_of_value vs)
  | List_Val _ ->
      raise (Runtime_Error "unhashable type: list")
  | Dict_Val _ ->
      raise (Runtime_Error "unhashable type: dict")
  | Function_Val _ ->
      raise (Runtime_Error "unhashable type: function")
  | None_Val ->
      raise (Runtime_Error "unhashable type: NoneType")
;;

let rec value_of_dict_key k =
  match k with
  | Int_Key n ->
      Int_Val n
  | Float_Key x ->
      Float_Val x
  | Bool_Key b ->
      Bool_Val b
  | String_Key s ->
      String_Val s
  | Tuple_Key ks ->
      Tuple_Val (List.map value_of_dict_key ks)
;;

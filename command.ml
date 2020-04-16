type command = 
  | Balance
  | Add of float
  | Remove of float
  | Use of float
  | Move of float
  | Quit
  | CreateCategory of string
  | CreateBudget 
  | Status
  | GeneralHist
  | Share
  | Help
  | History of string
  | Lookup of string
  | Switch
  | Reset
  | Logout

exception Empty

exception Malformed

(** [parse str] returns the command associated with the string [str] after 
    parsing it. *)
let parse str = 
  let parsed_input = List.filter (fun x -> not (x = "")) 
      (String.split_on_char ' ' str) in
  match parsed_input with 
  | [] -> raise Empty
  | h::[] -> (match h with 
      | "balance" -> Balance
      | "status" -> Status
      | "quit" -> Quit
      | "share" -> Share
      | "history" -> GeneralHist
      | "switch" -> Switch
      | "help" -> Help
      | "reset" -> Reset
      | "logout" -> Logout
      | _ -> raise Malformed)
  | h::t::[] -> (match h with 
      | "create" -> if t = "budget" then CreateBudget else raise Malformed
      | "add" -> Add (float_of_string t)
      | "use" -> Use (float_of_string t)
      | "remove" -> Remove (float_of_string t)
      | "move" -> Move (float_of_string t)
      | "history" -> History (t)
      | "lookup" -> Lookup (t)
      | _ -> raise Malformed)
  | h::t -> (match h with 
      | "create" -> (match t with 
          | a::b::[] -> if a = "category" then CreateCategory b 
            else raise Malformed
          |_-> raise Malformed)
      | _ -> raise Malformed)
  | _ -> raise Malformed


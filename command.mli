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
val parse : string -> command

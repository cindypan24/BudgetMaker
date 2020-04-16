open Transaction
open Budget

type t = {
  transactions: transaction list;
  username : string;
  password: string;
  budgets: string list;
  active_budget : string
}

exception NotFound

let init_user_state username password  = {
  transactions = [];
  username = username;
  password = password;
  budgets = [];
  active_budget = ""
}

let get_transactions user = 
  user.transactions

let get_username user = 
  user.username

let get_budgets user =
  user.budgets  

let get_active_budget user = 
  user.active_budget

let add_transaction user transaction = 
  let new_transactions = transaction::(user.transactions) in 
  {user with transactions = new_transactions}

let rec get_user_from_username username list = 
  match list with 
  | h::t -> if username = h.username then h else get_user_from_username username t
  | [] -> raise NotFound

let check_password password user = 
  password = user.password

let add_budget budget user = 
  let new_budgets = budget::(user.budgets) in
  {user with budgets = new_budgets}

let rec remove_budget name budgets acc = 
  match budgets with 
  | [] -> acc
  | h::t -> if h = name then acc@t else remove_budget name t (h::acc) 

let set_active_budget string user = 
  {user with active_budget = string}
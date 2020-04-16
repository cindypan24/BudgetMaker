open Transaction

type t = {
  curr_balance: float;
  init_balance: float;
  categories: (string * float * float) list;
  transactions: transaction list;
  members: string list;
  name: string;
}

let init_budget name first_member balance = {
  curr_balance = balance;
  init_balance = balance;
  categories = [];
  transactions = [];
  members = [first_member];
  name = name
}

exception NotFound

let get_balance budget = 
  budget.curr_balance

let get_init_balance budget = 
  budget.init_balance

let get_name budget = 
  budget.name

let get_categories budget = 
  budget.categories

let get_transactions budget = 
  budget.transactions

let get_members budget = 
  budget.members

let add_transaction budget transaction = 
  let new_transactions = transaction::(budget.transactions) in 
  {budget with transactions = new_transactions}

let add_member member budget = 
  { budget with members = member::budget.members}

let rec get_budget_from_name name list = 
  match list with 
  | h::t -> if name = h.name then h else get_budget_from_name name t
  | [] -> raise NotFound

let  add_to_category amount category budget = 
  let rec updated_categories amount category list acc= match list with 
    | [] -> acc
    | (a,b,c)::t -> if a = category then ((a,b+.amount,c+.amount)::t)@acc
      else updated_categories amount category t ((a,b,c)::acc) in
  {budget with categories = updated_categories amount category 
                   budget.categories []}

let  use_from_category amount category budget = 
  let rec updated_categories amount category list acc= match list with 
    | [] -> acc
    | (a,b,c)::t -> if a = category then ((a,b-.amount,c)::t)@acc
      else updated_categories amount category t ((a,b,c)::acc) in
  {budget with categories = updated_categories amount category 
                   budget.categories []}

let add amount budget = 
  let new_balance = budget.curr_balance +. amount in
  let new_init = budget.init_balance +. amount in
  {budget with curr_balance = new_balance; init_balance = new_init}

let remove amount budget = 
  let new_balance = budget.curr_balance -. amount in
  {budget with curr_balance = new_balance}

let add_categories category budget = 
  let new_categories = (category,0.0,0.0)::budget.categories in
  {budget with categories = new_categories}

let rec valid_category category list = 
  match list with 
  | [] -> false
  | (a,b,c)::t -> if a = category then true else valid_category category t

let reset_budget budget = 
  let rec refill list acc = 
    match list with 
    |[] ->  acc
    | (a,b,c)::t -> refill t ((a,c,c)::acc) in
  let new_cats = refill (budget.categories) [] in
  {budget with curr_balance = budget.init_balance; 
               categories = new_cats; transactions = []}
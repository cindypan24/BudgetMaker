open UserState
open Budget

type t = {
  users: UserState.t list;
  budgets: Budget.t list;
  active_user : UserState.t
}

let init_state = {
  users = [];
  budgets = [];
  active_user = init_user_state "" ""
}

let get_users st = 
  st.users

let get_active_user st = 
  st.active_user

let get_budgets st = 
  st.budgets  

let set_active_user st user = 
  {st with active_user = user}

let add_user user st = 
  let new_users = user::(st.users) in 
  {st with users = new_users}

let add_budget budget st = 
  let new_budgets = budget::(st.budgets) in
  {st with budgets = new_budgets}

let set_users st users_list =
  {st with users = users_list}

let set_budgets st budgets_list = 
  {st with budgets = budgets_list}

let rec check_valid_username username users_lst = 
  match users_lst with 
  | [] -> false
  | hd::tl -> 
    if (String.lowercase_ascii hd.username) = (String.lowercase_ascii username)
    then true else check_valid_username username tl

let rec check_valid_budget_name budget_name budget_list = 
  match budget_list with 
  | [] -> false
  | h::t -> if budget_name = get_name h then true
    else check_valid_budget_name budget_name t
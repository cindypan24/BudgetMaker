open UserState
open Budget

type t = {
  users: UserState.t list;
  budgets : Budget.t list;
  active_user : UserState.t
}

(** [init_state] is the initial state of a state.t record where there are no 
    users, no budgets, and the active user is essentially a null user. *)
val init_state : t

(** [get_users st] returns the list of users in a given state [st]. *)
val get_users : t -> UserState.t list

(** [get_active_user st] returns the active user from a given state [st]. *)
val get_active_user : t -> UserState.t

(** [get_budgets st] returns the list of budgets in a given state [st]. *)
val get_budgets : t -> Budget.t list

(** [set_active_user st user] returns the state after setting user [user] to be 
    the active user in state [st]. *)
val set_active_user : t -> UserState.t -> t

(** [add_user] returns the state after adding user [user] to the list of users
    in state [st]. *)
val add_user : UserState.t -> t -> t

(** [add_budget budget st] returns the state after adding budget [budget] to 
    state [st]. *)
val add_budget : Budget.t -> t -> t

(** [set_users] returns the state after setting the users in state [st] to 
    the list of users [users_list]. *)
val set_users : t -> UserState.t list -> t

(** [set_budgets st budgets_list] returns the state after setting the budgets in 
    state [st] to the list of budgets [budgets_list]. *)
val set_budgets : t -> Budget.t list -> t

(** [check_valid_username username users_lst] returns true if username 
    [username] is a valid username in the list of users [users_lst] and false
    otherwise. *)
val check_valid_username : string -> UserState.t list -> bool

(** [check_valid_budget_name budget_name budget_list] returns true is the budget
    [budget_name] is a valid budget in the list of budgets [budget_list]. *)
val check_valid_budget_name : string -> Budget.t list -> bool

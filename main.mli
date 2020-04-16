open UserState
open State
open Transaction
open Command
open BudgetUser
open Budget


(** [update_users new_user_state users_list acc] returns a new list of user 
    states after updating the user's state [new_user_state] in the list 
    [users_list]. *)
val update_users : UserState.t -> UserState.t list -> UserState.t list -> UserState.t list

(** [update_state_users state new_user_state] returns the new state after 
    getting the new updated users list from the state [state]. *)
val update_state_users : State.t -> UserState.t -> State.t

(** [update_budgets new_budget budget_list acc] returns the new list of budgets
    [budget_list] after updating the budget [new_budget] in the list of budgets
    [budget_list]. *)
val update_budgets : Budget.t -> Budget.t list -> Budget.t list -> Budget.t list

(** [update_state_budgets state new_budget] returns the new state after adding 
    the new budget [new_budget] to the state [state]. *)
val update_state_budgets : State.t -> Budget.t -> State.t

(** [setup_user st] returns the state after setting up a new user from state
    [st]. *)
val setup_user : State.t -> State.t

(** [start_budget state] returns the state after getting the budget and setting 
    it up from [state]. *)
val start_budget : State.t -> State.t

(** [put_password user] prompts the user [user] to enter their password and 
    returns true if the password is entered correctly or false if it is not. *)
val put_password : UserState.t -> bool

(** [switch_user state] returns the state after prompting the user to change 
    the active user and then returning the state from [state]. *)
val switch_user : State.t -> State.t

(** [print_budgets budgets] prints the budgets in the list of budgets 
    [budgets]. *)
val print_budgets : string list -> string

(** [switch_budget state] returns the state that results from allowing the 
    user to switch budgets from state [state]. *)
val switch_budget : State.t -> State.t

(** [share_budget state] returns the state after prompting the user to share
    the current budget and  *)
val share_budget : State.t -> State.t

(** [continue_budget state] uses the current state [state] to continue the 
    budget and deal with the commands the user inputs.  *)
val continue_budget : State.t -> unit

(** [log_in st] returns the state after starting the budget from state [st]. *)
val log_in : State.t -> State.t

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit


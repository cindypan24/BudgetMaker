open Transaction
open Budget

(** Type [t] represents a budget, a budget has a current_balance (curr_balance), 
    an initial balance (init_balance), and a list of categories (categories) *)
type t = {
  transactions: transaction list;
  username : string;
  password: string;
  budgets: string list;
  active_budget : string
}


(** [init_state b] creates a new budget with initial ballance [b]
    and a current balance of [b], the categories are not yet defined
     in this budget*)
val init_user_state : string -> string -> t

(** [get_transactions u] returns the list of transactions
    of user [u] *)
val get_transactions : t -> Transaction.transaction list

(** [get_username u] returns the username of user [u]*)
val get_username : t -> string

(** [get_budgets u] returns a list of the budget names that user [u]
    has access to *)
val get_budgets : t -> string list

(** [get_user_from_username u list] returns the user in [list] that has the
    username [u] *)
val get_user_from_username : string -> t list -> t

(** [get_active_budget u] returns the active budget of user [u] *)
val get_active_budget : t -> string

(** [add_transaction u tran] returns user [u] with a new list of transactions
    that includes [tran] *)
val add_transaction : t -> Transaction.transaction -> t

(** [check_password pass u] returns true if the password [pass] matches
    the password of user [u] *)
val check_password : string -> t -> bool 

(** [add_budget name u] returns user [u] with a new list of budget names
    that includes budget [name] *)
val add_budget : string -> t -> t

(** *)
val remove_budget : string -> string list -> string list -> string list

(** [set_active_budget name u] returns user [u] with a new 
    active budget [name]*)
val set_active_budget: string -> t -> t
open Transaction

type t = {
  curr_balance: float;
  init_balance: float;
  categories: (string * float * float) list;
  transactions: transaction list;
  members: string list;
  name: string
}

exception NotFound

val init_budget : string -> string -> float -> t

(** [get_balance st] returns the current balance of the budget [st] *)
val get_balance : t -> float

(** [get_init_balance st] returns the initial balance of the budget [st] *)
val get_init_balance : t -> float

val get_transactions : t -> Transaction.transaction list

val get_name : t -> string

val get_members : t -> string list

(** [get_categories st] returns the list of categories in budget [st] *)
val get_categories: t -> (string * float * float) list

(** [add a st] returns a new budget with amount [a] added to the initial
    balance and current balance of budget [st] *)
val add : float -> t -> t

val get_budget_from_name : string -> t list -> t


(** [remove a st] returns a new budget with amount [a] removed from the 
    current balance of budget [st] *)
val remove : float -> t -> t

(** [add_categories s st] returns a new budget with a new category appended to
    categories list of budget [st], with name [s], and current amount 
    and initial amount equal to 0 *)
val add_categories : string -> t -> t

(** [use_from_category a s st] returns a new budget with amount [a] removed 
    from the current amount of category named [s] in budget [st] *)
val use_from_category : float -> string -> t -> t

(** [add_to_category a s st] returns a new budget with amount [a] added to both 
    the current amount and the initial amount of category named [s] 
    in budget [st] *)
val add_to_category: float -> string -> t -> t


(** [valid_category s list] returns true if the string [s] is found in the list 
    of categories [lst] *)
val valid_category: string -> (string * float * float) list -> bool

val add_transaction : t -> Transaction.transaction -> t

val add_member : string -> t -> t

val reset_budget : t -> t
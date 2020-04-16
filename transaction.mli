
(** Type [t] represents a transaction, a transaction has a date (date: 1-7), 
    an description (description), an amount (amount), and a category (category) *)
type transaction = {
  date : int;
  description : string;
  amount : float;
  category : string 
} 

type t = transaction list

exception NotValidDate

(** [get_date t] matches t.date, an integer 1-7, to its corresponding day of 
    the week Monday-Sunday. *)
val get_date : transaction -> string

(** [get_description t] returns the description of t. *)
val get_description : transaction -> string

(** [get_amount t] returns the amount of t. *)
val get_amount : transaction -> float

(** [get_category t] returns the category of t. *)
val get_category : transaction -> string

(** [get_transactions_from_category category lst acc] returns a list [acc] 
    of transactions from [list] from the specified category [category]. *)
val get_transactions_from_category : string -> t -> t -> t

(** [get_transactions_from_day day lst acc] returns a list [acc] 
    of transactions from [list] from the specified day [day]. *)
val get_transactions_from_day : int -> t -> t -> t

(** [create_transaction date desc amount cat] returns Transaction.t with 
    date = date, description = desc, amount = amount, category = cat. *)
val create_transaction : int -> string -> float -> string -> transaction

(** [add_transactions transaction lst] returns and adds a transaction 
    [transaction] to a list of transactions [list]. *)
val add_transaction : transaction -> t -> t

(** [read_day str] matchs a day of the week Monday-Sunday [str] 
    to its corresponding integer value. *)
val read_day : string -> int

(** [contains_word word_list str] returns true if a word [str] is contained 
    in a string [word_list] and false if it is not. *)
val contains_word : string list -> string -> bool

(** [lookup_by_description word lst acc] returns a list of 
    transactions [acc] from a transaction list [acc] that contain
    the specificed word [word] in the description. *)
val lookup_by_description : string -> t -> t -> t
open State
open Transaction
open Command
open Budget

(** [clear 0] clears the terminal screen. *)
val clear : int -> unit

(** [space 0] prints 10 newlines on the terminal screen. *)
val space : int -> unit 

(** [enter 0] prompts the user to press 'enter' to continue. *)
val enter : int -> unit

(** [which_category amount st] asks the users which categeory they would like 
    to add or remove [amount] from and then performs the operation. Returns 
    the resulting state after the operation is performed from the initial [st].
*)
val which_category : float -> Budget.t -> Budget.t

(** [use_helper_category st] prompts users to select the category they want to 
    use money from and makes sure it is a valid category in state [st] and 
    returns that category. *)
val use_helper_category: Budget.t -> string

(** [use_helper_day amount] prompts users to enter the day of the week the 
    transaction is taking place and returns the day. *)
val use_helper_day : float -> int

(** [use_helper_description amount] prompts user to enter a description for the 
    transaction and returns the description. *)
val use_helper_description : float -> string

(** [use_from_cat amount category st] returns the state after using [amount] 
    from [category] from the initial state [st]. *)
val use_from_cat : float -> string -> Budget.t -> Budget.t

(** [print_status st transactions] prints the current balance to the terminal 
    and creates the bar graphs for the current balance remaining in each 
    category given the current state [st] and list of transcations 
    performed [transactions]. It also prints the bar graph of the spending 
    done on each day so far. *)
val print_status : Budget.t -> Transaction.t -> unit

(** [print_transactions lst] prints the transactions from the list of 
    transactions [lst]. *)
val print_transactions : Transaction.transaction list -> unit

(** [get_daily_spending transactions day acc] returns the total amount of money
    spent on day [day] from a list of transactions [transactions]. *)
val get_daily_spending : Transaction.transaction list -> int -> float -> float

(** [set_up_categories budget] returns the budget that results from setting up
    the categories in budget [budget]. *)
val set_up_categories : Budget.t -> Budget.t 

(** [divide_initial_balance balance list budget] returns the budget after adding 
    the initial balance [balance] to the different categories [list] to 
    budget [budget]. *)
val divide_initial_balance : float -> (string * float * float) list -> Budget.t -> Budget.t




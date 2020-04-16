
type transaction = {
  date : int;
  description : string;
  amount : float;
  category : string 
} 

exception NotValidDate

type t = transaction list

let get_date t = 
  match t.date with 
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | 7 -> "Sunday"
  | _ -> "NA"

let get_description t = 
  t.description

let get_amount t = 
  t.amount

let get_category t = 
  t.category

let create_transaction date desc amount cat = 
  {date = date; description = desc; amount = amount; category = cat}

let add_transaction transaction lst = 
  transaction::lst

let read_day str = 
  let parsed_input = List.filter (fun x -> not (x = "")) 
      (String.split_on_char ' ' str) in
  match parsed_input with 
  | h::[] -> (match (String.lowercase_ascii h) with 
      | "monday" -> 1
      | "tuesday" ->  2
      | "wednesday" -> 3
      | "thursday" -> 4
      | "friday" -> 5
      | "saturday" -> 6
      | "sunday" -> 7
      | _ -> raise NotValidDate)
  | _ -> raise NotValidDate 

let rec get_transactions_from_category category lst acc = 
  match lst with
  | [] -> acc
  | h::t -> if h.category = category 
    then get_transactions_from_category category t (h::acc)
    else get_transactions_from_category category t acc

let rec get_transactions_from_day day lst acc = 
  match lst with
  | [] -> acc
  | h::t -> if h.date = day 
    then get_transactions_from_day day t (h::acc)
    else get_transactions_from_day day t acc

let rec contains_word word_list str = 
  match word_list with 
  | [] -> false
  | h::t -> 
    if ((String.lowercase_ascii h) = (String.lowercase_ascii str)) then true
    else contains_word t str

let rec lookup_by_description word lst acc = 
  match lst with 
  | [] -> acc 
  | h::t -> 
    let word_list = String.split_on_char ' ' h.description in 
    if contains_word word_list word 
    then lookup_by_description word t (h::acc)
    else lookup_by_description word t acc

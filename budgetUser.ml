open Budget
open Transaction
open Command
open State


let clear 0 = ANSITerminal.erase Screen

let space 0 = print_string "\n\n\n\n\n\n\n\n\n\n"

let enter 0 = 
  ANSITerminal.(print_string [cyan; Bold] "Enter to continue.");
  match read_line () with 
  | _-> clear 0

let rec print_tics number total color = 
  (if number > 0 
   then (if color = "red" then ANSITerminal.(print_string [red] "|")
         else if color = "yellow" then ANSITerminal.(print_string [yellow] "|")
         else if color = "green" then ANSITerminal.(print_string [green] "|")
         else print_char '|';
         print_tics (number-1) (total-1) color)
   else if total > 0 
   then (print_string " "; print_tics number (total-1) color)
   else print_string " ") 

let rec make_status_graph total list =
  match list with 
  | []-> print_string "\n"
  | (a,b,c)::t -> 
    let num_tics = int_of_float (b/.c*.100.) in 
    let get_color num_tics = 
      if num_tics < 20 then "red" 
      else if num_tics > 80 then "green" 
      else "yellow" in 
    ANSITerminal.(print_string [cyan; Bold] a); 
    ANSITerminal.(print_string [cyan; Bold] " -> $"); 
    ANSITerminal.(print_string [cyan; Bold] (string_of_float b));
    print_string "\n [ ";
    print_tics num_tics 100 (get_color num_tics); print_string "] \n \n"; 
    make_status_graph total t

let rec get_daily_spending transactions day acc = 
  match transactions with 
  | [] -> acc
  | h::t -> 
    if h.date = day then get_daily_spending t day (h.amount +. acc)
    else get_daily_spending t day acc

let print_status st transactions = 
  let categories = get_categories st in
  let balance = get_balance st in
  ANSITerminal.(print_string [cyan; Bold]"\n\nCurrent Budget: ");
  ANSITerminal.(print_string [cyan; Bold] (get_name st));
  ANSITerminal.(print_string [cyan; Bold]"\n\nYou have $");
  ANSITerminal.(print_string [cyan; Bold] (string_of_float balance)); 
  ANSITerminal.(print_string [cyan; Bold] 
                  " left in your budget. You started with $");
  ANSITerminal.(print_string [cyan; Bold] 
                  (string_of_float (get_init_balance st))); 
  print_string "\n \n";
  make_status_graph balance categories;
  enter 0

let rec print_categories list = 
  match list with 
  | [] -> print_string ""
  | (a,b,c)::t -> ANSITerminal.(print_string [cyan; Bold] "Category: "); 
    ANSITerminal.(print_string [cyan; Bold] a); 
    ANSITerminal.(print_string [cyan; Bold] " | Current Balance: $"); 
    ANSITerminal.(print_string [cyan; Bold] (string_of_float b)); 
    ANSITerminal.(print_string [cyan; Bold] " | Overall Budget: $"); 
    ANSITerminal.(print_string [cyan; Bold] (string_of_float c)); 
    print_string "\n"; print_categories t

let rec which_category  amount st = 
  clear 0;
  if amount > 0.0 then 
    ANSITerminal.(print_string [cyan; Bold] 
                    ("\nWhich category would you like to add to?\n"))
  else ANSITerminal.(print_string [cyan; Bold] 
                       ("\nWhich category would you like to remove from?\n"));
  print_categories (get_categories st); space 0;
  print_string "> ";
  match read_line () with 
  | input -> 
    (if valid_category input (get_categories st) 
     then add_to_category amount input st else
       (clear 0; ANSITerminal.(print_string [red; Bold] 
                                 "Error: enter a valid category. \n"); 
        space 0; enter 0;
        let new_state = add_categories input st 
        in add_to_category amount input new_state ) )

let rec use_helper_day amount = 
  clear 0;
  ANSITerminal.(print_string [cyan; Bold] ("\nWhat day of the week is it?\n"));
  space 0;
  print_string "> ";
  match read_line () with 
  | input -> (match read_day input with 
      | exception NotValidDate -> 
        clear 0;
        ANSITerminal.(print_string [red; Bold] 
                        ("\nError: enter a valid day of the week.")); space 0; 
        enter 0;
        use_helper_day amount
      | a -> a
    )

let rec use_helper_category st = 
  clear 0;
  ANSITerminal.(print_string [cyan; Bold] 
                  ("\nWhich category would you like to use from?\n"));
  print_categories (get_categories st);
  space 0;
  print_string "> ";
  match read_line () with 
  | input -> if valid_category input (get_categories st) then input 
    else (ANSITerminal.(print_string [cyan; Bold] "\nNot a valid category.\n"); 
          use_helper_category st)

let use_from_cat amount category st =
  use_from_category amount category st 

let use_helper_description amount = 
  clear 0;
  ANSITerminal.(print_string [cyan; Bold] 
                  ("\nGive a brief description of the transaction.\n"));
  space 0;
  print_string "> ";
  match read_line () with 
  | input -> input 

let rec print_transactions lst = 
  match lst with 
  |[] -> space 0; enter 0
  |h::t -> ANSITerminal.(print_string [magenta; Bold] "\nDay: "); 
    ANSITerminal.(print_string [magenta; Bold] (get_date h)); 
    ANSITerminal.(print_string [magenta; Bold] " | Category: "); 
    ANSITerminal.(print_string [magenta; Bold] (get_category h));
    ANSITerminal.(print_string [magenta; Bold] " | Amount: $"); 
    ANSITerminal.(print_string [magenta; Bold] 
                    (string_of_float (get_amount h)));
    ANSITerminal.(print_string [magenta; Bold] "\nDescription: "); 
    ANSITerminal.(print_string [magenta; Bold] (get_description h)); 
    ANSITerminal.(print_string [magenta; Bold] 
                    "\n----------------------------------------\n");
    print_transactions t


let rec set_up_categories budget = 
  print_string "+ ";
  match read_line () with 
  | input -> if input = "done" then budget else 
      let new_budget = add_categories input budget in 
      set_up_categories new_budget


let rec divide_initial_balance balance list budget = 
  if balance > 0.0 then
    match list with 
    | [] ->  divide_initial_balance balance (get_categories budget) budget
    | (a,b,c)::t -> clear 0; 
      ANSITerminal.(print_string 
                      [cyan; Bold] "\nHow much would you like to add to ");
      ANSITerminal.(print_string [cyan; Bold] a); 
      ANSITerminal.(print_string [cyan; Bold] "? \nYou have this much left: $"); 
      ANSITerminal.(print_string [cyan; Bold] (string_of_float balance)); 
      space 0;
      print_string "\n$ ";
      match read_line () with 
      | input -> let amount = float_of_string input in 
        if amount <= balance 
        then divide_initial_balance (balance -. amount) t 
            (add_to_category amount a budget) 
        else (ANSITerminal.(print_string [cyan; Bold] 
                              "\nYou do not have sufficient funds.\n"); 
              divide_initial_balance balance list budget)
  else budget

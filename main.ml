open UserState
open State
open Transaction
open Command
open BudgetUser
open Budget


let rec update_users new_user_state users_list acc = 
  let username = get_username new_user_state in 
  match users_list with 
  | [] -> acc
  | hd::tl -> 
    if (get_username hd) = username then (new_user_state::tl)@acc
    else update_users new_user_state tl (hd::acc)


let update_state_users state new_user_state = 
  let new_state = 
    set_users state (update_users new_user_state (get_users state) []) in 
  (set_active_user new_state new_user_state)


let rec update_budgets new_budget budget_list acc = 
  let name = get_name new_budget in 
  match budget_list with 
  | [] -> acc
  | h::t -> 
    if (get_name h) = name then (new_budget::t)@acc
    else update_budgets new_budget t (h::acc)


let update_state_budgets state new_budget = 
  set_budgets state (update_budgets new_budget (get_budgets state) [])


let rec setup_user st = 
  ANSITerminal.(print_string [cyan; Bold]
                  "\n\nPlease create a username: \n");
  space 0; 
  print_string "> ";
  match read_line () with 
  | input -> 
    let rec check username list = match list with
      |[] -> true
      |h::t -> if get_username h = username then false else check username t in
    if check input (get_users st) 
    then (let username = input in
          clear 0;
          ANSITerminal.(print_string [cyan; Bold]
                          "\nNow please create a password: \n");
          space 0;
          print_string "> ";
          match read_line () with 
          | input -> let password = input in
            let user = UserState.init_user_state username password in
            let st_user = add_user user st in 
            let updated_state = set_active_user st_user user in
            updated_state)
    else 
      (clear 0; 
       ANSITerminal.(print_string 
                       [red; Bold] "Error: username already taken \n");
       space 0; enter 0; setup_user st)


let start_budget state = 
  clear 0;
  let active_user_st = get_active_user state in 
  ANSITerminal.(print_string [cyan; Bold] 
                  "\nWhat would you like to name this budget?\n");
  space 0; print_string "> ";
  match read_line () with 
  | input -> let name = input in
    clear 0;
    ANSITerminal.(print_string [cyan; Bold] 
                    "\nPlease insert the amount you want in your budget?\n");
    space 0;
    print_string "$ ";
    match read_line () with 
    | input -> let balance = float_of_string input in
      let budget = init_budget name (get_username active_user_st) balance in
      clear 0;
      ANSITerminal.(print_string [cyan; Bold] 
                      "\nWhat categories would you like to have? Type \"done\" when you are finished.\n");
      space 0;
      let budget2 = set_up_categories budget in
      let budget3 = divide_initial_balance balance 
          (get_categories budget2) budget2 in
      let new_user = UserState.add_budget name active_user_st in 
      let new_user2 = set_active_budget name new_user in 
      clear 0;
      ANSITerminal.(print_string [cyan; Bold] "\nYou are all setup! \n");
      space 0;
      enter 0;
      (update_state_users (add_budget budget3 state) new_user2)


let rec put_password user = 
  match read_line () with
  | input -> if check_password input user then true 
    else (print_string "Incorrect Password, please try again \n >"; 
          put_password user)


let rec switch_user state = 
  clear 0;
  ANSITerminal.(print_string [cyan; Bold] 
                  "\nPlease enter your username or enter create account if you do not have an account. \n");
  space 0; print_string "> ";
  match read_line () with 
  | input -> (match input with 
      | "create account" -> clear 0;
        let new_state = setup_user state in
        clear 0;
        start_budget new_state
      | username -> 
        if (check_valid_username username (get_users state)) 
        then let user = get_user_from_username username (get_users state)in
          clear 0;
          ANSITerminal.(print_string [cyan; Bold] "Enter Password: \n");
          space 0;
          print_string "> ";
          if put_password user then (set_active_user state user) 
          else switch_user state
        else (ANSITerminal.
                (print_string [red; Bold] 
                   "Error: not a valid username. Please try logging in again. \n");
              space 0; enter 0;
              switch_user state))


let rec print_budgets budgets = 
  match budgets with 
  | [] -> ""
  | h::t -> ANSITerminal.(print_string [cyan; Bold] h); 
    ANSITerminal.(print_string [cyan; Bold] "/"); print_budgets t


let rec switch_budget state = 
  ANSITerminal.(print_string [cyan; Bold] "\nHere are your current budgets: ");
  print_budgets (UserState.get_budgets (get_active_user state));
  ANSITerminal.(print_string [cyan; Bold] 
                  "\nPlease enter what budget you would like to switch into. \n");
  space 0; print_string "> ";
  match read_line () with 
  | input -> if check_valid_budget_name input (get_budgets state) then 
      if ( let active_user = get_active_user state in
           let budget_list = UserState.get_budgets active_user in 
           let rec check_valid list name = 
             match list with 
             | [] -> false
             | h::t -> if h = name then true else check_valid t name in
           check_valid budget_list input) 
      then ( clear 0;
             ANSITerminal.(print_string [green; Bold] 
                             "\nYou have successfully switched into budget "); 
             ANSITerminal.(print_string [green; Bold] input); 
             space 0; enter 0;
             update_state_users state (set_active_budget input (get_active_user state))) 
      else (clear 0;
            ANSITerminal.(print_string [red; Bold] 
                            "\nError: you do not have access to this budget.\n"); 
            space 0; enter 0;
            switch_budget state)
    else (clear 0;
          ANSITerminal.(print_string [red; Bold] "\nError: not a valid budget name.\n"); 
          space 0; enter 0;switch_budget state)


let rec share_budget state = 
  clear 0;
  ANSITerminal.(print_string 
                  [cyan; Bold] "\nWho would you like to share with? \n");
  ANSITerminal.(print_string 
                  [cyan; Bold] "Enter \"quit\" if you changed your mind.\n");
  space 0;
  print_string "> ";
  match read_line () with 
  | input -> if input = "quit" then (clear 0; state) else (
      if check_valid_username input (get_users state) then 
        let user = get_user_from_username input (get_users state) in 
        let active_budget = get_active_budget (get_active_user state) in 
        let new_user_state = UserState.add_budget active_budget user in
        let budget = get_budget_from_name active_budget (get_budgets state) in 
        let new_budget_state = add_member (get_username user) budget in 
        let new_state = update_state_users 
            (update_state_budgets state new_budget_state) new_user_state in
        ANSITerminal.(print_string 
                        [green; Bold] 
                        "You have successfully shared your budget with \n");
        ANSITerminal.(print_string [cyan; Bold] input); space 0; enter 0;
        update_state_users new_state (get_active_user state);
      else (ANSITerminal.(print_string 
                            [red; Bold] "Error: user does not exist. \n"); 
            space 0; enter 0;share_budget state))


let rec continue_budget state =
  let active_usr_st = get_active_user state in 
  let budget = get_budget_from_name 
      (get_active_budget active_usr_st) (get_budgets state) in 
  let transactions = Budget.get_transactions budget in 
  ANSITerminal.(print_string [cyan; Bold] "\nYou are currently in budget \"");
  ANSITerminal.(print_string [cyan; Bold] (get_name budget));
  ANSITerminal.(print_string [cyan; Bold] "\".\n");
  ANSITerminal.(print_string [cyan; Bold] 
                  ("\n\nWhat would you like to do next? \n"));
  ANSITerminal.(print_string [cyan; Bold] 
                  ("You may enter \"help\" to see a list of options.\n"));
  space 0; 
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | input -> 
    match parse input with 
    | Balance -> clear 0; ANSITerminal.(print_string 
                                          [yellow; Bold] "Your balance is $");
      ANSITerminal.(print_string [yellow; Bold]  
                      (string_of_float (get_balance budget)));
      print_string ("\n"); space 0; enter 0;
      continue_budget state
    | Add t -> 
      if t >= 0.0 then (
        let new_budget = add t (which_category t budget) in 
        clear 0;
        ANSITerminal.(print_string [green; Bold] "\nSuccess!\n"); space 0; enter 0;
        continue_budget (update_state_budgets state new_budget))
      else  ANSITerminal.(print_string [red; Bold]  
                            "Error: you must add a positive integer. \n"); 
      enter 0;
      continue_budget state
    | Use t -> 
      if t > 0.0 
      then (let category = use_helper_category budget in
            let trans = create_transaction (use_helper_day t) 
                (use_helper_description t) (t) (category) in 
            let new_budget = use_from_cat t category budget in 
            clear 0;
            ANSITerminal.(print_string [green; Bold] "\nSuccess!\n"); 
            space 0; enter 0;
            let new_budget_state = Budget.add_transaction 
                (remove t new_budget) trans in 
            continue_budget (update_state_budgets state new_budget_state ))
      else print_string "You must add a positive integer. \n"; 
      continue_budget state
    | Move t -> if t > 0.0 
      then (let removebudget = which_category  (-.t) budget in
            let addbudget = which_category  (t) removebudget in 
            ANSITerminal.(print_string [cyan; Bold] "\nSuccess!"); 
            continue_budget (update_state_budgets state addbudget))
      else clear 0; ANSITerminal.
                      (print_string [red; Bold] 
                         "Error: you must add a positive integer. \n");
      space 0; enter 0; continue_budget state
    | Remove t -> let new_state = which_category (-.t) budget in clear 0;
      ANSITerminal.(print_string [green; Bold] "\nSuccess!\n"); space 0; enter 0;
      continue_budget (update_state_budgets state (add (-.t) new_state) )
    | CreateCategory t -> clear 0; 
      continue_budget (update_state_budgets state (add_categories t budget) )
    | CreateBudget -> continue_budget (start_budget state)
    | Switch -> clear 0; continue_budget (switch_budget state)
    | Share -> clear 0; continue_budget (share_budget state)
    | Status -> clear 0; 
      print_status budget transactions; continue_budget state
    | GeneralHist -> clear 0; print_transactions transactions; 
      continue_budget state
    | History t -> clear 0;
      (try (
         (try (let trans = get_transactions_from_day (read_day t) 
                   transactions [] in 
               ANSITerminal.(print_string [cyan; Bold] "Total Spending on "); 
               ANSITerminal.(print_string [cyan; Bold] t); 
               ANSITerminal.(print_string [cyan; Bold] ": $"); 
               ANSITerminal.
                 (print_string [cyan; Bold] 
                    (string_of_float (get_daily_spending transactions 
                                        (read_day t) 0.0)));  
               print_string "\n"; print_transactions trans;
               continue_budget state) 
          with _ -> 
            (let trans = get_transactions_from_category t transactions [] in 
             print_transactions trans; continue_budget state )) )
       with _ -> clear 0; 
         ANSITerminal.(print_string [red; Bold] 
                         "Error: not a valid category or date. \n");
         space 0; enter 0; continue_budget state)
    | Lookup t -> clear 0;
      let trans = lookup_by_description t transactions [] in 
      print_transactions trans; continue_budget state
    | Reset -> clear 0; 
      ANSITerminal.(print_string 
                      [green; Bold] 
                      "\nYou have successfully reset your budget.\n");
      space 0; enter 0;
      continue_budget (update_state_budgets state (reset_budget budget))
    | Logout -> clear 0; 
      ANSITerminal.(print_string [cyan; Bold] 
                      "You have successfully logged out! \n"); 
      space 0; enter 0;
      let new_state = switch_user state in
      clear 0; enter 0;
      continue_budget new_state
    | Help -> clear 0;
      print_string ("\nEnter \"balance\" to check your current balance. \n");
      print_string ("Enter \"add/remove $__\" to increase/decrease money in your budget. \n");
      print_string ("Enter \"use $__\" to spend money. \n");
      print_string ("Enter \"move $__\" to move money between categories. \n");
      print_string ("Enter \"create category ____\" to create a new category. \n");
      print_string ("Enter \"create budget\" to create a new budget. \n");
      print_string ("Enter \"switch\" to switch into another budget. \n");
      print_string ("Enter \"share\" to share your budget with another user. \n");
      print_string ("Enter \"status\" to check the status of your budget. \n");
      print_string ("Enter \"history\" to check your transactions history. \n");
      print_string ("Enter \"history ____\" to check the transaction history of a specific category. \n");
      print_string ("Enter \"lookup ____\"  to search your transactions. \n");
      print_string ("Enter \"logout\" to share your budget with another user. \n");
      print_string ("Enter \"reset\" to reset your budget to its initial values. \n\n\n");
      enter 0;
      continue_budget state
    | Quit -> exit 0
    | exception Malformed -> clear 0; 
      ANSITerminal.(print_string [red; Bold]  "Error: not a valid command."); 
      space 0; enter 0; continue_budget state
    | exception Empty -> continue_budget state


let log_in st = 
  start_budget st

let main () =
  clear 0;
  ANSITerminal.(print_string [cyan; Bold]
                  "\n\nWelcome to your Budget Planner! \n");
  let setup_st = setup_user init_state in 
  (*start_budget (get_active_user setup_st)*) 
  let login_st = log_in setup_st in 
  continue_budget login_st

(* Execute the game engine. *)
let () = main ()

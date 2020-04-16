open OUnit2
open Command
open Transaction
open Budget
open UserState
open State

(* Command Tests *)
let command_tests = [
  "balance" >:: (fun _ -> assert_equal (parse "balance") (Balance));
  "add" >:: (fun _ -> assert_equal (parse "add 1") (Add (1.)));
  "remove" >:: (fun _ -> assert_equal (parse "remove 1") (Remove 1.));
  "use" >:: (fun _ -> assert_equal (parse "use 100") (Use 100.));
  "move" >:: (fun _ -> assert_equal (parse "move 1") (Move 1.));
  "quit" >:: (fun _ -> assert_equal (parse "quit") (Quit));
  "createcategory" >:: (fun _ -> assert_equal (parse "create category house") (CreateCategory "house"));
  "createbudget" >:: (fun _ -> assert_equal (parse "create budget") (CreateBudget));
  "status" >:: (fun _ -> assert_equal (parse "status") (Status));
  "genhistory" >:: (fun _ -> assert_equal (parse "history") (GeneralHist));
  "share" >:: (fun _ -> assert_equal (parse "share") (Share));
  "help" >:: (fun _ -> assert_equal (parse "help") (Help));
  "historyofstring" >:: (fun _ -> assert_equal (parse "history food") (History "food"));
  "lookup" >:: (fun _ -> assert_equal (parse "lookup cat") (Lookup "cat"));
  "switch" >:: (fun _ -> assert_equal (parse "switch") (Switch));
  "reset" >:: (fun _ -> assert_equal (parse "reset") (Reset));
  "logout" >:: (fun _ -> assert_equal (parse "logout") (Logout));
]

(* Transaction Tests *)
let transaction = 
  {
    date = 1;
    description = "ice cream";
    amount = 5.;
    category = "food"
  }

let transaction2 =
  {
    date = 3; 
    description = "gas";
    amount = 50.;
    category = "car"
  }

let tlist = [transaction]

let transaction_tests = [
  (* Test Get Functions *)
  "get description" >:: (fun _ -> assert_equal (get_description transaction) "ice cream");
  "get amount" >:: (fun _ -> assert_equal (get_amount transaction) 5.);
  "get category" >:: (fun _ -> assert_equal (get_category transaction) "food");
  "get date" >:: (fun _ -> assert_equal (get_date transaction) "Monday");
  "get from existing category" >:: (fun _ -> assert_equal (get_transactions_from_category "food" tlist []) [transaction]);
  "get from non-existing category" >:: (fun _ -> assert_equal (get_transactions_from_category "car" tlist []) []);
  "get from existing day" >:: (fun _ -> assert_equal (get_transactions_from_day 1 tlist []) [transaction]);
  "get from non-existing day" >:: (fun _ -> assert_equal (get_transactions_from_day 6 tlist []) []);

  (* Test Create Function *)
  "test create transaction" >:: (fun _ -> assert_equal (create_transaction 1 "ice cream" 5. "food") transaction);

  (* Test Add Function *)
  "test add transaction" >:: (fun _ -> assert_equal (Transaction.add_transaction transaction2 tlist) [transaction2;transaction;]);

  (* Test Read Function *)
  "test read_day" >:: (fun _ -> assert_equal (read_day "SuNdAy") 7);

  (* Test Contains Function *)
  "test contains_word true" >:: (fun _ -> assert_equal (contains_word ["i";"am";"hungry"] "hungry") true);
  "test contains_word false" >:: (fun _ -> assert_equal (contains_word ["i";"am";"hungry"] "ham") false);

  (* Test Lookup Function *)
  "test lookup present" >:: (fun _ -> assert_equal (lookup_by_description "ice" tlist []) [transaction]);
  "test lookup not present" >:: (fun _ -> assert_equal (lookup_by_description "icee" tlist []) []);
]

(* Budget Tests *)
let init_budget = 
  {
    curr_balance = 10.;
    init_balance = 10.;
    categories = [];
    transactions = [];
    members = ["joe"];
    name = "personal"
  }

let budget = 
  {
    curr_balance = 100.;
    init_balance = 100.;
    categories = [("food", 100.,100.)];
    transactions = [];
    members = ["bob"];
    name = "bob"
  }

let budget2 = 
  {
    budget with transactions = [transaction];
  }

let budget3 = 
  {
    budget2 with members = ["sarah";"bob"];
  }

let budget4 = 
  {
    budget3 with categories = [("food", 110.,110.)];
  }

let budget5 = 
  { 
    budget4 with curr_balance = 110.; init_balance = 110.;
  } 

let budget6 = 
  {
    budget5 with categories = [("clothes", 0.,0.);("food", 110.,110.)];
  }

let budget7 = 
  {
    budget6 with categories = [("food", 100.,110.);("clothes", 0.,0.)];
  }

let budget_tests = [
  (* Test Init Function *)
  "init budget" >:: (fun _ -> assert_equal (Budget.init_budget "personal" "joe" 10.) init_budget);

  (* Test Get Functions *)
  "get balance" >:: (fun _ -> assert_equal (get_balance budget) 100.);
  "get init balance" >:: (fun _ -> assert_equal (get_init_balance budget) 100.);
  "get name" >:: (fun _ -> assert_equal (get_name budget) "bob");
  "get categories" >:: (fun _ -> assert_equal (get_categories budget) [("food", 100.,100.)]);
  "get transactions" >:: (fun _ -> assert_equal (Budget.get_transactions budget ) []);
  "get members" >:: (fun _ -> assert_equal (get_members budget) ["bob"]);
  "get budget from name" >:: (fun _ -> assert_equal (get_budget_from_name "bob" [budget]) budget);

  (* Test Add Functions *)
  "test add transaction" >:: (fun _ -> assert_equal (Budget.add_transaction budget transaction) budget2);
  "test add member" >:: (fun _ -> assert_equal (add_member "sarah" budget2) budget3);
  "test add to category" >:: (fun _ -> assert_equal (add_to_category 10. "food" budget3) budget4);
  "test add to budget" >:: (fun _ -> assert_equal (add 10. budget4) budget5);
  "test add category" >:: (fun _ -> assert_equal (add_categories "clothes" budget5) budget6);

  (* Test Use Functions *)
  "test use" >:: (fun _ -> assert_equal (use_from_category 10. "food" budget6) budget7);

  (* Test Remove Function *)
  "test remove" >:: (fun _ -> assert_equal (remove 10. budget5) {budget4 with init_balance = 110.});

  (* Test Valid Function *)
  "test valid category" >:: (fun _ -> assert_equal (valid_category "food" [("food",0.,0.)]) true);
  "test not valid category" >:: (fun _ -> assert_equal (valid_category "car" [("food",0.,0.)]) false);

  (* Test Reset Function *)
  "test reset" >:: (fun _ -> assert_equal (reset_budget budget2) budget);
]

(* UserState Tests *)
let init_user = 
  {
    transactions = [];
    username = "a";
    password = "123";
    budgets = [];
    active_budget = ""
  }

let user =
  {
    transactions = [transaction];
    username = "cindy";
    password = "cindy";
    budgets = ["budget";"budget2"];
    active_budget = "budget"
  }

let user2 = {
  user with username = "dan"
}

let user_state_tests = [
  (* Test Init Function *)
  "init user" >:: (fun _ -> assert_equal (init_user_state "a" "123") init_user);

  (* Test Get Functions *)
  "get transactions" >:: (fun _ -> assert_equal (get_transactions user) [transaction]);
  "get username" >:: (fun _ -> assert_equal (get_username user) "cindy");
  "get budgets" >:: (fun _ -> assert_equal (UserState.get_budgets user) ["budget";"budget2"]);
  "get active budget" >:: (fun _ -> assert_equal (get_active_budget user) "budget");
  "get user from name" >:: (fun _ -> assert_equal (get_user_from_username "cindy" [user]) user);

  (* Test Add Functions *)
  "test add transaction" >:: (fun _ -> assert_equal (add_transaction user transaction) {user with transactions = [transaction;transaction]});
  "test add budget" >:: (fun _ -> assert_equal (UserState.add_budget "cat" user) {user with budgets = ["cat";"budget";"budget2"]});

  (* Test Check Function *)
  "test check password true" >:: (fun _ -> assert_equal (check_password "cindy" user) true);
  "test check password false" >:: (fun _ -> assert_equal (check_password "cat" user) false);

  (* Test Remove Function *)
  "test remove" >:: (fun _ -> assert_equal (remove_budget "budget" ["budget";"b"] []) ["b"]);

  (* Test Set Function *)
  "test set active budget" >:: (fun _ -> assert_equal (set_active_budget "budget2" user) {user with active_budget = "budget2"});
]

(* State Tests *)
let state = {
  users = [user;user2];
  budgets = [budget;budget2];
  active_user = user;
}

let state_tests = [
  (* Test Get Functions *)
  "get user" >:: (fun _ -> assert_equal (get_users state) [user;user2]);
  "get active user" >:: (fun _ -> assert_equal (get_active_user state) user);
  "get budgets" >:: (fun _ -> assert_equal (get_budgets state) [budget;budget2]);

  (* Test Set Functions *)
  "test set active user" >:: (fun _ -> assert_equal (set_active_user state user2) {state with active_user = user2});
  "test set users" >:: (fun _ -> assert_equal (set_users state [user]) {state with users = [user]});
  "test set budgets" >:: (fun _ -> assert_equal (set_budgets state [budget3;budget4]) {state with budgets = [budget3;budget4]});

  (* Test Add Functions *)
  "test add user" >:: (fun _ -> assert_equal (add_user user state) {state with users = [user;user;user2]});
  "test add budget" >:: (fun _ -> assert_equal (add_budget budget7 state) {state with budgets = [budget7;budget;budget2]});

  (* Test Check Functions *)
  "test check username true" >:: (fun _ -> assert_equal (check_valid_username "cindy" [user;user2]) true);
  "test check username false" >:: (fun _ -> assert_equal (check_valid_username "c" [user;user2]) false);
  "test check budget true" >:: (fun _ -> assert_equal (check_valid_budget_name "bob" [budget;budget4]) true);
  "test check budget false" >:: (fun _ -> assert_equal (check_valid_budget_name "c" [budget;budget4]) false);
]

let suite = "budget test suite" >::: List.flatten [
    command_tests;
    transaction_tests;
    budget_tests;
    user_state_tests;
    state_tests
  ]

let _ = run_test_tt_main suite

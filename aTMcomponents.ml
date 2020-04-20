

type id = int

(* Possible actions that an ATM customer can perform *)
type action =
  | Balance           (* balance inquiry *)
  | Withdraw of int   (* withdraw an amount *)
  | Deposit of int    (* deposit an amount *)
  | Next              (* finish this customer and move on to the next one *)
  | Finished          (* shut down the ATM and exit entirely *)
;;

(*....................................................................
 Initializing database of accounts
*)

(* A specification of a customer name and initial balance for
   initializing the account database *)
type account_spec = {name : string; id : id; balance : int} ;;

let database = ref []

let current_id = ref 0 ;;

let initialize (lst: account_spec list): unit =
  database := lst ;;

let rec acquire_id () : id =
  Printf.printf "Enter customer id: ";
  try 
    let id = int_of_string (read_line ()) in
    let found = List.hd (List.filter (fun x -> x.id = id) !database) in 
    current_id := id;
    found.id
  with 
  | Failure _ -> Printf.printf "Invalid id \n";
                  acquire_id ();;


let rec acquire_amount () : int =
  Printf.printf "Enter amount: ";
  try 
    let amt = int_of_string (read_line ()) in
    if amt <= 0 then raise (Failure "amount is non-positive");
    amt
  with
  | Failure _ -> Printf.printf "Invalid amount\n";
                 acquire_amount ();;

let rec acquire_act () : action =
  Printf.printf "Enter action: (B) Balance (-) Withdraw (+) Deposit (=) Done (X) Exit: %!";
  let str = (read_line ()) in
  match str with
  | "B" -> Balance
  | "-" -> Withdraw (acquire_amount ())
  | "+" -> Deposit (acquire_amount ())
  | "=" -> Next
  | "x" | "X" -> Finished
  | "" | _ -> Printf.printf " invalid choice\n";
              acquire_act ();;
(*raise (Invalid_argument "invalid action") ;;*)

let get_balance (account: id) : int =
  let account_lst = !database in
  let rec get_balance_rec (account: id) (lst: account_spec list) : int =
    match lst with
    | [] -> raise (Invalid_argument "account does not exist")
    | hd :: tl -> if hd.id = account then hd.balance else get_balance_rec account tl in
  get_balance_rec account account_lst ;;

let get_name (account:id) : string =
  let account_lst = !database in
  let rec get_name_rec (account: id) (lst: account_spec list) : string =
    match lst with
    | [] -> raise (Invalid_argument "account does not exist")
    | hd :: tl -> if hd.id = account then hd.name else get_name_rec account tl in
  get_name_rec account account_lst ;;

let update_balance (account:id) (new_bal: int) : unit =
  let account_lst = !database in
  let curr, rest = List.partition (fun x -> x.id = account) account_lst in
  let curr_acc = List.hd curr in
  database := ({name = curr_acc.name; id = curr_acc.id; balance = new_bal} :: rest)  ;;

let present_message (msg: string) : unit =
  print_endline msg ;; 

let deliver_cash (c: int) : unit =
  Printf.printf "Here's your cash: ";
  for _i = 1 to (c / 20) do 
    Printf.printf "[20 @ 20]"
  done;
  Printf.printf " and %d more\n" (c mod 20) ;;

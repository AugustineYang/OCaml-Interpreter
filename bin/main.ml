open Interpreterlib
open Ast

let rec string_of_expr (e : expr) : string = 
  match e with
  | Int n -> Printf.sprintf "Int %d" n
  | Var id -> Printf.sprintf "Var %s" id
  | Bool b -> 
    let b_str = 
      match b with 
      | true -> "true"
      | false -> "false"
    in
    Printf.sprintf "Bool %s" b_str
  | Binop (binop, e1, e2) ->
    let binop_str = 
      match binop with 
      | Add -> "Add"
      | Mul -> "Mul"
      | Sub -> "Sub"
      | Div -> "Div"
      | Leq -> "Leq"
    in
    Printf.sprintf "Binop (%s, %s, %s)" binop_str (string_of_expr e1) (string_of_expr e2)
  | Let (var, e1, e2) -> Printf.sprintf "Let (%s, %s, %s)" var (string_of_expr e1) (string_of_expr e2)
  | If (e1, e2, e3) -> Printf.sprintf "If (%s, %s, %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | Func (var, e) -> Printf.sprintf "Func (%s, %s)" var (string_of_expr e)
  | App (e1, e2) -> Printf.sprintf "App (%s, %s)" (string_of_expr e1) (string_of_expr e2)


let parse s : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast


(* check if an expression is a value (i.e., fully evaluated) *)
let is_value : expr -> bool = function
  | Int _ | Bool _ | Func _ -> true
  | Var _ | Binop _ | If _ | Let _ | App _ -> false

module VarSet = Set.Make(String)
let singleton = VarSet.singleton
let union = VarSet.union
let diff = VarSet.diff
let mem = VarSet.mem

(** [fv e] is a set-like list of the free variables of [e]. *)
let rec fv : expr -> VarSet.t = function
  | Var x -> singleton x
  | App (e1, e2) -> union (fv e1) (fv e2)
  | Func (x, e) -> diff (fv e) (singleton x)

(** [gensym ()] is a fresh variable name. *)
let gensym =
  let counter = ref 0 in
  fun () ->
    incr counter; "$x" ^ string_of_int !counter

(** [replace e y x] is [e] with the name [x] replaced
    by the name [y] anywhere it occurs. *)
let rec replace e y x = match e with
  | Var z -> if z = x then Var y else e
  | App (e1, e2) -> App (replace e1 y x, replace e2 y x)
  | Func (z, e') -> Func ((if z = x then y else z), replace e' y x)

(** [subst e v x] is [e] with [v] substituted for [x], that
    is, [e{v/x}]. *)
let rec subst e v x = match e with
  | Var y -> if x = y then v else e
  | Bool _ -> e
  | Int _ -> e
  | Binop (binop, e1, e2) -> Binop (binop, subst e1 v x, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    if x = y
    then Let (y, e1', e2)
    else Let (y, e1', subst e2 v x)
  | App (e1, e2) -> App (subst e1 v x, subst e2 v x)
  | Func (y, e') -> 
    if x = y then e 
    else if not (mem y (fv v)) then Func (y, subst e' v x)
    else 
      let fresh = gensym () in
      let new_body = replace e' y fresh in
      Func (fresh, subst new_body v x)


(* takes a single step of evaluation of [e] *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step on a number"

  | Var _ -> failwith "Unbound variable"

  (* No need for further stepping if both sides are already values *)
  | Binop (binop, e1, e2) when is_value e1 && is_value e2 -> 
    step_binop binop e1 e2

  (* Evaluate the right side of the binop if the left side is a value *)
  | Binop (binop, e1, e2) when is_value e1 -> Binop (binop, e1, step e2)

  (* Leftmost step for binop *)
  | Binop (binop, e1, e2) -> Binop (binop, step e1, e2)

  | If (Bool true, e2, _) -> e2
  
  | If (Bool false, _, e3) -> e3

  | If (Int _, _, _) -> failwith "Condition must be a boolean"

  | If (e1, e2, e3) -> If (step e1, e2, e3)

  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x

  | Let (x, e1, e2) -> Let (x, step e1, e2)


(* implement the primitive operation [v1 binop v2].
   Requires: [v1] and [v2] are both values. *)
and step_binop binop v1 v2 = match binop, v1, v2 with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b when b <> 0 -> Int (a / b)
  | Div, Int _, Int 0 -> failwith "Division by zero"
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"


(* fully evaluate [e] to a value [v] *)
let rec eval (e : expr) : expr =
  if is_value e then e else
    e |> step |> eval


(* interpret [s] by lexing -> parsing -> evaluating and converting the result to a string *)
let interp (s : string) : string = 
  s |> parse |> eval |> string_of_expr


(* CBV stands for call-by-value, and CBN stands for call-by-name.
   If strategy is set to be CBV, the call-by-value strategy is adopted. 
   And vice versa. 

   Warning 37 should be disabled for this statement.*)
type eval_strategy = CBV | CBN
let strategy = CBV

let rec eval_big (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith "Unbound variable"
  | Binop (binop, e1, e2) -> eval_bop binop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | If (e1, e2, e3) -> eval_if e1 e2 e3
  | App (e1, e2) -> eval_app e1 e2
  | Func _ -> e

and eval_bop binop e1 e2 = match binop, eval_big e1, eval_big e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Sub, Int a, Int b -> Int (a - b)
  | Mul, Int a, Int b -> Int (a * b)
  | Div, Int a, Int b when b <> 0 -> Int (a / b)
  | Div, Int _, Int 0 -> failwith "Division by zero"
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "Operator and operand type mismatch"

and eval_if e1 e2 e3 = match eval_big e1 with
  | Bool true -> eval_big e2
  | Bool false -> eval_big e3
  | _ -> failwith "Condition must be a boolean"


(** [eval_app e1 e2] is the [e] such that [e1 e2 ==> e]. *)
and eval_app e1 e2 = match eval_big e1 with
  | Func (x, e) -> 
    let e2' = 
      match strategy with
      | CBV -> eval_big e2
      | CBN -> e2
    in subst e e2' x |> eval_big
  | _ -> failwith "Cannot apply non-function"

let interp_big (s : string) : string = 
  s |> parse |> eval_big |> string_of_expr
  
let () =
  (* let filename = "test/lambda_test1.in" in *)
  (* let filename = "test/lambda_test2.in" in *)
  (* let filename = "test/lambda_test3.in" in *)
  (* let filename = "test/lambda_test4.in" in *)
  let filename = "test/lambda_test5.in" in
  let in_channel = open_in filename in
  let file_content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;

  (* let res = interp file_content in
  Printf.printf "Result of interpreting %s:\n%s\n\n" filename res; *)

  let res = interp_big file_content in
  Printf.printf "Result of interpreting %s with big-step model:\n%s\n\n" filename res;

  let ast = parse file_content in 
  Printf.printf "AST: %s\n" (string_of_expr ast);

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


let parse s : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast


(* check if an expression is a value (i.e., fully evaluated) *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Binop _ | If _ | _ -> false


(* takes a single step of evaluation of [e] *)
let rec step : expr -> expr = function
  | Int _ | Bool _ -> failwith "Does not step on a number"

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

  | _ -> failwith "Not Implemented"


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


let rec eval_big (e : expr) : expr = match e with
  | Int _ | Bool _ -> e
  | Binop (binop, e1, e2) -> eval_bop binop e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

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


let interp_big (s : string) : string = 
  s |> parse |> eval_big |> string_of_expr
  
let () =
  (* let filename = "test/simpl_test1.in" in *)
  let filename = "test/simpl_test2.in" in
  let in_channel = open_in filename in
  let file_content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;

  let res = interp file_content in
  Printf.printf "Result of interpreting %s:\n%s\n\n" filename res;

  let res = interp_big file_content in
  Printf.printf "Result of interpreting %s with big-step model:\n%s\n\n" filename res;

  let ast = parse file_content in 
  Printf.printf "AST: %s\n" (string_of_expr ast);

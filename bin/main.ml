open Mathlib
open Ast

let rec string_of_expr (e : expr) : string = 
  match e with
  | Int n -> Printf.sprintf "Int %d" n
  | Binop (binop, e1, e2) ->
    let binop_str = 
      match binop with 
      | Add -> "Add"
      | Mul -> "Mul"
      | Sub -> "Sub"
      | Div -> "Div"
    in
    Printf.sprintf "Binop (%s, %s, %s)" binop_str (string_of_expr e1) (string_of_expr e2)


let parse s : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast


(* check if an expression is a value (i.e., fully evaluated) *)
let is_value : expr -> bool = function
  | Int _ -> true
  | Binop _ -> false


(* takes a single step of evaluation of [e] *)
let rec step : expr -> expr = function
  | Int _ -> failwith "Does not step on a number"

  (* No need for further stepping if both sides are already values *)
  | Binop (binop, e1, e2) when is_value e1 && is_value e2 -> 
    step_binop binop e1 e2

  (* Evaluate the right side of the binop if the left side is a value *)
  | Binop (binop, e1, e2) when is_value e1 -> Binop (binop, e1, step e2)

  (* Leftmost step for binop *)
  | Binop (binop, e1, e2) -> Binop (binop, step e1, e2)


(* implement the primitive operation [v1 binop v2].
   Requires: [v1] and [v2] are both values. *)
and step_binop binop v1 v2 = match binop, v1, v2 with
| Add, Int a, Int b -> Int (a + b)
| Sub, Int a, Int b -> Int (a - b)
| Mul, Int a, Int b -> Int (a * b)
| Div, Int a, Int b when b <> 0 -> Int (a / b)
| Div, Int _, Int 0 -> failwith "division by zero"
| _ -> failwith "precondition violated"


(* fully evaluate [e] to a value [v] *)
let rec eval (e : expr) : expr =
  if is_value e then e else
    e |> step |> eval


(* interpret [s] by lexing -> parsing -> evaluating and converting the result to a string *)
let interp ( s : string ) : string = 
  s |> parse |> eval |> string_of_expr

  
let () =
  let filename = "test/math_test2.in" in
  let in_channel = open_in filename in
  let file_content = really_input_string in_channel (in_channel_length in_channel) in
  close_in in_channel;

  let res = interp file_content in
  Printf.printf "Result of interpreting %s:\n%s\n\n" filename res;

  let ast = parse file_content in 
  Printf.printf "AST: %s\n" (string_of_expr ast);

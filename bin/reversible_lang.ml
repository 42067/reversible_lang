open Types
open Inference
open Util

type err = Syntax of string | Type of string | Runtime of string

let read_program path = open_in path |> In_channel.input_all |> Lexer.parse
let report name e = boldred name ^ ": " ^ e |> print_endline
let to_syntax r = Result.map_error (fun e -> Syntax e) r
let to_type r = Result.map_error (fun e -> Type e) r
let to_runtime r = Result.map_error (fun e -> Runtime e) r

let to_whatever r =
  Result.map_error (fun _ -> Type "the program does not have base type") r

let () =
  let res =
    let** { t; ts } = read_program Sys.argv.(1) |> to_syntax in
    (* show_term t |> print_endline; *)
    let gen = new_generator () in
    let** ctx = build_ctx gen ts |> to_type in
    let** inferred = Result.bind (infer_term t gen ctx) finalize |> to_type in
    let** _ = base_of_any inferred |> to_whatever in
    let++ evaluated = Eval.eval t |> to_runtime in
    print_newline ();
    evaluated |> show_term |> print_endline;
    "- : " ^ show_any (tvar_map [ inferred ]) inferred |> print_endline
  in
  match res with
  | Ok () -> ()
  | Error (Syntax e) -> report "Syntax Error" e
  | Error (Type e) -> report "Error" e
  | Error (Runtime e) -> report "Runtime Error" e

open Parser
open Evaluator
open Lexer
open Ast
open Tokens

let show_parse_error = function
  | UnexpectedToken { expected; got } ->
      Printf.sprintf "Parse error: expected %s, got %s" expected
        (describeToken got)
  | NoPrefixFunction token ->
      Printf.sprintf "Parse error: no prefix parse function for %s"
        (describeToken token)
  | UnexpectedEOF -> "Parse error: unexpected end of file"
  | NoClosingBrace -> "Parse error: missing closing brace"

let global_env = ref empty_env

let rec replLoop () =
  print_string ">> ";
  flush stdout;
  match read_line () with
  | exception End_of_file ->
      print_endline "\nBye!";
      exit 0
  | line when String.trim line = "" -> replLoop ()
  | line ->
      (let program = line |> lex |> newParser |> parseProgram in
       match program with
       | Ok program ->
           let result, env' = eval (Program program) !global_env in
           global_env := env';
           Printf.printf "%s\n" (describeObject result)
       | Error e -> Printf.printf "%s\n" (show_parse_error e));
      replLoop ()

let repl () =
  print_endline "Welcome to the Zin REPL!";
  replLoop ()

let read_file file_name : string =
  let ic = open_in file_name in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let run_program input =
  let tokens = lex input in
  let parser = newParser tokens in
  match parseProgram parser with
  | Ok program ->
      let _, _ = eval (Program program) empty_env in
      ()
  | Error err -> Printf.printf "\nParse failed: %s\n" (show_parse_error err)

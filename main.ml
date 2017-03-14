open uLang
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    try let lexbuf = Lexing.from_channel c in
            parser_main lexer_main lexbuf
    with Parsing.Parse_error -> failwith "Parse failure!" ;;


let arg = ref stdin in

let setProg p = arg := open_in p in
let usage = "Usage: ./main PROGRAM_FILE INPUT_FILE" in
parse [] setProg usage ;

let parsedProg = parseProgram !arg in
let () = print_string "Program Parsed" ; print_newline() in
let _ = typeProg parsedProg in
let () = print_string "Program Type Checked" ; print_newline() in

open ULang
open Lexer
open Parser
open Arg
open Printf

let parseProgram c =
    try let lexbuf = Lexing.from_channel c in
            parser_main lexer_main lexbuf
    with Parsing.Parse_error -> failwith "Program Parse failure!" ;;

let parseInputs c =
        try let lexbuf = Lexing.from_channel c in
                parser_inputs lexer_inputs lexbuf
        with Parsing.Parse_error -> failwith "Input Parse failure!" ;;


let arg = ref stdin in
let inputs = ref stdin in

let setProg p = arg := open_in p in
let usage = "Usage: ./main PROGRAM_FILE" in
parse [] setProg usage ;

let parsedInputs = parseInputs !inputs in
let () = print_string "Inputs Parsed" ; print_newline() in

let () = ULang.evalInputs parsedInputs in

let parsedProg = parseProgram !arg in
let () = print_string "Program Parsed" ; print_newline() in
(* let _ = typeProg parsedProg in
let () = print_string "Program Type Checked" ; print_newline() in *)
let result = ULang.eval parsedProg in
let () = print_string "Program Evaluated to ==> " ;  print_res result ; print_newline() in
flush stdout

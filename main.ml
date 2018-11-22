open Prop_def;;
open Fichier;;
open List;;
open Prop_lexer;;


let boucle in_channel =
let lexbuffer = Lexing.from_channel in_channel in
let lire_prop_expr () =
  Prop_parser.programme Prop_lexer.token lexbuffer in
 
	let p = lire_prop_expr () in print_term(fnc p);
 

exit 0;;


while boucle stdin  do

print_string "Entrer une valeur : ";

done;



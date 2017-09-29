{
open Parser
}

let entier = ['0'-'9']+
let comment = '{' [^ '}']* '}'
let space = [' ' '\t' '\n']+

rule scan =
parse	"BEGINNING-OF-PROGRAM"		{ BEGIN_PROG }
|		"BEGINNING-OF-EXECUTION"	{ BEGIN_EXEC }
|		"END-OF-EXECUTION"			{ END_EXEC }
|		"END-OF-PROGRAM"			{ END_PROG }
|		"move"						{ MOVE }
|		"turnleft"					{ TURN_LEFT }
|		"turnoff"					{ TURN_OFF }
|		"BEGIN"						{ BEGIN }
|		"END"						{ END }

|		"pickbeeper"				{ scan lexbuf }
|		"putbeeper"					{ scan lexbuf }
|		"next-to-a-beeper"			{ scan lexbuf } 



|		";"							{ SEMI }

|		entier						{ scan lexbuf }
|		space						{ scan lexbuf }
|		comment						{ scan lexbuf }
|		_ as c						{ raise (Common.LexerError (Printf.sprintf "unknown character '%c'" c)) }

%{

open Quad
open Common
open Comp
open Karel


%}

%token BEGIN_PROG
%token BEGIN_EXEC
%token END_EXEC
%token END_PROG

%token MOVE
%token TURN_LEFT
%token TURN_OFF

%token SEMI
%token BEGIN
%token END

%token PICK_BEEPER
%token PUT_BEEPER
%token NEXT_TO_A_BEEPER

%token FRONT_IS_CLEAR
%token FRONT_IS_BLOCKED
%token LEFT_IS_CLEAR
%token LEFT_IS_BLOCKED
%token RIGHT_IS_CLEAR
%token RIGHT_IS_BLOCKED
%token NOT_NEXT_TO_A_BEEPER
%token FACING_NORTH
%token NOT_FACING_NORTH
%token FACING_SOUTH
%token NOT_FACING_SOUTH
%token FACING_EAST
%token NOT_FACING_EAST
%token FACING_WEST
%token NOT_FACING_WEST
%token ANY_BEEPERS_IN_BEEPER_BAG
%token NO_BEEPERS_IN_BEEPER_BAG

%token <int> INT

%type <unit> prog
%start prog

%%

prog:	BEGIN_PROG BEGIN_EXEC stmts_opt END_EXEC END_PROG
			{ () }
;



stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		simple_stmt
				{ () }
;

test:			FRONT_IS_CLEAR 		{ () }
|			FRONT_IS_BLOCKED	{ () }
|			LEFT_IS_CLEAR		{ () }
|			LEFT_IS_BLOCKED		{ () }
|			RIGHT_IS_CLEAR		{ () }
|			RIGHT_IS_BLOCKED	{ () }
|			NEXT_TO_A_BEEPER	{ () }
|			NOT_NEXT_TO_A_BEEPER	{ () }
|			FACING_NORTH		{ () }
|			NOT_FACING_NORTH	{ () }
|			FACING_SOUTH		{ () }
|			NOT_FACING_SOUTH	{ () }
|			FACING_EAST		{ () }
|			NOT_FACING_EAST		{ () }
|			FACING_WEST		{ () }
|			NOT_FACING_WEST		{ () }
;


simple_stmt: TURN_LEFT
				{ gen (INVOKE (turn_left, 0, 0)) }
|			TURN_OFF
				{ gen STOP  }
|			MOVE
				{ gen (INVOKE (move, 0, 0)) }
|			PICK_BEEPER
				{ gen (INVOKE (pick_beeper, 0, 0)) }
|			PUT_BEEPER
				{ gen (INVOKE (put_beeper, 0, 0)) }
;



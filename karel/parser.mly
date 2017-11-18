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

%token ITERATE
%token TIMES

%token WHILE
%token DO

%token IF
%token THEN
%token ELSE

%token <string> ID


%token <int> INT

%token DEFINE_NEW_INSTRUCTION
%token AS

%type <unit> prog
%start prog

%%

prog:	BEGIN_PROG sous_prog BEGIN_EXEC stmts_opt END_EXEC END_PROG
			{ () }
;


stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		simple_stmt	{ () }
|		iterate		{ () }
|		whil		{ () }
|		IF if_test THEN stmt	{ backpatch $2 (nextquad()) }	
|		IF if_test THEN stmt_special ELSE stmt_special { () }
;

stmt_special:	simple_stmt	{ () }
|		iterate_special		{ () }
|		whil_special		{ () }
|		IF if_test THEN stmt_special ELSE stmt_special { () }
;

iterate:	ITERATE	INT TIMES stmt	{
				 let i = new_temp() in
		  		 let n = new_temp() in
		  		 let _ = gen(SETI(i,0)) in
		  		 let _ = gen(SETI(n,$2)) in
		  		 let a = nextquad() in
		  		 let _ = gen(GOTO_GE(0,i,n)) in 
		  		 (i,a) 
				}	
;

iterate_special:	ITERATE	INT TIMES stmt_special	{ 
				 let i = new_temp() in
		  		 let n = new_temp() in
		  		 let _ = gen(SETI(i,0)) in
		  		 let _ = gen(SETI(n,$2)) in
		  		 let a = nextquad() in
		  		 let _ = gen(GOTO_GE(0,i,n)) in 
		  		 (i,a)
				}
;

whil: 		WHILE cut while_test DO stmt	{ let _ = gen(GOTO($2)) in backpatch $3 (nextquad()) }
;


whil_special:WHILE cut while_test DO stmt_special	{ let _ = gen(GOTO($2)) in backpatch $3 (nextquad()) }
;

cut: { nextquad() }


define_new:	DEFINE_NEW_INSTRUCTION ID AS{ 
					if is_defined $2 then raise (SyntaxError "un sous programme est declaré 2 fois") 
					else define $2 { nextquad() }
				}
;

define:		define_new stmts
				{ gen RETURN };

sous_prog:	define_new sous_prog	{ () }
|		{ () }
;


test:			FRONT_IS_CLEAR 		{ let b=new_temp() in gen(INVOKE(is_clear,front,b));b }
|			FRONT_IS_BLOCKED	{ let b=new_temp() in gen(INVOKE(is_blocked,front,b));b }
|			LEFT_IS_CLEAR		{let b=new_temp() in gen(INVOKE(is_clear,left,b));b }
|			LEFT_IS_BLOCKED		{let b=new_temp() in gen(INVOKE(is_blocked,left,b));b }
|			RIGHT_IS_CLEAR		{let b=new_temp() in gen(INVOKE(is_clear,right,b));b }
|			RIGHT_IS_BLOCKED	{let b=new_temp() in gen(INVOKE(is_blocked,right,b));b }
|			NEXT_TO_A_BEEPER	{let a=new_temp() in gen(INVOKE(next_beeper,a,0));a }
|			NOT_NEXT_TO_A_BEEPER	{let a=new_temp() in gen(INVOKE(no_next_beeper,a,0));a}	
|			FACING_NORTH		{let b=new_temp() in gen(INVOKE(facing,north,b));b }
|			NOT_FACING_NORTH	{let b=new_temp() in gen(INVOKE(not_facing,north,b));b }
|			FACING_SOUTH		{let b=new_temp() in gen(INVOKE(facing,south,b));b }
|			NOT_FACING_SOUTH	{let b=new_temp() in gen(INVOKE(not_facing,south,b));b }
|			FACING_EAST		{let b=new_temp() in gen(INVOKE(facing,east,b));b }
|			NOT_FACING_EAST		{let b=new_temp() in gen(INVOKE(not_facing,east,b));b }
|			FACING_WEST		{let b=new_temp() in gen(INVOKE(facing,west,b));b }
|			NOT_FACING_WEST		{let b=new_temp() in gen(INVOKE(not_facing,west,b));b }
;


simple_stmt:BEGIN stmts END   { () } 
|			TURN_LEFT
				{ gen (INVOKE (turn_left, 0, 0)) }
|			TURN_OFF
				{ gen STOP  }
|			MOVE
				{ gen (INVOKE (move, 0, 0)) }
|			PICK_BEEPER
				{ gen (INVOKE (pick_beeper, 0, 0)) }
|			PUT_BEEPER
				{ gen (INVOKE (put_beeper, 0, 0)) }
|			ID		{  if is_defined $1 
					then gen (CALL (get_define $1)) 
					else raise(SyntaxError "undefined")  }
;

if_test: test
	{ let v = new_temp() in
	  let _ = gen(SETI(v,0)) in
	  let a = nextquad() in
	  let _ = gen(GOTO_EQ(0,$1,v)) in
	  a
	}
;

while_test: test
	{ let v = new_temp() in
	  let _ = gen(SETI(v,0)) in
	  let a = nextquad() in
	  let _ = gen(GOTO_EQ(0,$1,v)) in
	  a
	}
;


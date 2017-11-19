%{

open Quad
open Common
open Comp
open Karel

let iteration (i, a) =
	let t = new_temp () in
	gen (SETI (t, 1));
	gen (ADD (i, i, t));
	gen (GOTO (a));
	backpatch a (nextquad())

let boucle (i, a) =
	gen (GOTO i);
	backpatch a (nextquad ())

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

prog:	BEGIN_PROG initial_goto sous_prog BEGIN_EXEC destination stmts_opt END_EXEC END_PROG
			{ () }
;


initial_goto:				{ gen (GOTO 0) }
;

destination:				{ backpatch 0 (nextquad ()) }
;


stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;


define_new:	DEFINE_NEW_INSTRUCTION ID AS { 
					if is_defined $2 then raise (SyntaxError "un sous programme est declaré 2 fois") 
					else define $2 (nextquad())
				}
;

define:		define_new stmts
				{ gen RETURN }
;

sous_prog:	define sous_prog	{ () }
|		{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		simple_stmt									{ () }
|		iterate_times	stmt 							{ iteration $1 }
|		while_test	stmt								{ boucle $1 }
|		IF if_test THEN stmt							{ backpatch $2 (nextquad()) }	
|		IF if_test THEN stmt_special if_cut ELSE cut stmt_special 	{ let _ = backpatch $5 (nextquad()) in backpatch $2 $7 }
;

stmt_special:	simple_stmt								{ () }
|		iterate_times stmt_special						{ iteration $1 }
|		while_test stmt_special							{ boucle $1 }
|		IF if_test THEN stmt_special if_cut ELSE cut stmt_special 	{ let _ = backpatch $5 (nextquad()) in backpatch $2 $7 }
;

if_cut:	{ let a =nextquad() in let _ = gen(GOTO(0)) in a }
;

if_test: test { 
		let v = new_temp() in
	  	let _ = gen(SETI(v,0)) in
	  	let a = nextquad() in
	  	let _ = gen(GOTO_EQ(0,$1,v)) in
	  	a
	}
;

iterate_times:	ITERATE	INT TIMES {
				let i = new_temp() in
		  		let n = new_temp() in
		  		let _ = gen(SETI(i,0)) in
		  		let _ = gen(SETI(n,$2)) in
		  		let a = nextquad() in
		  		let _ = gen(GOTO_GE(0,i,n)) in 
		  		(i,a) 
			}	
;


while_test: 	WHILE cut test DO { 
				let t = new_temp () in
				let _ = gen (SETI (t, 0)) in
				let a = nextquad () in
				let _ = gen (GOTO_EQ (0, $3, t)) in
				($2, a) 
			}
;


cut: { nextquad() }





test:		FRONT_IS_CLEAR 		{ let b=new_temp() in gen(INVOKE(is_clear,front,b));b }
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


simple_stmt: BEGIN stmts END   { () } 
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
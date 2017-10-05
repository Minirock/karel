open Common
open Quad

(** Tapez votre programme ici. *)

(*
let prog = [
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	STOP
]
*)

(*
let prog = [
	SETI(0,5);
	SETI(1,1);
	SETI(2,1);
	(*Se retourne*)
	INVOKE(Karel.turn_left,0,0);
	INVOKE(Karel.turn_left,0,0);
	INVOKE(Karel.move,0,0);
	ADD(1,1,2);
	GOTO_LT(5,1,0);
	STOP
]
*)

(*
let prog = [
	
	INVOKE(Karel.put_beeper,0,0);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.move,0,0);
	(*Se retourne*)
	INVOKE(Karel.turn_left,0,0);
	INVOKE(Karel.turn_left,0,0);
	(*Cherche le beeper*)
	SETI(0,0);
	SETI(1,1);
	INVOKE(Karel.move,0,0);
	INVOKE(Karel.next_beeper,0,0);
	GOTO_EQ(13,0,1);
	GOTO_EQ(9,0,0);
	STOP
]
*)

let prog = [
	(*Cherche s'il y a un mur*)
	(*false*)
	SETI(0,0); 
	(*true*)
	SETI(1,1); 
	(*mur ?*) 
	SETI(2,0); 
	(*beeper ?*)
	SETI(3,0); 
	(*GAUCHE OU DROITE*)
	SETI(4,0);
	INVOKE(karel.is_clear,karel.front,2); 
	(*Si un mur*)
	GOTO_EQ(8,2,0); 
	(*Si pas de mur*)
	GOTO_EQ(22,2,1); 
	(*droite ou gauche ? *)
	GOTO_EQ(10,4,0);
	GOTO_EQ(15,4,1);
	(*ouest*)
	INVOKE(karel.turn_left,0,0); 
	INVOKE(karel.move,0,0); 
	(*sud*)
	INVOKE(karel.turn_left,0,0);  
	SETI(4,1);
	(*On check beeper*)
	GOTO(19);
	(*est*)
	INVOKE(karel.turn_right,0,0); 
	INVOKE(karel.move,0,0); 
	(*nord*)
	INVOKE(karel.turn_right,0,0); 
	SETI(4,0);

	INVOKE(karel.next_beeper,3,0);
	(*S'il y a un beeper fini sinon on boucle*)
	GOTO_EQ(24,3,1); 
	GOTO_EQ(5,3,0); 
	(*On avance si pas de mur*)
	INVOKE(karel.move,0,0);
	GOTO(19); 
	(*On ramasse le beeper et on termine*)
	INVOKE(karel.pick_beeper,0,0); 
	STOP
]


(*** traitement des options et interprétation ***)

(* configuration des options *)
let opts = [
]
let  doc = "Karel Game Emulator"

(* exécution du programme *)
let process (world: Karel.world) =
	let prog = Array.of_list prog in

	let rec exec vs =
	if Vm.ended vs
	then print_string "Execution ended!\n"
	else
		begin
			let pc = Vm.get_pc vs in
			Printf.printf "%4d: %s\n" pc (Quad.to_string prog.(pc));
			let ns = Vm.step vs prog in
			let _ = Vm.invoke ns Karel.display 0 0 in
			exec ns
		end in

	let kstate = Karel.init_state world in
	let vstate = Vm.new_state Karel.invoke kstate in
	let _ = Vm.invoke vstate Karel.display 0 0 in
	exec vstate
	

(* analyse des paramètres *)
let scan args =

	let load_world path =
		let file = open_in path in
		let lexbuf = Lexing.from_channel file in
		try
			Wparser.top Wlexer.scan lexbuf;
			!Karel.world
		with
		|	Parsing.Parse_error		-> print_fatal lexbuf path "syntax error in world file"
		|	Common.LexerError msg 	-> print_fatal lexbuf path msg in
	
	match args with
	| []		-> process Karel.empty_world
	| [ world]	-> process (load_world world)
	| _			-> Arg.usage opts "ERROR: syntax: game program [map]"

(* lancement du programme *)
let _ = 
	let free_args = ref [] in
	Arg.parse opts (fun arg -> free_args := arg :: !free_args) doc;
	scan !free_args


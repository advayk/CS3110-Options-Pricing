open Game

let rec game_loop adv state = 
  let () = print_endline "Enter Command: " in 
  let command = Command.parse (read_line ()) in 
    match command with 
    | Quit -> let () = print_endline "Terminating..." in exit 0 
    | Go x -> let ex = String.concat " " x in 
      let visited = State.visited state in 
        match State.go  (String.trim ex) adv state with 
        | Legal y -> let description = Adventure.description adv (State.current_room_id y) in  
        if not (List.mem (State.current_room_id y) visited) then (print_string "Description: "; print_endline description; game_loop adv y) 
        else game_loop adv y
        | Illegal -> 
        game_loop adv state 

(** [play_game f] starts the adventure in file [f]. *)
let play_game f = 
  if not (Sys.file_exists f) then let () = print_endline "File does not exist. Please enter another." in exit 0; else ();
  let adventure = Adventure.from_json (Yojson.Basic.from_file f) in
    let init_state = State.init_state adventure in
      game_loop adventure init_state
    
let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game (data_dir_prefix ^ file_name ^ ".json")

(* Execute the game engine. *)
let () = main ()
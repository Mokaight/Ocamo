(*
 * Copyright (C) 2016  Joseph Le Roux
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Core.Std

(* aliases *)
let (>>=) = Lwt.(>>=)
		   
(* block the thread and listen to an event *)
let rec read_key term =
  LTerm.read_event term >>=
  function
  (* key pressed *)
  | LTerm_event.Key keyPressed ->
     begin
       match keyPressed.code with
       | LTerm_key.Escape    -> Lwt.return Action.Escape
       | LTerm_key.Up        -> Lwt.return Action.Up
       | LTerm_key.Down      -> Lwt.return Action.Down
       | LTerm_key.Right     -> Lwt.return Action.Right
       | LTerm_key.Left      -> Lwt.return Action.Left
       | LTerm_key.Backspace -> Lwt.return Action.Backspace
       | LTerm_key.Enter     -> Lwt.return Action.Return
       | LTerm_key.Char lTChar ->
	  (match keyPressed.control, CamomileLibrary.UChar.char_of lTChar with
	   (* second part 
	   | true, 'r'       -> Lwt.return Action.Redo
	   | true, 'z'       -> Lwt.return Action.Undo *)
	   | _,    ch        -> Lwt.return (Action.Char ch)
	  )
       | _ -> read_key term (* a key we do not process *)
     end
  | _ -> read_key term (* not a key : we do not process *)


let main filename () =
  (* recursively get a user input and ask for an action *)
  let rec loop term tz =
    read_key term
    >>= fun key ->
    match Action.action key tz filename with
    | None -> Lwt.return ()
    | Some tz' -> loop term tz'
  in
  (* some wrapper for LTerm and Lwt code *)
  let main =
    Lazy.force LTerm.stdout
    >>= fun term ->
    (* we take care of the cursor *)
    LTerm.hide_cursor term >>=
    (* enter a mode where we dont wait a full line of user input *)
    fun () -> LTerm.enter_raw_mode term >>=
    fun mode ->
    (* execute the first function, and finally (always) the second *)
    Lwt.finalize
      (fun () ->
         let z = TextZipper.empty in
         TextZipper.print_current_line z;
         loop term z)
      (fun () ->
         LTerm.show_cursor term >>=
         fun () -> LTerm.leave_raw_mode term mode)
  in
  Lwt_main.run main (* ask for the execution *)

(* second part
let main_part2 filename () =
  (* recursively get a user input and ask for an action *)
  let rec loop term urdTz =
    read_key term
    >>= fun key ->
    match Action.action_part2 key urdTz filename with
    | None -> Lwt.return ()
    | Some urdTz' -> loop term urdTz'
  in
  (* some wrapper for LTerm and Lwt code *)
  let main =
    Lazy.force LTerm.stdout
    >>= fun term ->
    (* we take care of the cursor *)
    LTerm.hide_cursor term >>=
    (* enter a mode where we dont wait a full line of user input *)
    fun () -> LTerm.enter_raw_mode term >>=
    fun mode ->
    (* execute the first function, and finally (always) the second *)
    Lwt.finalize
      (fun () ->
         let urdTz = UnReDo.return () in
         TextZipper.print_current_line (UnReDo.getCurrent urdTz) ;
         loop term urdTz)
      (fun () ->
         LTerm.show_cursor term >>=
         fun () -> LTerm.leave_raw_mode term mode)
  in
  Lwt_main.run main (* ask for the execution *) *)
	       
let command =
  Command.basic
    ~summary:"FLOTE : functional line oriented text editor"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty +> anon ("filename" %: string)) main
                                                     (* main_part2 *)
							
let () = Command.run ~version:"0.01" ~build_info:"jlr" command

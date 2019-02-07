open Core
open TextZipper
open TextBuffer
type key =
  | Left
  | Right
  | Up
  | Down
  | Escape
  | Char of char
  | Return
  | Backspace

(* get a key, a textzipper and a filename *)
(* modifies the zipper, do some IO *)
(* returns a zipper option, with None to stop the program *)
let action (key : key) (tz : TextZipper.t) (filename :string) =
  match key with
  | Left -> left tz; print_current_line tz
  | Right -> right tz; print_current_line tz
  | Up -> up tz; print_current_line tz
  | Down -> down tz; print_current_line tz
  | Escape -> write_buffer filename (buffer_from_text_zipper tz); None
  | Char of Char -> insert char tz; print_current_line tz
  | Return -> break_current_line tz; print_current_line tz
  | Backspace -> delete_previos tz; print_current_line tz
  | _ ->
    Printf.printf "affiche zipper et %s\n%!" filename; Some tz

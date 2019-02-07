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
val action : key -> TextZipper.t -> string -> TextZipper.t option

(* second part
val action_part2 : key -> unit UnReDo.t -> string -> (unit UnReDo.t) option *)

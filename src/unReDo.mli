
type 'a t

val bind : 'a t -> ('a -> 'b t) -> ('b t)

val return : 'a -> 'a t

(* val doFun : (TextZipper.t -> TextZipper.t) -> 'a -> 'a t *)

val newCurrent : TextZipper.t -> 'a -> 'a t

val undo : 'a -> 'a t

val redo : 'a -> 'a t

val getCurrent : 'a t -> TextZipper.t

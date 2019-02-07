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

type t

(* create/save *)
val empty : t
(*val text_zipper_from_buffer : TextBuffer.buffer -> t*)
val buffer_from_text_zipper : t -> TextBuffer.buffer

(* edit *)
val left : t -> t option
val right : t -> t option
val up : t -> t option
val down : t -> t option

(* add/delete *)
val insert : TextBuffer.glyph -> t -> t
(*val delete : t -> t option*)
val delete_previous : t -> t option

(* extended move/add/delete*)
(*val move_cursor : t -> l:int -> c:int -> t*)
val break_current_line : t -> t
(*val kill_end_of_line : t -> t
val goto_end_of_line : t -> t
val goto_beginning_of_line : t -> t*)

(* printing *)
(* TODO should take an out_channel as additional parameter *)
val print_current_line : t -> unit
(* val current_line_to_string : t -> Core.Std.bytes *)

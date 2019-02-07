type glyph
type line
type buffer

val glyph_of_char : char -> glyph
val char_of_glyph : glyph -> char

val glyphs_to_line : glyph list -> line
val line_to_glyphs : line -> glyph list

val lines_to_buffer : line list -> buffer
val buffer_to_lines : buffer -> line list

val write_buffer : string -> buffer -> unit

val glyph_to_string : glyph -> string
val line_to_string : line -> string

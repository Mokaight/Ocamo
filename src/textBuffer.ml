open Core.Std.Char
open String
open Pervasives
open List

    type glyph = int
    type line =  glyph list
    type buffer = line list


    let glyph_of_char x  :glyph = Char.code x
    let char_of_glyph (x: glyph) :char = Char.chr x
    
    let glyphs_to_line (lg : glyph list) :line = 
        lg

    let line_to_glyphs (l : line) :glyph list= 
         l


    let lines_to_buffer (ll : line list) :buffer = 
        ll

    let buffer_to_lines (b : buffer) :line list = 
        b


    let glyph_to_string g :string =  
        Char.escaped (char_of_glyph g)
    
    let rec line_to_string (lts :line) :string = 
        match lts with
        | [] -> ""
        | hd:: tl -> (glyph_to_string hd) ^ (line_to_string tl)
    
    let write_buffer (file :string) (b :buffer) =
        let f = open_out file in
        let rec writeFile l =
                     match l with
			        | []-> close_out f
			        | hd::tl -> output_string f ((line_to_string hd)^"\n") ; writeFile tl
	in writeFile (buffer_to_lines b)


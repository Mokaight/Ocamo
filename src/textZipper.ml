open Core.Std.Char
open Core.Std
open TextBuffer
(*
Le tline :
        La ligne a gauche , le curseur, la ligne a droite
        si la ligne a gauche est vide = on est a gauche de la ligne
        la ligne de droite est vide = on est tout a droite de la ligne

Le t : 
        les ligne avant celle qu'on regarde // La ligne actuelle // La ligne après celle que l'on regarde
        


*)
type tline = TextBuffer.line * TextBuffer.line
type t = TextBuffer.line list* tline * TextBuffer.line list

let empty  = ([],([],[]),[]);

(* tline_to_list
        transforme un tline en liste
        *)
let tline_to_list (tline :tline)  :line list =
        let (listeG,listeD) = tline in
         (listeG@listeD)

(*
        elle doit transformer t en buffer
Donc crée un buffer contenant Buffer + tline + buffer
Mais j'ai changer buffer car c'était impossible a concatener , donc on prends le type TextBuffer.line
donc
        line list + tline + line list 
 

*)
let buffer_from_text_zipper (t :t) :TextBuffer.buffer = 
        let t_to_buffer (hd,cursor,tl) =
                hd@(tline_to_list cursor)@tl
        in t_to_buffer t
                

(* edit *)
(* L'énoncé nous dit qu'on doit remmetre le curseur au début a chaque changement de ligne*)
let rec curseur_start (liste,liste2) =
        match liste with
        |[] -> liste,liste2 (* Le curseur est au début de la ligne *)
        |hd::liste -> curseur_start(hd,liste::liste2)

let left t = 
        let (hd,tline,tl) = t
        in
        match tline with
        |[],liste2 -> None
        (*Pas sur de ca , techniquement le h est une liste aussi , donc le header est un enchainement de caractère , donc on concatène la fin de la liste de gauche, un seul élement de la liste ( qui ici s'appelle liste) avec la liste de droite , *)
        |h::liste,liste2 -> Some(h,(h,liste::liste2),tl)

let right t = 
        let (hd,tline,tl) = t
        in
        match tline with
        |liste,[] -> None
        (*Ici c'est clair, le h qui est le header de tout ce qu'il y a a droite de mon curseur, va être stocké à la suite de tout ce qu'il y a a gauche *)
        |liste,h::liste2 -> Some(h,(liste::h,liste2),tl)
        
(*Up and down ne sont pas très bon je pense , car on ne stock pas notre ligne active ?? *)        
let up t = 
        let (hd,tline,tl) = t
        in
        match hd with
        |[] -> None
        (*
        Le hd est une liste de line
        Donc la queue du head passe en tete de la liste de droite
        *)
        |h::q -> Some (h,(curseur_start tline),q::tl)
        
        
let down t =  
        let (hd,tline,tl) = t
        in
        match tl with
        |[] -> None
        (*
        Le tl est une liste de line
        Donc le head de la TL passe en tete de la liste de gauche
        *)
        |h::q -> Some (hd::h,(curseur_start c),q)
        

(*  après l'insersion du caractère arpès insert a la liste droite*)
let insert g t = 
        let(hd,(listeG,listeD),tl) = t in
        (hd,(listeG,g::listeD),tl)
        
(* On appelle Some avec hd car on gardera le header de la ligne de gauche, le caracètre suprimé étant dans liste *)
let delete_previous t = 
        let (hd,tline,tl) = t
        match tline with
        | [],liste2 ->None
        | h::liste,liste2 ->Some(h,liste2)


let break_current_line t = 
        let (hd,(listeG,listeD),tl) = t in
        match t with
        |(hd,([],listeD),tl) -> (hd::[],([],listeD),tl)
        |(hd,(listeG,[]),tl) -> (hd::listeG,([],[]),tl)
        |(hd,(listeG,listeD),tl) -> (hd::listeG,([],listeD),tl)


let print_current_line t = 
        let (hd,(listeG,listeD),tl) = t in
        print_current_line_aux (line_to_glyphs listeG) (line_to_glyphs listeD)
        

(* code spécial du terminal pour inverser les couleurs *)
let underline_code = Core.Std.sprintf "%c[1;7m" (Core.Std.Char.of_int_exn 27)
(* code spécial du terminal pour remettre les couleurs à zéro *)
let reset_code = Core.Std.sprintf "%c[1;0m" (Core.Std.Char.of_int_exn 27)

let print_current_line_aux left current =
        printf "\r";
        printf "                ";
        printf "                ";
        printf "\r";
        printf "%s" (TextBuffer.line_to_string (TextBuffer.glyphs_to_line left)) ;
        (match current with
                | [] -> printf "%s %s" underline_code reset_code
                | h::t ->
                        printf "%s%s%s" underline_code (TextBuffer.glyph_to_string h) reset_code ;
                        printf "%s" (TextBuffer.line_to_string (TextBuffer.glyphs_to_line t))
        );
printf "%!"


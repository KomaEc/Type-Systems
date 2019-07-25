{
  open Lexing

  let next_line lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    lexbuf.lex_curr_p <- {
      pos with pos_bol = lexbuf.lex_curr_pos
             ; pos_lnum = pos.pos_lnum + 1
    }

  exception LexicalError of string

  let get_info lexbuf = 
    let pos = lexbuf.lex_curr_p in 
    {
      filename = pos.pos_fname
    ; line_num = pos.pos_lnum
    ; offset = pos.pos_cnum - pos.pos_bol + 1
    }

  let keyword_table = Hashtbl.create 64

  let keywords = [
    "fun", Parser.LAMBDA;
    "lambda", Parser.LAMBDA;
    "let", Parser.LET;
    "forall", Parser.FORALL;
    "in", Parser.IN;
    "->", Parser.ARROW;
    "(", Parser.LPAREN;
    ")", Parser.RPAREN;
    "=", Parser.EQ;
  ]

  let _ = 
    List.iter (fun (key, val) -> Hashtbl.add keyword_table key val)
      keywords
}

let wsp = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule read = 
  parse
    | wsp { read lexbuf }
    | newline { read lexbuf }
    | "(" | ")" | "->" | "=" { Hashtbl.find keyword_table (lexing.lexeme lexbuf) }
    | id 
      { let id = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table id 
        with
          Not_found -> 
          Parser.IDENT (Lexing.lexeme lexbuf, get_info lexbuf) }
    | eof { Parser.EOF }
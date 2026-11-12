importScripts("toplevel.js");
const CHUNKS = [String.raw`module Common = struct
type token =
  | INT of int | FLOAT of float | STRING of string | IDENT of string
  | NEG | PLUS | MINUS | TIMES | DIV | DPLUS | DMINUS | DTIMES | DDIV | MOD | EXP | CARAT
  | LT | GT | LEQ | GEQ | EQUALS | NEQ | PIPE | ARROW | SEMI | DSEMI | DCOLON | AT | NIL
  | LET | REC | AND | END | IN | IF | THEN | ELSE | FUN | RAISE | TRY | WITH | NOT
  | LOGICALAND | LOGICALOR | LBRAC | RBRAC | LPAREN | RPAREN | COMMA | UNDERSCORE
  | UNIT | TRUE | FALSE | EOF
end;;`,
String.raw`module Lx_rt = struct
  type re = S of Bytes.t | E | Eps | Q of re * re | A of re * re | K of re | P of re | O of re | B of int * re

  let set ranges =
    let b = Bytes.make 256 '\000' in
    let rec go = function
      | lo :: hi :: rest -> for i = lo to hi do Bytes.set b i '\001' done; go rest
      | _ -> () in
    go ranges; S b

  (* longest match (or shortest), ties go to the earliest rule *)
  let exec shortest (rules : re array) (lexbuf : Lexing.lexbuf) =
    while not lexbuf.Lexing.lex_eof_reached do lexbuf.Lexing.refill_buff lexbuf done;
    let buf = lexbuf.Lexing.lex_buffer and len = lexbuf.Lexing.lex_buffer_len in
    let start = lexbuf.Lexing.lex_curr_pos in
    let best = ref (-1) and best_end = ref (-1) and best_b = ref [] in
    Array.iteri (fun i r ->
      let rend = ref (-1) and rb = ref [] in
      let rec m r pos bs k = match r with
        | Eps -> k pos bs
        | S s -> if pos < len && Bytes.get s (Char.code (Bytes.get buf pos)) <> '\000' then k (pos + 1) bs
        | E -> if pos >= len then k pos bs
        | Q (a, b) -> m a pos bs (fun p bs -> m b p bs k)
        | A (a, b) -> m a pos bs k; m b pos bs k
        | O a -> m a pos bs k; k pos bs
        | K a -> k pos bs; m a pos bs (fun p bs -> if p > pos then m r p bs k)
        | P a -> m a pos bs (fun p bs -> m (K a) p bs k)
        | B (n, a) -> m a pos bs (fun p bs -> k p ((n, pos, p) :: bs)) in
      m r start [] (fun p bs ->
        if !rend < 0 || (if shortest then p < !rend else p > !rend) then (rend := p; rb := bs));
      if !rend >= 0 && (!best < 0 || (if shortest then !rend < !best_end else !rend > !best_end)) then
        (best := i; best_end := !rend; best_b := !rb)) rules;
    if !best < 0 then failwith "lexing: empty token";
    let abs = lexbuf.Lexing.lex_abs_pos in
    lexbuf.Lexing.lex_start_pos <- start;
    lexbuf.Lexing.lex_curr_pos <- !best_end;
    lexbuf.Lexing.lex_start_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_cnum = abs + start };
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_cnum = abs + !best_end };
    (!best, !best_b)

  let rec find n = function
    | [] -> None
    | (m, s, e) :: r -> if m = n then Some (s, e) else find n r
  let sub lexbuf (s, e) = Bytes.sub_string lexbuf.Lexing.lex_buffer s (e - s)
  let ostr lexbuf bs n = Option.map (sub lexbuf) (find n bs)
  let ochr lexbuf bs n = Option.map (fun (s, _) -> Bytes.get lexbuf.Lexing.lex_buffer s) (find n bs)
  let str lexbuf bs n = match ostr lexbuf bs n with Some s -> s | None -> ""
  let chr lexbuf bs n = match ochr lexbuf bs n with Some c -> c | None -> '\000'
end;;`,
String.raw`module Grader = struct
  open Common
  let q s = "\"" ^ String.escaped s ^ "\""
  let string_of_token = function
    | INT n -> "INT " ^ string_of_int n
    | FLOAT f -> "FLOAT " ^ string_of_float f
    | STRING s -> "STRING " ^ q s
    | IDENT s -> "IDENT " ^ q s
    | NEG -> "NEG" | PLUS -> "PLUS" | MINUS -> "MINUS" | TIMES -> "TIMES" | DIV -> "DIV"
    | DPLUS -> "DPLUS" | DMINUS -> "DMINUS" | DTIMES -> "DTIMES" | DDIV -> "DDIV" | MOD -> "MOD"
    | EXP -> "EXP" | CARAT -> "CARAT" | LT -> "LT" | GT -> "GT" | LEQ -> "LEQ" | GEQ -> "GEQ"
    | EQUALS -> "EQUALS" | NEQ -> "NEQ" | PIPE -> "PIPE" | ARROW -> "ARROW" | SEMI -> "SEMI"
    | DSEMI -> "DSEMI" | DCOLON -> "DCOLON" | AT -> "AT" | NIL -> "NIL" | LET -> "LET" | REC -> "REC"
    | AND -> "AND" | END -> "END" | IN -> "IN" | IF -> "IF" | THEN -> "THEN" | ELSE -> "ELSE"
    | FUN -> "FUN" | RAISE -> "RAISE" | TRY -> "TRY" | WITH -> "WITH" | NOT -> "NOT"
    | LOGICALAND -> "LOGICALAND" | LOGICALOR -> "LOGICALOR" | LBRAC -> "LBRAC" | RBRAC -> "RBRAC"
    | LPAREN -> "LPAREN" | RPAREN -> "RPAREN" | COMMA -> "COMMA" | UNDERSCORE -> "UNDERSCORE"
    | UNIT -> "UNIT" | TRUE -> "TRUE" | FALSE -> "FALSE" | EOF -> "EOF"

  let attempt f s =
    try Ok (f s) with
    | Stack_overflow -> Error "Stack overflow (infinite recursion?)"
    | Failure m -> Error ("Failure " ^ q m)
    | e -> Error (Printexc.to_string e)

  let show = function
    | Ok ts -> "[" ^ String.concat "; " (List.map string_of_token ts) ^ "]"
    | Error e -> "Exception: " ^ e

  (* comment errors must carry the exact message; any other failure just has to fail *)
  let strict e = e = "Failure " ^ q "unmatched open comment" || e = "Failure " ^ q "unmatched closed comment"

  let clean s = String.map (fun c -> if c = '\t' || c = '\n' then ' ' else c) s

  let run sol stu inputs =
    List.iter (fun s ->
        let want = attempt sol s and got = attempt stu s in
        let pass, exp_str = match want, got with
          | Ok a, Ok b -> a = b, show want
          | Error a, Error b when strict a -> a = b, show want
          | Error _, r -> (match r with Error _ -> true | Ok _ -> false), "an exception"
          | Ok _, Error _ -> false, show want in
        print_string ("@@T\t" ^ (if pass then "PASS" else "FAIL") ^ "\t" ^ clean ("get_all_tokens " ^ q s)
                      ^ "\t" ^ clean exp_str ^ "\t" ^ clean (show got) ^ "\n"))
      inputs
end;;`];
let initOut = CHUNKS.map(c => ocamlEval(c, false)).join("");
postMessage({type:"ready", out: initOut});
onmessage = (ev) => {
  const {id, code, print} = ev.data;
  let out;
  try { out = ocamlEval(code, !!print); } catch (e) { out = "Internal error: " + e; }
  postMessage({type:"result", id, out});
};

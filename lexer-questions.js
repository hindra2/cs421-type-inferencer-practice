(function (root) {
  const HEADER = `{
open Common;;

}

(* You can assign names to commonly-used regular expressions in this part
   of the code, to save the trouble of re-typing them each time they are used *)
let numeric = ['0' - '9']
let lowercase = ['a' - 'z']
let uppercase = ['A' - 'Z']
let alpha = lowercase | uppercase
let alphanum = alpha | numeric
let ident_char = alphanum | '_' | '\\''

let printable_char = alphanum
                      | ' ' | '~' | '\`' | '!' | '@' | '#' | '$'
                      | '%' | '^' | '&' | '*' | '(' | ')' | '-' | '_'
                      | '+' | '=' | '{' | '[' | '}' | ']' | '|'
                      | ':' | ';' | '\\'' | '<' | ',' | '>' | '.' | '?' | '/'


let whitespace = [' ' '\\t' '\\n']

rule token = parse
  | whitespace { token lexbuf }  (* skip over whitespace *)
  | eof        { EOF          }
`;
  // provided rules, added out of sight when grading (they are questions of their own)
  const HIDDEN_KEYWORDS = `"*" {Common.TIMES} | "=" {Common.EQUALS} | ";" {Common.SEMI}
    | "try" {Common.TRY} | "then" {Common.THEN} | "not" {Common.NOT}`;
  const HIDDEN_IDENT = `['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\\'']* as lx_id {Common.IDENT lx_id}`;
  const IDENT = `  | lowercase+ ident_char* as i {IDENT i}
`;
  const TRAILER = `

{(* do not modify this function: *)
 let lextest s = token (Lexing.from_string s)

 let get_all_tokens s =
     let b = Lexing.from_string (s^"\\n") in
     let rec g () =
     match token b with EOF -> []
     | t -> t :: g () in
     g ()


let get_all_token_options s =
    let b = Lexing.from_string (s^"\\n") in
    let rec g () =
        match (try Some (token b) with _ -> None) with
            Some EOF -> []
            | None -> [None]
            | t -> t :: g ()
        in
    g ()
 }
`;
  const TODO = label => `  (* YOUR CODE HERE: ${label} *)\n`;

  function file(slot, extra) { return HEADER + slot + extra + TRAILER; }

  const KEYWORD_TABLE = [
    ["~", "NEG"], ["+", "PLUS"], ["-", "MINUS"], ["*", "TIMES"], ["/", "DIV"], ["+.", "DPLUS"],
    ["-.", "DMINUS"], ["*.", "DTIMES"], ["/.", "DDIV"], ["^", "CARAT"], ["<", "LT"], [">", "GT"],
    ["<=", "LEQ"], [">=", "GEQ"], ["=", "EQUALS"], ["<>", "NEQ"], ["|", "PIPE"], ["->", "ARROW"],
    [";", "SEMI"], [";;", "DSEMI"], ["::", "DCOLON"], ["@", "AT"], ["[]", "NIL"], ["let", "LET"],
    ["rec", "REC"], ["and", "AND"], ["end", "END"], ["in", "IN"], ["if", "IF"], ["then", "THEN"],
    ["else", "ELSE"], ["fun", "FUN"], ["mod", "MOD"], ["raise", "RAISE"], ["try", "TRY"],
    ["with", "WITH"], ["not", "NOT"], ["&&", "LOGICALAND"], ["||", "LOGICALOR"], ["[", "LBRAC"],
    ["]", "RBRAC"], ["(", "LPAREN"], [")", "RPAREN"], [",", "COMMA"], ["_", "UNDERSCORE"],
    ["true", "TRUE"], ["false", "FALSE"], ["()", "UNIT"],
  ];

  const QUESTIONS = [
    {
      id: "floats", title: "Floats", tag: "PE3.5 Q5",
      spec: `Implement floats using regular expressions. There is a token <code>FLOAT</code> that takes a float as an argument. Floats must have a decimal point and at least one digit before the decimal point (and optionally may have digits after). You may use <code>float_of_string : string -&gt; float</code> to convert strings to floats.`,
      sample: `# get_all_tokens "3.14 100.5 1.414";;\n- : Common.token list = [FLOAT 3.14; FLOAT 100.5; FLOAT 1.414]`,
      sol: `  | numeric+ '.' numeric* as f {FLOAT (float_of_string f)}\n`,
      tests: ["3.14 100.5 1.414", "123.45 00.44 44.00 ", "0. 7. 530.", ".053 530.", "1.5x 2.0;", "3.14*2.=6.28", "1.2.3", "try 9.9 then not"],
    },
    {
      id: "sci", title: "Scientific floats", tag: "MP8.1",
      spec: `Implement floats which support scientific notation. A plain float has a decimal point and at least one digit before it (digits after are optional); it is augmented with an optional exponent part: the character <code>e</code> followed by one or more digits. There is a token <code>FLOAT</code> that takes a float. You may use <code>float_of_string : string -&gt; float</code>.`,
      sample: `# get_all_tokens "2.7e10";;\n- : Common.token list = [FLOAT 27000000000.]`,
      sol: `  | numeric+ '.' numeric* ('e' numeric+)? as f {FLOAT (float_of_string f)}\n`,
      tests: ["2.7e10", "123.456e7890 555.e0 0.e001", "3.14 100.5 1.414", "1.5e", "1.5e3e4", ".5e3", "6.02e23*2.=x"],
    },
    {
      id: "ints", title: "Integers", tag: "MP8.1",
      spec: `Implement (decimal) integers using regular expressions. There is a token constructor <code>INT</code> that takes an integer as an argument. Do not worry about negative integers, but make sure that integers have at least one digit. You may use <code>int_of_string : string -&gt; int</code>.`,
      sample: `# get_all_tokens "42 0 7";;\n- : Common.token list = [INT 42; INT 0; INT 7]`,
      sol: `  | numeric+ as n {INT (int_of_string n)}\n`,
      tests: ["42 0 7", "57 23 197 012983475", "12345s", "s12345", "3*4=12;", "try 1 then 2"],
    },
    {
      id: "binary", title: "Binary numbers", tag: "MP8.1",
      spec: `Implement binary numbers: the prefix <code>0b</code> followed by at least one symbol, each of which is <code>0</code> or <code>1</code>. Use the token constructor <code>INT</code>, which takes a (decimal) integer. You may use <code>int_of_string : string -&gt; int</code> (it understands the <code>0b</code> prefix).`,
      sample: `# get_all_tokens "0b110100101";;\n- : Common.token list = [INT 421]`,
      sol: `  | "0b" ['0' '1']+ as b {INT (int_of_string b)}\n`,
      tests: ["0b110100101", "0b10100011 0b0 0b111 0b0011", "0b1 0b01111111", "0b1010*0b11=0b11110;", "0b1x"],
    },
    {
      id: "hex", title: "Hex numbers", tag: "MP8.1",
      spec: `Implement hexadecimal numbers: the prefix <code>0x</code> followed by at least one symbol, each one of <code>0</code>–<code>9</code> and <code>a</code>–<code>f</code>. Use the token constructor <code>INT</code>, which takes a (decimal) integer. You may use <code>int_of_string : string -&gt; int</code> (it understands the <code>0x</code> prefix).`,
      sample: `# get_all_tokens "0x8844ffaa11";;\n- : Common.token list = [INT 585273158161]`,
      note: `In the browser, OCaml <code>int</code> is 32-bit, so the tests stay below 2<sup>31</sup>; the sample above overflows here.`,
      sol: `  | "0x" ['0'-'9' 'a'-'f']+ as h {INT (int_of_string h)}\n`,
      tests: ["0x1a4 0xff 0x0", "0xdeadbee", "0xabcdef 0x9", "0xabcz", "0x10*0x2=0x20;"],
    },
    {
      id: "keywords", title: "Keywords & operators", tag: "MP8.1", replaces: "keywords",
      spec: `Define all the keywords and operator symbols below. Each is a constructor of <code>token</code>. Each token should have its own rule. Be sure that, for instance, <code>&lt;&gt;</code> is lexed as <code>NEQ</code> and not <code>LT</code> then <code>GT</code> (rules are tried by longest match first, then top to bottom). The six PE3.5 keyword rules are removed from the starter, since they are part of this task.`,
      table: KEYWORD_TABLE,
      sample: `# get_all_tokens "let = in + , ; ( - )";;\n- : Common.token list = [LET; EQUALS; IN; PLUS; COMMA; SEMI; LPAREN; MINUS; RPAREN]`,
      sol: KEYWORD_TABLE.map(([s, c]) => `  | ${JSON.stringify(s)} {${c}}\n`).join(""),
      tests: ["let = in + , ; ( - )", "~ * / +. -. *. /. ^ ", " < > <= >= <> | -> ;;", ":: @ [] rec and end if then",
        "else fun mod raise try with not && ||", "|| ] [ true false () ", "<>= ;;; ->- [ ] ( ) []()", "ifthen truefalse letrec funin", "x_y _ _z"],
    },
    {
      id: "ident", title: "Identifiers", tag: "MP8.1", replaces: "ident",
      spec: `Implement identifiers. An identifier is any sequence of letter and number characters, along with <code>_</code> (underscore) or <code>'</code> (prime), that begins with a lowercase letter. If a token can be matched by two rules, the longest match wins, then the first rule. This applies to identifiers and alphabetic keywords. Use the <code>IDENT</code> constructor, which takes the name as a string.<br><br>Identifiers: <code>asdf1234</code> <code>abcd_</code> <code>a'</code> <code>xABC_d</code> <code>a'b_c'd'''e</code> · Not identifiers: <code>1234asdf</code> <code>_123</code> <code>then</code> <code>'abc</code> <code>A_Nice_Name</code>`,
      sample: `# get_all_tokens "this is where if";;\n- : Common.token list = [IDENT "this"; IDENT "is"; IDENT "where"; IDENT "if"]`,
      sol: IDENT,
      tests: ["this is where it", "asdf1234 abcd_ a' xABC_d a'b_c'd'''e", "n_______'", "i'm identifying_you", "trythen notx then not try",
        "a*b=c;", "Salam", "'alam"],
    },
    {
      id: "boolunit", title: "Bool & unit", tag: "MP8.1",
      spec: `Implement booleans and the unit expression. The relevant constructors are <code>UNIT</code> (for <code>()</code>), <code>TRUE</code> and <code>FALSE</code>.`,
      sample: `# get_all_tokens "true false () true";;\n- : Common.token list = [TRUE; FALSE; UNIT; TRUE]`,
      sol: `  | "true" {TRUE}\n  | "false" {FALSE}\n  | "()" {UNIT}\n`,
      tests: ["true false () true", "true false () truefalse ()()", "trueness falsey", "not true=false;"],
    },
    {
      id: "comments", title: "Comments", tag: "MP8.1",
      spec: `Implement comments. Line comments start with <code>//</code> and go to the end of the line (or end of file). Block comments begin with <code>(*</code>, end with <code>*)</code>, and can be nested. Reaching end of file inside a block comment must <code>raise (Failure "unmatched open comment")</code>; an unmatched <code>*)</code> must <code>raise (Failure "unmatched closed comment")</code> (use these texts verbatim). There is no token for comments. A block comment may contain <code>//</code>, and a line comment may contain <code>(*</code> and <code>*)</code>.<br><br>Hint: a new entry point (taking the depth as an argument) handles block comments; a single regex handles line comments. Put new entry points after the <code>token</code> rule with <code>and</code>.`,
      sample: `# get_all_tokens "this (* is a *) test";;\n- : Common.token list = [IDENT "this"; IDENT "test"]\n# get_all_tokens "this // is a test";;\n- : Common.token list = [IDENT "this"]\n# get_all_tokens "this // is a\\n test";;\n- : Common.token list = [IDENT "this"; IDENT "test"]\n# get_all_tokens "this (* is (* a test *)";;\nException: Failure "unmatched open comment".`,
      sol: `  | "//" [^ '\\n']* { token lexbuf }\n  | "(*" { comment 1 lexbuf }\n  | "*)" { raise (Failure "unmatched closed comment") }\n`,
      solExtra: `and comment depth = parse
  | "(*" { comment (depth + 1) lexbuf }
  | "*)" { if depth = 1 then token lexbuf else comment (depth - 1) lexbuf }
  | eof  { raise (Failure "unmatched open comment") }
  | _    { comment depth lexbuf }
`,
      tests: ["this (* is a *) test", "this // is a test", "this // is a\n test", "this (* is (* a test *)",
        "//(* (*TEst*) \n try", "(* \\ try come \n have \n \\ *) then", "this (* is (* a (* so (* so *) deep *) *) *) test",
        "this is (* a test *) of unmatched *) comments", "a * b", "(**) x (* // *) y", "a *) b", "(* unterminated"],
    },
    {
      id: "strings", title: "Strings", tag: "PE3.5 / MP8.1",
      spec: `Implement strings, using the <code>STRING</code> constructor. A string begins with <code>"</code>, followed by a (possibly empty) sequence of printable characters and escaped sequences, and ends with <code>"</code>.
      <ul>
      <li>Printable: the 52 letters, 10 digits, space, and the 30 special characters other than <code>"</code> and <code>\\</code>.</li>
      <li>Two-character escapes, each converted to the single character: <code>\\\\</code> <code>\\'</code> <code>\\"</code> <code>\\t</code> <code>\\n</code> <code>\\r</code> <code>\\b</code> <code>\\␣</code> (backslash-space).</li>
      <li><code>\\ddd</code>: three digits giving a value 0–255, mapped to that character (<code>\\100</code> is <code>'d'</code>).</li>
      <li><code>\\</code> followed by a newline, then any spaces and tabs at the start of the next line, is ignored.</li>
      <li><code>\\</code> cannot precede any other character.</li>
      </ul>
      A new entry point with an accumulator argument is easiest. Useful: <code>String.make 1 c</code>, <code>char_of_int</code>, <code>int_of_string</code>.`,
      sample: `# get_all_tokens "\\"some string\\"";;\n- : Common.token list = [STRING "some string"]\n# get_all_tokens "\\" she said, \\\\\\"hello\\\\\\"\\"";;\n- : Common.token list = [STRING " she said, \\"hello\\""]\n# get_all_tokens "\\" \\\\100 \\\\001 \\"";;\n- : Common.token list = [STRING " d \\001 "]`,
      sol: `  | '"' { string_rule "" lexbuf }\n`,
      solExtra: `and string_rule acc = parse
  | '"' { STRING acc }
  | [' '-'!' '#'-'[' ']'-'~']+ as s { string_rule (acc ^ s) lexbuf }
  | "\\\\\\\\" { string_rule (acc ^ "\\\\") lexbuf }
  | "\\\\'" { string_rule (acc ^ "'") lexbuf }
  | "\\\\\\"" { string_rule (acc ^ "\\"") lexbuf }
  | "\\\\t" { string_rule (acc ^ "\\t") lexbuf }
  | "\\\\n" { string_rule (acc ^ "\\n") lexbuf }
  | "\\\\r" { string_rule (acc ^ "\\r") lexbuf }
  | "\\\\b" { string_rule (acc ^ "\\b") lexbuf }
  | "\\\\ " { string_rule (acc ^ " ") lexbuf }
  | '\\\\' (numeric numeric numeric as d) {
      let n = int_of_string d in
      if n <= 255 then string_rule (acc ^ String.make 1 (char_of_int n)) lexbuf
      else raise (Failure "bad escape") }
  | '\\\\' '\\n' [' ' '\\t']* { string_rule acc lexbuf }
`,
      tests: ['"some string"', '" she said, \\"hello\\""', '"a line \\n starts here; indent \\t starts here next time"',
        '"Hello, \\n \\ world! "', '" \\100 \\001 "', '" \\171 \\249 "', '" \\281 "', '" \\999 "', '" \\1039 \\2504503 "',
        '" \\ \n \t \\ "', 'then hi "by \\\n \t \\t well"', '"""', ' "some" ; "salam" ', ' "\\6" ', ' "\\x26" ',
        '"1111111111111111111"', '" (*Hello*)" ', '" // " ', '" try not" try not ', '""           ', '"unterminated'],
    },
  ];

  for (const q of QUESTIONS) {
    q.starter = file(TODO(q.title), q.solExtra ? "\n  (* add any new entry points here, with `and` *)\n" : "");
    q.solution = file(q.sol, q.solExtra ? "\n" + q.solExtra : "");
    q.inject = { first: q.replaces === "keywords" ? "" : HIDDEN_KEYWORDS, last: q.replaces === "ident" ? "" : HIDDEN_IDENT };
  }

  root.LEX_QUESTIONS = QUESTIONS;
})(typeof self !== "undefined" ? self : globalThis);

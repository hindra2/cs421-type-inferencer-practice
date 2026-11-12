// Minimal ocamllex: translates a .mll file into OCaml that runs on the Lx_rt
// matcher (longest match, then earliest rule), so lexers run in the browser toplevel.
(function (root) {
  class MllError extends Error {
    constructor(msg, line, col) { super(msg); this.line = line; this.col = col; }
  }

  const ESC = { n: 10, t: 9, b: 8, r: 13, " ": 32, "\\": 92, "'": 39, '"': 34 };

  function tokenize(src) {
    const toks = [];
    let i = 0, line = 1, bol = 0;
    const err = (m, at = i) => { throw new MllError(m, line, at - bol); };
    const adv = () => { if (src[i] === "\n") { line++; bol = i + 1; } i++; };
    const tok = (t, v, start) => toks.push({ t, v, line, col: start - bol });

    function escape() { // at backslash inside a char/string literal; returns char code
      adv();
      const c = src[i];
      if (c in ESC) { adv(); return ESC[c]; }
      if (/[0-9]/.test(c) && /[0-9]{3}/.test(src.substr(i, 3))) {
        const n = parseInt(src.substr(i, 3), 10);
        if (n > 255) err("illegal escape sequence");
        i += 3; return n;
      }
      if (c === "x" && /[0-9a-fA-F]{2}/.test(src.substr(i + 1, 2))) { const n = parseInt(src.substr(i + 1, 2), 16); i += 3; return n; }
      err("illegal escape sequence");
    }
    function comment() { // at "(*"
      const sl = line, sc = i - bol;
      i += 2;
      let depth = 1;
      while (depth > 0) {
        if (i >= src.length) throw new MllError("unterminated comment", sl, sc);
        if (src.startsWith("(*", i)) { depth++; i += 2; }
        else if (src.startsWith("*)", i)) { depth--; i += 2; }
        else if (src[i] === '"') skipOcamlString();
        else adv();
      }
    }
    function skipOcamlString() {
      const sl = line, sc = i - bol;
      adv();
      while (src[i] !== '"') {
        if (i >= src.length) throw new MllError("unterminated string", sl, sc);
        if (src[i] === "\\") adv();
        adv();
      }
      adv();
    }
    function action() { // at "{"; returns OCaml text between the braces
      const sl = line, sc = i - bol;
      adv();
      const start = i, aline = line, acol = i - bol;
      let depth = 1;
      while (true) {
        if (i >= src.length) throw new MllError("unterminated action", sl, sc);
        const c = src[i];
        if (c === "{") { depth++; adv(); }
        else if (c === "}") { if (--depth === 0) break; adv(); }
        else if (c === '"') skipOcamlString();
        else if (src.startsWith("(*", i)) comment();
        else if (c === "'") {
          if (src[i + 1] === "\\") { adv(); while (i < src.length && src[i + 1] !== "'") adv(); adv(); adv(); }
          else if (src[i + 2] === "'" && src[i + 1] !== "\n") { adv(); adv(); adv(); }
          else adv();
        } else adv();
      }
      const text = src.slice(start, i);
      adv();
      return { text, line: aline, col: acol };
    }

    while (i < src.length) {
      const c = src[i], start = i;
      if (/\s/.test(c)) { adv(); continue; }
      if (src.startsWith("(*", i)) { comment(); continue; }
      if (c === "{") { const a = action(); toks.push({ t: "action", v: a, line: a.line, col: a.col }); continue; }
      if (c === "'") {
        adv();
        let code;
        if (src[i] === "\\") code = escape();
        else { if (i >= src.length || src[i] === "\n") err("illegal character literal", start); code = src.charCodeAt(i); adv(); }
        if (src[i] !== "'") err("illegal character literal", start);
        adv(); tok("char", code, start); continue;
      }
      if (c === '"') {
        adv();
        const codes = [];
        while (src[i] !== '"') {
          if (i >= src.length) err("unterminated string", start);
          if (src[i] === "\\") codes.push(escape()); else { codes.push(src.charCodeAt(i)); adv(); }
        }
        adv(); tok("string", codes, start); continue;
      }
      if (/[A-Za-z_]/.test(c)) {
        let j = i; while (j < src.length && /[A-Za-z0-9_']/.test(src[j])) j++;
        const w = src.slice(i, j); i = j;
        if (["rule", "parse", "shortest", "and", "let", "as", "eof"].includes(w)) tok(w, w, start);
        else if (w === "_") tok("_", w, start);
        else tok("ident", w, start);
        continue;
      }
      if ("=|*+?()[]^-#".includes(c)) { adv(); tok(c, c, start); continue; }
      err(`illegal character ${JSON.stringify(c)}`);
    }
    toks.push({ t: "end", line, col: i - bol });
    return toks;
  }

  // regex AST: set (Uint8Array 256) | eps | eof | seq | alt | star | plus | opt | bind
  const setOf = codes => { const s = new Uint8Array(256); for (const c of codes) s[c] = 1; return { t: "set", s }; };

  function parse(toks, inject) {
    let p = 0;
    const peek = () => toks[p];
    const err = (m, k = peek()) => { throw new MllError(m, k.line, k.col); };
    const expect = t => { if (peek().t !== t) err("syntax error"); return toks[p++]; };
    const defs = new Map();
    const startsAtom = k => ["char", "string", "_", "eof", "[", "(", "ident"].includes(k.t);

    function regex() {
      let r = alt();
      while (peek().t === "as") { p++; const id = expect("ident"); r = { t: "bind", name: id.v, r }; }
      return r;
    }
    function alt() {
      let r = seq();
      while (peek().t === "|" && startsAtom(toks[p + 1])) { p++; r = { t: "alt", a: r, b: seq() }; }
      return r;
    }
    function seq() {
      let r = post();
      while (startsAtom(peek())) r = { t: "seq", a: r, b: post() };
      return r;
    }
    function post() {
      let r = hash();
      while (["*", "+", "?"].includes(peek().t)) {
        const op = toks[p++].t;
        r = { t: op === "*" ? "star" : op === "+" ? "plus" : "opt", r };
      }
      return r;
    }
    function hash() {
      let r = atom();
      while (peek().t === "#") {
        const k = toks[p++], b = atom();
        if (r.t !== "set" || b.t !== "set") err("character set expected before and after #", k);
        const s = new Uint8Array(256);
        for (let c = 0; c < 256; c++) s[c] = r.s[c] && !b.s[c] ? 1 : 0;
        r = { t: "set", s };
      }
      return r;
    }
    function atom() {
      const k = peek();
      if (!startsAtom(k)) err("syntax error");
      p++;
      switch (k.t) {
        case "char": return setOf([k.v]);
        case "_": { const s = new Uint8Array(256).fill(1); return { t: "set", s }; }
        case "eof": return { t: "eof" };
        case "string":
          if (!k.v.length) return { t: "eps" };
          return k.v.map(c => setOf([c])).reduce((a, b) => ({ t: "seq", a, b }));
        case "ident":
          if (!defs.has(k.v)) err(`Reference to unbound regexp name \`${k.v}'.`, k);
          return defs.get(k.v);
        case "(": { const r = regex(); expect(")"); return r; }
        case "[": {
          const neg = peek().t === "^" && (p++, true);
          const s = new Uint8Array(256);
          while (peek().t !== "]") {
            const a = peek();
            if (a.t === "char") {
              p++;
              if (peek().t === "-") {
                p++; const b = expect("char");
                for (let c = a.v; c <= b.v; c++) s[c] = 1;
              } else s[a.v] = 1;
            } else if (a.t === "string") { p++; for (const c of a.v) s[c] = 1; }
            else err("syntax error");
          }
          p++;
          if (neg) for (let c = 0; c < 256; c++) s[c] = s[c] ? 0 : 1;
          return { t: "set", s };
        }
      }
    }

    let header = null, trailer = null;
    if (peek().t === "action") header = toks[p++].v;
    while (peek().t === "let") {
      p++;
      const id = expect("ident"); expect("=");
      defs.set(id.v, regex());
    }
    if (peek().t !== "rule") err("syntax error (expected `rule`)");
    const entries = [];
    do {
      p++; // rule / and
      const name = expect("ident").v, args = [];
      while (peek().t === "ident") args.push(toks[p++].v);
      expect("=");
      if (peek().t !== "parse" && peek().t !== "shortest") err("syntax error (expected `parse`)");
      const shortest = toks[p++].t === "shortest";
      if (peek().t === "|") p++;
      const cases = [];
      while (true) {
        const r = regex();
        if (peek().t !== "action") err("syntax error (expected { action })");
        cases.push({ r, action: toks[p++].v });
        if (peek().t === "|") { p++; continue; }
        break;
      }
      if (!entries.length && inject) {
        // hidden rules: `first` go before the student's cases, `last` after them but before a catch-all `_`
        const casesFrom = text => {
          const saved = [toks, p], cs = [];
          toks = tokenize(text); p = 0;
          while (peek().t !== "end") {
            if (peek().t === "|") p++;
            const r = regex();
            if (peek().t !== "action") err("syntax error in provided rules");
            cs.push({ r, action: toks[p++].v });
          }
          [toks, p] = saved;
          return cs;
        };
        const catchAll = cases.findIndex(c => c.r.t === "set" && c.r.s.every(Boolean));
        cases.splice(catchAll < 0 ? cases.length : catchAll, 0, ...casesFrom(inject.last || ""));
        cases.unshift(...casesFrom(inject.first || ""));
      }
      entries.push({ name, args, shortest, cases });
    } while (peek().t === "and");
    if (peek().t === "action") trailer = toks[p++].v;
    if (peek().t !== "end") err("syntax error");
    return { header, entries, trailer };
  }

  // name -> {chr, opt}: char if bound to a single-char set; option if not always bound
  function bindings(r) {
    switch (r.t) {
      case "set": case "eps": case "eof": return new Map();
      case "seq": { const m = bindings(r.a); for (const [k, v] of bindings(r.b)) m.set(k, v); return m; }
      case "alt": {
        const a = bindings(r.a), b = bindings(r.b), m = new Map();
        for (const [k, v] of a) m.set(k, { chr: v.chr && (!b.has(k) || b.get(k).chr), opt: v.opt || !b.has(k) || b.get(k).opt });
        for (const [k, v] of b) if (!a.has(k)) m.set(k, { chr: v.chr, opt: true });
        return m;
      }
      case "star": case "opt": { const m = bindings(r.r); for (const v of m.values()) v.opt = true; return m; }
      case "plus": return bindings(r.r);
      case "bind": { const m = bindings(r.r); m.set(r.name, { chr: r.r.t === "set", opt: false }); return m; }
    }
  }

  function emitRe(r, ids) {
    switch (r.t) {
      case "set": {
        const rs = [];
        for (let c = 0; c < 256; c++) if (r.s[c] && !r.s[c - 1]) { let d = c; while (d < 255 && r.s[d + 1]) d++; rs.push(c, d); }
        return `(Lx_rt.set [${rs.join(";")}])`;
      }
      case "eps": return "Lx_rt.Eps";
      case "eof": return "Lx_rt.E";
      case "seq": return `(Lx_rt.Q (${emitRe(r.a, ids)}, ${emitRe(r.b, ids)}))`;
      case "alt": return `(Lx_rt.A (${emitRe(r.a, ids)}, ${emitRe(r.b, ids)}))`;
      case "star": return `(Lx_rt.K ${emitRe(r.r, ids)})`;
      case "plus": return `(Lx_rt.P ${emitRe(r.r, ids)})`;
      case "opt": return `(Lx_rt.O ${emitRe(r.r, ids)})`;
      case "bind": return `(Lx_rt.B (${ids.get(r.name)}, ${emitRe(r.r, ids)}))`;
    }
  }

  function translate(src, fname = "student.mll", inject = null) {
    let ast;
    try { ast = parse(tokenize(src), inject); }
    catch (e) {
      if (e instanceof MllError) return { ok: false, error: `File "${fname}", line ${e.line}, character ${e.col}: ${e.message}` };
      throw e;
    }
    const at = a => `\n# ${a.line} "${fname}"\n${" ".repeat(a.col)}${a.text}\n`;
    let out = "";
    if (ast.header) out += at(ast.header) + "\n;;\n";
    ast.entries.forEach((en, k) => {
      const binds = en.cases.map(c => bindings(c.r));
      en.binds = binds;
      out += `let lx_tbl_${k} = [|\n` + en.cases.map((c, j) => {
        const ids = new Map([...binds[j].keys()].map((n, i) => [n, i]));
        return "  " + emitRe(c.r, ids);
      }).join(";\n") + `\n|];;\n`;
    });
    ast.entries.forEach((en, k) => {
      out += `${k === 0 ? "let rec" : "and"} ${en.name} ${en.args.join(" ")} lexbuf =\n`;
      out += `  match Lx_rt.exec ${en.shortest ? "true" : "false"} lx_tbl_${k} lexbuf with\n`;
      en.cases.forEach((c, j) => {
        let lets = "";
        [...en.binds[j].entries()].forEach(([n, b], i) => {
          const fn = (b.opt ? "o" : "") + (b.chr ? "chr" : "str");
          lets += `let[@ocaml.warning "-26"] ${n} = Lx_rt.${fn} lexbuf lx_b ${i} in `;
        });
        out += `  | (${j}, lx_b) -> ${lets}(${at(c.action)})\n`;
      });
      out += `  | _ -> assert false\n`;
    });
    out += ";;\n";
    if (ast.trailer) out += at(ast.trailer) + "\n;;\n";
    return { ok: true, code: out };
  }

  root.MLL = { translate };
})(typeof self !== "undefined" ? self : globalThis);

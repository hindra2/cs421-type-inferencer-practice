// Parses CS 421-style regular expressions (E, V, *, +, parens) and right regular grammars,
// and compares languages by DFA equivalence, reporting the shortest strings that differ.
(function (root) {
  class GErr extends Error {}

  function parseRegex(src, alpha) {
    let i = 0;
    const peek = () => { while (i < src.length && /\s/.test(src[i])) i++; return src[i]; };
    const err = m => { throw new GErr(`${m} (at character ${i + 1})`); };
    function alt() {
      let r = seq();
      while (peek() === "V") { i++; r = { t: "alt", a: r, b: seq() }; }
      return r;
    }
    function seq() {
      let r = null;
      for (let c = peek(); c !== undefined && c !== ")" && c !== "V"; c = peek()) {
        const p = post();
        r = r ? { t: "seq", a: r, b: p } : p;
      }
      if (!r) err(peek() === undefined ? "the expression ends where a regular expression was expected" : `expected a regular expression before '${peek()}'`);
      return r;
    }
    function post() {
      let r = atom();
      while (peek() === "*" || peek() === "+") { r = { t: src[i] === "*" ? "star" : "plus", r }; i++; }
      return r;
    }
    function atom() {
      const c = peek();
      if (c === "(") { i++; const r = alt(); if (peek() !== ")") err("missing ')'"); i++; return r; }
      if (c === "E") { i++; return { t: "eps" }; }
      if (c === "*" || c === "+") err(`'${c}' has nothing before it to apply to`);
      if (alpha.includes(c)) { i++; return { t: "ch", c }; }
      err(`'${c}' is not in the alphabet`);
    }
    if (peek() === undefined) err("empty regular expression");
    const r = alt();
    if (peek() !== undefined) err(`unexpected '${peek()}'`);
    return r;
  }

  function nfaFromRegex(ast) {
    const st = [];
    const ns = () => (st.push({ e: [], t: {} }), st.length - 1);
    const ed = (a, c, b) => { if (c === null) st[a].e.push(b); else (st[a].t[c] ||= []).push(b); };
    function frag(r) {
      const s = ns(), f = ns();
      switch (r.t) {
        case "eps": ed(s, null, f); break;
        case "ch": ed(s, r.c, f); break;
        case "seq": { const [a1, a2] = frag(r.a), [b1, b2] = frag(r.b); ed(s, null, a1); ed(a2, null, b1); ed(b2, null, f); break; }
        case "alt": { const [a1, a2] = frag(r.a), [b1, b2] = frag(r.b); ed(s, null, a1); ed(s, null, b1); ed(a2, null, f); ed(b2, null, f); break; }
        case "star": case "plus": {
          const [a1, a2] = frag(r.r);
          ed(s, null, a1); ed(a2, null, a1); ed(a2, null, f);
          if (r.t === "star") ed(s, null, f);
          break;
        }
      }
      return [s, f];
    }
    const [s, f] = frag(ast);
    return { st, start: s, acc: new Set([f]) };
  }

  function tokenizeGrammar(src) {
    const toks = [];
    let i = 0, line = 1;
    while (i < src.length) {
      const c = src[i], rest = src.slice(i);
      let m;
      if (c === "\n") { line++; i++; continue; }
      if (/\s/.test(c)) { i++; continue; }
      if (rest.startsWith("::=")) { toks.push({ t: "def", v: "::=", line }); i += 3; continue; }
      if (c === "|") { toks.push({ t: "bar", v: "|", line }); i++; continue; }
      if ((m = /^Start\s+Symbol\s*=/.exec(rest))) { toks.push({ t: "start", v: "Start Symbol =", line }); line += (m[0].match(/\n/g) || []).length; i += m[0].length; continue; }
      if ((m = /^epsilon(?![A-Za-z0-9_'])/.exec(rest))) { toks.push({ t: "eps", v: "epsilon", line }); i += m[0].length; continue; }
      if ((m = /^[A-Z][A-Za-z0-9_']*/.exec(rest))) { toks.push({ t: "nt", v: m[0], line }); i += m[0].length; continue; }
      toks.push({ t: "term", v: c, line }); i++;
    }
    return toks;
  }

  function parseGrammar(src, alpha) {
    const toks = tokenizeGrammar(src);
    let p = 0, start = null;
    const err = (m, tok) => { throw new GErr(tok ? `line ${tok.line}: ${m}` : m); };
    const readStart = () => {
      const k = toks[p++];
      if (start !== null) err("the start symbol is declared more than once", k);
      const n = toks[p++];
      if (!n || n.t !== "nt") err("expected a nonterminal after 'Start Symbol ='", n || k);
      start = n.v;
    };
    if (!toks.length) err("empty grammar");
    if (toks[0].t === "start") readStart();
    const rules = new Map(), used = [];
    while (p < toks.length && toks[p].t !== "start") {
      const lhs = toks[p];
      if (lhs.t !== "nt" || !toks[p + 1] || toks[p + 1].t !== "def") err(`expected a rule 'Nonterminal ::= ...' but found '${lhs.v}'`, lhs);
      p += 2;
      const alts = [[]];
      while (p < toks.length && toks[p].t !== "start" && !(toks[p].t === "nt" && toks[p + 1] && toks[p + 1].t === "def")) {
        if (toks[p].t === "def") err("'::=' must follow a single nonterminal", toks[p]);
        if (toks[p].t === "bar") alts.push([]); else alts[alts.length - 1].push(toks[p]);
        p++;
      }
      if (!rules.has(lhs.v)) rules.set(lhs.v, []);
      for (const a of alts) {
        const show = `${lhs.v} ::= ${a.map(t => t.v).join(" ")}`;
        if (!a.length) err(`${lhs.v} has an empty alternative (write epsilon for the empty string)`, lhs);
        for (const t of a) if (t.t === "term" && !alpha.includes(t.v)) err(`'${t.v}' in '${show}' is not in the alphabet`, t);
        const shape = a.map(t => t.t).join(" ");
        if (shape === "eps") rules.get(lhs.v).push({ eps: true });
        else if (shape === "term") rules.get(lhs.v).push({ c: a[0].v });
        else if (shape === "term nt") { rules.get(lhs.v).push({ c: a[0].v, n: a[1].v }); used.push(a[1]); }
        else err(`'${show}' is not right regular: each alternative must be epsilon, a terminal, or a terminal followed by a nonterminal`, a[0]);
      }
    }
    if (p < toks.length) {
      if (toks[p].t !== "start") err("unexpected input", toks[p]);
      readStart();
      if (p < toks.length) err("the start symbol must be declared at the very beginning or the very end", toks[p]);
    }
    if (start === null) err("missing start symbol declaration ('Start Symbol = ...' at the beginning or end)");
    if (!rules.has(start)) err(`the start symbol ${start} has no rules`);
    for (const u of used) if (!rules.has(u.v)) err(`nonterminal ${u.v} is used but has no rules`, u);
    return { start, rules };
  }

  function nfaFromGrammar(g) {
    const names = [...g.rules.keys()], idx = new Map(names.map((n, k) => [n, k]));
    const fin = names.length;
    const st = names.map(() => ({ e: [], t: {} })).concat([{ e: [], t: {} }]);
    const acc = new Set([fin]);
    names.forEach((n, k) => {
      for (const r of g.rules.get(n)) {
        if (r.eps) acc.add(k);
        else (st[k].t[r.c] ||= []).push(r.n === undefined ? fin : idx.get(r.n));
      }
    });
    return { st, start: idx.get(g.start), acc };
  }

  function toDfa(N, alpha) {
    const close = set => {
      const out = new Set(set), stack = [...set];
      while (stack.length) for (const x of N.st[stack.pop()].e) if (!out.has(x)) { out.add(x); stack.push(x); }
      return out;
    };
    const key = s => [...s].sort((a, b) => a - b).join(",");
    const first = close([N.start]);
    const ids = new Map([[key(first), 0]]), sets = [first], tr = [], acc = [];
    for (let k = 0; k < sets.length; k++) {
      const s = sets[k];
      acc[k] = [...s].some(x => N.acc.has(x));
      tr[k] = {};
      for (const c of alpha) {
        const nxt = new Set();
        for (const x of s) for (const y of N.st[x].t[c] || []) nxt.add(y);
        const cl = close(nxt), kk = key(cl);
        if (!ids.has(kk)) { ids.set(kk, sets.length); sets.push(cl); }
        tr[k][c] = ids.get(kk);
      }
    }
    return { tr, acc };
  }

  // shortest strings in want-but-not-got ("missing") and got-but-not-want ("extra")
  function compare(want, got, alpha, limit = 4) {
    const seen = new Set(["0,0"]), q = [[0, 0, ""]], missing = [], extra = [];
    while (q.length && (missing.length < limit || extra.length < limit)) {
      const [a, b, s] = q.shift();
      if (want.acc[a] && !got.acc[b] && missing.length < limit) missing.push(s);
      if (!want.acc[a] && got.acc[b] && extra.length < limit) extra.push(s);
      for (const c of alpha) {
        const na = want.tr[a][c], nb = got.tr[b][c], k = na + "," + nb;
        if (!seen.has(k)) { seen.add(k); q.push([na, nb, s + c]); }
      }
    }
    return { equal: !missing.length && !extra.length, missing, extra };
  }

  const accepts = (d, s) => { let k = 0; for (const c of s) { if (!(c in d.tr[k])) return false; k = d.tr[k][c]; } return d.acc[k]; };

  function build(kind, src, alpha) {
    try {
      const nfa = kind === "regex" ? nfaFromRegex(parseRegex(src, alpha)) : nfaFromGrammar(parseGrammar(src, alpha));
      return { ok: true, dfa: toDfa(nfa, alpha) };
    } catch (e) {
      if (e instanceof GErr) return { ok: false, error: e.message };
      throw e;
    }
  }

  root.RX = { build, compare, accepts };
})(typeof self !== "undefined" ? self : globalThis);

(function (root) {
  const m = s => `<i class="math">${s}</i>`;
  const set = cs => `{ ${cs.map(c => `<code>${c}</code>`).join(", ")} }`;

  const QUESTIONS = [
    {
      id: "c-after-a", title: "Every c after an a", source: "Instructor", alphabet: ["a", "b", "c"],
      prompt: `Consider the set of all strings over the alphabet ${set(["a", "b", "c"])} that represent sequences where every c is preceded by at least one a.`,
      examples: `EXAMPLES:<pre class="ex">ac, abcb, aabbcbc, acbbbbcc, aaaaabb.</pre>`,
      accept: ["ac", "abcb", "aabbcbc", "acbbbbcc", "aaaaabb"], reject: ["c", "bc", "bbcab"],
      regex: "(E V b* V (b*a(a* V b* V c*)*))",
      grammar: "S ::= epsilon | bS | aA\nA ::= epsilon | aA | bA | cA\nStart Symbol = S",
    },
    {
      id: "acdb", title: "a ⋯ c ⋯ d ⋯ b", source: "Instructor", alphabet: ["a", "b", "c", "d"],
      prompt: `Consider all strings ${m("w")} over ${set(["a", "b", "c", "d"])} such that there is an ${m("a")} that is eventually followed by a ${m("b")}, and in some such substring ${m("a ⋯ b")} of ${m("w")}, there is a ${m("c")} that is eventually followed by a ${m("d")} in that substring.`,
      examples: `Two examples are ${m("babdcabdab")} and ${m("dcacdadcba")}, and two non-examples are ${m("babdcab")} and ${m("abbacd")}.`,
      accept: ["babdcabdab", "dcacdadcba"], reject: ["babdcab", "abbacd"],
      regex: "(a V b V c V d)* a (a V b V c V d)* c (a V b V c V d)* d (a V b V c V d)* b (a V b V c V d)*",
      grammar: "S ::= aA | bS | cS | dS\nA ::= aA | bA | cB | dA\nB ::= aB | bB | cB | dC\nC ::= aC | bD | cC | dC\nD ::= a | b | c | d | aD | bD | cD | dD | epsilon\n\nStart Symbol = S",
    },
    {
      id: "vectors", title: "Set of 2-D vectors", source: "Instructor", alphabet: ["{", "}", "[", "]", ",", "4", "8"],
      prompt: `Consider the set of all strings over the alphabet ${set(["{", "}", "[", "]", ",", "4", "8"])} which represent a set of two-dimensional vectors. This set is represented in its canonical fashion, where the beginning of a set is denoted by a single open curly brace (<code>{</code>), followed by a series of vectors (explained later) which are comma delimited (<code>,</code>), with the end of the set denoted by a single close curly brace (<code>}</code>). As is usual with sets, there is no comma following the last element in the set. A vector is denoted by an open square bracket (<code>[</code>), then to be followed by one or more of the integers in the alphabet (as the first dimension), with the second dimension denoted by a comma (<code>,</code>), followed by a series of one or more of the integers in the alphabet, with the end of the vector denoted by the close square bracket (<code>]</code>).`,
      examples: `Some example strings are <code>{[48,4]}</code> and <code>{[48,4],[8,8]}</code>, but not <code>{[48,4],}</code> or <code>{[48,4][8,8]}</code>.`,
      accept: ["{[48,4]}", "{[48,4],[8,8]}", "{}"], reject: ["{[48,4],}", "{[48,4][8,8]}", "{[,4]}"],
      regex: "{(EV[(4V8)(4V8)*,(4V8)(4V8)*](,[(4V8)(4V8)*,(4V8)(4V8)*])*)}",
      grammar: "S ::= {F\nF ::= } | [G\nG ::= 4B | 8B\nB ::= 4B | 8B | ,C\nC ::= 4D | 8D\nD ::= 4D | 8D | ]E\nE ::= } | ,H\nH ::= [G\n\nStart Symbol = S",
    },
    {
      id: "c-right-after-a", title: "Every c right after an a", source: "Variant", alphabet: ["a", "b", "c"],
      prompt: `Consider the set of all strings over the alphabet ${set(["a", "b", "c"])} that represent sequences where every c is immediately preceded by an a.`,
      examples: `EXAMPLES:<pre class="ex">ac, bacab, aacbac, bbb, acac.</pre>`,
      accept: ["ac", "bacab", "aacbac", "bbb", "acac", ""], reject: ["c", "abc", "acc", "ca"],
      regex: "(a V b V ac)*",
      grammar: "S ::= epsilon | aS | bS | aC\nC ::= cS\nStart Symbol = S",
    },
    {
      id: "a-then-b", title: "Every a eventually followed by b", source: "Variant", alphabet: ["a", "b", "c"],
      prompt: `Consider the set of all strings over the alphabet ${set(["a", "b", "c"])} that represent sequences where every a is eventually followed by a b.`,
      examples: `EXAMPLES:<pre class="ex">ab, cacb, aacbab, bcc, acccb.</pre>`,
      accept: ["ab", "cacb", "aacbab", "bcc", "acccb", ""], reject: ["a", "aba", "cbca", "bac"],
      regex: "(b V c)* V (a V b V c)* b (b V c)*",
      grammar: "S ::= epsilon | bS | cS | aA\nA ::= aA | cA | bS\nStart Symbol = S",
    },
    {
      id: "ac-no-b", title: "a ⋯ c with no b between", source: "Variant", alphabet: ["a", "b", "c", "d"],
      prompt: `Consider all strings ${m("w")} over ${set(["a", "b", "c", "d"])} such that there is an ${m("a")} that is eventually followed by a ${m("c")}, and in some such substring ${m("a ⋯ c")} of ${m("w")}, there is no ${m("b")}.`,
      examples: `Two examples are ${m("bacdb")} and ${m("dadacb")}, and two non-examples are ${m("abc")} and ${m("cabd")}.`,
      accept: ["bacdb", "dadacb", "ac"], reject: ["abc", "cabd", "abdc"],
      regex: "(a V b V c V d)* a (a V d)* c (a V b V c V d)*",
      grammar: "S ::= aA | bS | cS | dS\nA ::= aA | dA | bS | cF\nF ::= epsilon | aF | bF | cF | dF\nStart Symbol = S",
    },
    {
      id: "bdca", title: "b ⋯ d ⋯ c ⋯ a", source: "Variant", alphabet: ["a", "b", "c", "d"],
      prompt: `Consider all strings ${m("w")} over ${set(["a", "b", "c", "d"])} such that there is a ${m("b")} that is eventually followed by an ${m("a")}, and in some such substring ${m("b ⋯ a")} of ${m("w")}, there is a ${m("d")} that is eventually followed by a ${m("c")} in that substring.`,
      examples: `Two examples are ${m("cbadcba")} and ${m("abdcaab")}, and two non-examples are ${m("bdac")} and ${m("dcbab")}.`,
      accept: ["cbadcba", "abdcaab", "bdca"], reject: ["bdac", "dcbab", "bcda"],
      regex: "(a V b V c V d)* b (a V b V c V d)* d (a V b V c V d)* c (a V b V c V d)* a (a V b V c V d)*",
      grammar: "S ::= aS | bA | cS | dS\nA ::= aA | bA | cA | dB\nB ::= aB | bB | cC | dB\nC ::= aD | bC | cC | dC\nD ::= epsilon | aD | bD | cD | dD\nStart Symbol = S",
    },
    {
      id: "lists", title: "OCaml int lists", source: "Variant", alphabet: ["[", "]", ";", "1", "2"],
      prompt: `Consider the set of all strings over the alphabet ${set(["[", "]", ";", "1", "2"])} which represent OCaml lists of integers. The beginning of a list is denoted by a single open square bracket (<code>[</code>), followed by a series of integers (explained later) which are semicolon delimited (<code>;</code>), with the end of the list denoted by a single close square bracket (<code>]</code>). There is no semicolon following the last element in the list, and the empty list is allowed. An integer is denoted by a series of one or more of the digits in the alphabet.`,
      examples: `Some example strings are <code>[12;1]</code>, <code>[2]</code> and <code>[]</code>, but not <code>[1;]</code> or <code>[;2]</code> or <code>[1;;2]</code>.`,
      accept: ["[12;1]", "[2]", "[]", "[211;1;22]"], reject: ["[1;]", "[;2]", "[1;;2]", "[1", "[[1]]"],
      regex: "[(E V (1V2)+(;(1V2)+)*)]",
      grammar: "S ::= [A\nA ::= ] | 1B | 2B\nB ::= 1B | 2B | ;C | ]\nC ::= 1B | 2B\nStart Symbol = S",
    },
    {
      id: "vectors3", title: "Non-empty set of 3-D vectors", source: "Variant", alphabet: ["{", "}", "[", "]", ",", "4", "8"],
      prompt: `Consider the set of all strings over the alphabet ${set(["{", "}", "[", "]", ",", "4", "8"])} which represent a non-empty set of three-dimensional vectors. The beginning of a set is denoted by a single open curly brace (<code>{</code>), followed by a series of one or more vectors (explained later) which are comma delimited (<code>,</code>), with the end of the set denoted by a single close curly brace (<code>}</code>). There is no comma following the last element in the set. A vector is denoted by an open square bracket (<code>[</code>), followed by three dimensions separated by commas (<code>,</code>), each a series of one or more of the integers in the alphabet, with the end of the vector denoted by the close square bracket (<code>]</code>).`,
      examples: `Some example strings are <code>{[4,8,44]}</code> and <code>{[4,8,4],[88,4,8]}</code>, but not <code>{}</code> or <code>{[4,8]}</code> or <code>{[4,8,4],}</code>.`,
      accept: ["{[4,8,44]}", "{[4,8,4],[88,4,8]}"], reject: ["{}", "{[4,8]}", "{[4,8,4],}", "{[4,8,4,4]}"],
      regex: "{[(4V8)+,(4V8)+,(4V8)+](,[(4V8)+,(4V8)+,(4V8)+])*}",
      grammar: "S ::= {A\nA ::= [B\nB ::= 4C | 8C\nC ::= 4C | 8C | ,D\nD ::= 4E | 8E\nE ::= 4E | 8E | ,F\nF ::= 4G | 8G\nG ::= 4G | 8G | ]H\nH ::= } | ,A\nStart Symbol = S",
    },
    {
      id: "no-aa", title: "No two a's adjacent", source: "Variant", alphabet: ["a", "b"],
      prompt: `Consider the set of all strings over the alphabet ${set(["a", "b"])} that represent sequences where no two a's are adjacent.`,
      examples: `EXAMPLES:<pre class="ex">abab, bbab, a, babba, b.</pre>`,
      accept: ["abab", "bbab", "a", "babba", "b", ""], reject: ["aa", "baab", "abaa"],
      regex: "(b V ab)*(E V a)",
      grammar: "S ::= epsilon | bS | aA\nA ::= epsilon | bS\nStart Symbol = S",
    },
    {
      id: "zero-11", title: "Every 0 followed by 11", source: "Variant", alphabet: ["0", "1"],
      prompt: `Consider the set of all strings over the alphabet ${set(["0", "1"])} that represent sequences where every 0 is immediately followed by at least two 1's.`,
      examples: `EXAMPLES:<pre class="ex">011, 1011, 0111011, 111.</pre>`,
      accept: ["011", "1011", "0111011", "111", ""], reject: ["0", "01", "0011", "0110"],
      regex: "(1 V 011)*",
      grammar: "S ::= epsilon | 1S | 0A\nA ::= 1B\nB ::= 1S\nStart Symbol = S",
    },
    {
      id: "a-mod-3", title: "Number of a's divisible by 3", source: "Variant", alphabet: ["a", "b", "c"],
      prompt: `Consider the set of all strings over the alphabet ${set(["a", "b", "c"])} that represent sequences where the number of a's is divisible by 3.`,
      examples: `EXAMPLES:<pre class="ex">bc, aaa, abacab, cabaab.</pre>`,
      accept: ["bc", "aaa", "abacab", "cabaab", "", "aaaaaa"], reject: ["a", "aa", "abab", "aaaa"],
      regex: "((b V c)* a (b V c)* a (b V c)* a)* (b V c)*",
      grammar: "S ::= epsilon | bS | cS | aA\nA ::= bA | cA | aB\nB ::= bB | cB | aS\nStart Symbol = S",
    },
  ];

  root.RX_QUESTIONS = QUESTIONS;
})(typeof self !== "undefined" ? self : globalThis);

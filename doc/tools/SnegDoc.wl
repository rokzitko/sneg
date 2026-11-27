(* ::Package:: *)

(* SnegDoc: generator of the SNEG documentation notebooks.

   Sources are Markdown files in doc/src (one file per reference page, guide
   page, or tech note). This package parses them, converts inline TeX to
   TraditionalForm boxes, and assembles documentation source notebooks in
   the format of the Wolfram DocumentationTools templates, which
   PacletDocumentationBuild (called by PacletBuild) turns into the final help
   pages. Example outputs are computed by run-examples.wl in a separate kernel
   and cached in doc/cache. See doc/README.md for the source format. *)

BeginPackage["SnegDoc`"];

ParseDocFile::usage = "ParseDocFile[file] parses a documentation source file.";
ParseDocString::usage = "ParseDocString[string] parses documentation source text.";
ExampleBlocks::usage = "ExampleBlocks[doc] gives the code blocks of the examples as {code, reset} pairs, where reset is True if the line counter is reset before the block.";
TeXBoxes::usage = "TeXBoxes[tex] converts a TeX formula to TraditionalForm boxes.";
ReferenceNotebook::usage = "ReferenceNotebook[doc, cache] gives the source notebook of a reference page.";
GuideNotebook::usage = "GuideNotebook[doc] gives the source notebook of a guide page.";
TechNoteNotebook::usage = "TechNoteNotebook[doc, cache] gives the source notebook of a tech note.";
StudyNotebook::usage = "StudyNotebook[doc, cache, chapter] builds a stand-alone notebook from a chapter case study and its cached results.";
TeXExampleBlocks::usage = "TeXExampleBlocks[text] gives the code blocks \\begin{wl} ... \\end{wl} of a manual chapter as {code, reset} pairs; the lines are numbered continuously.";
RunExamples::usage = "RunExamples[{<|\"Name\" -> name, \"File\" -> source, \"Cache\" -> cache|>, ...}] evaluates the examples of the pages with run-examples.wl in parallel kernels and gives True if all succeeded.";
$DocPages::usage = "$DocPages is an association of the documented pages, e.g. \"nc\" -> \"paclet:SNEG/ref/nc\".";
$SnegPublicNames::usage = "$SnegPublicNames is the list of public SNEG symbol names.";
$CodeParser::usage = "$CodeParser[string] gives the boxes of an input string.";
DocError::usage = "DocError[...] is thrown for errors in documentation sources.";

Begin["`Private`"];

$DocPages = <||>;
$SnegPublicNames = {};
$CodeParser = codeBoxesFE;

fail[msg__] := Throw[DocError[StringJoin[ToString /@ {msg}]], DocError];

(* ::Section:: *)
(* Markdown parser *)

ParseDocFile[file_String] := Module[{doc},
  doc = ParseDocString[Import[file, "Text", CharacterEncoding -> "UTF-8"]];
  Append[doc, "File" -> file]];

ParseDocString[text_String] := Module[{lines, meta = <||>, end, title, sections, body},
  lines = StringSplit[StringReplace[text, "\r\n" -> "\n"], "\n", All];
  If[lines =!= {} && StringTrim[First[lines]] === "---",
    end = FirstPosition[Rest[lines], s_String /; StringTrim[s] === "---", None, {1}, Heads -> False];
    If[end === None, fail["unterminated front matter"]];
    end = First[end] + 1;
    meta = parseMeta[lines[[2 ;; end - 1]]];
    lines = Drop[lines, end]];
  {title, body} = splitTitle[lines];
  sections = splitHeadings[body, "## "];
  <|"Meta" -> meta, "Title" -> title, "Intro" -> parseBlocks[sections[[1, 2]]],
    "Sections" -> Association[Map[#[[1]] -> #[[2]] &, Rest[sections]]]|>];

parseMeta[lines_] := Association[Map[Function[l,
  With[{p = StringPosition[l, ":", 1]},
    If[p === {}, fail["bad front matter line: ", l]];
    ToLowerCase[StringTrim[StringTake[l, p[[1, 1]] - 1]]] ->
      StringTrim[StringDrop[l, p[[1, 1]]]]]], Select[lines, StringTrim[#] =!= "" &]]];

metaList[doc_, key_] := Module[{v = Lookup[doc["Meta"], key, ""]},
  Select[StringTrim /@ StringSplit[v, ","], # =!= "" &]];

splitTitle[lines_] := Module[{pos},
  pos = FirstPosition[lines, s_String /; StringStartsQ[s, "# "], None, {1}, Heads -> False];
  If[pos === None, fail["missing title line '# ...'"]];
  pos = First[pos];
  {StringTrim[StringDrop[lines[[pos]], 2]], Drop[lines, pos]}];

(* Split lines at headings with the given prefix, ignoring code fences.
   Gives {{None, lines before the first heading}, {heading, lines}, ...}. *)
splitHeadings[lines_, prefix_] := Module[{out = {}, cur = {}, head = None, fence = False},
  Scan[Function[l,
    If[StringStartsQ[l, "```"], fence = !fence];
    If[!fence && StringStartsQ[l, prefix] && !StringStartsQ[l, prefix <> "#"],
      AppendTo[out, {head, cur}]; head = StringTrim[StringDrop[l, StringLength[prefix]]]; cur = {},
      AppendTo[cur, l]]], lines];
  Append[out, {head, cur}]];

blankQ[l_] := StringTrim[l] === "";
displayQ[l_] := StringStartsQ[StringTrim[l], "$$"];
specialQ[l_] := StringStartsQ[l, "```" | "#" | "|" | "- " | "* "] || displayQ[l] || StringTrim[l] === "---";

parseBlocks[lines_List] := Module[{i = 1, n = Length[lines], out = {}, l, buf, j},
  While[i <= n,
    l = lines[[i]];
    Which[
      blankQ[l], i++,
      StringStartsQ[l, "```"],
        buf = {}; i++;
        While[i <= n && !StringStartsQ[lines[[i]], "```"], AppendTo[buf, lines[[i]]]; i++];
        If[i > n, fail["unterminated code block"]];
        i++;
        AppendTo[out, {"Code", StringRiffle[buf, "\n"]}],
      StringTrim[l] === "---", AppendTo[out, {"Delimiter"}]; i++,
      StringStartsQ[l, "#### "], AppendTo[out, {"Subsection", StringTrim[StringDrop[l, 5]]}]; i++,
      displayQ[l],
        buf = StringTrim[l]; i++;
        While[!StringEndsQ[StringTrim[buf], "$$"] || StringTrim[buf] === "$$",
          If[i > n, fail["unterminated display formula"]];
          buf = buf <> " " <> lines[[i]]; i++];
        AppendTo[out, {"Math", StringTrim[StringTake[StringTrim[buf], {3, -3}]]}],
      StringStartsQ[l, "|"],
        buf = {};
        While[i <= n && StringStartsQ[lines[[i]], "|"], AppendTo[buf, lines[[i]]]; i++];
        AppendTo[out, {"Table", parseTable[buf]}],
      StringStartsQ[l, "- " | "* "],
        buf = StringDrop[l, 2]; i++;
        While[i <= n && StringStartsQ[lines[[i]], "  "] && !blankQ[lines[[i]]] && !displayQ[lines[[i]]],
          buf = buf <> " " <> StringTrim[lines[[i]]]; i++];
        AppendTo[out, {"Bullet", buf}],
      True,
        buf = StringTrim[l]; i++;
        While[i <= n && !blankQ[lines[[i]]] && !specialQ[lines[[i]]],
          buf = buf <> " " <> StringTrim[lines[[i]]]; i++];
        AppendTo[out, {"Para", buf}]]];
  out];

parseTable[rows_] := Module[{cells},
  cells = Map[StringTrim /@ StringSplit[StringTrim[StringTrim[#], "|"], RegularExpression["(?<!\\\\)\\|"]] &, rows];
  cells = Select[cells, !AllTrue[#, StringMatchQ[#, RegularExpression[":?-+:?"]] &] &];
  Map[StringReplace[#, "\\|" -> "|"] &, cells, {2}]];

(* Ordered example sections of a reference page. *)
$exampleSections = {"Basic Examples", "Scope", "Generalizations & Extensions", "Options",
  "Applications", "Properties & Relations", "Possible Issues", "Interactive Examples",
  "Neat Examples"};

exampleSections[doc_] := Module[{ex = Lookup[doc["Sections"], "Examples", {}], parts},
  parts = splitHeadings[ex, "### "];
  If[parseBlocks[parts[[1, 2]]] =!= {}, fail["text before the first example section in ", doc["Title"]]];
  Map[Function[p,
    If[!MemberQ[$exampleSections, p[[1]]], fail["unknown example section: ", p[[1]]]];
    p[[1]] -> parseBlocks[p[[2]]]], Rest[parts]]];

(* Code blocks in document order, with the line-counter resets ($Line = 0)
   at example section headings, subsection headings, and example delimiters
   of reference pages. Tech notes are numbered continuously. *)
ExampleBlocks[doc_] := Module[{out = {}, reset, ref = KeyExistsQ[doc["Sections"], "Examples"]},
  Scan[Function[sec,
    reset = True;
    Scan[Function[b, Switch[First[b],
      "Code", AppendTo[out, {b[[2]], reset}]; reset = False,
      "Delimiter" | "Subsection", reset = True,
      _, Null]], Last[sec]]],
    If[ref, exampleSections[doc],
      (* tech notes: introduction and all sections, in order *)
      Join[{"Intro" -> doc["Intro"]},
        KeyValueMap[#1 -> parseBlocks[#2] &, KeyDrop[doc["Sections"], {"Related Guides", "Related Tech Notes"}]]]]];
  (* tech notes are numbered continuously *)
  If[!ref && out =!= {}, out = MapIndexed[{#1[[1]], #2 === {1}} &, out]];
  out];

(* Manual chapters (LaTeX): code blocks \begin{wl} ... \end{wl}, each on its
   own lines. *)
TeXExampleBlocks[text_String] := MapIndexed[{#1, #2 === {1}} &,
  StringCases[StringReplace[text, "\r\n" -> "\n"],
    RegularExpression["(?ms)^[ \\t]*\\\\begin\\{wl\\}[ \\t]*\n(.*?)\n[ \\t]*\\\\end\\{wl\\}"] :> "$1"]];

(* ::Section:: *)
(* TeX to boxes *)

$texSymbols = <|
  "alpha" -> "\[Alpha]", "beta" -> "\[Beta]", "gamma" -> "\[Gamma]", "delta" -> "\[Delta]",
  "epsilon" -> "\[Epsilon]", "varepsilon" -> "\[CurlyEpsilon]", "zeta" -> "\[Zeta]",
  "eta" -> "\[Eta]", "theta" -> "\[Theta]", "vartheta" -> "\[CurlyTheta]", "iota" -> "\[Iota]",
  "kappa" -> "\[Kappa]", "lambda" -> "\[Lambda]", "mu" -> "\[Mu]", "nu" -> "\[Nu]",
  "xi" -> "\[Xi]", "pi" -> "\[Pi]", "rho" -> "\[Rho]", "sigma" -> "\[Sigma]",
  "tau" -> "\[Tau]", "upsilon" -> "\[Upsilon]", "phi" -> "\[Phi]", "varphi" -> "\[CurlyPhi]",
  "chi" -> "\[Chi]", "psi" -> "\[Psi]", "omega" -> "\[Omega]",
  "Gamma" -> "\[CapitalGamma]", "Delta" -> "\[CapitalDelta]", "Theta" -> "\[CapitalTheta]",
  "Lambda" -> "\[CapitalLambda]", "Xi" -> "\[CapitalXi]", "Pi" -> "\[CapitalPi]",
  "Sigma" -> "\[CapitalSigma]", "Upsilon" -> "\[CapitalUpsilon]", "Phi" -> "\[CapitalPhi]",
  "Psi" -> "\[CapitalPsi]", "Omega" -> "\[CapitalOmega]",
  "dagger" -> "\[Dagger]", "uparrow" -> "\[UpArrow]", "downarrow" -> "\[DownArrow]",
  "langle" -> "\[LeftAngleBracket]", "rangle" -> "\[RightAngleBracket]",
  "cdot" -> "\[CenterDot]", "times" -> "\[Times]", "pm" -> "\[PlusMinus]", "mp" -> "\[MinusPlus]",
  "leq" -> "\[LessEqual]", "le" -> "\[LessEqual]", "geq" -> "\[GreaterEqual]", "ge" -> "\[GreaterEqual]",
  "gg" -> "\[GreaterGreater]", "ll" -> "\[LessLess]",
  "neq" -> "\[NotEqual]", "ne" -> "\[NotEqual]", "approx" -> "\[TildeTilde]", "sim" -> "\[Tilde]",
  "simeq" -> "\[TildeEqual]", "equiv" -> "\[Congruent]", "propto" -> "\[Proportional]",
  "to" -> "\[Rule]", "rightarrow" -> "\[Rule]", "Rightarrow" -> "\[DoubleRightArrow]",
  "leftrightarrow" -> "\[LeftRightArrow]", "mapsto" -> "\[Function]",
  "infty" -> "\[Infinity]", "partial" -> "\[PartialD]", "nabla" -> "\[Del]", "hbar" -> "\[HBar]",
  "ell" -> "\[ScriptL]", "otimes" -> "\[CircleTimes]", "oplus" -> "\[CirclePlus]",
  "sum" -> "\[Sum]", "prod" -> "\[Product]", "int" -> "\[Integral]", "in" -> "\[Element]",
  "ldots" -> "\[Ellipsis]", "dots" -> "\[Ellipsis]", "cdots" -> "\[CenterEllipsis]",
  "prime" -> "\[Prime]", "circ" -> "\[SmallCircle]", "ast" -> "*", "star" -> "\[Star]", "colon" -> ":",
  "mid" -> "|", "vert" -> "|", "Vert" -> "\[DoubleVerticalBar]", "lvert" -> "|", "rvert" -> "|",
  "lbrace" -> "{", "rbrace" -> "}", "{" -> "{", "}" -> "}", "|" -> "\[DoubleVerticalBar]",
  "," -> "\[ThinSpace]", ";" -> "\[MediumSpace]", ":" -> "\[MediumSpace]", "!" -> "", " " -> " ",
  "quad" -> "\[MediumSpace]\[MediumSpace]", "qquad" -> "\[MediumSpace]\[MediumSpace]\[MediumSpace]\[MediumSpace]",
  "forall" -> "\[ForAll]", "exists" -> "\[Exists]", "emptyset" -> "\[EmptySet]",
  "subset" -> "\[Subset]", "cup" -> "\[Union]", "cap" -> "\[Intersection]",
  "uparrow" -> "\[UpArrow]", "downarrow" -> "\[DownArrow]", "updownarrow" -> "\[UpDownArrow]",
  "Uparrow" -> "\[DoubleUpArrow]", "Downarrow" -> "\[DoubleDownArrow]",
  "perp" -> "\[UpTee]", "parallel" -> "\[DoubleVerticalBar]", "angle" -> "\[Angle]",
  "lfloor" -> "\[LeftFloor]", "rfloor" -> "\[RightFloor]", "lceil" -> "\[LeftCeiling]",
  "rceil" -> "\[RightCeiling]", "deg" -> "\[Degree]", "bullet" -> "\[Bullet]",
  "Re" -> "Re", "Im" -> "Im"|>;

(* Functions typeset upright. *)
$texFunctions = {"sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "coth", "exp", "ln",
  "log", "det", "tr", "Tr", "sgn", "max", "min", "lim", "arg", "deg", "dim", "ker", "sup", "inf"};

$texAccents = <|"hat" -> "^", "tilde" -> "~", "bar" -> "\[HorizontalLine]", "dot" -> ".",
  "ddot" -> "..", "vec" -> "\[RightVector]", "overline" -> "\[HorizontalLine]"|>;

$texDelimSizes = {"left", "right", "big", "Big", "bigg", "Bigg", "bigl", "bigr", "Bigl", "Bigr",
  "biggl", "biggr", "Biggl", "Biggr", "displaystyle", "textstyle", "limits", "nolimits"};

texTokenize[s_String] := Module[{chars = Characters[s], i = 1, n, out = {}, c, name, depth, j},
  n = Length[chars];
  While[i <= n,
    c = chars[[i]];
    Which[
      c === "\\",
        If[i == n, fail["TeX: trailing backslash in ", s]];
        If[LetterQ[chars[[i + 1]]],
          j = i + 1; While[j <= n && LetterQ[chars[[j]]], j++];
          name = StringJoin[chars[[i + 1 ;; j - 1]]]; i = j;
          (* raw text arguments *)
          If[MemberQ[{"text", "mathrm", "operatorname", "textrm", "textit", "mathit"}, name],
            While[i <= n && chars[[i]] === " ", i++];
            If[i > n || chars[[i]] =!= "{", fail["TeX: \\", name, " needs a braced argument in ", s]];
            depth = 1; j = i + 1;
            While[j <= n && depth > 0,
              If[chars[[j]] === "{", depth++]; If[chars[[j]] === "}", depth--]; j++];
            If[depth > 0, fail["TeX: unbalanced braces in ", s]];
            AppendTo[out, {"Text", name, StringJoin[chars[[i + 1 ;; j - 2]]]}]; i = j,
            AppendTo[out, {"Cmd", name}]],
          AppendTo[out, {"Cmd", chars[[i + 1]]}]; i += 2],
      c === " " || c === "\n" || c === "\t", i++,
      c === "{" || c === "}" || c === "^" || c === "_", AppendTo[out, {"Sym", c}]; i++,
      DigitQ[c],
        j = i; While[j <= n && (DigitQ[chars[[j]]] || (chars[[j]] === "." && j < n && DigitQ[chars[[j + 1]]])), j++];
        AppendTo[out, {"Atom", StringJoin[chars[[i ;; j - 1]]]}]; i = j,
      c === "'", AppendTo[out, {"Prime"}]; i++,
      True, AppendTo[out, {"Atom", c}]; i++]];
  out];

(* Recursive-descent parser over the token list; $tok and $pos are local to
   TeXBoxes. *)
TeXBoxes[tex_String] := Block[{$tok = texTokenize[tex], $pos = 1, $tex = tex, row},
  row = texSequence[];
  If[$pos <= Length[$tok], fail["TeX: unexpected '}' in ", tex]];
  FormBox[rowBox[row], TraditionalForm]];

rowBox[{x_}] := x;
rowBox[l_List] := RowBox[l];

peek[] := If[$pos <= Length[$tok], $tok[[$pos]], {"End"}];
next[] := With[{t = peek[]}, $pos++; t];

(* As in TeX, adjacent letters are set without space (e.g. the indexes in
   \delta_{ij}): runs of single letters become one italic string. *)
texSequence[] := Module[{out = {}, t, atom},
  While[True,
    t = peek[];
    If[t === {"End"} || t === {"Sym", "}"}, Break[]];
    atom = texAtom[];
    If[atom === None, Continue[]];
    AppendTo[out, texScripts[atom]]];
  Flatten[Replace[Split[out, letterQ[#1] && letterQ[#2] &],
    run : {_, __} /; AllTrue[run, letterQ] :> StyleBox[StringJoin[run], FontSlant -> "Italic"], {1}]]];

letterQ[s_String] := StringLength[s] == 1 && LetterQ[s];
letterQ[_] := False;

texGroup[] := Module[{t = next[], row},
  If[t =!= {"Sym", "{"}, $pos--; Return[texScripts[texAtomNoScripts[]]]];
  row = texSequence[];
  If[next[] =!= {"Sym", "}"}, fail["TeX: missing '}' in ", $tex]];
  rowBox[row]];

(* An argument: a braced group or a single atom without scripts. As in TeX,
   an unbraced argument is a single digit, e.g. \frac12. *)
texArg[] := Module[{t = peek[]},
  Which[
    t === {"Sym", "{"}, texGroup[],
    MatchQ[t, {"Atom", s_String /; StringLength[s] > 1 && DigitQ[StringTake[s, 1]]}],
      $tok[[$pos]] = {"Atom", StringDrop[t[[2]], 1]}; StringTake[t[[2]], 1],
    True, texAtomNoScripts[]]];

texAtomNoScripts[] := Module[{a = texAtom[]},
  If[a === None, fail["TeX: missing argument in ", $tex]]; a];

texScripts[base_] := Module[{sub = None, sup = None, t, primes = 0, b = base},
  While[True,
    t = peek[];
    Which[
      t === {"Sym", "_"}, next[]; If[sub =!= None, fail["TeX: double subscript in ", $tex]]; sub = texArg[],
      t === {"Sym", "^"}, next[]; If[sup =!= None, fail["TeX: double superscript in ", $tex]]; sup = texArg[],
      t === {"Prime"}, next[]; primes++,
      True, Break[]]];
  If[primes > 0, sup = If[sup === None, StringRepeat["\[Prime]", primes], RowBox[{StringRepeat["\[Prime]", primes], sup}]]];
  If[MemberQ[{"\[Sum]", "\[Product]"}, b],
    Which[sub =!= None && sup =!= None, UnderoverscriptBox[b, sub, sup],
      sub =!= None, UnderscriptBox[b, sub], sup =!= None, OverscriptBox[b, sup], True, b],
    Which[sub =!= None && sup =!= None, SubsuperscriptBox[b, sub, sup],
      sub =!= None, SubscriptBox[b, sub], sup =!= None, SuperscriptBox[b, sup], True, b]]];

upright[s_String] := StyleBox[s, FontSlant -> "Plain"];

texAtom[] := Module[{t = next[], name, a, b},
  Switch[First[t],
    "End", fail["TeX: unexpected end of ", $tex],
    "Atom", Last[t],
    "Prime", "\[Prime]",
    "Text", Switch[t[[2]],
      "textit" | "mathit", StyleBox[t[[3]], FontSlant -> "Italic"],
      _, upright[t[[3]]]],
    "Sym", Switch[t[[2]],
      "{", $pos--; texGroup[],
      _, fail["TeX: unexpected '", t[[2]], "' in ", $tex]],
    "Cmd",
      name = t[[2]];
      Which[
        MemberQ[$texDelimSizes, name],
          (* \left. and \right. produce no delimiter *)
          If[peek[] === {"Atom", "."}, next[]; None, None],
        KeyExistsQ[$texSymbols, name], $texSymbols[name],
        MemberQ[$texFunctions, name], upright[name],
        name === "frac" || name === "tfrac" || name === "dfrac", a = texArg[]; b = texArg[]; FractionBox[a, b],
        name === "binom", a = texArg[]; b = texArg[]; TemplateBox[{a, b}, "Binomial"],
        name === "sqrt", SqrtBox[texArg[]],
        name === "mathbf" || name === "bm" || name === "boldsymbol",
          StyleBox[texArg[], FontWeight -> "Bold", FontSlant -> "Plain"],
        name === "mathcal",
          a = texArg[];
          If[!StringQ[a] || StringLength[a] != 1 || !UpperCaseQ[a], fail["TeX: \\mathcal needs a capital letter in ", $tex]];
          ToExpression["\"\\[ScriptCapital" <> a <> "]\""],
        KeyExistsQ[$texAccents, name], OverscriptBox[texArg[], $texAccents[name]],
        name === "underline", UnderscriptBox[texArg[], "_"],
        True, fail["TeX: unknown command \\", name, " in ", $tex]]]];

(* ::Section:: *)
(* Inline markup *)

identifierQ[s_String] := StringMatchQ[s, RegularExpression["[A-Za-z$][A-Za-z0-9$]*"]];
identifierQ[_] := False;

$systemNames := $systemNames = Association[Map[# -> True &, Names["System`*"]]];

symbolLink[s_String] := Which[
  KeyExistsQ[$DocPages, s], ButtonBox[s, BaseStyle -> "Link", ButtonData -> $DocPages[s]],
  KeyExistsQ[$systemNames, s], ButtonBox[s, BaseStyle -> "Link"],
  True, None];

(* Links for documented symbols, upright for other SNEG and System symbols,
   italic (argument style) for all other identifiers. The symbol of the page
   itself is not linked. *)
linkify[boxes_, self_:None] := boxes /. s_String /; identifierQ[s] :> Which[
  s === self, s,
  symbolLink[s] =!= None, symbolLink[s],
  MemberQ[$SnegPublicNames, s], s,
  True, StyleBox[s, "TI"]];

codeBoxesFE[code_String] := Module[{r},
  r = FrontEndExecute[FrontEnd`UndocumentedTestFEParserPacket[code, False]];
  If[!MatchQ[r, {BoxData[_], ___}], fail["cannot parse code: ", code]];
  r[[1, 1]]];

(* Boxes for a code fragment written by the author: a string literal stays a
   string, everything else is parsed. *)
codeBoxes[code_String] := $CodeParser[code];

inlineCode[code_String, self_:None] := Cell[BoxData[linkify[codeBoxes[code], self]], "InlineFormula"];

$inlinePattern = RegularExpression[
  "`[^`]+`|\\$[^$]+\\$|\\*\\*[^*]+\\*\\*|\\*[^*\\s][^*]*\\*|\\[[^\\]]+\\]\\([^)]+\\)"];

inlineText[text_String, self_:None] := Module[{parts},
  parts = DeleteCases[StringSplit[text, x : $inlinePattern :> tok[x]], ""];
  Flatten[Map[Function[p, Switch[p,
    _String, p,
    tok[_], inlineToken[First[p], self]]], parts]]];

inlineToken[x_String, self_] := Which[
  StringStartsQ[x, "`"], inlineCode[StringTake[x, {2, -2}], self],
  StringStartsQ[x, "$"], Cell[BoxData[TeXBoxes[StringTake[x, {2, -2}]]], "InlineFormula"],
  StringStartsQ[x, "**"], StyleBox[StringTake[x, {3, -3}], FontWeight -> "Bold"],
  StringStartsQ[x, "*"], Cell[BoxData[StyleBox[StringTake[x, {2, -2}], "TI"]], "InlineFormula"],
  StringStartsQ[x, "["], Module[{t, u},
    {t, u} = StringCases[x, RegularExpression["\\[([^\\]]+)\\]\\(([^)]+)\\)"] :> {"$1", "$2"}][[1]];
    hyperlink[t, u]],
  True, x];

hyperlink[text_, url_String] := Which[
  StringStartsQ[url, "http"], ButtonBox[text, BaseStyle -> "Hyperlink", ButtonData -> {URL[url], None}],
  StringStartsQ[url, "paclet:"], ButtonBox[text, BaseStyle -> "Link", ButtonData -> url],
  StringStartsQ[url, "ref/" | "guide/" | "tutorial/"], ButtonBox[text, BaseStyle -> "Link", ButtonData -> "paclet:SNEG/" <> url],
  True, fail["unknown link target: ", url]];

textData[l_List] := TextData[l];

(* ::Section:: *)
(* Notebook assembly *)

group[cells_List] := Cell[CellGroupData[cells, Open]];

modInfo[] := Cell["      ", "ModInfo"];

usageCell[blocks_, self_] := Module[{items},
  items = Cases[blocks, {"Bullet", t_} :> t];
  If[items === {}, fail["missing usage for ", self]];
  Cell[TextData[Riffle[Map[Function[t, Module[{m},
      m = StringCases[t, RegularExpression["^`([^`]+)`\\s*(.*)$"] :> {"$1", "$2"}];
      If[m === {}, fail["usage line must start with a `template`: ", t]];
      Join[{Cell["   ", "ModInfo"], inlineCode[m[[1, 1]], self], " "}, inlineText[m[[1, 2]], self]]]], items],
    "\n"] // Flatten], "Usage"]];

notesCells[blocks_, self_, style_:"Notes"] := Flatten[Map[Function[b, Switch[First[b],
  "Bullet" | "Para", Cell[TextData[inlineText[b[[2]], self]], style],
  "Math", Cell[TextData[{Cell[BoxData[TeXBoxes[b[[2]]]], "InlineFormula"]}], style],
  "Table", tableCell[b[[2]], self],
  "Code", Cell[BoxData[codeBoxes[b[[2]]]], "Input"],
  _, fail["unsupported block in notes: ", b]]], blocks]];

tableEntry[s_String, self_] := Module[{m = StringCases[s, RegularExpression["^`([^`]+)`$"] :> "$1"]},
  If[m =!= {}, linkify[codeBoxes[First[m]], self], Cell[TextData[inlineText[s, self]], "TableText"]]];

tableCell[rows_, self_] := Module[{ncol = Length[First[rows]]},
  If[!MemberQ[{2, 3}, ncol] || !AllTrue[rows, Length[#] == ncol &], fail["tables need 2 or 3 columns: ", rows]];
  Cell[BoxData[GridBox[Map[Join[{modInfo[]}, Most[tableEntry[#, self] & /@ #],
      {Cell[TextData[inlineText[Last[#], self]], "TableText"]}] &, rows]]],
    ToString[ncol] <> "ColumnTableMod"]];

(* Output cells from the cache: {"Output", boxes, label} | {"Print", boxes} |
   {"Message", text}. *)
outputCells[res_] := Map[Function[o, Switch[First[o],
  "Output", Cell[BoxData[o[[2]]], "Output", CellLabel -> o[[3]]],
  "Print", Cell[BoxData[o[[2]]], "Print", CellLabel -> "During evaluation of " <> res["InLabel"]],
  "Message", Cell[o[[2]], "Message", "MSG", CellLabel -> "During evaluation of " <> res["InLabel"]],
  _, fail["bad cached output: ", o]]], res["Cells"]];

(* Example cells for a list of blocks. $cache is a list of results consumed in
   order. *)
exampleCells[blocks_, self_] := Module[{out = {}, sub = None, subcells = {}, flush},
  flush[] := If[sub =!= None,
    AppendTo[out, group[Prepend[subcells, exampleHeading[sub, "ExampleSubsection"]]]];
    sub = None; subcells = {}];
  Scan[Function[b, Module[{cell = Switch[First[b],
      "Para" | "Bullet", Cell[TextData[inlineText[b[[2]], self]], "ExampleText"],
      "Math", Cell[TextData[{Cell[BoxData[TeXBoxes[b[[2]]]], "InlineFormula"]}], "ExampleText"],
      "Code", codeGroup[b[[2]]],
      "Delimiter", Cell[BoxData[InterpretationBox[Cell["\t", "ExampleDelimiter"], $Line = 0; ]], "ExampleDelimiter"],
      "Subsection", flush[]; sub = b[[2]]; Nothing,
      "Table", tableCell[b[[2]], self],
      _, fail["unsupported block in examples: ", b]]},
    If[sub === None, AppendTo[out, cell], AppendTo[subcells, cell]]]], blocks];
  flush[];
  out];

(* Section headings reset the line counter. InterpretationBox holds its
   arguments, so the title is inserted with With. *)
exampleHeading[title_String, style_] := With[{t = title},
  Cell[BoxData[InterpretationBox[Cell[t, style], $Line = 0; ]], style]];

codeGroup[code_String] := Module[{res},
  If[$cache === {}, fail["missing cached results; run the examples (doc/build-docs.wl)"]];
  res = First[$cache]; $cache = Rest[$cache];
  If[res["Code"] =!= code, fail["cached results are stale for: ", code]];
  If[res["Cells"] === {},
    Cell[BoxData[codeBoxes[code]], "Input", CellLabel -> res["InLabel"]],
    group[Join[{Cell[BoxData[codeBoxes[code]], "Input", CellLabel -> res["InLabel"]]}, outputCells[res]]]]];

seeAlsoCell[names_] := Cell[TextData[Riffle[Map[Function[n,
    With[{l = symbolLink[n]},
      If[l === None,
        If[!MemberQ[$SnegPublicNames, n], fail["See also: unknown symbol ", n]];
        Print["WARNING: See also: no page yet for ", n]];
      Cell[BoxData[If[l === None, n, l]], "InlineSeeAlsoFunction", TaggingRules -> {"PageType" -> "Function"}]]], names],
    StyleBox[" \[FilledVerySmallSquare] ", "InlineSeparator"]]], "SeeAlso"];

linkCells[items_, style_] := If[items === {}, {Cell["XXXX", style]},
  Map[Function[it, Module[{m = StringCases[it, RegularExpression["^\\[([^\\]]+)\\]\\(([^)]+)\\)$"] :> {"$1", "$2"}]},
    If[m === {}, fail["expected [title](link): ", it]];
    Cell[TextData[hyperlink[m[[1, 1]], m[[1, 2]]]], style]]], items]];

(* Items separated by ';' in front matter fields with links. *)
metaLinks[doc_, key_] := Select[StringTrim /@ StringSplit[Lookup[doc["Meta"], key, ""], ";"], # =!= "" &];

metadataCells[doc_, type_, uri_] := {
  group[{Cell["Metadata", "MetadataSection"],
    Cell[TextData[{"New in: ", Cell[Lookup[doc["Meta"], "new in", "XX"], "HistoryData", CellTags -> "New"],
      " | Modified in: ", Cell[Lookup[doc["Meta"], "modified in", " "], "HistoryData", CellTags -> "Modified"],
      " | Obsolete in: ", Cell[" ", "HistoryData", CellTags -> "Obsolete"],
      " | Excised in: ", Cell[" ", "HistoryData", CellTags -> "Excised"]}], "History"],
    group[{Cell["Categorization", "CategorizationSection"],
      Cell[type, "Categorization", CellLabel -> "Entity Type"],
      Cell["SNEG", "Categorization", CellLabel -> "Paclet Name"],
      Cell["Sneg`", "Categorization", CellLabel -> "Context"],
      Cell["SNEG/" <> uri, "Categorization", CellLabel -> "URI"]}],
    group[Join[{Cell["Keywords", "KeywordsSection"]},
      Map[Cell[#, "Keywords"] &, Replace[metaList[doc, "keywords"], {} -> {"XXXX"}]]]]}]};

notebook[cells_, style_] := Notebook[cells,
  TaggingRules -> <|"Paclet" -> "SNEG"|>,
  StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, style, CharacterEncoding -> "UTF-8"]];

ReferenceNotebook[doc_, cache_List] := Block[{$cache = cache}, Module[
  {self = doc["Title"], secs = doc["Sections"], ex, primary, extended, details, dev, cells},
  If[!identifierQ[self], fail["reference page title must be a symbol name: ", self]];
  details = notesCells[parseBlocks[Lookup[secs, "Details", {}]], self];
  dev = parseBlocks[Lookup[secs, "Developer Notes", {}]];
  If[dev =!= {}, details = Join[details,
    {Cell[TextData[{StyleBox["Developer notes", FontWeight -> "Bold"]}], "Notes"]},
    notesCells[dev, self]]];
  ex = exampleSections[doc];
  primary = Lookup[ex, "Basic Examples", fail["missing Basic Examples in ", self]];
  extended = DeleteCases[ex, "Basic Examples" -> _];
  cells = {
    group[Join[{Cell[self, "ObjectName"], usageCell[parseBlocks[Lookup[secs, "Usage", {}]], self]}, details]],
    group[{Cell["See Also", "SeeAlsoSection"],
      If[metaList[doc, "see also"] === {}, Cell["XXXX", "SeeAlso"], seeAlsoCell[metaList[doc, "see also"]]]}],
    group[Join[{Cell["Tech Notes", "TechNotesSection"]}, linkCells[metaLinks[doc, "tech notes"], "Tutorials"]]],
    group[Join[{Cell["Related Guides", "MoreAboutSection"]},
      linkCells[Replace[metaLinks[doc, "guides"], {} -> {"[SNEG](guide/SNEG)"}], "MoreAbout"]]],
    group[Join[{Cell["Related Links", "RelatedLinksSection"]}, linkCells[metaLinks[doc, "related links"], "RelatedLinks"]]],
    group[{Cell["Examples Initialization", "ExamplesInitializationSection"],
      Cell[BoxData[RowBox[{"Needs", "[", "\"Sneg`\"", "]"}]], "ExampleInitialization"]}],
    group[Join[{Cell[BoxData[InterpretationBox[GridBox[{{StyleBox["Examples", "PrimaryExamplesSection"],
        ButtonBox[RowBox[{RowBox[{"More", " ", "Examples"}], " ", "\[RightTriangle]"}],
          BaseStyle -> "ExtendedExamplesLink", ButtonData :> "ExtendedExamples"]}}], $Line = 0; ]],
        "PrimaryExamplesSection"]}, exampleCells[primary, self]]],
    group[Join[{Cell["More Examples", "ExtendedExamplesSection", CellTags -> "ExtendedExamples"]},
      Map[Function[s, group[Join[{exampleHeading[s[[1]], "ExampleSection"]},
        exampleCells[s[[2]], self]]]], extended]]]};
  If[$cache =!= {}, fail["unused cached results for ", self]];
  notebook[Join[cells, metadataCells[doc, "Symbol", "ref/" <> FileBaseName[doc["File"]]]], "FunctionPageStylesExt.nb"]]];

(* Guide pages: "## heading" starts a function group; bullets "`f` text"
   give a function with a description; bullets with only code items
   "`f`, `g`, ..." give an inline listing; "---" separates groups. The
   sections "Tech Notes", "Related Guides", and "Related Links" contain
   "[title](link)" bullets. *)
GuideNotebook[doc_] := Module[{secs = doc["Sections"], special = {"Tech Notes", "Related Guides", "Related Links"}, body, cells},
  body = Map[Function[h, group[Join[{Cell[h, "GuideFunctionsSubsection"]}, guideItems[parseBlocks[secs[h]]]]]],
    Select[Keys[secs], !MemberQ[special, #] &]];
  cells = {
    group[Join[{Cell[doc["Title"], "GuideTitle"]},
      Map[Cell[TextData[inlineText[#[[2]]]], "GuideAbstract"] &, Cases[doc["Intro"], {"Para", _}]]]],
    group[Join[{Cell["", "GuideFunctionsSection"]}, Riffle[body, Cell["\t", "GuideDelimiter"]]]],
    group[Join[{Cell["Tech Notes", "GuideTutorialsSection"]}, guideLinks[secs, "Tech Notes", "GuideTutorial"]]],
    group[Join[{Cell["Related Guides", "GuideMoreAboutSection"]}, guideLinks[secs, "Related Guides", "GuideMoreAbout"]]],
    group[Join[{Cell["Related Links", "GuideRelatedLinksSection"]}, guideLinks[secs, "Related Links", "GuideRelatedLinks"]]]};
  notebook[Join[cells, metadataCells[doc, "Guide", "guide/" <> Lookup[doc["Meta"], "name", "SNEG"]]], "GuidePageStylesExt.nb"]];

guideLinks[secs_, key_, style_] := Module[{items = Cases[parseBlocks[Lookup[secs, key, {}]], {"Bullet", t_} :> t]},
  If[items === {}, {Cell["XXXX", style]}, linkCells[items, style]]];

guideFunction[name_] := With[{l = symbolLink[name]},
  If[!MemberQ[$SnegPublicNames, name], fail["guide: unknown symbol ", name]];
  Cell[BoxData[If[l === None, name, l]], "InlineGuideFunction"]];

guideItems[blocks_] := Flatten[Map[Function[b, Switch[First[b],
  "Bullet", Module[{t = b[[2]], m},
    If[StringMatchQ[t, RegularExpression["^\\s*`[^`]+`(\\s*,\\s*`[^`]+`)*\\s*$"]],
      Cell[TextData[Riffle[guideFunction /@ StringCases[t, RegularExpression["`([^`]+)`"] :> "$1"],
        StyleBox[" \[FilledVerySmallSquare] ", "InlineSeparator"]]], "InlineGuideFunctionListing"],
      m = StringCases[t, RegularExpression["^`([^`]+)`\\s*(.*)$"] :> {"$1", "$2"}];
      If[m === {}, fail["guide item must start with `name`: ", t]];
      Cell[TextData[Join[{guideFunction[m[[1, 1]]], " \[LongDash] "}, inlineText[m[[1, 2]]]]], "GuideText"]]],
  "Para", Cell[TextData[inlineText[b[[2]]]], "GuideText"],
  "Delimiter", Cell["\t", "GuideDelimiter"],
  _, fail["unsupported block in guide: ", b]]], blocks]];

(* Tech notes: "## heading" gives a section, "### heading" a subsection;
   text, formulas, tables, and evaluated code. *)
TechNoteNotebook[doc_, cache_List] := Block[{$cache = cache}, Module[{secs = doc["Sections"], cells},
  cells = Join[
    {Cell[doc["Title"], "Title"]},
    techCells[doc["Intro"]],
    Flatten[KeyValueMap[Function[{h, lines}, Module[{parts = splitHeadings[lines, "### "]},
      {group[Join[{Cell[h, "Section"]}, techCells[parseBlocks[parts[[1, 2]]]],
        Map[group[Join[{Cell[#[[1]], "Subsection"]}, techCells[parseBlocks[#[[2]]]]]] &, Rest[parts]]]]}]],
      KeyDrop[secs, {"Related Guides", "Related Tech Notes"}]]],
    {group[Join[{Cell["Related Guides", "TutorialMoreAboutSection"]}, guideLinks[secs, "Related Guides", "TutorialMoreAbout"]]],
     group[Join[{Cell["Related Tech Notes", "RelatedTutorialsSection"]}, guideLinks[secs, "Related Tech Notes", "RelatedTutorials"]]]}];
  If[$cache =!= {}, fail["unused cached results for ", doc["Title"]]];
  notebook[Join[cells, metadataCells[doc, "Tech Note", "tutorial/" <> Lookup[doc["Meta"], "name", fail["tech note needs a name"]]]],
    "TechNotePageStylesExt.nb"]]];

techCells[blocks_] := Flatten[Map[Function[b, Switch[First[b],
  "Para" | "Bullet", Cell[TextData[inlineText[b[[2]]]], If[First[b] === "Bullet", "Item", "Text"]],
  "Math", Cell[TextData[{Cell[BoxData[TeXBoxes[b[[2]]]], "InlineFormula"]}], "Text", TextAlignment -> Center],
  "Table", Cell[BoxData[GridBox[Map[Function[r, tableEntry[#, None] & /@ r], b[[2]]]]], "DefinitionBox"],
  "Code", codeGroup[b[[2]]],
  "Delimiter", Nothing,
  "Subsection", Cell[b[[2]], "Subsubsection"],
  _, fail["unsupported block in tech note: ", b]]], blocks]];

StudyNotebook[doc_, cache_, chapter_String] := Block[{$cache = cache}, Module[{cells},
  cells = techCells[doc["Intro"]];
  If[$cache =!= {}, fail["unused outputs in ", doc["Title"]]];
  Notebook[Join[{Cell[doc["Title"], "Title"],
    Cell["Second Quantization with SNEG | " <> chapter, "Subtitle"],
    Cell["Start a fresh Mathematica kernel with SNEG installed. Evaluate the cells in order.", "Text"],
    Cell[BoxData[codeBoxes["Needs[\"sneg`\"]"]], "Input"]}, cells,
    {Cell["Source: Second Quantization with SNEG, SNEG documentation project. Licensed under Creative Commons Attribution 4.0 International (https://creativecommons.org/licenses/by/4.0/).", "Text"]}],
    StyleDefinitions -> "Default.nb", WindowTitle -> doc["Title"]]]];

(* ::Section:: *)
(* Evaluation of examples *)

$runner = FileNameJoin[{DirectoryName[$InputFileName], "run-examples.wl"}];

(* The number of parallel kernels is at most 4, or SNEGDOC_JOBS if set. *)
RunExamples[jobs_List] := Module[{queue = jobs, running = {}, ok = True, done,
    nmax = Replace[Quiet[ToExpression[Environment["SNEGDOC_JOBS"]]],
      {n_Integer /; n > 0 :> n, _ :> Max[1, Min[4, $ProcessorCount - 1]]}]},
  While[queue =!= {} || running =!= {},
    While[queue =!= {} && Length[running] < nmax,
      With[{p = First[queue]},
        Quiet[CreateDirectory[DirectoryName[p["Cache"]], CreateIntermediateDirectories -> True]];
        AppendTo[running, {p, StartProcess[{First[$CommandLine], "-script", $runner, p["File"], p["Cache"]}]}]];
      queue = Rest[queue]];
    Pause[0.5];
    done = Select[running, ProcessStatus[#[[2]]] === "Finished" &];
    Scan[Function[r, Module[{out = ReadString[ProcessConnection[r[[2]], "StandardOutput"]]},
      Print["== ", r[[1]]["Name"]];
      If[StringQ[out], Print[StringTrim[StringReplace[out, RegularExpression["(?m)^sneg [0-9.]+ Copyright.*\n?"] -> ""]]]];
      If[ProcessInformation[r[[2]], "ExitCode"] =!= 0, ok = False; Print["ERROR: examples failed for ", r[[1]]["Name"]]]]], done];
    running = Complement[running, done]];
  ok];

End[];
EndPackage[];

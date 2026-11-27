<|"Source" -> "schriefferwolff.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nH0 = U (hubbard[c[1]] + hubbard[c[2]]);\nV = -t hop[c[1], c[2]];\nP = \
nc[projector1[c[1]], projector1[c[2]]];\nheff = schriefferwolff[H0, V, P, \
4];", "Reset" -> True, "InLabel" -> "In[1]:=", "Cells" -> {}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nExpand[xtospin[fermionstox[#, c -> X], X -> S]] & /@ heff", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
           RowBox[{RowBox[{"-", FractionBox[SuperscriptBox["t", "2"], "U"]}], 
             "+", FractionBox[RowBox[{"4", " ", SuperscriptBox["t", "2"], 
                " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                      TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                       StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], "U"], "+", 
             FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                      TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                       StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], "U"], "+", 
             FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                      TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                       StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], "U"]}], ",", "0", ",", 
           RowBox[{FractionBox[RowBox[{"4", " ", SuperscriptBox["t", "4"]}], 
              SuperscriptBox["U", "3"]], "-", FractionBox[RowBox[{"16", " ", 
                SuperscriptBox["t", "4"], " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"S", RowBox[{"1"}], TagBox[StyleBox["\"z\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"S", RowBox[{"2"}], TagBox[StyleBox["\"z\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SuperscriptBox["U", "3"]], 
             "-", FractionBox[RowBox[{"8", " ", SuperscriptBox["t", "4"], 
                " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                      TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                       StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SuperscriptBox["U", "3"]], 
             "-", FractionBox[RowBox[{"8", " ", SuperscriptBox["t", "4"], 
                " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                      TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                       StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SuperscriptBox["U", 
               "3"]]}]}], "}"}], "Out[9]="}}|>, 
   <|"Code" -> "snegfermionoperators[a, d];\nsnegrealconstants[ed, \
U];\nsnegrealfunctions[eps, v];\nsnegfreeindexes[k];\nH0 = sum[eps[k] \
number[a[k]], {k}] + ed number[d[]] + U hubbard[d[]];\nV = sum[v[k] hop[a[k], \
d[]], {k}];\nheff = schriefferwolff[H0, V, projector1[d[]], 2];", 
    "Reset" -> True, "InLabel" -> "In[1]:=", "Cells" -> {}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nkondo = xtospin[fermionstox[heff[[3]], d -> X], X -> \
S];\nCollect[Select[Expand[kondo /. HoldPattern[sum[x_, _]] :> x], !FreeQ[#, \
S[4]] &], _nc, Simplify]", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["1", "2"], " ", 
         RowBox[{"(", RowBox[{FractionBox["1", RowBox[{"ed", "+", "U", "-", 
                RowBox[{"eps", "[", "k", "]"}]}]], "+", FractionBox["1", 
              RowBox[{RowBox[{"-", "ed"}], "+", RowBox[{"eps", "[", "k", 
                  "]"}]}]], "+", FractionBox["1", RowBox[{"ed", "+", "U", 
                "-", RowBox[{"eps", "[", "k1", "]"}]}]], "+", 
             FractionBox["1", RowBox[{RowBox[{"-", "ed"}], "+", 
                RowBox[{"eps", "[", "k1", "]"}]}]]}], ")"}], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox["\"+\"", 
                 FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 0, 1]] & ]}, "Superscript", SyntaxForm -> 
               SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"a", RowBox[{"k", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"a", RowBox[{"k1", TagBox[
                  StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], " ", RowBox[{"v", "[", "k", "]"}], " ", 
         RowBox[{"v", "[", "k1", "]"}]}], "Out[11]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nsneglatticeindexes[{R}, Ns];\nH0 = U sum[hubbard[c[R]], {R}];\nV = -t \
sum[hop[c[R], c[R + 1]], {R}];\nheff = schriefferwolff[H0, V, {c, \
projector1}, 2];\nsnegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nExpand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", FractionBox[RowBox[{"Ns", " ", SuperscriptBox["t", 
               "2"]}], "U"]}], "+", RowBox[{"sum", "[", 
           RowBox[{RowBox[{FractionBox[RowBox[{"4", " ", SuperscriptBox["t", 
                   "2"], " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", 
                        RowBox[{"R"}], TagBox[StyleBox["\"z\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                       SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"S", RowBox[{RowBox[{"1", "+", "R"}]}], 
                        TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                "U"], "+", FractionBox[RowBox[{"2", " ", SuperscriptBox["t", 
                   "2"], " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", 
                        RowBox[{"R"}], TagBox[StyleBox["\"+\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                       SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"S", RowBox[{RowBox[{"1", "+", "R"}]}], 
                        TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                "U"], "+", FractionBox[RowBox[{"2", " ", SuperscriptBox["t", 
                   "2"], " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", 
                        RowBox[{"R"}], TagBox[StyleBox["\"-\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                       SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"S", RowBox[{RowBox[{"1", "+", "R"}]}], 
                        TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                "U"]}], ",", RowBox[{"{", "R", "}"}]}], "]"}]}], 
       "Out[9]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\nP = nc[projector1[c[1]], projector1[c[2]]];\n{heff, gen} = \
schriefferwolff[U (hubbard[c[1]] + hubbard[c[2]]), -t hop[c[1], c[2]], P, 2, \
\"Generator\" -> True];\ngen[[1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "-", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "-", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "-", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                      FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                      FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "-", 
         FractionBox[RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"]}], 
       "Out[5]="}}|>, <|"Code" -> "Expand[conj[gen[[1]]] + gen[[1]]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U];\neigenoperato\
rs[c[CR, 1, UP] ~nc~ c[AN, 2, UP], U (hubbard[c[1]] + hubbard[c[2]])][[All, \
1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "U", ",", 
           RowBox[{"-", "U"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> 
     "Series[(Sqrt[U^2 + 16 t^2] - U)/2, {t, 0, 4}, Assumptions -> U > 0]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        RowBox[{FractionBox[RowBox[{"4", " ", SuperscriptBox["t", "2"]}], 
           "U"], "-", FractionBox[RowBox[{"16", " ", SuperscriptBox["t", 
              "4"]}], SuperscriptBox["U", "3"]], "+", InterpretationBox[
           SuperscriptBox[RowBox[{"O", "[", "t", "]"}], "5"], 
           SeriesData[t, 0, {}, 2, 5, 1], Editable -> False]}], 
        SeriesData[t, 0, {4/U, 0, -16/U^3}, 2, 5, 1], Editable -> False], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c, \
d];\nsnegrealconstants[v];\nschriefferwolff[0, v hop[c[], d[]], \
projector1[d[]], 2]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "schriefferwolff::res: Resonance: the \
block-off-diagonal component v nc[c[0, 0], d[1, 0]] + v nc[c[0, 1], d[1, 1]] \
+ v nc[d[0, 0], c[1, 0]] + v nc[d[0, 1], c[1, 1]] has a vanishing energy \
denominator, so P and Q are degenerate."}, {"Output", "$Failed", 
       "Out[3]="}}|>, <|"Code" -> "snegrealconstants[U];\nsnegrealconstants[t\
];\nschriefferwolff[U hubbard[c[1]] + t hop[c[1], c[2]], -t hop[c[1], c[2]], \
nc[projector1[c[1]], projector1[c[2]]], 2]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "eigenoperators::diag: H0 \
is not diagonal in the occupation numbers: [H0, c[0, 1, 0]] is not \
proportional to c[0, 1, 0]."}, {"Output", "$Failed", "Out[3]="}}|>}|>

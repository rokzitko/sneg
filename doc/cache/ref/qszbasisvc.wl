<|"Source" -> "qszbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nqszbasisvc[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[
                  RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                      FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "InputForm[%]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["{{{-1, 0}, {vc[0, 0]}}, {{0, \
-1/2}, {vc[0, 1]}}, {{0, 1/2}, {vc[1, 0]}}, {{1, 0}, {vc[1, 1]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1, 0}, {Sneg`vc[0, 0]}}, {{0, -1/2}, {Sneg`vc[0, 1]}}, 
          {{0, 1/2}, {Sneg`vc[1, 0]}}, {{1, 0}, {Sneg`vc[1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{First[#], Length[Last[#]]} & /@ \
qszbasisvc[{c[1], c[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", 
               "2"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox["1", 
                    "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", "4"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"1", ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", "1"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "Total[Length /@ Last /@ qszbasisvc[{c[1], c[2]}]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "16", "Out[3]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[d, c];\nqszbasisvc[{d[], c[1]}] // Length", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "9", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[{f, 1}];\nqszbasisvc[{f[]}] // \
InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{{{-3/2, 0}, {vc[0, 0, \
0]}}, {{-1/2, -1}, {vc[0, 0, 1]}}, {{-1/2, 0}, {vc[0, 1, 0]}}, {{-1/2, 1}, \
{vc[1, 0, 0]}}, {{1/2, -1}, {vc[0, 1, 1]}}, {{1/2, 0}, {vc[1, 0, 1]}}, {{1/2, \
1}, {vc[1, 1, 0]}}, {{3/2, 0}, {vc[1, 1, 1]}}}", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[
         {{{-3/2, 0}, {Sneg`vc[0, 0, 0]}}, {{-1/2, -1}, {Sneg`vc[0, 0, 1]}}, 
          {{-1/2, 0}, {Sneg`vc[0, 1, 0]}}, {{-1/2, 1}, {Sneg`vc[1, 0, 0]}}, 
          {{1/2, -1}, {Sneg`vc[0, 1, 1]}}, {{1/2, 0}, {Sneg`vc[1, 0, 1]}}, 
          {{1/2, 1}, {Sneg`vc[1, 1, 0]}}, {{3/2, 0}, {Sneg`vc[1, 1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> 
     "snegspinlessfermionoperators[a];\nqszbasisvc[{a[1]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{{{-1/2, 0}, {vc[0]}}, {{1/2, 0}, {vc[1]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1/2, 0}, {Sneg`vc[0]}}, {{1/2, 0}, {Sneg`vc[1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nMap[Sort, qszbasisvc[{c[1], c[2]}, \
1/2], {2}] === Map[Sort, qszbasisvc[{c[1], c[2]}], {2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps, \
U];\nmakematricesbzvc[eps number[d[]] + U hubbard[d[]], qszbasisvc[{d[]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "eps", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[{"{", "eps", 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"{", RowBox[{RowBox[{"2", " ", 
                       "eps"}], "+", "U"}], "}"}], "}"}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U, B];\nH = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]] - B \
(spinz[c[1]] + spinz[c[2]]);\nmats = makematricesbzvc[H, qszbasisvc[{c[1], \
c[2]}]];\n{First[#], Eigenvalues[Last[#]]} & /@ Select[mats, First[First[#]] \
== 0 &]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", "1"}]}], "}"}], ",", RowBox[{"{", "B", 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "U", ",", RowBox[{FractionBox["1", 
                      "2"], " ", RowBox[{"(", RowBox[{"U", "-", SqrtBox[
                          RowBox[{RowBox[{"16", " ", SuperscriptBox["t", 
                          "2"]}], "+", SuperscriptBox["U", "2"]}]]}], 
                       ")"}]}], ",", RowBox[{FractionBox["1", "2"], " ", 
                     RowBox[{"(", RowBox[{"U", "+", SqrtBox[RowBox[
                          {RowBox[{"16", " ", SuperscriptBox["t", "2"]}], 
                          "+", SuperscriptBox["U", "2"]}]]}], ")"}]}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", RowBox[
                {"{", RowBox[{"-", "B"}], "}"}]}], "}"}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[d, \
c];\nsnegrealconstants[epsd, U, V];\nH = epsd number[d[]] + U hubbard[d[]] + \
V hop[d[], c[]];\nbz = qszbasisvc[{d[], \
c[]}];\nFirst[Select[makematricesbzvc[H, bz], First[#] == {-1, 1/2} &]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
               FractionBox["1", "2"]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "V"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"V", ",", "epsd"}], "}"}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nqszbasis[{d[]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                      {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[StyleBox[
                   TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"-", RowBox[
                    {"(", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                          {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}]}], 
         "}"}], "Out[2]="}}|>, 
   <|"Code" -> "bzvc2bzop[qszbasisvc[{d[]}]] === qszbasis[{d[]}]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], c[2]}];\nQ = \
number[c[1]] + number[c[2]] - 2;\nSz = spinz[c[1]] + spinz[c[2]];\nAnd @@ \
Flatten[Table[{Expand[ap[Q, v] - sub[[1, 1]] v] === 0, Expand[ap[Sz, v] - \
sub[[1, 2]] v] === 0}, {sub, bz}, {v, sub[[2]]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nMap[Sort, \
mergebasis[{{First[#[[1]]]}, #[[2]]} & /@ qszbasisvc[{c[1], c[2]}]], {2}] === \
Map[Sort, qbasisvc[{c[1], c[2]}], {2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, d];\nbz1 = qszbasisvc[{c[1]}];\nbz2 = \
qszbasisvc[{d[1]}];\n{bz1[[2, 2, 1]], vc2ops[bz1[[2, 2, 1]]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                {"1", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[4]="}}|>}|>

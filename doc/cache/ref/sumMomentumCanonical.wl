<|"Source" -> "sumMomentumCanonical.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k\
, q}, Ns];\nsumMomentumCanonical[sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], \
{k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {RowBox[{"k", "-", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, q}, Ns];\nsumMomentumCanonical[sum[nc[ck[CR, q - k, UP], ck[AN, k, \
UP]], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {RowBox[{RowBox[{"-", "k"}], "+", "q"}], TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, q}, Ns];\nsumMomentumCanonical[sum[blochphase[k, 1] number[ck[k + \
q], UP], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                 RowBox[{"k", "-", "q"}], "\[CenterDot]", "1"}]], 
              DisplayForm], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{p, q}, \
Ns];\nrho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];\nc2 = \
sumSimplifyKD[sumExpand[komutator[rho[q], rho[-q]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"sum", "[", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                      {"p", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "p", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{"p1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"p1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "p1", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "sumMomentumCanonical[c2]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsnegbosonoperators[b, \
bk];\nsnegrealconstants[g];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k, q}, Ns];\nHep = g sum[nc[number[c[R]], b[CR, R] \
+ b[AN, R]], {R}];\nHq = fourier[fourier[Hep, c -> ck, R -> k], b -> bk, R -> \
q]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"g", " ", RowBox[
                 {TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k1", 
                          "-", "q"}], TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k1", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k1", "-", "q"}], 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]], "+", FractionBox[RowBox[{"g", " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k1", 
                          "+", "q"}], TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k1", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k1", "+", "q"}], 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]]}], ",", RowBox[{"{", RowBox[{"k1", ",", "q"}], 
             "}"}]}], "]"}], "Out[7]="}}|>, 
   <|"Code" -> "sumCollect[sumMomentumCanonical[sumExpand[Hq]]]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"g", " ", RowBox[
                 {TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k", "+", "q"}], 
                        TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", 
                          "+", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k", "-", "q"}], 
                        TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", 
                          "-", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "q"}], "}"}]}], "]"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, p, q}, Ns];\nsumCollect[sumMomentumCanonical[sum[number[ck[k + q], \
UP], {k}] - sum[number[ck[p], UP], {p}]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "-", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{"p", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "p", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "sumCollect[sumMomentumCanonical[sum[number[ck[k + q], UP], \
{k}] - (sum[number[ck[p], UP], {p}] /. p -> k)]]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>}|>

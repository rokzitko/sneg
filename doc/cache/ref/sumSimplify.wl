<|"Source" -> "sumSimplify.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
p];\nsnegrealfunctions[eps];\nsumSimplify[komutator[sum[eps[k] number[c[k], \
UP], {k}], c[CR, p, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"eps", "[", "p", "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f];\nsumSimplify[sum[KroneckerDelta[i, j] f[i], {i}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"f", "[", "j", "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{p, q}, \
Ns];\nrho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];\nc2 = \
komutator[rho[q], rho[-q]];\n{sumSimplify[c2], sumFullSimplify[c2]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", RowBox[{"sum", "[", RowBox[
                  {RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"p", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                        "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                   RowBox[{"{", "p", "}"}]}], "]"}]}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                      {"ck", RowBox[{"p1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"p1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                 RowBox[{"{", "p1", "}"}]}], "]"}]}], ",", "0"}], "}"}], 
       "Out[5]="}}|>}|>

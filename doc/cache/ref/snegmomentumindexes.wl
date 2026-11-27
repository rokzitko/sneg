<|"Source" -> "snegmomentumindexes.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegmomentumindexes[{k, p, q}, \
Ns];\n{momentumindexQ[k], latticesize[k], sum[1, {k}]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "Ns", ",", "Ns"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, p, q}, Ns];\nsumSimplifyKD[komutator[ck[AN, p, UP], sum[nc[ck[CR, k \
+ q, UP], ck[AN, k, UP]], {k}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"ck", RowBox[{RowBox[{"p", "-", "q"}], TagBox[StyleBox[
               "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
              StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
        DisplayForm], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[U];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[U sum[hubbard[c[R]], {R}], c -> \
ck, R -> k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"U", " ", 
          RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                   {"ck", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                        FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[
                       {RowBox[{"-", "k"}], "+", "k2", "+", "k3"}], 
                      TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                         RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                 FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                   {"ck", RowBox[{"k2", TagBox[StyleBox["\"\[DownArrow]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                 FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                   {"ck", RowBox[{"k3", TagBox[StyleBox["\"\[UpArrow]\"", 
                        FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm]}], ",", RowBox[{"{", RowBox[
                 {"k", ",", "k2", ",", "k3"}], "}"}]}], "]"}]}], "Ns"], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k}, Ns];\nnc[sum[ck[CR, k, UP], {k}], sum[ck[AN, k, UP], {k}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"k1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "{momentumindexQ[k1], listmomentumindexes}", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{"{", RowBox[{"k", ",", "k1", ",", "k2", ",", "k3", ",", 
               "p", ",", "q"}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k, q}, \
Ns];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[2 k, q] f[k], \
{k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], " ", 
             TemplateBox[{RowBox[{RowBox[{"2", " ", "k"}], ",", "q"}]}, 
              "KroneckerDeltaSeq"]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>}|>

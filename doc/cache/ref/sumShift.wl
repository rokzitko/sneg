<|"Source" -> "sumShift.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k\
, q}, Ns];\nsumShift[sum[number[ck[k + q]], {k}], k -> k - q]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", 
                   RowBox[{"k", TagBox[StyleBox["\"\[DownArrow]\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", 
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
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k, q}, \
Ns];\nsumShift[sum[number[ck[k], UP], {k}], k -> -k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {RowBox[{"-", "k"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"-", "k"}], 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsneglatticeindexes[{R}, \
Ns];\nsneglatticevectors[a1];\nsumShift[sum[hop[c[R], c[R + a1]], {R}], R -> \
R - a1]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"R", TagBox[StyleBox["\"\[DownArrow]\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{RowBox[{RowBox[{"-", 
                         "a1"}], "+", "R"}], TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"R", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{RowBox[{RowBox[{"-", 
                         "a1"}], "+", "R"}], TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                    {RowBox[{RowBox[{"-", "a1"}], "+", "R"}], TagBox[
                      StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                         0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"R", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                    {RowBox[{RowBox[{"-", "a1"}], "+", "R"}], TagBox[
                      StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                         0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"R", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "R", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k, q}, \
Ns];\nrho1 = sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}];\nrho2 = \
sum[nc[ck[CR, k, UP], ck[AN, k - q, UP]], {k}];\nsumShift[rho1, k -> k - q] - \
rho2", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[n];\nsnegrealfunctions[f];\nsumShift[sum[f[n]\
, {n}], n -> n + 1]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "sumShift::index: n is not a declared lattice or momentum index."}, 
      {"Output", RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "n", "]"}], 
           ",", RowBox[{"{", "n", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k}, \
Ns];\nsumShift[sum[number[ck[k]], {k}], k -> 2 k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "sumShift::shift: The \
substitution k -> 2 k is not a shift of the form +-k + const."}, 
      {"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                    {"k", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                        RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                         RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{"k", TagBox[StyleBox["\"\[DownArrow]\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
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
   <|"Code" -> "snegfermionoperators[a];\nordering[a] = \
SEA;\nsnegmomentumindexes[{k, q}, Ns];\nsumShift[sum[number[a[k + q], UP], \
{k}], k -> k - q]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "sumShift::sea: Sums containing SEA-ordered operators are not \
shifted."}, {"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                  {RowBox[{"k", "+", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", RowBox[{RowBox[{"k", "+", 
                     "q"}], TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[a];\nordering[a] = \
SEA;\nsnegmomentumindexes[{k, q}, Ns];\nrho[qq_] := sum[nc[a[CR, k + qq, UP], \
a[AN, k, UP]], {k}];\nsumSimplifyKD[vev[komutator[rho[q], rho[-q]]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k1"}], ")"}], DisplayForm], ",", 
             RowBox[{"{", "k1", "}"}]}], "]"}], "-", 
         RowBox[{"sum", "[", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{RowBox[{"-", "k1"}], "+", "q"}], ")"}], DisplayForm], 
             ",", RowBox[{"{", "k1", "}"}]}], "]"}]}], "Out[5]="}}|>}|>

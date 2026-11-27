<|"Source" -> "sumSimplifyKD.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[i, j] f[i], \
{i}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"f", "[", "j", "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[i, \
sigma];\nsumSimplifyKD[sum[KroneckerDelta[sigma, UP] c[CR, i, sigma], \
{sigma}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"i", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j, l];\nsnegrealfunctions[f, \
g];\n{sumSimplifyKD[sum[KroneckerDelta[i, j] f[i] + g[i], {i}]], \
sumSimplifyKD[sum[KroneckerDelta[i, j] f[i] g[l], {i, l}]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"f", "[", "j", "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "i", "]"}], ",", 
                 RowBox[{"{", "i", "}"}]}], "]"}]}], ",", 
           RowBox[{RowBox[{"f", "[", "j", "]"}], " ", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "l", "]"}], ",", 
                 RowBox[{"{", "l", "}"}]}], "]"}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j, \
l];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[i, j] \
KroneckerDelta[j, l] f[i], {i, j}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"f", "[", "l", "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k, p, q}, \
Ns];\nsnegrealfunctions[f];\n{sumSimplifyKD[sum[KroneckerDelta[p, k + q] \
f[k], {k}]], sumSimplifyKD[sum[KroneckerDelta[p, q - k] f[k], {k}]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"f", "[", RowBox[{"p", "-", "q"}], "]"}], ",", 
           RowBox[{"f", "[", RowBox[{RowBox[{"-", "p"}], "+", "q"}], "]"}]}], 
         "}"}], "Out[3]="}}|>, 
   <|"Code" -> 
     "sumSimplifyKD[sum[KroneckerDelta[q, k - p] f[k, p], {k, p}]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{"f", "[", RowBox[{"k", ",", RowBox[{"k", "-", 
                 "q"}]}], "]"}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[4]="}}|>, <|"Code" -> "sneglatticeindexes[{R, Rp}, \
Ns];\nsnegmomentumindexes[{k, p}, Ns];\n{sumSimplifyKD[sum[blochphase[k - p, \
R], {R}]], sumSimplifyKD[sum[blochphase[k, R - Rp], {k}]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"Ns", " ", TemplateBox[
              {RowBox[{"k", ",", "p"}]}, "KroneckerDeltaSeq"]}], ",", 
           RowBox[{"Ns", " ", TemplateBox[{RowBox[{"R", ",", "Rp"}]}, 
              "KroneckerDeltaSeq"]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "sneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k, q}, \
Ns];\n{KroneckerDelta[R, R + 1], sumSimplifyKD[KroneckerDelta[R, R + 1]], \
sumSimplifyKD[KroneckerDelta[k, k + q]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{TemplateBox[
            {RowBox[{"R", ",", RowBox[{"1", "+", "R"}]}]}, 
            "KroneckerDeltaSeq"], ",", "0", ",", TemplateBox[
            {RowBox[{"0", ",", "q"}]}, "KroneckerDeltaSeq"]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, p, q}, Ns];\nsumSimplifyKD[komutator[ck[AN, p, UP], sum[nc[ck[CR, k \
+ q, UP], ck[AN, k, UP]], {k}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"ck", RowBox[{RowBox[{"p", "-", "q"}], TagBox[StyleBox[
               "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
              StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
        DisplayForm], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsneglatticeindexes[{R, Rp}, Ns];\nsnegmomentumindexes[{k, p}, Ns];\na = \
antikomutator[fourier[c[AN, R, UP], c -> ck, R -> k], fourier[c[CR, Rp, UP], \
c -> ck, R -> p]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox[RowBox[{"sum", "[", 
            RowBox[{RowBox[{TagBox[SuperscriptBox["e", RowBox[
                   {"\[ImaginaryI]", "k", "\[CenterDot]", "R"}]], 
                 DisplayForm], " ", TagBox[SuperscriptBox["e", RowBox[
                   {"\[ImaginaryI]", RowBox[{"-", "p"}], "\[CenterDot]", 
                    "Rp"}]], DisplayForm], " ", RowBox[{"(", RowBox[
                   {TemplateBox[{RowBox[{"k", ",", "p"}]}, 
                     "KroneckerDeltaSeq"], "-", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                  ")"}]}], ",", RowBox[{"{", RowBox[{"k", ",", "p"}], 
                "}"}]}], "]"}], "Ns"], "+", FractionBox[
          RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[SuperscriptBox["e", 
                  RowBox[{"\[ImaginaryI]", "k", "\[CenterDot]", "R"}]], 
                 DisplayForm], " ", TagBox[SuperscriptBox["e", RowBox[
                   {"\[ImaginaryI]", RowBox[{"-", "p"}], "\[CenterDot]", 
                    "Rp"}]], DisplayForm], " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
              RowBox[{"{", RowBox[{"k", ",", "p"}], "}"}]}], "]"}], "Ns"]}], 
       "Out[4]="}}|>, <|"Code" -> "Expand[sumSimplifyKD[sumExpand[a]]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", TemplateBox[{RowBox[{"R", ",", "Rp"}]}, 
        "KroneckerDeltaSeq"], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[l, m, \
sigma];\nsnegrealfunctions[eps];\nsumSimplifyKD[sumExpand[komutator[sum[eps[l\
] number[c[l], sigma], {l, sigma}], c[CR, m, UP]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"m", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"eps", "[", "m", "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f];\ns = sum[(1 + \
KroneckerDelta[i, j]) f[i], {i}];\n{sumSimplifyKD[s], \
sumSimplifyKD[sumExpand[s]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[
                {RowBox[{"f", "[", "i", "]"}], " ", RowBox[{"(", RowBox[
                    {"1", "+", TemplateBox[{RowBox[{"i", ",", "j"}]}, 
                      "KroneckerDeltaSeq"]}], ")"}]}], ",", RowBox[
                {"{", "i", "}"}]}], "]"}], ",", 
           RowBox[{RowBox[{"f", "[", "j", "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "i", "]"}], ",", 
                 RowBox[{"{", "i", "}"}]}], "]"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[n, \
j];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[j, n + 1] f[n], \
{n}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "n", "]"}], " ", 
             TemplateBox[{RowBox[{"j", ",", RowBox[{"1", "+", "n"}]}]}, 
              "KroneckerDeltaSeq"]}], ",", RowBox[{"{", "n", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "sneglatticeindexes[{R, Rp}, \
Ns];\nsumSimplifyKD[sum[KroneckerDelta[Rp, R + 1] f[R], {R}]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"f", "[", RowBox[{RowBox[{"-", "1"}], "+", 
           "Rp"}], "]"}], "Out[5]="}}|>, 
   <|"Code" -> "snegmomentumindexes[{k, q}, \
Ns];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[2 k, q] f[k], \
{k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], " ", 
             TemplateBox[{RowBox[{RowBox[{"2", " ", "k"}], ",", "q"}]}, 
              "KroneckerDeltaSeq"]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "sneglatticeindexes[{R}, \
Ns];\nsneglatticevectors[a1];\nsumSimplifyKD[KroneckerDelta[R + a1, R]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TemplateBox[
        {RowBox[{"0", ",", RowBox[{"-", "a1"}]}]}, "KroneckerDeltaSeq"], 
       "Out[3]="}}|>, <|"Code" -> "snegfreeindexes[i, \
l];\nsnegrealfunctions[f, g];\nsumSimplifyKD[sum[f[i] g[l], {i, l}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], " ", 
             RowBox[{"g", "[", "l", "]"}]}], ",", 
           RowBox[{"{", RowBox[{"i", ",", "l"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "sum[f[i] g[l], {i, l}] /. rulesumFactor", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "i", "]"}], ",", 
             RowBox[{"{", "i", "}"}]}], "]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "l", "]"}], ",", 
             RowBox[{"{", "l", "}"}]}], "]"}]}], "Out[4]="}}|>}|>

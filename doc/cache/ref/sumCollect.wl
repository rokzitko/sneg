<|"Source" -> "sumCollect.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, \
g];\nsnegrealconstants[x];\nsumCollect[2 sum[f[k], {k}] - x sum[g[k], {k}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"2", " ", RowBox[{"f", "[", "k", "]"}]}], 
             "-", RowBox[{"x", " ", RowBox[{"g", "[", "k", "]"}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, \
g];\nsumCollect[sum[f[i, j], {i, j}] + sum[g[i], {i}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{"g", "[", "i", "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", RowBox[{"i", ",", 
                     "j"}], "]"}], ",", RowBox[{"{", "j", "}"}]}], "]"}]}], 
           ",", RowBox[{"{", "i", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f];\nsumCollect[sum[f[\
k], {k}] + 3]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"3", "+", RowBox[{"sum", "[", 
           RowBox[{RowBox[{"f", "[", "k", "]"}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
sigma];\nsumCollect[sum[number[c[k], sigma], {k, sigma}] - sum[1, {k}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"-", "1"}], "+", RowBox[{"sum", "[", RowBox[
                {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                         "sigma"}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"k", "sigma"}], "\"\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], ",", RowBox[{"{", "sigma", 
                   "}"}]}], "]"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, q}, Ns];\ndn = sum[number[ck[k + q], UP], {k}] - sum[number[ck[k], \
UP], {k}];\nsumCollect[dn]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
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
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                 ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{RowBox[{"k", "+", "q"}], TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", "+", "q"}], 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "sumCollect[sumMomentumCanonical[dn]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, \
g];\nsumExpand[sum[f[k] + g[k], {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {RowBox[{"f", "[", "k", "]"}], ",", RowBox[{"{", "k", "}"}]}], 
           "]"}], "+", RowBox[{"sum", "[", RowBox[
            {RowBox[{"g", "[", "k", "]"}], ",", RowBox[{"{", "k", "}"}]}], 
           "]"}]}], "Out[3]="}}|>, <|"Code" -> "sumCollect[%]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], "+", 
             RowBox[{"g", "[", "k", "]"}]}], ",", RowBox[{"{", "k", "}"}]}], 
         "]"}], "Out[4]="}}|>, <|"Code" -> "snegfreeindexes[k, \
l];\nsnegrealfunctions[f, g];\nsumCollect[sum[f[k], {k}] + sum[g[l], {l}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", "]"}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "l", "]"}], ",", 
             RowBox[{"{", "l", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "sumCollect[sum[f[k], {k}] + (sum[g[l], {l}] /. l -> k)]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], "+", 
             RowBox[{"g", "[", "k", "]"}]}], ",", RowBox[{"{", "k", "}"}]}], 
         "]"}], "Out[4]="}}|>}|>

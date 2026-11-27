<|"Source" -> "sumAbstractIndex.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f];\nsumAbstractIndex[sum[f[i, j], {i, j}], idx]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{"f", "[", RowBox[{RowBox[{"idx", "[", "1", "]"}], 
               ",", RowBox[{"idx", "[", "2", "]"}]}], "]"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"idx", "[", "1", "]"}], ",", RowBox[
                {"idx", "[", "2", "]"}]}], "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, p];\ns = \
sum[number[c[k]], {k}] - sum[number[c[p]], {p}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                      {"k", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "-", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "p", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "sumAbstractIndex[s, idx]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[a];\nsnegfreeindexes[l, Q];\nrho[qq_] := \
sum[nc[a[CR, l + qq, UP], a[AN, l, UP]], {l}];\nr = \
sumSimplifyKD[sumExpand[komutator[rho[Q], rho[-Q]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"sum", "[", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                      {"l", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", RowBox[{"l", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "l", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", RowBox[{"l1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"a", RowBox[{"l1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "l1", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "sumNameIndex[sumAbstractIndex[r, idx], idx, {l}]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, \
g];\nsumAbstractIndex[sum[f[i] g[j], {i, j}] - sum[f[j] g[i], {i, j}], idx]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"sum", "[", RowBox[{RowBox[
                {RowBox[{"f", "[", RowBox[{"idx", "[", "2", "]"}], "]"}], 
                 " ", RowBox[{"g", "[", RowBox[{"idx", "[", "1", "]"}], 
                   "]"}]}], ",", RowBox[{"{", RowBox[{RowBox[{"idx", "[", 
                     "1", "]"}], ",", RowBox[{"idx", "[", "2", "]"}]}], 
                 "}"}]}], "]"}]}], "+", RowBox[{"sum", "[", 
           RowBox[{RowBox[{RowBox[{"f", "[", RowBox[{"idx", "[", "1", "]"}], 
                 "]"}], " ", RowBox[{"g", "[", RowBox[{"idx", "[", "2", 
                   "]"}], "]"}]}], ",", RowBox[{"{", RowBox[
                {RowBox[{"idx", "[", "1", "]"}], ",", RowBox[{"idx", "[", 
                   "2", "]"}]}], "}"}]}], "]"}]}], "Out[3]="}}|>}|>

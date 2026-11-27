<|"Source" -> "sneglinearoperatorFirst.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
sneglinearoperatorFirst[ev];\nev[3 + t number[c[1]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"3", "+", RowBox[{"t", " ", 
           RowBox[{"(", RowBox[{RowBox[{"ev", "[", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], 
               "+", RowBox[{"ev", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                      {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                 "]"}]}], ")"}]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsnegrealfuncti\
ons[eps, nF];\nsneglinearoperatorFirst[ev];\nev[nc[c[CR, i_, s_], c[AN, i_, \
s_]]] := nF[eps[i]];\nev[sum[a_, it_List]] := sum[ev[a], it];\nev[sum[eps[k] \
number[c[k]], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", RowBox[{"sum", "[", 
           RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", RowBox[
                {"nF", "[", RowBox[{"eps", "[", "k", "]"}], "]"}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\nvev[t + 2 \
nc[c[AN, 1, UP], c[CR, 1, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", "+", "t"}], "Out[3]="}}|>, 
   <|"Code" -> "sneglinearoperator[g];\nsneglinearoperatorFirst[h];\n{g[2], \
h[2], g[2, b], h[2, b]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"2", " ", RowBox[{"g", "[", "]"}]}], ",", "2", 
           ",", RowBox[{"2", " ", RowBox[{"g", "[", "b", "]"}]}], ",", 
           RowBox[{"2", " ", RowBox[{"h", "[", "b", "]"}]}]}], "}"}], 
       "Out[3]="}}|>}|>

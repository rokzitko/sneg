<|"Source" -> "snegAssuming.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[k];\n{k > 0, snegAssuming[k /: k \
> 0 = True, k > 0], k > 0}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"k", ">", "0"}], ",", 
           "True", ",", RowBox[{"k", ">", "0"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfreeindexes[k, q];\nsnegAssuming[{k /: k > 0 = True, q /: \
q < 0 = True}, {k > 0, q < 0}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k];\n{isannihilation[f[AN, k, UP]], snegAssuming[k /: k \
> 0 = True, isannihilation[f[AN, k, UP]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"k", ">", "0"}], ",", "True"}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "{zeroonvac[nc[f[CR, 2, UP], f[AN, \
k, UP]]], snegAssuming[k /: k > 0 = True, zeroonvac[nc[f[CR, 2, UP], f[AN, k, \
UP]]]]}", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"If", "[", RowBox[{RowBox[{"k", ">", "0"}], 
               ",", "0", ",", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"f", RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "]"}], 
           ",", "0"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegAssuming[k /: k > 0 = True, {k > 0, \
k < 0, k >= 0, UnitStep[k]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{"k", "<", "0"}], ",", RowBox[{"k", "\[GreaterEqual]", 
             "0"}], ",", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], "(", "k", ")"}], DisplayForm]}], 
         "}"}], "Out[2]="}}|>}|>

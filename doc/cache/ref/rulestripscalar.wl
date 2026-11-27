<|"Source" -> "rulestripscalar.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
p];\nsnegrealfunctions[eps];\ne = sumSimplifyKD[sum[scalar[eps[k] \
KroneckerDelta[k, p]] c[CR, k, UP], {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"p", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"scalar", "[", RowBox[{"eps", "[", "p", "]"}], "]"}]}], 
       "Out[4]="}}|>, <|"Code" -> "e /. rulestripscalar", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"p", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"eps", "[", "p", "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegrealconstants[t];\n2 scalar[t] + 3 scalar[t^2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"scalar", "[", 
         RowBox[{RowBox[{"2", " ", "t"}], "+", RowBox[{"3", " ", 
             SuperscriptBox["t", "2"]}]}], "]"}], "Out[2]="}}|>}|>

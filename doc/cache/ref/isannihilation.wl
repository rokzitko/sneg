<|"Source" -> "isannihilation.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{isannihilation[c[AN, \
1, UP]], isannihilation[c[CR, 1, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\nordering[d] = \
SEA;\n{isannihilation[d[AN, 2, UP]], isannihilation[d[CR, -2, UP]], nc[d[CR, \
-2, UP], VACUUM]}", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "0"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nzeroonvac[nc[c[CR, 1, UP], c[AN, 2, \
UP]] + c[CR, 3, UP]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"3", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[2]="}}|>}|>

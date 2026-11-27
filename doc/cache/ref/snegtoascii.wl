<|"Source" -> "snegtoascii.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\nsnegtoascii[nc[c[CR, 1, UP], c[AN, 2, DO]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"c+(1,1)c(2,0)\"", "Out[2]="}}|>, 
   <|"Code" -> "asciitosneg[%]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                 "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegtoascii[nc[c[CR, 1, UP], c[CR, \
2, DO], ket[0]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"c+(1,1)c+(2,0)|0>\"", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\nsnegtoascii[\
t nc[c[CR, 1, UP], c[AN, 2, UP]] - c[CR, 3, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "\"c+(3,1)\""}], "+", 
         RowBox[{"\"c+(1,1)c(2,1)\"", " ", "t"}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegtoascii[nc[a[CR], a[AN]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"a+()a()\"", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nx = 2 nc[c[CR, 1, UP], c[CR, 2, \
DO], c[AN, 3, UP]] - c[CR, 3, UP];\nasciitosneg[snegtoascii[x]] == x", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegtoascii[nc[c[CR, 1, UP], u]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"\"c+(1,1)\"", StyleBox["\[CenterDot]", 
          FontColor -> RGBColor[0, 1, 0]], "u"}], "Out[2]="}}|>}|>

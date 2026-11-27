<|"Source" -> "op2ndx.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, d];\nmakebasis[{c[], \
d[]}];\n{op2ndx[c[CR, UP]], op2ndx[c[AN, DO]], op2ndx[d[CR, UP]], \
op2ndx[d[AN, DO]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "2", ",", "3", ",", 
           "4"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c, d];\nmakebasis[{c[], d[]}];\nop2ndx[BASIS]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "2", ",", "3", ",", 
           "4"}], "}"}], "Out[3]="}}|>, <|"Code" -> "op2ndx[c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"op2ndx", "[", 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "]"}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c, \
d];\nmakebasis[{c[], d[]}];\nndx2op[op2ndx[d[AN, DO]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[StyleBox[TemplateBox[
          {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                  1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[3]="}}|>}|>

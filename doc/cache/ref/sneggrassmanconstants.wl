<|"Source" -> "sneggrassmanconstants.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "sneggrassmanconstants[x, y];\n{nc[y, x], nc[x, x], conj[x]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{"x", StyleBox[
                  "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], "y"}], 
               ")"}]}], ",", "0", ",", OverscriptBox["x", "_"]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "sneggrassmanconstants[x];\nsnegfermionoper\
ators[c];\nsnegbosonoperators[a];\n{nc[c[CR, 1, UP], x], nc[a[CR], x]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{"x", StyleBox[
                  "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], ")"}]}], ",", 
           RowBox[{"x", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                0]], TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x, y];\nconj[nc[x, y]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{OverscriptBox["x", "_"], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             OverscriptBox["y", "_"]}], ")"}]}], "Out[2]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x];\nsnegfermionoperators[c];\ncoh = \
nc[1 - nc[x, c[CR, UP]], VACUUM];\nnc[c[AN, UP], coh] - nc[x, coh]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x, \
y];\nsnegfermionoperators[c];\n{isnumericQ[x], nc[c[CR, 1, UP], x, y] - nc[x, \
y, c[CR, 1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"False", ",", "0"}], "}"}], 
       "Out[3]="}}|>}|>

<|"Source" -> "invertspin.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\ninvertspin[nc[c[CR, 1, UP], c[AN, 2, DO]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nExpand[invertspin[spinz[c[1]]] + \
spinz[c[1]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[invertspin[spinplus[c[1]]] - \
spinminus[c[1]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\[Sigma]];\ninverts\
pin[c[CR, 1, \[Sigma]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"1", RowBox[{"1", "-", "\[Sigma]"}]}], 
           "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
           SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, U, B];\nH = -t \
hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);\nZ = B (spinz[c[1]] + \
spinz[c[2]]);\n{Expand[invertspin[H] - H], Expand[invertspin[Z] + Z]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nx = nc[c[CR, 1, \
UP], c[CR, 2, DO], c[AN, 3, UP]];\ninvertspin[invertspin[x]] == x", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[{f, 3/2}];\ninvertspin[f[CR, 1, 3/2]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"f", RowBox[{"1", RowBox[{"-", FractionBox["1", "2"]}]}], 
           "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
           SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\ninvertspin[a[CR]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"invertspin", "[", 
         TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "]"}], 
       "Out[2]="}}|>}|>

<|"Source" -> "ops2vc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nops2vc[nc[c[CR, 2, DO], c[CR, 1, UP]]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["-vc[1, 0, 0, 1]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[-Sneg`vc[1, 0, 0, 1]], Editable -> True, 
        AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "s = (nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, \
2, UP]])/Sqrt[2];\nops2vc[s] // InputForm", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox[
         "(-vc[0, 1, 1, 0] + vc[1, 0, 0, 1])/Sqrt[2]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[(-Sneg`vc[0, 1, 1, 0] + Sneg`vc[1, 0, 0, 1])/Sqrt[2]], 
        Editable -> True, AutoDelete -> True], "Out[5]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nops2vc[c[CR, 2, UP], vc[1, 0, 0, 0]] // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["-vc[1, 0, 1, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[-Sneg`vc[1, 0, 1, 0]], Editable -> True, 
        AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvc2ops[ops2vc[nc[c[CR, 2, DO], c[CR, 1, UP]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"-", RowBox[{"(", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[3]="}}|>}|>

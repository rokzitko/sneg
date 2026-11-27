<|"Source" -> "makebasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "InputForm[BASIS]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox[
         "{c[0, 1, 1], c[0, 1, 0], c[0, 2, 1], c[0, 2, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{c[0, 1, 1], c[0, 1, 0], c[0, 2, 1], c[0, 2, 0]}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> 
     "{vacuum[], ap[nc[c[CR, 1, UP], c[CR, 2, DO]], vacuum[]]} // InputForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{vc[0, 0, 0, 0], vc[1, 0, 0, 1]}", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[{Sneg`vc[0, 0, 0, 0], 
          Sneg`vc[1, 0, 0, 1]}], Editable -> True, AutoDelete -> True], 
       "Out[4]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[d, c];\nmakebasis[d[], c[1]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{d[0, 1], d[0, 0], c[0, 1, 1], c[0, 1, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{d[0, 1], d[0, 0], c[0, 1, 1], c[0, 1, 0]}], 
        Editable -> True, AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[{f, 1}];\nmakebasis[{f[]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{f[0, 1], f[0, 0], f[0, -1]}", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[{f[0, 1], f[0, 0], 
          f[0, -1]}], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, <|"Code" -> "snegspinlessfermionoperators[a]\
;\nmakebasis[{a[1], a[2]}] // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["{a[0, 1, 0], a[0, 2, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{a[0, 1, 0], a[0, 2, 0]}], Editable -> True, 
        AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps, \
U];\nmakebasis[{d[]}];\nstates = vc @@@ Tuples[{0, 1}, Length[BASIS]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "matrixrepresentationvc[eps number[d[]] + U hubbard[d[]], \
states] // MatrixForm", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"0", "0", "0", "0"}, {"0", "eps", "0", "0"}, 
            {"0", "0", "eps", "0"}, {"0", "0", "0", 
             RowBox[{RowBox[{"2", " ", "eps"}], "+", "U"}]}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[5]//MatrixForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nqszbasisvc[{c[7]}];\nBASIS", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"7", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"7", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\n{op2ndx[c[AN, 2, UP]], ndx2op[2]} // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["{3, c[0, 1, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{3, c[0, 1, 0]}], Editable -> True, AutoDelete -> True], 
       "Out[3]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nmakebasis[{c[CR, 1, UP]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{c[0, 0, 1, 1, 1], c[0, 0, 1, 1, 0]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{c[0, 0, 1, 1, 1], c[0, 0, 1, 1, 0]}], Editable -> True, 
        AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c, d];\nmakebasis[{c[1]}];\nv = ap[c[CR, \
1, UP], vacuum[]];\nmakebasis[{d[1]}];\nvc2ops[v]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[StyleBox[TemplateBox[
          {"d", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[5]="}}|>}|>

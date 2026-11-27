<|"Source" -> "vc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "states = {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
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
                   0, 0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "InputForm[states]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[0, 0], Sneg`vc[1, 0], Sneg`vc[0, 1], 
          Sneg`vc[1, 1]}], Editable -> True, AutoDelete -> True], 
       "Out[4]//InputForm="}}|>, 
   <|"Code" -> "ap[nc[d[CR, UP], d[CR, DO]], vacuum[]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox[
            "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
              0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[5]="}}|>, 
   <|"Code" -> "ap[nc[d[CR, DO], d[CR, UP]], vacuum[]] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["-vc[1, 1]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[-Sneg`vc[1, 1]], Editable -> True, AutoDelete -> True], 
       "Out[6]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nBASIS", 
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
       "Out[3]="}}|>, 
   <|"Code" -> "ap[c[CR, 1, DO], vc[1, 0, 1, 0]] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["-vc[1, 1, 1, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[-Sneg`vc[1, 1, 1, 0]], Editable -> True, 
        AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nsinglet = \
(vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];\n{normvc[singlet], \
expvvc[spinss[c[1]] + spinss[c[2]] + 2 spinspin[c[1], c[2]], singlet]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}]\
;\nv = ap[ket[1/2], vc[1, 0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
               FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                 RGBColor[1, 0.5, 0]] & ], RowBox[{FractionBox["1", "2"]}], 
             TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                  0.5, 0]] & ]}], DisplayForm], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "InputForm[v]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[1, 0, ket[1/2]]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[1, 0, Sneg`ket[1/2]]], Editable -> True, 
        AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nstates = {vc[0, \
0], vc[1, 0], vc[0, 1], vc[1, 1]};\nOuter[scalarproductvc, states, states]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0", ",", "0", ",", 
               "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "1", ",", 
               "0", ",", "0"}], "}"}], ",", RowBox[
            {"{", RowBox[{"0", ",", "0", ",", "1", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "1"}], "}"}]}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "vc2ops[vc[1, 1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[1]="}}|>, <|"Code" -> "ops2vc[vc2ops[vc[1, 1]]] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[1, 1]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[1, 1]], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, 
   <|"Code" -> "N[vc[1, 0]/Sqrt[2]] // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["0.7071067811865475*vc[1, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[0.7071067811865475*Sneg`vc[1, 0]], Editable -> True, 
        AutoDelete -> True], "Out[1]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c, d];\nmakebasis[{c[]}];\nvc2ops[vc[1, 0]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "makebasis[{d[]}];\nvc2ops[vc[1, 0]]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", TagBox[StyleBox[TemplateBox[
          {"d", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[5]="}}|>, 
   <|"Code" -> "vc2ops[vc[1, 0, 0]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", "vc2ops::incom: Incompatible lenghts of vc[] and BASIS."}, 
      {"Output", "\[Infinity]", "Out[1]="}}|>}|>

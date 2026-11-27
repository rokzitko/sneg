<|"Source" -> "ap.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvac = vacuum[];\nap[c[CR, 1, UP], vac] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[1, 0, 0, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[1, 0, 0, 0]], Editable -> True, 
        AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "ap[c[AN, 1, UP], vac]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "ap[nc[c[CR, 2, UP], c[CR, 1, DO]], vac] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["-vc[0, 1, 1, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[-Sneg`vc[0, 1, 1, 0]], Editable -> True, 
        AutoDelete -> True], "Out[6]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nv = vc[1, \
1, 1, 0];\n{ap[number[c[1]], v], ap[spinz[c[2]], v]} // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{2*vc[1, 1, 1, 0], vc[1, 1, 1, 0]/2}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{2*Sneg`vc[1, 1, 1, 0], Sneg`vc[1, 1, 1, 0]/2}], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "ap[hop[c[1], c[2]], vc[1, 0, 0, 1]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["vc[0, 0, 1, 1] + vc[1, 1, 0, 0]", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[Sneg`vc[0, 0, 1, 1] + 
          Sneg`vc[1, 1, 0, 0]], Editable -> True, AutoDelete -> True], 
       "Out[1]//InputForm="}}|>, <|"Code" -> "snegrealconstants[t, U];\nap[t \
c[CR, 1, UP] + U c[CR, 2, DO], vacuum[]] // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox[
         "U*vc[0, 0, 0, 1] + t*vc[1, 0, 0, 0]", ShowStringCharacters -> True, 
         NumberMarks -> True], InputForm[U*Sneg`vc[0, 0, 0, 1] + 
          t*Sneg`vc[1, 0, 0, 0]], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, 
   <|"Code" -> "nc[c[AN, 1, UP], c[CR, 1, UP], vacuum[]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[0, 0, 0, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[0, 0, 0, 0]], Editable -> True, 
        AutoDelete -> True], "Out[1]//InputForm="}}|>, 
   <|"Code" -> 
     "ap[{c[CR, 1, UP], c[CR, 1, DO], c[AN, 1, UP]}, vacuum[]] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{vc[1, 0, 0, 0], vc[0, 1, 0, 0], 0}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{Sneg`vc[1, 0, 0, 0], Sneg`vc[0, 1, 0, 0], 0}], 
        Editable -> True, AutoDelete -> True], "Out[1]//InputForm="}}|>, 
   <|"Code" -> "ap[c[CR, 2, DO], VACUUM] // InputForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox["vc[0, 0, 0, 1]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[0, 0, 0, 1]], Editable -> True, 
        AutoDelete -> True], "Out[1]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[d];\nmakebasis[{d[]}];\nv = ap[ket[-1/2], vc[1, \
0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
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
                 RGBColor[1, 0.5, 0]] & ], RowBox[{RowBox[{"-", FractionBox[
                  "1", "2"]}]}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
               FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                 RGBColor[1, 0.5, 0]] & ]}], DisplayForm], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "ap[nc[ket[1/2], bra[-1/2]], v] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[1, 0, ket[1/2]]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[1, 0, Sneg`ket[1/2]]], Editable -> True, 
        AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\nmakebasis[{c\
[1], c[2]}];\nH = -t hop[c[1], c[2]];\npsi = ap[nc[(c[CR, 1, UP] + c[CR, 2, \
UP])/Sqrt[2], (c[CR, 1, DO] + c[CR, 2, DO])/Sqrt[2]], \
vacuum[]];\nExpand[ap[H, psi] + 2 t psi]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[U];\nmakebasis[{d\
[]}];\nap[U hubbard[d[]], {vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}] // \
InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{0, 0, 0, U*vc[1, 1]}", ShowStringCharacters -> True, 
         NumberMarks -> True], InputForm[{0, 0, 0, U*Sneg`vc[1, 1]}], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nv = vc[0, \
1, 1, 0];\nap[nc[c[AN, 1, UP], c[CR, 1, UP]], v] + ap[nc[c[CR, 1, UP], c[AN, \
1, UP]], v] === v", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "op = c[CR, 2, UP] + c[AN, 1, DO];\nv = vc[1, 1, 0, \
0];\nExpand[ap[op, v] - ops2vc[nc[op, vc2ops[v]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1]}];\nap[x c[CR, 1, \
UP], vacuum[]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"ap", "[", 
         RowBox[{RowBox[{"x", " ", TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ]}], DisplayForm]}], "]"}], 
       "Out[3]="}}|>, 
   <|"Code" -> 
     "snegrealconstants[x];\nap[x c[CR, 1, UP], vacuum[]] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["x*vc[1, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[x*Sneg`vc[1, 0]], Editable -> True, AutoDelete -> True], 
       "Out[5]//InputForm="}}|>, <|"Code" -> "snegfermionoperators[c, \
e];\nmakebasis[{c[1]}];\nap[e[CR, UP], vacuum[]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"ap", "[", RowBox[
          {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ]}], DisplayForm]}], "]"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nmakebasis[{f[1]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", TagBox[StyleBox[
           TemplateBox[{"f", RowBox[{"1", ""}], "\"\[Dagger]\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "{ap[f[CR, 1], vacuum[]], ap[f[CR, 1, 0], vacuum[]]}", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"ap", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{"1"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
               TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[
                      0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                       0]] & ], "0", TagBox[StyleBox[
                    "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                      0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                       0]] & ]}], DisplayForm]}], "]"}], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], "1", TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 0]], 
               StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
            DisplayForm]}], "}"}], "Out[3]="}}|>}|>

<|"Source" -> "ket.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "{ket[1], bra[1], ket[x, y]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"1"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"x", "y"}], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0.5, 0]] & ]}], DisplayForm]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> 
     "{nc[bra[1], ket[1]], nc[bra[1], ket[2]], nc[bra[x], ket[y]]}", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", 
           TemplateBox[{RowBox[{"x", ",", "y"}]}, "KroneckerDeltaSeq"]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "nc[ket[0], bra[1], ket[1]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftBracketingBar]\"", FontColor -> 
             RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
              RGBColor[1, 0.5, 0]] & ], RowBox[{"0"}], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
              RGBColor[1, 0.5, 0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "nc[ket[1/2, Null], ket[Null, -1/2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
          RowBox[{FractionBox["1", "2"], RowBox[{"-", FractionBox["1", 
               "2"]}]}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
        DisplayForm], "Out[1]="}}|>, 
   <|"Code" -> "nc[bra[1/2, -1/2], ket[1/2, -1/2]]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> {{"Output", "1", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnc[ket[1], bra[1], c[CR, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], 
            TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ]}], DisplayForm], StyleBox["\[CenterDot]", 
          FontColor -> RGBColor[0, 1, 0]], 
         TagBox[RowBox[{TagBox[StyleBox["\"\[LeftAngleBracket]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], 
            TagBox[StyleBox["\"\[RightBracketingBar]\"", FontColor -> 
               RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                 0.5, 0]] & ]}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "conj[2 I nc[ket[1], bra[2]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "2"}], " ", "\[ImaginaryI]", " ", 
         RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"2"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"1"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}]}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[]}]\
;\nv = ap[ket[1/2], ap[c[CR, UP], vacuum[]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox[
            "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
              0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
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
   <|"Code" -> "sp = nc[ket[1/2], bra[-1/2]];\nsm = nc[ket[-1/2], \
bra[1/2]];\nsz = (nc[ket[1/2], bra[1/2]] - nc[ket[-1/2], \
bra[-1/2]])/2;\nExpand[nc[sp, sm] - nc[sm, sp] - 2 sz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "b = Sum[Sqrt[n] nc[ket[n - 1], bra[n]], {n, 1, 2}];\nbd = \
conj[b];\nExpand[nc[b, bd] - nc[bd, b]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[
        {RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"0"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"0"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], "+", 
         RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"1"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], "-", 
         RowBox[{"2", " ", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
                RowBox[{"2"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ]}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[RowBox[{TagBox[StyleBox["\"\[LeftAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ], RowBox[{"2"}], 
                TagBox[StyleBox["\"\[RightBracketingBar]\"", FontColor -> 
                   RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}]}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[J];\nH = J Expand[spinxyz[c[]] ~inner~ spinketbraXYZ[1/2]];\nstates = \
Flatten[Outer[nc, {c[CR, UP], c[CR, DO]}, {ket[1/2], \
ket[-1/2]}]];\nEigenvalues[matrixrepresentationop[H, states]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", FractionBox[RowBox[{"3", " ", "J"}], 
              "4"]}], ",", FractionBox["J", "4"], ",", FractionBox["J", "4"], 
           ",", FractionBox["J", "4"]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "nc[ket[1], ket[2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], 
            TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ]}], DisplayForm], StyleBox["\[CenterDot]", 
          FontColor -> RGBColor[0, 1, 0]], 
         TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"2"}], 
            TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ]}], DisplayForm]}], "Out[1]="}}|>, 
   <|"Code" -> "ketbratensorproduct[ket[1], ket[2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
          RowBox[{"1", "2"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
        DisplayForm], "Out[1]="}}|>, 
   <|"Code" -> "nc[bra[1, Null], ket[1, 2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftAngleBracket]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"1", "\[SmallCircle]"}], 
            TagBox[StyleBox["\"\[RightBracketingBar]\"", FontColor -> 
               RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                 0.5, 0]] & ]}], DisplayForm], StyleBox["\[CenterDot]", 
          FontColor -> RGBColor[0, 1, 0]], 
         TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
              FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                RGBColor[1, 0.5, 0]] & ], RowBox[{"1", "2"}], 
            TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ]}], DisplayForm]}], "Out[1]="}}|>}|>

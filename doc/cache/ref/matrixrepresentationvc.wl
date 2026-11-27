<|"Source" -> "matrixrepresentationvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nBASIS", 
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
       "Out[3]="}}|>, <|"Code" -> "states = {vc[1, 0, 0, 0], vc[0, 0, 1, \
0]};\nmatrixrepresentationvc[nc[c[CR, 1, UP], c[AN, 2, UP]] + nc[c[CR, 2, \
UP], c[AN, 1, UP]], states]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "twoel = {vc[1, 1, 0, 0], vc[0, 1, 1, 0], \
vc[1, 0, 0, 1], vc[0, 0, 1, 1]};\nmatrixrepresentationvc[c[CR, 1, DO], twoel, \
states]", "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}]\
;\nmatrixrepresentationvc[#, {vc[1, 0], vc[0, 1]}] & /@ spinxyz[d[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{FractionBox["1", "2"], ",", "0"}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", FractionBox["\[ImaginaryI]", "2"]}]}], "}"}], 
               ",", RowBox[{"{", RowBox[{FractionBox["\[ImaginaryI]", "2"], 
                   ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrea\
lconstants[t];\nmakebasis[{c[1], c[2]}];\nmatrixrepresentationvc[-t hop[c[1], \
c[2]], {(vc[1, 0, 0, 0] + vc[0, 0, 1, 0])/Sqrt[2], (vc[1, 0, 0, 0] - vc[0, 0, \
1, 0])/Sqrt[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "t"}], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "t"}], "}"}]}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nmakebas\
is[{d[]}];\nmatrixrepresentationvc[phononx[2], {vc[1, 0, ket[0]], vc[1, 0, \
ket[1]], vc[1, 0, ket[2]]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"1", ",", "0", ",", SqrtBox["2"]}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", SqrtBox["2"], ",", 
               "0"}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nfock = nonebasisvc[{c[1], \
c[2]}][[1, 2]];\ncr = matrixrepresentationvc[#, fock] & /@ BASIS;\nan = \
ConjugateTranspose /@ cr;\n{Union[Flatten[Table[cr[[i]] . cr[[j]] + cr[[j]] . \
cr[[i]], {i, 4}, {j, 4}]]], Union[Flatten[Table[an[[i]] . cr[[j]] + cr[[j]] . \
an[[i]] - KroneckerDelta[i, j] IdentityMatrix[16], {i, 4}, {j, 4}]]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
           RowBox[{"{", "0", "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegrealconstants[t, \
U];\nSort[N[Eigenvalues[matrixrepresentationvc[-t hop[c[1], c[2]] + U \
Total[hubbard[{c[1], c[2]}]], fock] /. {t -> 1, U -> 4}]]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"-", "1.`"}], ",", 
           RowBox[{"-", "1.`"}], ",", RowBox[{"-", "0.8284271247461903`"}], 
           ",", "0.`", ",", "0.`", ",", "0.`", ",", "0.`", ",", "1.`", ",", 
           "1.`", ",", "3.`", ",", "3.`", ",", "4.`", ",", 
           "4.82842712474619`", ",", "5.`", ",", "5.`", ",", "8.`"}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "snegfermionoperators[c];\nbz = \
qszbasisvc[{c[1], c[2]}];\nsec = keyselect[bz, {0, 0}][[1, 2]];\nh = \
matrixrepresentationvc[-hop[c[1], c[2]] + 4 Total[hubbard[{c[1], c[2]}]], \
sec];\n{vals, vecs} = Eigensystem[h];\neigenstates = vecs . sec;\ns = \
spinxyz[c[1]] + spinxyz[c[2]];\nTranspose[{vals, \
Diagonal[Simplify[matrixrepresentationvc[Expand[inner[s, s]], \
Simplify[eigenstates/(Norm /@ vecs)]]]]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"2", " ", RowBox[{"(", RowBox[
                    {"1", "+", SqrtBox["2"]}], ")"}]}], ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"4", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 RowBox[{"(", RowBox[{RowBox[{"-", "1"}], "+", SqrtBox[
                      "2"]}], ")"}]}], ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "2"}], "}"}]}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nfock = nonebasisvc[{c[1], c[2]}][[1, 2]];\nop = I nc[c[CR, 1, UP], \
c[AN, 2, DO]];\nmatrixrepresentationvc[conj[op], fock] == \
ConjugateTranspose[matrixrepresentationvc[op, fock]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "matrixrepresentationvc[op, {vc[1, 0, 0, 0]}, {vc[0, 0, 0, \
1]}] == {{braketvc[vc[1, 0, 0, 0], op, vc[0, 0, 0, 1]]}}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "matrixrepresentationvc[op, fock] == \
matrixrepresentationop[op, bzvc2bzop[{{{}, fock}}][[1, 2]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nmatrixrepresentationvc[1, {vc[1, 0, 0, 0], vc[1, 0, 0, 0] + vc[0, 0, \
1, 0]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"1", ",", "2"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c, \
d];\nmakebasis[{c[1]}];\nstates = {vc[1, 0], vc[0, \
1]};\nmakebasis[{d[]}];\nmatrixrepresentationvc[c[CR, 1, UP], states]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"scalarproductvc", "[", 
                 RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                        "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                        "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                       StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ]}], DisplayForm], ",", 
                   RowBox[{"ap", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                          {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], ",", TagBox[RowBox[
                         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                          FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], "]"}]}], "]"}], ",", 
               RowBox[{"scalarproductvc", "[", RowBox[{TagBox[RowBox[
                     {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                        FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                       StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                         RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                        "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                        "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm], ",", RowBox[{"ap", "[", 
                     RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], ",", TagBox[RowBox[
                         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                          FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                          StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], "]"}]}], "]"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"scalarproductvc", "[", 
                 RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                        "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                        "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                       StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ]}], DisplayForm], ",", 
                   RowBox[{"ap", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                          {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], ",", TagBox[RowBox[
                         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                          FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], "]"}]}], "]"}], ",", 
               RowBox[{"scalarproductvc", "[", RowBox[{TagBox[RowBox[
                     {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                        FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                       StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                         RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                        "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                        "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm], ",", RowBox[{"ap", "[", 
                     RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], ",", TagBox[RowBox[
                         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                          FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                          StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], "]"}]}], "]"}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>}|>

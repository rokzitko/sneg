<|"Source" -> "scalarproductvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\n{sca\
larproductvc[vc[1, 0], vc[1, 0]], scalarproductvc[vc[1, 0], vc[0, 1]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "psi = (vc[1, 0] + I vc[0, \
1])/Sqrt[2];\n{scalarproductvc[psi, vc[0, 1]], scalarproductvc[vc[0, 1], \
psi]}", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", FractionBox["\[ImaginaryI]", 
              SqrtBox["2"]]}], ",", FractionBox["\[ImaginaryI]", 
            SqrtBox["2"]]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nsnegcomplexconst\
ants[z];\nsnegrealconstants[x];\nscalarproductvc[z vc[1, 0] + x vc[0, 1], \
vc[1, 0] + vc[0, 1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"x", "+", RowBox[{"Conjugate", "[", "z", 
           "]"}]}], "Out[5]="}}|>, <|"Code" -> "{scalarproductvc[vc[1, 0, \
ket[1/2]], vc[1, 0, ket[1/2]]], scalarproductvc[vc[1, 0, ket[1/2]], vc[1, 0, \
ket[-1/2]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "nc[conj[vc[1, 1]], vc[1, 1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[1]="}}|>, 
   <|"Code" -> 
     "scalarproductvc[{vc[0, 0], vc[1, 0], vc[0, 1], vc[1, 1]}, vc[0, 1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "1", ",", 
           "0"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nstates = Flatten[Last /@ \
qsbasisvc[{c[1], c[2]}]];\nOuter[scalarproductvc, states, states] == \
IdentityMatrix[Length[states]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nheitlerlondon = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];\nbonding \
= ap[nc[c[CR, 1, UP] + c[CR, 2, UP], c[CR, 1, DO] + c[CR, 2, DO]]/2, \
vacuum[]];\nscalarproductvc[heitlerlondon, bonding]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", FractionBox["1", SqrtBox["2"]], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nv = 3 vc[1, 0] + \
4 I vc[0, 1];\n{scalarproductvc[v, v], normvc[v]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"25", ",", "5"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "a = vc[1, 0] + I vc[1, 1];\nb = vc[1, 1];\nscalarproductvc[a, \
b] == scalarproductop[vc2ops[a], vc2ops[b]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nscalarproductvc[\
y vc[1, 0], vc[1, 0]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"scalarproductvc", "[", 
         RowBox[{InterpretationBox[RowBox[{"y", TagBox[RowBox[
                {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                   FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                   "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm]}], y*Sneg`vc[1, 0]], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ]}], DisplayForm]}], "]"}], 
       "Out[3]="}}|>, 
   <|"Code" -> 
     "snegrealconstants[y];\nscalarproductvc[y vc[1, 0], vc[1, 0]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "y", "Out[5]="}}|>}|>

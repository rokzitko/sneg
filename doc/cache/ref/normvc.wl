<|"Source" -> "normvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nnorm\
vc[3 vc[1, 0] + 4 I vc[0, 1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "5", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nsnegrealconstant\
s[a, b];\nnormvc[a vc[0, 0] + b vc[1, 1]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", SqrtBox[RowBox[{SuperscriptBox["a", "2"], "+", 
          SuperscriptBox["b", "2"]}]], "Out[4]="}}|>, 
   <|"Code" -> "normvc[{vc[1, 0], vc[1, 0] + vc[0, 1], 0}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", SqrtBox["2"], ",", "0"}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebas\
is[{c[1], c[2]}];\nv = ap[spinminus[c[1]] + spinminus[c[2]], vc[1, 0, 1, \
0]];\nv/normvc[v] // InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["(vc[0, 1, 1, 0] + vc[1, 0, 0, 1])/Sqrt[2]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[(Sneg`vc[0, 1, 1, 0] + Sneg`vc[1, 0, 0, 1])/Sqrt[2]], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nv = vc[1, 0] - 2 \
vc[0, 1];\n{normvc[v], normop[vc2ops[v]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{SqrtBox["5"], ",", SqrtBox["5"]}], 
         "}"}], "Out[4]="}}|>}|>

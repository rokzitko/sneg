<|"Source" -> "braketrule.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "{braketrule[bra[1], ket[1]], braketrule[bra[1], \
ket[2]], braketrule[bra[x], ket[y]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", 
           TemplateBox[{RowBox[{"x", ",", "y"}]}, "KroneckerDeltaSeq"]}], 
         "}"}], "Out[1]="}}|>, 
   <|"Code" -> "braketrule[bra[a, Null, b], ket[c, Null, d]]", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"a", ",", "c"}]}, 
          "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"b", ",", "d"}]}, 
          "KroneckerDeltaSeq"]}], "Out[2]="}}|>, 
   <|"Code" -> "nc[bra[x, y], ket[1, 2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TemplateBox[{RowBox[{"1", ",", "x"}]}, 
          "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"2", ",", "y"}]}, 
          "KroneckerDeltaSeq"]}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nscalarproductvc[\
vc[1, 0, ket[1/2]], vc[1, 0, ket[-1/2]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "DownValues[braketrule] = Prepend[DownValues[braketrule],\n  \
HoldPattern[braketrule[bra[coherent[a_]], ket[coherent[b_]]]] :>\n    \
Exp[-Abs[a]^2/2 - Abs[b]^2/2 + Conjugate[a] b]];\nnc[bra[coherent[1]], \
ket[coherent[2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox["1", SqrtBox["\[ExponentialE]"]], 
       "Out[2]="}}|>, <|"Code" -> "nc[bra[1], ket[2]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "braketrule[bra[gaussian[a_]], ket[gaussian[b_]]] := Exp[-(a - \
b)^2/4];\nnc[bra[gaussian[0]], ket[gaussian[1]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[2]="}}|>}|>

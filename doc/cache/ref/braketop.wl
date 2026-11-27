<|"Source" -> "braketop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nbraketop[c[CR, 2, UP], \
hop[c[1], c[2]], c[CR, 1, UP]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[2]="}}|>, 
   <|"Code" -> "braketop[1, nc[c[AN, 1, DO], c[AN, 1, UP]], nc[c[CR, 1, UP], \
c[CR, 1, DO]]]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "1", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[i, j, \
k];\nbraketop[c[CR, i, UP], hop[c[1], c[2]], c[CR, k, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TemplateBox[{RowBox[{"1", ",", "k"}]}, 
            "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"2", ",", "i"}]}, 
            "KroneckerDeltaSeq"]}], "+", 
         RowBox[{TemplateBox[{RowBox[{"1", ",", "i"}]}, "KroneckerDeltaSeq"], 
           " ", TemplateBox[{RowBox[{"2", ",", "k"}]}, 
            "KroneckerDeltaSeq"]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\na = \
nc[c[CR, 1, UP], c[CR, 2, DO]];\nb = nc[c[CR, 1, DO], c[CR, 2, \
UP]];\n{braketop[a, spinspin[c[1], c[2]], b], braketvc[ops2vc[a], \
spinspin[c[1], c[2]], ops2vc[b]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           FractionBox["1", "2"]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbraketop[c[CR, p, UP], \
number[c[q]], c[CR, r, UP]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"vev", "[", 
         RowBox[{TemplateBox[{RowBox[{"p", ",", "q"}]}, "KroneckerDeltaSeq"], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TemplateBox[{RowBox[{"q", ",", "r"}]}, "KroneckerDeltaSeq"]}], 
         "]"}], "Out[2]="}}|>, <|"Code" -> "snegfreeindexes[p, q, \
r];\nbraketop[c[CR, p, UP], number[c[q]], c[CR, r, UP]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{TemplateBox[{RowBox[{"p", ",", "q"}]}, 
          "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"q", ",", "r"}]}, 
          "KroneckerDeltaSeq"]}], "Out[4]="}}|>}|>

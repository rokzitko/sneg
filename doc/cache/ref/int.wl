<|"Source" -> "int.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "sneggrassmanconstants[x, y];\n{nc[int[x], 1], \
nc[int[x], x], nc[int[x], y, x], nc[int[x], y]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "1", ",", RowBox[{"-", "y"}], 
           ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x];\nsnegrealconstants[a];\nnc[int[conj\
[x]], int[x], 1 - a nc[conj[x], x]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "a", "Out[3]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x];\nsnegrealconstants[a];\nnc[int[conj\
[x]], int[x], Exp[-a nc[conj[x], x]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x, y];\n{nc[int[x], int[y], x, y], \
nc[int[x], nc[int[y], x, y]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           RowBox[{"-", "1"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[x1, x2];\nsnegrealconstants[a11, a12, \
a21, a22];\nS = a11 nc[conj[x1], x1] + a12 nc[conj[x1], x2] + a21 \
nc[conj[x2], x1] + a22 nc[conj[x2], x2];\nExpand[nc[int[conj[x1]], int[x1], \
int[conj[x2]], int[x2], 1 - S + pow[S, 2]/2]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"a12", " ", "a21"}], "-", 
         RowBox[{"a11", " ", "a22"}]}], "Out[6]="}}|>}|>

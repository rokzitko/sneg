<|"Source" -> "iscreation.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{iscreation[c[CR, 1, \
UP]], iscreation[c[AN, 1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\norderin\
g[d] = SEA;\n{iscreation[d[CR, 2, UP]], iscreation[d[AN, -2, UP]], \
iscreation[d[CR, -2, UP]]}", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "False"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nordering[d] = SEA;\nkp /: kp > 0 = \
True;\n{iscreation[d[CR, k, UP]], iscreation[d[CR, kp, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"k", ">", "0"}], ",", 
           "True"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a];\n{iscreation[a[CR]], iscreation[a[AN, 1]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\norderin\
g[d] = SEA;\nops = {d[CR, 1, UP], d[AN, 1, UP], d[CR, -1, UP], d[AN, -1, \
UP]};\nTranspose[{iscreation /@ ops, isannihilation /@ ops}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"True", ",", "False"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"False", ",", "True"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"False", ",", "True"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"True", ",", "False"}], "}"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nordering[d] = \
SEA;\n{iscreation[d[CR, 0, UP]], isannihilation[d[CR, 0, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"False", ",", "False"}], 
         "}"}], "Out[3]="}}|>}|>

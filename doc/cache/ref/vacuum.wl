<|"Source" -> "vacuum.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nvacuum[] // \
InputForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[0, 0, 0, 0]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[0, 0, 0, 0]], Editable -> True, 
        AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "ap[nc[c[CR, 1, UP], c[CR, 2, DO]], vacuum[]] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["vc[1, 0, 0, 1]", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[Sneg`vc[1, 0, 0, 1]], Editable -> True, 
        AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nmakebasis[{c[1]}];\nvc2ops[vacuum[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[3]="}}|>, 
   <|"Code" -> 
     "{ap[c[AN, 1, UP], vacuum[]], scalarproductvc[vacuum[], vacuum[]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "ap[1, VACUUM] === vacuum[]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

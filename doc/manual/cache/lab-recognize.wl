<|"Source" -> "lab-recognize.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[J,U];\
\nraw = Expand[J spinspin[c[1],c[2]] + U hubbard[c[1]]];\ncompact = \
SnegSimplify[raw];\nCases[compact, HoldPattern[HoldForm[h_[___]]] :> \
SymbolName[h], Infinity]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"\"hubbard\"", ",", 
           "\"spinspin\""}], "}"}], "Out[5]=", <|"Kind" -> "List", 
        "Parts" -> {"\\text{hubbard}", "\\text{spinspin}"}|>}}|>, 
   <|"Code" -> "Expand[ReleaseHold[compact] - raw]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> {{"Output", "0", "Out[6]=", 
       <|"Kind" -> "Single", "Parts" -> {"0"}|>}}|>}|>

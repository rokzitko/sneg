<|"Source" -> "nonebasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[d];\nnonebasisvc[{d[]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{{{}, {vc[0, 0], vc[0, 1], vc[1, 0], vc[1, 1]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{}, {Sneg`vc[0, 0], Sneg`vc[0, 1], Sneg`vc[1, 0], 
            Sneg`vc[1, 1]}}}], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, <|"Code" -> "snegfermionoperators[d];\nsnegr\
ealconstants[Delta, B];\nH = Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], \
d[AN, UP]]) - B spinx[d[]];\nm = makematricesbzvc[H, \
nonebasisvc[{d[]}]];\nMatrixForm[m[[1, 2]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"0", "0", "0", "Delta"}, {"0", "0", 
             RowBox[{"-", FractionBox["B", "2"]}], "0"}, 
            {"0", RowBox[{"-", FractionBox["B", "2"]}], "0", "0"}, 
            {"Delta", "0", "0", "0"}}, RowSpacings -> 1, ColumnSpacings -> 1, 
           RowAlignments -> Baseline, ColumnAlignments -> Center], 
          "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[5]//MatrixForm="}}|>, <|"Code" -> "Eigenvalues[m[[1, 2]]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", FractionBox["B", "2"]}], ",", 
           FractionBox["B", "2"], ",", RowBox[{"-", "Delta"}], ",", 
           "Delta"}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nLength[nonebasisvc[{c[1], c[2], \
c[3]}][[1, 2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "64", "Out[2]="}}|>}|>

<|"Source" -> "sbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[d];\nsbasisvc[{d[]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{{{0}, {vc[0, 0], -vc[1, 1]}}, {{1/2}, {vc[1, 0]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{0}, {Sneg`vc[0, 0], -Sneg`vc[1, 1]}}, 
          {{1/2}, {Sneg`vc[1, 0]}}}], Editable -> True, AutoDelete -> True], 
       "Out[2]//InputForm="}}|>, <|"Code" -> "snegfermionoperators[d];\nsnegr\
ealconstants[eps, U, Delta];\nH = eps number[d[]] + U hubbard[d[]] + Delta \
(nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]);\nmakematricesbzvc[H, \
sbasisvc[{d[]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                        {"-", "Delta"}]}], "}"}], ",", RowBox[{"{", 
                     RowBox[{RowBox[{"-", "Delta"}], ",", RowBox[
                        {RowBox[{"2", " ", "eps"}], "+", "U"}]}], "}"}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", FractionBox["1", "2"], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "eps", "}"}], "}"}]}], "}"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{First[#], \
Length[Last[#]]} & /@ sbasisvc[{c[1], c[2]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", "5"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", "2"], 
                 "}"}], ",", "4"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", "1"}], "}"}]}], 
         "}"}], "Out[2]="}}|>}|>

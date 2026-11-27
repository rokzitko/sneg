<|"Source" -> "szbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[d];\nszbasisvc[{d[]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{{{-1/2}, {vc[0, 1]}}, \
{{0}, {vc[0, 0], vc[1, 1]}}, {{1/2}, {vc[1, 0]}}}", ShowStringCharacters -> 
          True, NumberMarks -> True], InputForm[{{{-1/2}, {Sneg`vc[0, 1]}}, 
          {{0}, {Sneg`vc[0, 0], Sneg`vc[1, 1]}}, {{1/2}, {Sneg`vc[1, 0]}}}], 
        Editable -> True, AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps, Delta, \
B];\nH = eps number[d[]] + Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], \
d[AN, UP]]) - B spinz[d[]];\n{First[#], Eigenvalues[Last[#]]} & /@ \
makematricesbzvc[H, szbasisvc[{d[]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", FractionBox["1", 
                    "2"]}], "}"}], ",", RowBox[{"{", RowBox[{FractionBox["B", 
                    "2"], "+", "eps"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"eps", "-", SqrtBox[RowBox[
                       {SuperscriptBox["Delta", "2"], "+", SuperscriptBox[
                         "eps", "2"]}]]}], ",", RowBox[{"eps", "+", 
                     SqrtBox[RowBox[{SuperscriptBox["Delta", "2"], "+", 
                        SuperscriptBox["eps", "2"]}]]}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", "2"], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                      "B", "2"]}], "+", "eps"}], "}"}]}], "}"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{First[#], \
Length[Last[#]]} & /@ szbasisvc[{c[1], c[2]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"-", FractionBox["1", "2"]}], "}"}], ",", 
               "4"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", 
                 "}"}], ",", "6"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", "2"], "}"}], 
               ",", "4"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", "1", "}"}], ",", "1"}], "}"}]}], 
         "}"}], "Out[2]="}}|>}|>

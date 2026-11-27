<|"Source" -> "lab-gap.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[ck];\nsnegrealconstants[Dl, \
x, T];\nsnegrealfunctions[xi];\nsnegmomentumindexes[{k}, Ns];\nhk = \
First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, \
-k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> \
xi[k];\nF = FullSimplify[ExpToTrig[gaussianaverage[nc[ck[AN, -k, DO], ck[AN, \
k, UP]], hk, {ck}, T, \"Momentum\" -> k] /. xi[k] -> x], Dl > 0 && T > 0 && \
Element[x, Reals]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"Dl", " ", 
          RowBox[{"Tanh", "[", FractionBox[SqrtBox[RowBox[{SuperscriptBox[
                 "Dl", "2"], "+", SuperscriptBox["x", "2"]}]], 
             RowBox[{"2", " ", "T"}]], "]"}]}], 
        RowBox[{"2", " ", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
             SuperscriptBox["x", "2"]}]]}]], "Out[6]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\frac{\\text{Dl} \\tanh \
\\left(\\frac{\\sqrt{\\text{Dl}^2+x^2}}{2 T}\\right)}{2 \
\\sqrt{\\text{Dl}^2+x^2}}"}|>}}|>, <|"Code" -> "FindRoot[Dl == 3/4 Sum[F /. \
{x -> -2 Cos[q] + 1/2, T -> 1/10}, {q, {0, Pi/2, Pi, 3 Pi/2}}], {Dl, 1}]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"Dl", "\[Rule]", 
           "1.0296990620785584`"}], "}"}], "Out[7]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\{\\text{Dl}\\to 1.0297\\}"}|>}}|>}|>

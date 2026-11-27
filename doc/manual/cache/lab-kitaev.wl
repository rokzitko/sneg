<|"Source" -> "lab-kitaev.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t, mu, Dl];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[-t hop[f[R], f[R + 1]] - mu \
number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], \
f[AN, R, 0]]), {R}];\nhk = First[bdghamiltonian[fourier[H, f -> fk, R -> k], \
{fk}, k]];\ne2 = Simplify[ExpToTrig[blochphaseexp[Max[Eigenvalues[hk]]^2, {k \
-> q}]]];\nFactor[{e2 /. q -> 0, e2 /. q -> Pi}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{SuperscriptBox[
            RowBox[{"(", RowBox[{"mu", "+", RowBox[{"2", " ", "t"}]}], ")"}], 
            "2"], ",", SuperscriptBox[RowBox[{"(", RowBox[{"mu", "-", 
                RowBox[{"2", " ", "t"}]}], ")"}], "2"]}], "}"}], "Out[8]=", 
       <|"Kind" -> "List", "Parts" -> {"(\\mu +2 t)^2", 
          "(\\mu -2 t)^2"}|>}}|>}|>

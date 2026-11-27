<|"Source" -> "lab-square.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nH = -t \
sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}];\nh = \
blochhamiltonian[fourier[H, f -> fk, R -> k], {fk}, \
k];\nSimplify[ExpToTrig[blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, 0}, a2 -> \
{0, 1}}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
             RowBox[{"(", RowBox[{RowBox[{"Cos", "[", "kx", "]"}], "+", 
                 RowBox[{"Cos", "[", "ky", "]"}]}], ")"}]}], "}"}], "}"}], 
       "Out[8]=", <|"Kind" -> "Single", "Parts" -> {"\\left(\n\\begin{array}{\
c}\n -2 t (\\cos (\\text{kx})+\\cos (\\text{ky})) \
\\\\\n\\end{array}\n\\right)"}|>}}|>}|>

<|"Source" -> "phonons.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "split[n_,t_,g_] := Module[{id,nop,x,ph,h},\n  id \
= IdentityMatrix[n+1]; nop = DiagonalMatrix[Range[0,n]];\n  x = \
Normal[SparseArray[{Band[{1,2}] -> Sqrt[Range[n]],\n                  \
Band[{2,1}] -> Sqrt[Range[n]]},{n+1,n+1}]];\n  ph = KroneckerProduct[nop,id] \
+ KroneckerProduct[id,nop];\n  h = KroneckerProduct[IdentityMatrix[2],ph] -\n \
     t KroneckerProduct[PauliX,id,id] +\n      g \
(KroneckerProduct[DiagonalMatrix[{1,0}],x,id] +\n         \
KroneckerProduct[DiagonalMatrix[{0,1}],id,x]);\n  \
Differences[Take[Sort[Eigenvalues[N[h]]],2]][[1]]];\nTableForm[Table[{n,split\
[n,0.01,0.6]/0.02, Exp[-0.6^2]}, {n,{2,4,6}}],\n TableHeadings -> \
{None,{\"Cutoff\",\"Splitting / 2t\",\"Overlap\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Cutoff\"", HoldForm], 
            TagBox["\"Splitting / 2t\"", HoldForm], TagBox["\"Overlap\"", 
             HoldForm]}, {"2", "0.7263007781432684`", "0.697676326071031`"}, 
           {"4", "0.6980929172675676`", "0.697676326071031`"}, 
           {"6", "0.697630607726335`", "0.697676326071031`"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Cutoff", "Splitting / 2t", "Overlap"}}]]], 
       "Out[2]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{Cutoff} & \\text{Splitting \
/ 2t} & \\text{Overlap} \\\\\n\\hline\n 2 & 0.726301 & 0.697676 \
\\\\\n\\hline\n 4 & 0.698093 & 0.697676 \\\\\n 6 & 0.697631 & 0.697676 \
\\\\\n\\end{array}"}|>}}|>}|>

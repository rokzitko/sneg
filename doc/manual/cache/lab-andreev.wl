<|"Source" -> "lab-andreev.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d,f];\nsnegrealconstants[U];\
\nhp = U isozsq[d[]]/2 - bcs[f[]] + hop[f[],d[]]/2;\nmm = \
makematricesbzvc[hp,szbasisvc[{d[],f[]}]];\nemin[sz_,u_] := \
Min[Eigenvalues[N[First[keyselect[mm,{sz}]][[2]] /. U -> u]]];\nrows = \
Table[{u,emin[0,u],emin[1/2,u]}, {u,0,4}];\nTableForm[rows,TableHeadings -> \
{None,{\"U\",\"Lowest Sz = 0\",\"Lowest Sz = 1/2\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"U\"", HoldForm], 
            TagBox["\"Lowest Sz = 0\"", HoldForm], 
            TagBox["\"Lowest Sz = 1/2\"", HoldForm]}, 
           {"0", RowBox[{"-", "1.4142135623730954`"}], 
            RowBox[{"-", "1.2071067811865477`"}]}, 
           {"1", RowBox[{"-", "1.0706680578276822`"}], 
            RowBox[{"-", "1.1513878188659974`"}]}, 
           {"2", RowBox[{"-", "0.8019377358048385`"}], 
            RowBox[{"-", "1.1180339887498945`"}]}, 
           {"3", RowBox[{"-", "0.610838708758257`"}], 
            RowBox[{"-", "1.0962912017836266`"}]}, 
           {"4", RowBox[{"-", "0.4811943040920156`"}], 
            RowBox[{"-", "1.0811388300841895`"}]}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"U", "Lowest Sz = 0", "Lowest Sz = 1/2"}}]]], 
       "Out[7]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{U} & \\text{Lowest Sz = 0} \
& \\text{Lowest Sz = 1/2} \\\\\n\\hline\n 0 & -1.41421 & -1.20711 \
\\\\\n\\hline\n 1 & -1.07067 & -1.15139 \\\\\n 2 & -0.801938 & -1.11803 \
\\\\\n 3 & -0.610839 & -1.09629 \\\\\n 4 & -0.481194 & -1.08114 \
\\\\\n\\end{array}"}|>}}|>}|>

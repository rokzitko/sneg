<|"Source" -> "lab-reflection.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsites = c /@ \
Range[3];\nb = transformtoLRvc[qsbasis[sites],sites,vacuum[]];\none = \
Select[b, #[[1,1]] == -2 &];\nTableForm[{Sequence @@ \
First[#],Length[Last[#]]} & /@ one,\n TableHeadings -> \
{None,{\"Q\",\"S\",\"Parity\",\"Multiplets\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Q\"", HoldForm], 
            TagBox["\"S\"", HoldForm], TagBox["\"Parity\"", HoldForm], 
            TagBox["\"Multiplets\"", HoldForm]}, {RowBox[{"-", "2"}], 
            FractionBox["1", "2"], RowBox[{"-", "1"}], "1"}, 
           {RowBox[{"-", "2"}], FractionBox["1", "2"], "1", "2"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Q", "S", "Parity", "Multiplets"}}]]], 
       "Out[5]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cccc}\n \\text{Q} & \\text{S} & \
\\text{Parity} & \\text{Multiplets} \\\\\n\\hline\n -2 & \\frac{1}{2} & -1 & \
1 \\\\\n\\hline\n -2 & \\frac{1}{2} & 1 & 2 \\\\\n\\end{array}"}|>}}|>}|>

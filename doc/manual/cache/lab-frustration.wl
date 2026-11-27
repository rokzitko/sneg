<|"Source" -> "lab-frustration.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[{S,1/2}];\nh = \
Sum[spinspin[S[i],S[Mod[i,4]+1]], {i,4}];\nmm = spinclustermatrices[h,{S -> \
Range[4]}];\nrows = {#[[1,1]],Length[Last[#]],Min[Eigenvalues[Last[#]]]} & /@ \
mm;\nTableForm[rows,TableHeadings -> {None,{\"Sz\",\"Dimension\",\"Energy / \
J\"}}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Sz\"", HoldForm], TagBox["\"Dimension\"", HoldForm], 
            TagBox["\"Energy / J\"", HoldForm]}, {RowBox[{"-", "2"}], "1", 
            "1"}, {RowBox[{"-", "1"}], "4", RowBox[{"-", "1"}]}, 
           {"0", "6", RowBox[{"-", "2"}]}, {"1", "4", RowBox[{"-", "1"}]}, 
           {"2", "1", "1"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Sz", "Dimension", 
             "Energy / J"}}]]], "Out[5]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{Sz} & \\text{Dimension} & \
\\text{Energy / J} \\\\\n\\hline\n -2 & 1 & 1 \\\\\n\\hline\n -1 & 4 & -1 \
\\\\\n 0 & 6 & -2 \\\\\n 1 & 4 & -1 \\\\\n 2 & 1 & 1 \\\\\n\\end{array}"}|>}}\
|>}|>

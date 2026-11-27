<|"Source" -> "response.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "dop = DiagonalMatrix[{1,-1}];\nhd[h_] := \
{{h,-1},{-1,-h}};\n{vals,vecs} = Eigensystem[hd[0]];\nj = \
First[Ordering[vals,1]]; l = 3-j;\ndelta = vals[[l]]-vals[[j]];\nwgt = \
Abs[Conjugate[Normalize[vecs[[j]]]].dop.Normalize[vecs[[l]]]]^2;\nTableForm[{\
{delta,wgt,-2 wgt/delta,D[-Sqrt[1+h^2],{h,2}] /. h -> 0}},\n TableHeadings -> \
{None,{\"Gap\",\"Weight\",\"Retarded limit\",\"Curvature\"}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Gap\"", HoldForm], TagBox["\"Weight\"", HoldForm], 
            TagBox["\"Retarded limit\"", HoldForm], TagBox["\"Curvature\"", 
             HoldForm]}, {"2", "1", RowBox[{"-", "1"}], RowBox[{"-", "1"}]}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Gap", "Weight", "Retarded limit", "Curvature"}}]]], 
       "Out[7]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cccc}\n \\text{Gap} & \\text{Weight} & \
\\text{Retarded limit} & \\text{Curvature} \\\\\n\\hline\n 2 & 1 & -1 & -1 \
\\\\\n\\end{array}"}|>}}|>}|>

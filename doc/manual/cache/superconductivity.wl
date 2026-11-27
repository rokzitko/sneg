<|"Source" -> "superconductivity.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "ee[p_] := -Sqrt[1+(3/5)^2+2 (3/5) Cos[p]];\neo = \
-1;\nTableForm[Table[{p, N[ee[p]], eo,\n  If[ee[p] < eo, N[(3/5) \
Sin[p]/(-ee[p])], 0]}, {p,{0,Pi/3,2 Pi/3,Pi}}],\n TableHeadings -> \
{None,{\"Phase\",\"Even\",\"Odd\",\"Current / I0\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Phase\"", HoldForm], 
            TagBox["\"Even\"", HoldForm], TagBox["\"Odd\"", HoldForm], 
            TagBox["\"Current / I0\"", HoldForm]}, 
           {"0", RowBox[{"-", "1.6`"}], RowBox[{"-", "1"}], "0.`"}, 
           {FractionBox["\[Pi]", "3"], RowBox[{"-", "1.4`"}], 
            RowBox[{"-", "1"}], "0.3711537444790451`"}, 
           {FractionBox[RowBox[{"2", " ", "\[Pi]"}], "3"], 
            RowBox[{"-", "0.8717797887081349`"}], RowBox[{"-", "1"}], "0"}, 
           {"\[Pi]", RowBox[{"-", "0.4`"}], RowBox[{"-", "1"}], "0"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Phase", "Even", "Odd", "Current / I0"}}]]], 
       "Out[3]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cccc}\n \\text{Phase} & \\text{Even} & \
\\text{Odd} & \\text{Current / I0} \\\\\n\\hline\n 0 & -1.6 & -1 & 0. \
\\\\\n\\hline\n \\frac{\\pi }{3} & -1.4 & -1 & 0.371154 \\\\\n \\frac{2 \\pi \
}{3} & -0.87178 & -1 & 0 \\\\\n \\pi  & -0.4 & -1 & 0 \\\\\n\\end{array}"}|>}}\
|>}|>

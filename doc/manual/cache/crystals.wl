<|"Source" -> "crystals.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "winding[v_,w_,n_] := Module[{q = N[v + w Exp[-2 \
Pi I Range[0,n-1]/n]]},\n  Round[Total[Arg[RotateLeft[q]/q]]/(2 \
Pi)]];\nTableForm[Table[{v,1,winding[v,1,40],winding[v,1,80]}, \
{v,{1/2,2}}],\n TableHeadings -> {None,{\"v\",\"w\",\"40 points\",\"80 \
points\"}}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"v\"", HoldForm], 
            TagBox["\"w\"", HoldForm], TagBox["\"40 points\"", HoldForm], 
            TagBox["\"80 points\"", HoldForm]}, {FractionBox["1", "2"], "1", 
            RowBox[{"-", "1"}], RowBox[{"-", "1"}]}, {"2", "1", "0", "0"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"v", "w", "40 points", "80 points"}}]]], 
       "Out[2]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cccc}\n \\text{v} & \\text{w} & \\text{40 \
points} & \\text{80 points} \\\\\n\\hline\n \\frac{1}{2} & 1 & -1 & -1 \
\\\\\n\\hline\n 2 & 1 & 0 & 0 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "harper[q_, ky_] := Table[-2 Cos[ky + 2 Pi i/q] \
KroneckerDelta[i, j] -\n  Boole[Mod[i-j, q] == 1] - Boole[Mod[j-i, q] == \
1],\n  {i, 0, q-1}, {j, 0, q-1}];\nharper[3, 0] // MatrixForm", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"-", "2"}], RowBox[{"-", "1"}], 
             RowBox[{"-", "1"}]}, {RowBox[{"-", "1"}], "1", 
             RowBox[{"-", "1"}]}, {RowBox[{"-", "1"}], RowBox[{"-", "1"}], 
             "1"}}, RowSpacings -> 1, ColumnSpacings -> 1, 
           RowAlignments -> Baseline, ColumnAlignments -> Center], 
          "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[4]//MatrixForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{ccc}\n -2 & -1 & -1 \\\\\n -1 & \
1 & -1 \\\\\n -1 & -1 & 1 \\\\\n\\end{array}\n\\right)"}|>}}|>}|>

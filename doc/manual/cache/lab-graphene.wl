<|"Source" -> "lab-graphene.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, Ns]; \
snegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nhg = -t \
sum[hop[f[R,\"A\"], f[R,\"B\"]] + hop[f[R,\"A\"], f[R-a1,\"B\"]] +\n  \
hop[f[R,\"A\"], f[R-a2,\"B\"]], {R}];\nh = blochhamiltonian[fourier[hg,f -> \
fk,R -> k], {fk[\"A\"],fk[\"B\"]}, k];\nExpand[-h[[1,2]]/t]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "+", TagBox[SuperscriptBox["e", 
           RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
             "a1"}]], DisplayForm], "+", TagBox[SuperscriptBox["e", 
           RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
             "a2"}]], DisplayForm]}], "Out[7]=", <|"Kind" -> "Sum", 
        "Parts" -> {"1", "+\\text{blochphase}(-k,\\text{a1})", 
          "+\\text{blochphase}(-k,\\text{a2})"}|>}}|>, 
   <|"Code" -> "bands[p_] := Sort[Eigenvalues[Simplify[blochphaseexp[h,\n  {k \
-> p, a1 -> {1,0}, a2 -> {1/2,Sqrt[3]/2}}] /. t -> 1]]];\nTableForm[bands /@ \
{{0,0},{4 Pi/3,0},{0,2 Pi/Sqrt[3]}},\n TableHeadings -> \
{{\"Centre\",\"Valley\",\"Midpoint\"},{\"Lower / t\",\"Upper / t\"}}]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{StyleBox["\[Null]", ShowStringCharacters -> False], 
            TagBox["\"Lower / t\"", HoldForm], TagBox["\"Upper / t\"", 
             HoldForm]}, {TagBox["\"Centre\"", HoldForm], RowBox[{"-", "3"}], 
            "3"}, {TagBox["\"Valley\"", HoldForm], "0", "0"}, 
           {TagBox["\"Midpoint\"", HoldForm], RowBox[{"-", "1"}], "1"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {False, True, {False}, False}, 
            "Rows" -> {False, True, {False}, False}}, ColumnAlignments -> 
           Left], {OutputFormsDump`HeadedRows, 
          OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {{"Centre", "Valley", 
             "Midpoint"}, {"Lower / t", "Upper / t"}}]]], 
       "Out[9]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{c|c|c}\n  & \\text{Lower / t} & \
\\text{Upper / t} \\\\\n\\hline\n \\text{Centre} & -3 & 3 \\\\\n\\hline\n \
\\text{Valley} & 0 & 0 \\\\\n \\text{Midpoint} & -1 & 1 \\\\\n\\end{array}"}|>\
}}|>}|>

<|"Source" -> "lab-anderson.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d,c]; \
snegrealconstants[U,V];\nh = -U number[d[]]/2 + U hubbard[d[]] + V \
hop[d[],c[]];\nb = Select[qsbasisvc[{d[],c[]}], #[[1,1]] == 0 &];\nm = \
makematricesbzvc[h,b];\nrows = Flatten[Table[{blockdata[[1,2]],#} & /@\n  \
Sort[Eigenvalues[Last[blockdata] /. {U -> 4,V -> 1/2}]],\n  \
{blockdata,m}],1];\nTableForm[rows,TableHeadings -> {None,{\"Total \
spin\",\"Energy\"}}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Total spin\"", HoldForm], TagBox["\"Energy\"", 
             HoldForm]}, {"0", "0"}, {"0", RowBox[{RowBox[{"-", "1"}], "-", 
              SqrtBox["2"]}]}, {"0", RowBox[{RowBox[{"-", "1"}], "+", 
              SqrtBox["2"]}]}, {"1", RowBox[{"-", "2"}]}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Total spin", "Energy"}}]]], "Out[6]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{Total \
spin} & \\text{Energy} \\\\\n\\hline\n 0 & 0 \\\\\n\\hline\n 0 & -1-\\sqrt{2} \
\\\\\n 0 & \\sqrt{2}-1 \\\\\n 1 & -2 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "gap = -U/2 + (U+Sqrt[U^2+64 V^2])/4;\nLimit[gap/V^2, V -> 0, \
Assumptions -> U > 0]", "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", FractionBox["8", "U"], "Out[8]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\frac{8}{U}"}|>}}|>}|>

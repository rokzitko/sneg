<|"Source" -> "lab-multiplets.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nb = qsbasisvc[c /@ \
Range[3]];\ndims = {Sequence @@ First[#], Length[Last[#]], (2 #[[1,2]]+1) \
Length[Last[#]]} & /@ b;\nTableForm[dims, TableHeadings -> \
{None,{\"Q\",\"S\",\"Multiplets\",\"States\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Q\"", HoldForm], 
            TagBox["\"S\"", HoldForm], TagBox["\"Multiplets\"", HoldForm], 
            TagBox["\"States\"", HoldForm]}, {RowBox[{"-", "3"}], "0", "1", 
            "1"}, {RowBox[{"-", "2"}], FractionBox["1", "2"], "3", "6"}, 
           {RowBox[{"-", "1"}], "0", "6", "6"}, {RowBox[{"-", "1"}], "1", 
            "3", "9"}, {"0", FractionBox["1", "2"], "8", "16"}, 
           {"0", FractionBox["3", "2"], "1", "4"}, {"1", "0", "6", "6"}, 
           {"1", "1", "3", "9"}, {"2", FractionBox["1", "2"], "3", "6"}, 
           {"3", "0", "1", "1"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Q", "S", 
             "Multiplets", "States"}}]]], "Out[4]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cccc}\n \\text{Q} & \
\\text{S} & \\text{Multiplets} & \\text{States} \\\\\n\\hline\n -3 & 0 & 1 & \
1 \\\\\n\\hline\n -2 & \\frac{1}{2} & 3 & 6 \\\\\n -1 & 0 & 6 & 6 \\\\\n -1 & \
1 & 3 & 9 \\\\\n 0 & \\frac{1}{2} & 8 & 16 \\\\\n 0 & \\frac{3}{2} & 1 & 4 \
\\\\\n 1 & 0 & 6 & 6 \\\\\n 1 & 1 & 3 & 9 \\\\\n 2 & \\frac{1}{2} & 3 & 6 \
\\\\\n 3 & 0 & 1 & 1 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "Total[dims[[All,-1]]] == 4^3", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> {{"Output", "True", "Out[5]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\text{True}"}|>}}|>}|>

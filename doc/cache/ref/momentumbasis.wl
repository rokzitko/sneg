<|"Source" -> "momentumbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[f];\nb = \
momentumbasis[qszbasis[{f[0],f[1]}],{2}];\nTableForm[{Last[First[#]],Length[L\
ast[#]]} & /@ Select[b,Take[First[#],2] == {0,0} &],\n TableHeadings -> \
{None,{\"K\",\"States\"}}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"K\"", HoldForm], 
            TagBox["\"States\"", HoldForm]}, {"0", "2"}, {"1", "2"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"K", "States"}}]]], "Out[3]//TableForm="}}|>}|>

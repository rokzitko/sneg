<|"Source" -> "getting-started.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "Needs[\"sneg`\"];\n$SnegVersion", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"2.2.0\"", "Out[2]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{2.2.0}"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nu = d[AN, UP]; v = d[AN, \
DO];\nTableForm[{{antikomutator[u, conj[u]], antikomutator[u, conj[v]]},\n    \
       {antikomutator[v, conj[u]], antikomutator[v, conj[v]]}},\n  \
TableHeadings -> {{\"up\", \"down\"}, {\"up\", \"down\"}}]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{StyleBox["\[Null]", ShowStringCharacters -> False], 
            TagBox["\"up\"", HoldForm], TagBox["\"down\"", HoldForm]}, 
           {TagBox["\"up\"", HoldForm], "1", "0"}, 
           {TagBox["\"down\"", HoldForm], "0", "1"}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {False, True, {False}, False}, 
            "Rows" -> {False, True, {False}, False}}, ColumnAlignments -> 
           Left], {OutputFormsDump`HeadedRows, 
          OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {{"up", "down"}, 
            {"up", "down"}}]]], "Out[5]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{c|c|c}\n  & \\text{up} & \\text{down} \
\\\\\n\\hline\n \\text{up} & 1 & 0 \\\\\n\\hline\n \\text{down} & 0 & 1 \
\\\\\n\\end{array}"}|>}}|>, <|"Code" -> "snegrealconstants[\\[Epsilon], \
U];\nh = \\[Epsilon] number[d[]] + U hubbard[d[]];\nblocks = \
makematricesbzvc[h, qsbasisvc[{d[]}]];\nTableForm[{#[[1, 1]], #[[1, 2]], \
#[[2, 1, 1]]} & /@ blocks,\n  TableHeadings -> {None, {\"Q\", \"Spin\", \
\"Energy\"}}]", "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Q\"", HoldForm], 
            TagBox["\"Spin\"", HoldForm], TagBox["\"Energy\"", HoldForm]}, 
           {RowBox[{"-", "1"}], "0", "0"}, {"0", FractionBox["1", "2"], 
            "\[Epsilon]"}, {"1", "0", RowBox[{"U", "+", RowBox[{"2", " ", 
                "\[Epsilon]"}]}]}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Q", "Spin", 
             "Energy"}}]]], "Out[9]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{Q} & \\text{Spin} & \
\\text{Energy} \\\\\n\\hline\n -1 & 0 & 0 \\\\\n\\hline\n 0 & \\frac{1}{2} & \
\\epsilon  \\\\\n 1 & 0 & U+2 \\epsilon  \\\\\n\\end{array}"}|>}}|>}|>

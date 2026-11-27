<|"Source" -> "FirstCalculation.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "Needs[\"sneg`\"];\nsnegfermionoperators[d];\nsneg\
realconstants[\\[Epsilon],U];", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {}|>, <|"Code" -> "Table[antikomutator[d[AN,s],d[CR,r]], \
{s,{UP,DO}}, {r,{UP,DO}}] // MatrixForm", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"1", "0"}, {"0", "1"}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[4]//MatrixForm="}}|>, <|"Code" -> "h = \\[Epsilon] number[d[]] + \
U hubbard[d[]];\nm = \
makematricesbzvc[h,qsbasisvc[{d[]}]];\nTableForm[{Sequence @@ First[#], \
#[[2,1,1]]} & /@ m,\n TableHeadings -> {None,{\"Q\",\"Spin\",\"Energy\"}}]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
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
             "Energy"}}]]], "Out[7]//TableForm="}}|>, 
   <|"Code" -> "Total[(2 #[[1,2]]+1) Length[Last[#]] & /@ m]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", "4", "Out[8]="}}|>, 
   <|"Code" -> "observables = \
{number[d[]],spinz[d[]],pow[spinz[d[]],2]};\nTableForm[{thermalaverage[#,h /. \
{\\[Epsilon] -> -1,U -> 2},{d},0]} & /@ observables,\n TableHeadings -> \
{{\"Occupation\",\"Spin z\",\"Squared spin z\"},{\"Mean\"}}]", 
    "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{StyleBox["\[Null]", ShowStringCharacters -> False], 
            TagBox["\"Mean\"", HoldForm]}, {TagBox["\"Occupation\"", 
             HoldForm], "1.`"}, {TagBox["\"Spin z\"", HoldForm], "0"}, 
           {TagBox["\"Squared spin z\"", HoldForm], "0.25`"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {False, True, {False}, False}, 
            "Rows" -> {False, True, {False}, False}}, ColumnAlignments -> 
           Left], {OutputFormsDump`HeadedRows, 
          OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {{"Occupation", "Spin z", 
             "Squared spin z"}, {"Mean"}}]]], "Out[10]//TableForm="}}|>}|>

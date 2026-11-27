<|"Source" -> "application-solutions.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "hm = {{-2,-1,-1},{-1,1,-1},{-1,-1,1}};\n{Eigenvalues[hm], \
Tr[hm], Tr[hm.hm]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "1"}], "-", 
                 SqrtBox["3"]}], ",", "2", ",", RowBox[{RowBox[{"-", "1"}], 
                 "+", SqrtBox["3"]}]}], "}"}], ",", "0", ",", "12"}], "}"}], 
       "Out[2]=", <|"Kind" -> "List", "Parts" -> 
         {"\\left\\{-1-\\sqrt{3},2,\\sqrt{3}-1\\right\\}", "0", "12"}|>}}|>, 
   <|"Code" -> "el[x_,y_] := x PauliZ + y PauliX;\nSort[Eigenvalues[el[3,4]]] \
== Sort[Eigenvalues[el[0,5]]]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "True", "Out[4]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nhp = 2 hubbard[d[]] - number[d[]] - \
bcs[d[]];\nmm = makematricesbzvc[hp, \
nonebasisvc[{d[]}]];\nSort[Eigenvalues[First[mm][[2]]]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
           RowBox[{"-", "1"}], ",", RowBox[{"-", "1"}], ",", "1"}], "}"}], 
       "Out[8]=", <|"Kind" -> "List", "Parts" -> {"-1", "-1", "-1", 
          "1"}|>}}|>, <|"Code" -> "snegspinoperators[{s,1/2}];\ntri = \
Total[spinspin @@@ Subsets[s /@ Range[3], {2}]];\nev = \
Flatten[Eigenvalues[Normal[Last[#]]] & /@\n  spinclustermatrices[tri,{s -> \
Range[3]}]];\nTableForm[Sort[Tally[ev]], TableHeadings -> {None, {\"Energy / \
J\",\"States\"}}]", "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Energy / J\"", HoldForm], TagBox["\"States\"", 
             HoldForm]}, {RowBox[{"-", FractionBox["3", "4"]}], "4"}, 
           {FractionBox["3", "4"], "4"}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Energy / J", "States"}}]]], "Out[12]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{Energy \
/ J} & \\text{States} \\\\\n\\hline\n -\\frac{3}{4} & 4 \\\\\n\\hline\n \
\\frac{3}{4} & 4 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[V];\nApart[greens\
function[d[AN,UP],d[CR,UP],V hop[c[],d[]],z],z]", "Reset" -> False, 
    "InLabel" -> "In[13]:=", "Cells" -> 
     {{"Output", RowBox[{FractionBox["1", RowBox[{"2", " ", 
            RowBox[{"(", RowBox[{RowBox[{"-", "V"}], "+", "z"}], ")"}]}]], 
         "+", FractionBox["1", RowBox[{"2", " ", RowBox[
             {"(", RowBox[{"V", "+", "z"}], ")"}]}]]}], "Out[15]=", 
       <|"Kind" -> "Sum", "Parts" -> {"\\frac{1}{2 (z-V)}", 
          "+\\frac{1}{2 (V+z)}"}|>}}|>, 
   <|"Code" -> "occ[e_] := thermalaverage[number[d[],UP], e number[d[]], {d}, \
1];\n{komutator[number[d[],UP],number[d[]]],\n \
Round[(occ[1/1000]-occ[-1/1000])/(2/1000),1/10000]}", "Reset" -> False, 
    "InLabel" -> "In[16]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"-", FractionBox["1", "4"]}]}], "}"}], "Out[17]=", 
       <|"Kind" -> "List", "Parts" -> {"0", "-\\frac{1}{4}"}|>}}|>}|>

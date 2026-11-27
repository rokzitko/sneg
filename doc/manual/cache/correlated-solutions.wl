<|"Source" -> "correlated-solutions.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "Needs[\"sneg`\"];\nsnegfermionoperators[c, \
d];\nsites4 = c /@ Range[4];\nb4 = Select[qbasisvc[sites4], First[#] == {0} \
&];\natomic4 = First[makematricesbzvc[Total[hubbard[sites4]], \
b4]][[2]];\nTableForm[Sort[Tally[Eigenvalues[atomic4]]],\n  TableHeadings -> \
{None, {\"Energy / U\", \"States\"}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Energy / U\"", HoldForm], 
            TagBox["\"States\"", HoldForm]}, {"0", "16"}, {"1", "48"}, 
           {"2", "6"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Energy / U", 
             "States"}}]]], "Out[6]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cc}\n \\text{Energy / U} & \\text{States} \
\\\\\n\\hline\n 0 & 16 \\\\\n\\hline\n 1 & 48 \\\\\n 2 & 6 \
\\\\\n\\end{array}"}|>}}|>, <|"Code" -> "snegrealconstants[t, U, \
delta];\nsites2 = {c[1], c[2]};\nhd = U Total[hubbard[sites2]] - t hop[c[1], \
c[2]];\nbiased = hd + delta (number[c[2]] - number[c[1]])/2;\nbs = \
Select[qsbasisvc[sites2], First[#] == {0, 0} &];\nhs = \
First[makematricesbzvc[biased, bs]][[2]];\npar = {U -> 10, delta -> 6, t -> \
1/10};\nj2 = 4 t^2 U/(U^2 - delta^2);\nN[{-Min[Eigenvalues[hs /. par]], j2 /. \
par}, 6]", "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.0062417225581339781`6.", 
           ",", "0.00625`6."}], "}"}], "Out[15]=", <|"Kind" -> "List", 
        "Parts" -> {"0.00624172", "0.00625000"}|>}}|>, 
   <|"Code" -> "bc = qbasisvc[sites2];\nmc = makematricesbzvc[hd /. {t -> 1, \
U -> 4}, bc];\ne[q_] := Min[Eigenvalues[First[Select[mc, First[#] == {q} \
&]][[2]]]];\nSimplify[e[1] + e[-1] - 2 e[0] == Sqrt[32] - 2]", 
    "Reset" -> False, "InLabel" -> "In[16]:=", 
    "Cells" -> {{"Output", "True", "Out[19]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>, 
   <|"Code" -> "snegrealconstants[\\[Epsilon]];\nha = \\[Epsilon] number[d[]] \
+ U hubbard[d[]];\nmom = spectralmoments[d[AN, UP], d[CR, UP], ha, 2] /.\n  \
avg[number[d[], DO]] -> w;\nExpand[mom - {1, \\[Epsilon] + U w, \\[Epsilon]^2 \
+ (2 \\[Epsilon] U + U^2) w}]", "Reset" -> False, "InLabel" -> "In[20]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], 
         "}"}], "Out[23]=", <|"Kind" -> "List", "Parts" -> 
         {"0", "0", "0"}|>}}|>}|>

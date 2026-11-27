<|"Source" -> "lab-lang-firsov.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nham[n_] := \
isozsq[d[]]/2 + phononnumber[n] +\n  nc[number[d[]]-1, \
phononx[n]]/2;\nempty[n_] := First[keyselect[makematricesbzvc[ham[n],\n  \
transformtoPH[qsbasisvc[{d[]}],{n}]], {-1,0}]][[2]];\nempty[2] // MatrixForm"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{FractionBox["1", "2"], RowBox[{"-", FractionBox["1", 
                "2"]}], "0"}, {RowBox[{"-", FractionBox["1", "2"]}], 
             FractionBox["3", "2"], RowBox[{"-", FractionBox["1", 
                SqrtBox["2"]]}]}, {"0", RowBox[{"-", FractionBox["1", 
                SqrtBox["2"]]}], FractionBox["5", "2"]}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], "Out[4]//MatrixForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\left(\n\\begin{array}{ccc}\n \
\\frac{1}{2} & -\\frac{1}{2} & 0 \\\\\n -\\frac{1}{2} & \\frac{3}{2} & \
-\\frac{1}{\\sqrt{2}} \\\\\n 0 & -\\frac{1}{\\sqrt{2}} & \\frac{5}{2} \
\\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "rows = Table[With[{e = Min[Eigenvalues[N[empty[n],35]]]},\n  \
{n, N[e,8], N[-Log[10,e-1/4],5]}], {n,{1,2,4,8}}];\nTableForm[rows, \
TableHeadings -> {None,{\"Cutoff\",\"Energy\",\"-log10(error)\"}}]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Cutoff\"", HoldForm], TagBox["\"Energy\"", HoldForm], 
            TagBox["\"-log10(error)\"", HoldForm]}, 
           {"1", "0.2928932188134524756`8.", "1.3676113620036885471`5."}, 
           {"2", "0.2553557140949606186`8.", "2.271182615060907412`5."}, 
           {"4", "0.2500296612665070367`8.", "4.5278103089625882003`5."}, 
           {"8", "0.2500000000713693274`8.", "10.1464883962541565122`5."}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Cutoff", "Energy", "-log10(error)"}}]]], 
       "Out[6]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{Cutoff} & \\text{Energy} & \
\\text{-log10(error)} \\\\\n\\hline\n 1 & 0.29289322 & 1.3676 \\\\\n\\hline\n \
2 & 0.25535571 & 2.2712 \\\\\n 4 & 0.25002966 & 4.5278 \\\\\n 8 & 0.25000000 \
& 10.146 \\\\\n\\end{array}"}|>}}|>}|>

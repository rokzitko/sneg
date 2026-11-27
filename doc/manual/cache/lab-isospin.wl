<|"Source" -> "lab-isospin.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t,U];\
\nsites = {c[1],c[2]};\ntm = isospinminus[c[1],0] + isospinminus[c[2],1];\nh \
= -t hop[c[1],c[2]] + U Total[isozsq[sites]]/2;\nExpand[komutator[h,tm]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[6]=", <|"Kind" -> "Single", 
        "Parts" -> {"0"}|>}}|>, 
   <|"Code" -> "bz = transformQStoIS[qsbasisvc[sites],tm];\nmm = \
makematricesbzvc[h,bz];\nrows = Join @@ Table[{Sequence @@ \
First[blockdata],#} & /@ Eigenvalues[Last[blockdata]], \
{blockdata,mm}];\nTableForm[rows, TableHeadings -> \
{None,{\"Isospin\",\"Spin\",\"Energy\"}}]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Isospin\"", HoldForm], 
            TagBox["\"Spin\"", HoldForm], TagBox["\"Energy\"", HoldForm]}, 
           {"1", "0", "U"}, {FractionBox["1", "2"], FractionBox["1", "2"], 
            RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                 {RowBox[{RowBox[{"-", "2"}], " ", "t"}], "+", "U"}], 
                ")"}]}]}, {FractionBox["1", "2"], FractionBox["1", "2"], 
            RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                 {RowBox[{"2", " ", "t"}], "+", "U"}], ")"}]}]}, 
           {"0", "1", "0"}, {"0", "0", RowBox[{FractionBox["1", "2"], " ", 
              RowBox[{"(", RowBox[{"U", "-", SqrtBox[RowBox[{RowBox[
                      {"16", " ", SuperscriptBox["t", "2"]}], "+", 
                     SuperscriptBox["U", "2"]}]]}], ")"}]}]}, 
           {"0", "0", RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", 
                RowBox[{"U", "+", SqrtBox[RowBox[{RowBox[{"16", " ", 
                       SuperscriptBox["t", "2"]}], "+", SuperscriptBox["U", 
                      "2"]}]]}], ")"}]}]}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Isospin", "Spin", "Energy"}}]]], "Out[10]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{ccc}\n \
\\text{Isospin} & \\text{Spin} & \\text{Energy} \\\\\n\\hline\n 1 & 0 & U \
\\\\\n\\hline\n \\frac{1}{2} & \\frac{1}{2} & \\frac{1}{2} (U-2 t) \\\\\n \
\\frac{1}{2} & \\frac{1}{2} & \\frac{1}{2} (2 t+U) \\\\\n 0 & 1 & 0 \\\\\n 0 \
& 0 & \\frac{1}{2} \\left(U-\\sqrt{16 t^2+U^2}\\right) \\\\\n 0 & 0 & \
\\frac{1}{2} \\left(\\sqrt{16 t^2+U^2}+U\\right) \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "Total[(2 #[[1]]+1)(2 #[[2]]+1) & /@ rows]", "Reset" -> False, 
    "InLabel" -> "In[11]:=", "Cells" -> {{"Output", "16", "Out[11]=", 
       <|"Kind" -> "Single", "Parts" -> {"16"}|>}}|>}|>

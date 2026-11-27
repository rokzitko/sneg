<|"Source" -> "tutorial-solutions.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nha = hubbard[d[]] - \
number[d[]]/3;\nen = #[[2,1,1]] & /@ makematricesbzvc[ha, \
qsbasisvc[{d[]}]];\nTableForm[{{First[en]-en[[2]], Last[en]-en[[2]]}},\n  \
TableHeadings -> {None, {\"Removal / U\", \"Addition / U\"}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Removal / U\"", HoldForm], TagBox["\"Addition / U\"", 
             HoldForm]}, {FractionBox["1", "3"], FractionBox["2", "3"]}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Removal / U", "Addition / U"}}]]], "Out[4]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{Removal \
/ U} & \\text{Addition / U} \\\\\n\\hline\n \\frac{1}{3} & \\frac{2}{3} \
\\\\\n\\end{array}"}|>}}|>, <|"Code" -> "snegmajoranaoperators[gm];\nf = \
(gm[1] + I gm[2])/Sqrt[2];\nExpand[{nc[f,f], antikomutator[f,conj[f]], \
nc[conj[f],f]}]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "1", ",", 
           RowBox[{FractionBox["1", "2"], "+", RowBox[{"\[ImaginaryI]", " ", 
               RowBox[{RowBox[{"gm", "[", "1", "]"}], StyleBox[
                  "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 RowBox[{"gm", "[", "2", "]"}]}]}]}]}], "}"}], "Out[7]=", 
       <|"Kind" -> "List", "Parts" -> {"0", "1", 
          "\\frac{1}{2}+i \\mathrm{gm}_{1} \\mathrm{gm}_{2}"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbm = \
Select[qszbasisvc[{c[1],c[2]}], First[#] == {-1,1/2} &];\nhy = \
-hop[c[1],c[2]];\nSort[Eigenvalues[#[[1,2]]]] & /@\n  (makematricesbzvc[#,bm] \
& /@ {hy,2 hy}) // MatrixForm", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"-", "1"}], "1"}, {RowBox[{"-", "2"}], "2"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[11]//MatrixForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{cc}\n -1 & 1 \\\\\n -2 & 2 \
\\\\\n\\end{array}\n\\right)"}|>}}|>, <|"Code" -> "ff = Table[Exp[I n (2 Pi j \
+ Pi/2)/3]/Sqrt[3], {n,0,2}, \
{j,0,2}];\nFullSimplify[ConjugateTranspose[ff].ff] // MatrixForm", 
    "Reset" -> False, "InLabel" -> "In[12]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"1", "0", "0"}, {"0", "1", "0"}, {"0", "0", "1"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[13]//MatrixForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{ccc}\n 1 & 0 & 0 \\\\\n 0 & 1 & \
0 \\\\\n 0 & 0 & 1 \\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "snegspinoperators[{s,1/2}];\neb = \
Flatten[Eigenvalues[Normal[Last[#]]] & /@\n  \
spinclustermatrices[spinspin[s[1],s[2]], \
{s[1],s[2]}]];\nFullSimplify[(eb.Exp[-eb])/Total[Exp[-eb]] == 3 (1-E)/(4 \
(E+3))]", "Reset" -> False, "InLabel" -> "In[14]:=", 
    "Cells" -> {{"Output", "True", "Out[16]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>, 
   <|"Code" -> "bb = qbasisvc[{d[]}];\nddm = \
makematricesbzvc[hubbard[d[]],bb];\nTableForm[{First[First[#]],Tr[Last[#]]/Le\
ngth[Last[#]]} & /@ ddm,\n  TableHeadings -> {None, {\"Charge Q\",\"Mean \
doublons\"}}]", "Reset" -> False, "InLabel" -> "In[17]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Charge Q\"", HoldForm], TagBox["\"Mean doublons\"", 
             HoldForm]}, {RowBox[{"-", "1"}], "0"}, {"0", "0"}, {"1", "1"}}, 
          RowSpacings -> 1, ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Charge Q", "Mean doublons"}}]]], "Out[19]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{Charge \
Q} & \\text{Mean doublons} \\\\\n\\hline\n -1 & 0 \\\\\n\\hline\n 0 & 0 \
\\\\\n 1 & 1 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "bb = qbasisvc[{c[1],c[2]}];\nmm = Last /@ \
makematricesbzvc[hy,bb];\n{Total[Tr /@ mm], Total[Tr[#.#] & /@ mm]}", 
    "Reset" -> False, "InLabel" -> "In[20]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "16"}], "}"}], 
       "Out[22]=", <|"Kind" -> "List", "Parts" -> {"0", "16"}|>}}|>}|>

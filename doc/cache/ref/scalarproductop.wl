<|"Source" -> "scalarproductop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\ns = nc[c[CR, 1, UP], \
c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];\nt0 = nc[c[CR, 1, UP], c[CR, \
2, DO]] + nc[c[CR, 1, DO], c[CR, 2, UP]];\n{scalarproductop[s, s], \
scalarproductop[s, t0], scalarproductop[t0, nc[c[CR, 2, DO], c[CR, 1, UP]]]}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"2", ",", "0", ",", 
           RowBox[{"-", "1"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z];\n{scalarpr\
oductop[z c[CR, 1, UP], c[CR, 1, UP]], scalarproductop[c[CR, 1, UP], z c[CR, \
1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"Conjugate", "[", "z", "]"}], ",", "z"}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[f];\norderin\
g[f] = SEA;\n{scalarproductop[nc[f[CR, 1, UP], f[AN, -1, UP]], nc[f[CR, 1, \
UP], f[AN, -1, UP]]], scalarproductop[f[CR, -1, UP], f[CR, -1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\nH = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);\nb = \
{nc[c[CR, 1, UP], c[CR, 2, DO]], nc[c[CR, 1, DO], c[CR, 2, UP]], nc[c[CR, 1, \
UP], c[CR, 1, DO]], nc[c[CR, 2, UP], c[CR, 2, \
DO]]};\nOuter[scalarproductop[#1, nc[H, #2]] &, b, b] // MatrixForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"0", "0", RowBox[{"-", "t"}], RowBox[{"-", "t"}]}, 
            {"0", "0", "t", "t"}, {RowBox[{"-", "t"}], "t", "U", "0"}, 
            {RowBox[{"-", "t"}], "t", "0", "U"}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[5]//MatrixForm="}}|>, <|"Code" -> "snegfermionoperators[c];\ns = \
nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, \
UP]];\n{Sqrt[scalarproductop[s, s]], normop[s]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{SqrtBox["2"], ",", SqrtBox["2"]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nb = \
{c[CR, 1, UP], c[CR, 2, UP]};\nmatrixrepresentationop[hop[c[1], c[2]], b]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nscalarproductop[\
c[CR, 1, UP], c[CR, 1, UP] + nc[c[CR, 1, UP], c[AN, 2, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[2]="}}|>}|>

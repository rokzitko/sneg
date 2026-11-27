<|"Source" -> "lab-rashba.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t, alpha];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[matrixhop[-t IdentityMatrix[2] + \
I alpha PauliY, c[R], c[R + 1]], {R}];\nhk = \
ExpToTrig[blochphaseexp[blochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, \
k]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 "t", " ", RowBox[{"Cos", "[", "k", "]"}]}], ",", RowBox[
                {"2", " ", "\[ImaginaryI]", " ", "alpha", " ", RowBox[
                  {"Sin", "[", "k", "]"}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 "\[ImaginaryI]", " ", "alpha", " ", RowBox[{"Sin", "[", "k", 
                   "]"}]}], ",", RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
                 RowBox[{"Cos", "[", "k", "]"}]}]}], "}"}]}], "}"}], 
       "Out[6]=", <|"Kind" -> "Single", "Parts" -> {"\\left(\n\\begin{array}{\
cc}\n -2 t \\cos (k) & 2 i \\alpha  \\sin (k) \\\\\n -2 i \\alpha  \\sin (k) \
& -2 t \\cos (k) \\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "Eigenvalues[hk]", "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"2", " ", RowBox[{"(", RowBox[
                {RowBox[{RowBox[{"-", "t"}], " ", RowBox[{"Cos", "[", "k", 
                     "]"}]}], "-", RowBox[{"alpha", " ", RowBox[{"Sin", "[", 
                     "k", "]"}]}]}], ")"}]}], ",", 
           RowBox[{"2", " ", RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "t"}], 
                   " ", RowBox[{"Cos", "[", "k", "]"}]}], "+", RowBox[
                  {"alpha", " ", RowBox[{"Sin", "[", "k", "]"}]}]}], 
               ")"}]}]}], "}"}], "Out[7]=", <|"Kind" -> "List", 
        "Parts" -> {"2 (-\\alpha  \\sin (k)-t \\cos (k))", 
          "2 (\\alpha  \\sin (k)-t \\cos (k))"}|>}}|>}|>

<|"Source" -> "lab-ssh.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[v, w];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nHssh = fourier[sum[v hop[f[R, \"A\"], \
f[R, \"B\"]] + w hop[f[R + 1, \"A\"], f[R, \"B\"]], {R}], f -> fk, R -> \
k];\nh = blochhamiltonian[Hssh, {fk[\"A\"], fk[\"B\"]}, k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", RowBox[{"v", "+", RowBox[
                  {"w", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "1"}]], DisplayForm]}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"v", "+", RowBox[{"w", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}]}], "}"}], "Out[6]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{cc}\n 0 & w \
\\text{blochphase}(-k,1)+v \\\\\n w \\text{blochphase}(k,1)+v & 0 \
\\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "Simplify[Eigenvalues[ExpToTrig[blochphaseexp[h]]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["v", "2"], 
                "+", SuperscriptBox["w", "2"], "+", RowBox[{"2", " ", "v", 
                  " ", "w", " ", RowBox[{"Cos", "[", "k", "]"}]}]}]]}], ",", 
           SqrtBox[RowBox[{SuperscriptBox["v", "2"], "+", SuperscriptBox["w", 
               "2"], "+", RowBox[{"2", " ", "v", " ", "w", " ", 
                RowBox[{"Cos", "[", "k", "]"}]}]}]]}], "}"}], "Out[7]=", 
       <|"Kind" -> "List", "Parts" -> {"-\\sqrt{2 v w \\cos (k)+v^2+w^2}", 
          "\\sqrt{2 v w \\cos (k)+v^2+w^2}"}|>}}|>}|>

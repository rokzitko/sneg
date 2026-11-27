<|"Source" -> "lab-hopping.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
H = -t Sum[hop[c[j], c[j + 1]], {j, 4}];\nh = matrixrepresentationop[H, \
Table[c[CR, j, UP], {j, 5}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[{"-", "t"}], ",", 
               "0", ",", "0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"-", "t"}], ",", "0", ",", RowBox[
                {"-", "t"}], ",", "0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", RowBox[{"-", "t"}], ",", "0", ",", 
               RowBox[{"-", "t"}], ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0", ",", RowBox[{"-", "t"}], ",", 
               "0", ",", RowBox[{"-", "t"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", RowBox[
                {"-", "t"}], ",", "0"}], "}"}]}], "}"}], "Out[4]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\left(\n\\begin{array}{ccccc}\n 0 \
& -t & 0 & 0 & 0 \\\\\n -t & 0 & -t & 0 & 0 \\\\\n 0 & -t & 0 & -t & 0 \\\\\n \
0 & 0 & -t & 0 & -t \\\\\n 0 & 0 & 0 & -t & 0 \\\\\n\\end{array}\n\\right)"}|>\
}}|>, 
   <|"Code" -> "Sort[Eigenvalues[h]] == Sort[Table[-2 t Cos[Pi m/6], {m, \
5}]]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", "True", "Out[5]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>}|>

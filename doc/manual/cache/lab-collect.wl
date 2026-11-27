<|"Source" -> "lab-collect.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g];\ns \
= sum[f[k] + g[k], {k}];\n{s == sum[f[k], {k}] + sum[g[k], {k}], \
sumExpand[s], sumCollect[sumExpand[s]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", 
                     "]"}], "+", RowBox[{"g", "[", "k", "]"}]}], ",", 
                 RowBox[{"{", "k", "}"}]}], "]"}], "\[Equal]", 
             RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", 
                     "]"}], ",", RowBox[{"{", "k", "}"}]}], "]"}], "+", 
               RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "k", "]"}], ",", 
                   RowBox[{"{", "k", "}"}]}], "]"}]}]}], ",", 
           RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", "]"}], 
                 ",", RowBox[{"{", "k", "}"}]}], "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "k", "]"}], ",", 
                 RowBox[{"{", "k", "}"}]}], "]"}]}], ",", 
           RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
                 "+", RowBox[{"g", "[", "k", "]"}]}], ",", RowBox[
                {"{", "k", "}"}]}], "]"}]}], "}"}], "Out[4]=", 
       <|"Kind" -> "List", "Parts" -> 
         {"\\sum_{k} \\left(f(k)+g(k)\\right)=\\sum_{k} f(k)+\\sum_{k} g(k)", 
          "\\sum_{k} f(k)+\\sum_{k} g(k)", 
          "\\sum_{k} \\left(f(k)+g(k)\\right)"}|>}}|>}|>

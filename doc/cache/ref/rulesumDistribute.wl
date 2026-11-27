<|"Source" -> "rulesumDistribute.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g, \
h];\ns = sum[f[k] h[k] + g[k] (1 + h[k]), {k}];\ns //. rulesumDistribute", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
               " ", RowBox[{"h", "[", "k", "]"}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"g", "[", "k", "]"}], 
               " ", RowBox[{"(", RowBox[{"1", "+", RowBox[{"h", "[", "k", 
                     "]"}]}], ")"}]}], ",", RowBox[{"{", "k", "}"}]}], 
           "]"}]}], "Out[4]="}}|>, <|"Code" -> "sumExpand[s]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "k", "]"}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
               " ", RowBox[{"h", "[", "k", "]"}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"g", "[", "k", "]"}], 
               " ", RowBox[{"h", "[", "k", "]"}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g, h];\nsum[(f[k] + \
g[k]) h[k], {k}] //. rulesumDistribute", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{"(", RowBox[{RowBox[{"f", "[", "k", "]"}], "+", 
                 RowBox[{"g", "[", "k", "]"}]}], ")"}], " ", 
             RowBox[{"h", "[", "k", "]"}]}], ",", RowBox[{"{", "k", "}"}]}], 
         "]"}], "Out[3]="}}|>}|>

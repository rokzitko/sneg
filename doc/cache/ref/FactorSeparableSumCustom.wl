<|"Source" -> "FactorSeparableSumCustom.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, j, k];\nsnegrealfunctions[f, \
g, h];\nFactorSeparableSumCustom[sum[f[i] g[j] h[i, k], {i, j, k}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "j", "]"}], ",", 
             RowBox[{"{", "j", "}"}]}], "]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], 
               " ", RowBox[{"h", "[", RowBox[{"i", ",", "k"}], "]"}]}], ",", 
             RowBox[{"{", RowBox[{"i", ",", "k"}], "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfreeindexes[i, j, \
k];\nsnegrealfunctions[f, g];\nFactorSeparableSumCustom[sum[f[i] g[j], {i, j, \
k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{"1", ",", RowBox[{"{", "k", "}"}]}], 
           "]"}], " ", RowBox[{"sum", "[", RowBox[
            {RowBox[{"f", "[", "i", "]"}], ",", RowBox[{"{", "i", "}"}]}], 
           "]"}], " ", RowBox[{"sum", "[", RowBox[
            {RowBox[{"g", "[", "j", "]"}], ",", RowBox[{"{", "j", "}"}]}], 
           "]"}]}], "Out[3]="}}|>, <|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f, g, h];\nFactorSeparableSumCustom[sum[f[i] g[j] h[i, \
j], {i, j}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], " ", 
             RowBox[{"g", "[", "j", "]"}], " ", RowBox[{"h", "[", RowBox[
                {"i", ",", "j"}], "]"}]}], ",", 
           RowBox[{"{", RowBox[{"i", ",", "j"}], "}"}]}], "]"}], 
       "Out[3]="}}|>}|>

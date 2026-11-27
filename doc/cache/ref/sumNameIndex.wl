<|"Source" -> "sumNameIndex.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, j, k, \
l];\nsnegrealfunctions[f, g];\ns = sum[f[i] g[j], {i, j}] + sum[f[k] g[l], \
{k, l}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], 
               " ", RowBox[{"g", "[", "j", "]"}]}], ",", 
             RowBox[{"{", RowBox[{"i", ",", "j"}], "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
               " ", RowBox[{"g", "[", "l", "]"}]}], ",", 
             RowBox[{"{", RowBox[{"k", ",", "l"}], "}"}]}], "]"}]}], 
       "Out[3]="}}|>, 
   <|"Code" -> "sumNameIndex[sumAbstractIndex[s, idx], idx, {i, j}]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", RowBox[{"sum", "[", 
           RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], " ", RowBox[
                {"g", "[", "j", "]"}]}], ",", RowBox[{"{", RowBox[
                {"i", ",", "j"}], "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f];\nsumNameIndex[sumAbstractIndex[sum[f[i, j], {i, \
j}], idx], idx, {i}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{"f", "[", RowBox[{"i", ",", RowBox[{"idx", "[", "2", 
                 "]"}]}], "]"}], ",", RowBox[{"{", RowBox[{"i", ",", RowBox[
                {"idx", "[", "2", "]"}]}], "}"}]}], "]"}], "Out[3]="}}|>}|>

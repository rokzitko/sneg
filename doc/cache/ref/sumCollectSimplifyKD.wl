<|"Source" -> "sumCollectSimplifyKD.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, g, \
h];\nsumCollectSimplifyKD[sum[(f[i] + g[i]) h[i] KroneckerDelta[i, j], {i}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"(", RowBox[{RowBox[{"f", "[", "j", "]"}], "+", 
             RowBox[{"g", "[", "j", "]"}]}], ")"}], " ", 
         RowBox[{"h", "[", "j", "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> 
     "sumCollectSimplifyKD[sum[(1 + KroneckerDelta[i, j]) f[i], {i}]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"f", "[", "j", "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "i", "]"}], ",", 
             RowBox[{"{", "i", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, g, h];\ns = \
sum[(f[i] + g[i]) h[i] (1 + KroneckerDelta[i, j]), \
{i}];\n{sumCollectSimplifyKD[s], sumSimplifyKD[sumExpand[s]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{RowBox[{"(", RowBox[{RowBox[{"f", "[", 
                     "j", "]"}], "+", RowBox[{"g", "[", "j", "]"}]}], ")"}], 
               " ", RowBox[{"h", "[", "j", "]"}]}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"(", RowBox[
                      {RowBox[{"f", "[", "i", "]"}], "+", RowBox[{"g", "[", 
                         "i", "]"}]}], ")"}], " ", RowBox[{"h", "[", "i", 
                     "]"}]}], ",", RowBox[{"{", "i", "}"}]}], "]"}]}], ",", 
           RowBox[{RowBox[{RowBox[{"f", "[", "j", "]"}], " ", RowBox[
                {"h", "[", "j", "]"}]}], "+", RowBox[{RowBox[{"g", "[", "j", 
                 "]"}], " ", RowBox[{"h", "[", "j", "]"}]}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "i", 
                     "]"}], " ", RowBox[{"h", "[", "i", "]"}]}], ",", 
                 RowBox[{"{", "i", "}"}]}], "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"g", "[", "i", 
                     "]"}], " ", RowBox[{"h", "[", "i", "]"}]}], ",", 
                 RowBox[{"{", "i", "}"}]}], "]"}]}]}], "}"}], "Out[4]="}}|>}|>

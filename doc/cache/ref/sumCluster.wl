<|"Source" -> "sumCluster.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f]; \
sneglatticeindexes[{R}, Ns];\nring = sumCluster[sum[hop[f[R], f[R+1]], {R}], \
{3}];\nExpand[ring - clusterhop[1, f, f, 1, {3}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c]; sneglatticeindexes[{R}, \
Ns];\nsumCluster[sum[number[c[R+3]] - number[c[R]], {R}], {3}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>}|>

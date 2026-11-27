<|"Source" -> "nnop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nnnop[c[1]]=0; \
nnop[c[2]]=1;\ntm = Total[isospinminus[#,nnop[#]] & /@ \
{c[1],c[2]}];\nExpand[komutator[hop[c[1],c[2]],tm]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>}|>

<|"Source" -> "braket.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nbraket[vc[0, 0, 1, 0], hop[c[1], c[2]], vc[1, 0, 0, 0]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nbraket[vc[1, 0, 0, 1], spinspin[c[1], c[2]], vc[0, 1, 1, 0]] === \
braketvc[vc[1, 0, 0, 1], spinspin[c[1], c[2]], vc[0, 1, 1, 0]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>}|>

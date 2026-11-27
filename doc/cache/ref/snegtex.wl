<|"Source" -> "snegtex.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nsnegtex[-t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]])]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"U (-c^\\\\dagger_{1\\\\downarrow} \
c^\\\\dagger_{1\\\\uparrow} c_{1\\\\downarrow} \
c_{1\\\\uparrow}-c^\\\\dagger_{2\\\\downarrow} c^\\\\dagger_{2\\\\uparrow} \
c_{2\\\\downarrow} c_{2\\\\uparrow})-t (c^\\\\dagger_{1\\\\downarrow} \
c_{2\\\\downarrow}+c^\\\\dagger_{1\\\\uparrow} \
c_{2\\\\uparrow}+c^\\\\dagger_{2\\\\downarrow} \
c_{1\\\\downarrow}+c^\\\\dagger_{2\\\\uparrow} c_{1\\\\uparrow})\"", 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\n{snegtex[c[CR, 1, UP]], snegtex[c[AN, k, 2, DO]], snegtex[c[CR, \
\[Sigma]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"\"c^\\\\dagger_{1\\\\uparrow}\"", ",", 
           "\"c_{k,2\\\\downarrow}\"", ",", "\"c^\\\\dagger_{\\\\sigma}\""}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegbosonoperators[a];\nsnegfermi\
onoperators[d];\nsnegrealconstants[\[Omega], g];\nsnegtex[\[Omega] \
number[a[]] + g nc[a[CR] + a[AN], number[d[]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\"g (a^\\\\dagger \
d^\\\\dagger_{\\\\downarrow} d_{\\\\downarrow}+a^\\\\dagger \
d^\\\\dagger_{\\\\uparrow} d_{\\\\uparrow}+a d^\\\\dagger_{\\\\downarrow} \
d_{\\\\downarrow}+a d^\\\\dagger_{\\\\uparrow} d_{\\\\uparrow})+a^\\\\dagger \
a \\\\omega\"", "Out[4]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegrealconstants[J];\nsnegtex[J (nc[S[1, 3], S[2, 3]] + (nc[S[1, 4], \
S[2, 5]] + nc[S[1, 5], S[2, 4]])/2)]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\"J \\\\left(S^{z}_{1} \
S^{z}_{2}+\\\\frac{S^{+}_{1} S^{-}_{2}+S^{-}_{1} S^{+}_{2}}{2}\\\\right)\"", 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, \
\[Sigma]];\nsnegrealfunctions[\[CurlyEpsilon]];\nsnegtex[sum[\[CurlyEpsilon][\
k] number[c[k], \[Sigma]], {k, \[Sigma]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\"\\\\sum_{k,\\\\sigma} \
c^\\\\dagger_{k\\\\sigma} c_{k\\\\sigma} \\\\varepsilon (k)\"", 
       "Out[4]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nsnegmajo\
ranaoperators[gam];\nsnegxoperators[X, \"HubbardAtom\"];\n{snegtex[nc[f[CR, \
1], f[AN, 2]]], snegtex[nc[gam[1], gam[2]]], snegtex[X[1, \"UP\", \"DO\"]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"\"f^\\\\dagger_{1} f_{2}\"", 
           ",", "\"\\\\mathrm{gam}_{1} \\\\mathrm{gam}_{2}\"", ",", 
           "\"X^{UPDO}_{1}\""}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegbosonoperators[ph];\nsnegtex[ph[CR, 1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"\\\\mathrm{ph}^\\\\dagger_{1}\"", 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\nsnegrealconstants[\[CapitalDelta]];\nsnegrealfunctions[\[Xi]];\nH = \
sum[\[Xi][k] number[c[k]] - \[CapitalDelta] (nc[c[CR, k, UP], c[CR, -k, DO]] \
+ nc[c[AN, -k, DO], c[AN, k, UP]]), {k}];\nsnegtex[H]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\"\\\\sum_{k} \
\\\\left(\\\\xi (k) (c^\\\\dagger_{k\\\\downarrow} \
c_{k\\\\downarrow}+c^\\\\dagger_{k\\\\uparrow} c_{k\\\\uparrow})-\\\\Delta  \
(c_{-k\\\\downarrow} c_{k\\\\uparrow}-c^\\\\dagger_{-k\\\\downarrow} \
c^\\\\dagger_{k\\\\uparrow})\\\\right)\"", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U, \
\[CapitalDelta]];\n{snegtex[U hubbard[c[]]], snegtex[\[CapitalDelta] nc[c[CR, \
UP], c[CR, DO]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"\"-c^\\\\dagger_{\\\\downarr\
ow} c^\\\\dagger_{\\\\uparrow} c_{\\\\downarrow} c_{\\\\uparrow} U\"", ",", 
           "\"\\\\Delta  (-c^\\\\dagger_{\\\\downarrow} \
c^\\\\dagger_{\\\\uparrow})\""}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[eps];\nsnegtex[ep\
s number[c[]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "\"\\\\text{eps} (c^\\\\dagger_{\\\\downarrow} \
c_{\\\\downarrow}+c^\\\\dagger_{\\\\uparrow} c_{\\\\uparrow})\"", 
       "Out[3]="}}|>}|>

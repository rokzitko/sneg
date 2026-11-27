<|"Source" -> "spinclustermatrices.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegrealconstants[J];\nspinclustermatrices[J spinspin[S[1], S[2]], \
{S[1], S[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"{", FractionBox["J", "4"], 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                          "J", "4"]}], ",", FractionBox["J", "2"]}], "}"}], 
                   ",", RowBox[{"{", RowBox[{FractionBox["J", "2"], ",", 
                       RowBox[{"-", FractionBox["J", "4"]}]}], "}"}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", "1", "}"}], ",", RowBox[{"{", RowBox[
                  {"{", FractionBox["J", "4"], "}"}], "}"}]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "spinclustermatrices[J \
spinspin[S[1], S[2]], {S[1], S[2]}, \"Blocks\" -> False] // MatrixForm", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{FractionBox["J", "4"], "0", "0", "0"}, 
            {"0", RowBox[{"-", FractionBox["J", "4"]}], FractionBox["J", 
              "2"], "0"}, {"0", FractionBox["J", "2"], 
             RowBox[{"-", FractionBox["J", "4"]}], "0"}, {"0", "0", "0", 
             FractionBox["J", "4"]}}, RowSpacings -> 1, ColumnSpacings -> 1, 
           RowAlignments -> Baseline, ColumnAlignments -> Center], 
          "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[4]//MatrixForm="}}|>, <|"Code" -> "snegspinoperators[{S, 1/2}, \
{T, 1}];\nspinclustermatrices[spinspin[S[1], T[1]], {S[1], T[1]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", 
                   FractionBox["3", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", FractionBox["1", "2"], "}"}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {"-", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                          "1", "2"]}], ",", FractionBox["1", SqrtBox["2"]]}], 
                     "}"}], ",", RowBox[{"{", RowBox[{FractionBox["1", 
                        SqrtBox["2"]], ",", "0"}], "}"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", "2"], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                      {"0", ",", FractionBox["1", SqrtBox["2"]]}], "}"}], 
                   ",", RowBox[{"{", RowBox[{FractionBox["1", SqrtBox["2"]], 
                       ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", FractionBox["3", "2"], "}"}], ",", RowBox[
                {"{", RowBox[{"{", FractionBox["1", "2"], "}"}], "}"}]}], 
             "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nspinclustermatrices[Sum[nc[S[0, \
a], S[1, a]], {a, 3}], {S -> {0, 1}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{"{", FractionBox["1", "4"], "}"}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", "0", "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"{", RowBox[{RowBox[{"-", FractionBox["1", "4"]}], 
                       ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                    {"{", RowBox[{FractionBox["1", "2"], ",", RowBox[
                        {"-", FractionBox["1", "4"]}]}], "}"}]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", 
               RowBox[{"{", RowBox[{"{", FractionBox["1", "4"], "}"}], 
                 "}"}]}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nspinclustermatrices[S[1, \"a\", \
3] + S[1, \"b\", 3], {S -> {{1, \"a\"}, {1, \"b\"}}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{"{", RowBox[{"-", "1"}], "}"}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                     "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0"}], 
                     "}"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{"{", "1", "}"}], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinoperators[{T, \
1}];\nsnegrealconstants[Dz, B];\nspinclustermatrices[Dz nc[T[1, 3], T[1, 3]] \
+ B T[1, 3], {T[1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"{", RowBox[
                    {RowBox[{"-", "B"}], "+", "Dz"}], "}"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{"{", RowBox[{"B", "+", "Dz"}], "}"}], "}"}]}], 
             "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nspinclustermatrices[S[1, 1], \
{S[1]}, \"Blocks\" -> False]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox["1", "2"]}], 
             "}"}], ",", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
               "0"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nm = \
spinclustermatrices[Sum[spinspin[S[i], S[i + 1]], {i, 9}], {S -> Range[10]}, \
\"Sparse\" -> True];\n{Length[m], Head[m[[6, 2]]], Dimensions[m[[6, 2]]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"11", ",", "SparseArray", 
           ",", RowBox[{"{", RowBox[{"252", ",", "252"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegrealconstants[J];\nH = J Sum[spinspin[S[i], S[Mod[i, 4] + 1]], \
{i, 4}];\n{First[#], Min[Eigenvalues[Last[#]] /. J -> 1]} & /@ \
spinclustermatrices[H, {S -> Range[4]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "2"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"-", "1"}], "}"}], ",", RowBox[{"-", "1"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"-", "2"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"-", "1"}]}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", "2", "}"}], ",", "1"}], "}"}]}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "snegspinoperators[{S, 1/2}];\nn = \
12;\nm = spinclustermatrices[Sum[spinspin[S[i], S[i + 1]], {i, n - 1}], {S -> \
Range[n]}, \"Sparse\" -> True];\nh0 = Last[First[Select[m, First[#] == {0} \
&]]];\n{Dimensions[h0], First[Eigenvalues[N[h0], 1]]/n, 1/4 - Log[2.]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"924", ",", "924"}], "}"}], ",", 
           RowBox[{"-", "0.42850755273671093`"}], ",", 
           RowBox[{"-", "0.4431471805599453`"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegspinoperators[{T, 1}];\nsnegrealconstants[J, Dz];\nH = J \
spinspin[T[1], T[2]] + Dz (stevens[2, 0, T[1]] + stevens[2, 0, T[2]] + \
4)/3;\n{First[#], Eigenvalues[Last[#]]} & /@ spinclustermatrices[H /. {J -> \
1, Dz -> 1/2}, {T[1], T[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "2"}], 
                 "}"}], ",", RowBox[{"{", "2", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{FractionBox["3", "2"], ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", SqrtBox["2"]}], ",", SqrtBox["2"], 
                   ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{FractionBox["3", "2"], ",", RowBox[{"-", 
                     FractionBox["1", "2"]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "2", "}"}], ",", RowBox[
                {"{", "2", "}"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[{T, 1}];\nTable[spinclustermatrices[T[1, \
t], {T[1]}, \"Blocks\" -> False], {t, {1, 4}}] == {spinmatrixX[1], \
spinmatrixP[1]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegspinlessfermionoperators[f];\nH = Sum[spinspin[S[i], S[i + 1]], \
{i, 3}];\ne1 = Sort[Flatten[Eigenvalues /@ spinclustermatrices[H, {S -> \
Range[4]}][[All, 2]]]];\ne2 = Sort[Flatten[Eigenvalues /@ \
makematricesbzop[jordanwigner[H, S -> f, Range[4]], qszbasis[Table[f[i], {i, \
4}]]][[All, 2]]]];\nChop[N[e1 - e2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0", ",", 
           "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", ",", "0", 
           ",", "0", ",", "0", ",", "0", ",", "0", ",", "0"}], "}"}], 
       "Out[6]="}}|>, 
   <|"Code" -> 
     "snegspinoperators[{S, 1/2}];\nspinclustermatrices[S[1, 1], {S[1]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "spinclustermatrices::sz: The operator does not \
conserve the total S^z; use \"Blocks\" -> False."}, {"Output", "$Failed", 
       "Out[2]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nspinclustermatrices[spinspin[S[1], S[2]], {S[1]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "spinclustermatrices::op: \
S[2, 1] is not an operator of the listed spin sites."}, 
      {"Output", "$Failed", "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[G];\nspinclustermatrices[G[1, 3], {G[1]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "Sneg`Private`snegspinsites::size: The spin G \
needs a declared numeric size, e.g. snegspinoperators[{G, 1/2}]."}, 
      {"Output", "$Failed", "Out[2]="}}|>}|>

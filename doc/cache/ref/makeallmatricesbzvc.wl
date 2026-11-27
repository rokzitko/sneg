<|"Source" -> "makeallmatricesbzvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nbz = \
qszbasisvc[{d[]}];\nmakeallmatricesbzvc[d[CR, UP], bz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                      {"0", ",", FractionBox["1", "2"]}], "}"}], ",", 
                   RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", "0"}], 
                     "}"}]}], "}"}], ",", RowBox[{"{", RowBox[{"{", "1", 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                      {"1", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[
                      {"0", ",", RowBox[{"-", FractionBox["1", "2"]}]}], 
                     "}"}]}], "}"}], ",", RowBox[{"{", RowBox[{"{", "1", 
                   "}"}], "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\nbz = \
qszbasisvc[{c[1], c[2]}];\nMap[{First[First[#]], Last[#]} &, \
makeallmatricesbzvc[-t hop[c[1], c[2]], bz]] == \
DeleteCases[makematricesbzvc[-t hop[c[1], c[2]], bz], {_, {{0}}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "First /@ makeallmatricesbzvc[spinplus[c[1]], bz]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", FractionBox["1", "2"]}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{"1", ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}]}], "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], c[2]}];\nH = \
-hop[c[1], c[2]] + 2 Total[isozsq[{c[1], c[2]}]];\neig = \
Association[Map[First[#] -> Eigensystem[N[Last[#]]] &, makematricesbzvc[H, \
bz]]];\n{vals, vecs} = eig[{0, 0}];\nE0 = Min[vals]; gs = \
vecs[[First[Ordering[vals]]]];\nE0", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"-", "0.8284271247461863`"}], 
       "Out[7]="}}|>, <|"Code" -> "cdag = Association[Rule @@@ \
makeallmatricesbzvc[c[CR, 1, UP], bz]];\ncann = Association[Rule @@@ \
makeallmatricesbzvc[c[AN, 1, UP], bz]];\nKeys[cdag]", "Reset" -> False, 
    "InLabel" -> "In[8]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", "2"}], ",", "0"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", RowBox[{"-", "1"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"1", ",", RowBox[{"-", FractionBox[
                      "1", "2"]}]}], "}"}]}], "}"}]}], "}"}], "Out[10]="}}|>, 
   <|"Code" -> "add = Transpose[{eig[{1, 1/2}][[1]] - E0, Abs[eig[{1, \
1/2}][[2]] . cdag[{{1, 1/2}, {0, 0}}] . gs]^2}]", "Reset" -> False, 
    "InLabel" -> "In[11]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"3.8284271247461863`", ",", 
               "0.07322330470336304`"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"1.8284271247461863`", ",", 
               "0.42677669529663703`"}], "}"}]}], "}"}], "Out[11]="}}|>, 
   <|"Code" -> "rem = Transpose[{E0 - eig[{-1, -1/2}][[1]], Abs[eig[{-1, \
-1/2}][[2]] . cann[{{-1, -1/2}, {0, 0}}] . gs]^2}]", "Reset" -> False, 
    "InLabel" -> "In[12]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"-", "3.8284271247461863`"}], ",", 
               "0.07322330470336316`"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"-", "1.8284271247461863`"}], ",", 
               "0.4267766952966363`"}], "}"}]}], "}"}], "Out[12]="}}|>, 
   <|"Code" -> "{Total[Join[add, rem][[All, 2]]], Sort[add[[All, 1]]] == \
Sort[-rem[[All, 1]]], N[Sqrt[4 + 4] + {-1, 1}]}", "Reset" -> False, 
    "InLabel" -> "In[13]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.9999999999999996`", ",", "True", 
           ",", RowBox[{"{", RowBox[{"1.8284271247461903`", ",", 
               "3.8284271247461903`"}], "}"}]}], "}"}], "Out[13]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qszbasisvc[{c[1], c[2]}];\na = \
makeallmatricesbzvc[c[CR, 1, DO], bz];\nb = makeallmatricesbzvc[c[AN, 1, DO], \
bz];\nSort[{Reverse[First[#]], ConjugateTranspose[Last[#]]} & /@ a] == \
Sort[b]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nFirst /@ makeallmatricesbzvc[c[CR, \
1, DO], qsbasisvc[{c[1], c[2]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", FractionBox["1", "2"]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "1"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"1", ",", FractionBox["1", "2"]}], 
                 "}"}]}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "First /@ makeallmatricesbzvc[c[CR, 1, DO], qszbasisvc[{c[1], c[2]}]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox[
                    "1", "2"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   "1"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"1", ",", FractionBox["1", "2"]}], 
                 "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>}|>

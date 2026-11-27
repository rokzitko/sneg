<|"Source" -> "schriefferwolffmatrix.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegrealconstants[D1, \
v];\nschriefferwolffmatrix[{{0, 0}, {0, D1}}, {{0, v}, {v, 0}}, {1}, 6]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", RowBox[{"-", FractionBox[SuperscriptBox[
                   "v", "2"], "D1"]}], "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", FractionBox[SuperscriptBox["v", "4"], 
                SuperscriptBox["D1", "3"]], "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"{", RowBox[{"-", FractionBox[RowBox[
                   {"2", " ", SuperscriptBox["v", "6"]}], SuperscriptBox[
                   "D1", "5"]]}], "}"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "Series[(D1 - Sqrt[D1^2 + 4 v^2])/2, {v, 0, 6}, Assumptions -> D1 > 0]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        RowBox[{RowBox[{"-", FractionBox[SuperscriptBox["v", "2"], "D1"]}], 
          "+", FractionBox[SuperscriptBox["v", "4"], SuperscriptBox["D1", 
            "3"]], "-", FractionBox[RowBox[{"2", " ", SuperscriptBox["v", 
              "6"]}], SuperscriptBox["D1", "5"]], "+", InterpretationBox[
           SuperscriptBox[RowBox[{"O", "[", "v", "]"}], "7"], 
           SeriesData[v, 0, {}, 2, 7, 1], Editable -> False]}], 
        SeriesData[v, 0, {-D1^(-1), 0, D1^(-3), 0, -2/D1^5}, 2, 7, 1], 
        Editable -> False], "Out[3]="}}|>, 
   <|"Code" -> "snegrealconstants[D1, v];\nheff = schriefferwolffmatrix[{{0, \
0, 0}, {0, 0, 0}, {0, 0, D1}}, {{0, 0, v}, {0, 0, v}, {v, v, 0}}, {1, 2}, \
2];\n{heff[[3]], Eigenvalues[heff[[3]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                      SuperscriptBox["v", "2"], "D1"]}], ",", RowBox[
                    {"-", FractionBox[SuperscriptBox["v", "2"], "D1"]}]}], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                      SuperscriptBox["v", "2"], "D1"]}], ",", RowBox[
                    {"-", FractionBox[SuperscriptBox["v", "2"], "D1"]}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"-", FractionBox[RowBox[{"2", " ", SuperscriptBox["v", 
                     "2"]}], "D1"]}], ",", "0"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegrealconstants[dl, D1, v1, \
v2];\nschriefferwolffmatrix[{{0, 0, 0}, {0, dl, 0}, {0, 0, D1}}, {{0, 0, v1}, \
{0, 0, v2}, {v1, v2, 0}}, {1, 2}, 2][[3]] // Simplify", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[SuperscriptBox["v1", 
                   "2"], "D1"]}], ",", FractionBox[RowBox[
                 {RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                        "D1"}], "+", "dl"}], ")"}], " ", "v1", " ", "v2"}], 
                RowBox[{"2", " ", "D1", " ", RowBox[{"(", RowBox[{"D1", "-", 
                      "dl"}], ")"}]}]]}], "}"}], ",", 
           RowBox[{"{", RowBox[{FractionBox[RowBox[{RowBox[{"(", RowBox[
                     {RowBox[{RowBox[{"-", "2"}], " ", "D1"}], "+", "dl"}], 
                    ")"}], " ", "v1", " ", "v2"}], RowBox[{"2", " ", "D1", 
                  " ", RowBox[{"(", RowBox[{"D1", "-", "dl"}], ")"}]}]], ",", 
               RowBox[{"-", FractionBox[SuperscriptBox["v2", "2"], 
                  RowBox[{"D1", "-", "dl"}]]}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "h0m = {{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, \
1.5, 0}, {0, 0, 0, 3}};\nvm = {{0, 0.2, 0.1, 0}, {0.2, 0, 0, 0.3}, {0.1, 0, \
0, 0.2}, {0, 0.3, 0.2, 0}};\nw = ArrayFlatten[{{1, 0, 0}, {0, \
RotationMatrix[0.4], 0}, {0, 0, 1}}];\nr1 = schriefferwolffmatrix[h0m, vm, \
{1, 4}, 3];\nr2 = schriefferwolffmatrix[w . h0m . Transpose[w], w . vm . \
Transpose[w], {1, 4}, 3];\nChop[r1 - r2]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}]}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "snegrealconstants[D1, v];\n{heff, gen} = \
schriefferwolffmatrix[{{0, 0}, {0, D1}}, {{0, v}, {v, 0}}, {1}, 2, \
\"Generator\" -> True];\ngen", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", FractionBox["v", "D1"]}]}], "}"}], ",", 
               RowBox[{"{", RowBox[{FractionBox["v", "D1"], ",", "0"}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\nbas = qszbasisvc[{c[1], c[2]}];\nh0m = \
Last[First[Select[makematricesbzvc[U (hubbard[c[1]] + hubbard[c[2]]), bas], \
First[#] == {0, 0} &]]];\nvm = Last[First[Select[makematricesbzvc[-t \
hop[c[1], c[2]], bas], First[#] == {0, 0} &]]];\n{MatrixForm[h0m], \
MatrixForm[vm]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {"U", "0", "0", "0"}, {"0", "0", "0", "0"}, {"0", "0", "0", 
                 "0"}, {"0", "0", "0", "U"}}, RowSpacings -> 1, 
               ColumnSpacings -> 1, RowAlignments -> Baseline, 
               ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", 
           TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{"0", "t", 
                 RowBox[{"-", "t"}], "0"}, {"t", "0", "0", "t"}, 
                {RowBox[{"-", "t"}], "0", "0", RowBox[{"-", "t"}]}, 
                {"0", "t", RowBox[{"-", "t"}], "0"}}, RowSpacings -> 1, 
               ColumnSpacings -> 1, RowAlignments -> Baseline, 
               ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "pind = Flatten[Position[Diagonal[h0m], \
0]];\nheff = schriefferwolffmatrix[h0m, vm, pind, \
4];\nEigenvalues[Total[heff]]", "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
             RowBox[{"(", RowBox[{RowBox[{"-", FractionBox[RowBox[{"8", " ", 
                      SuperscriptBox["t", "4"]}], SuperscriptBox["U", 
                     "3"]]}], "+", FractionBox[RowBox[{"2", " ", 
                    SuperscriptBox["t", "2"]}], "U"]}], ")"}]}], ",", "0"}], 
         "}"}], "Out[9]="}}|>, <|"Code" -> "Series[(U - Sqrt[U^2 + 16 \
t^2])/2, {t, 0, 4}, Assumptions -> U > 0]", "Reset" -> False, 
    "InLabel" -> "In[10]:=", "Cells" -> 
     {{"Output", InterpretationBox[RowBox[
         {RowBox[{"-", FractionBox[RowBox[{"4", " ", SuperscriptBox["t", 
                "2"]}], "U"]}], "+", FractionBox[RowBox[{"16", " ", 
             SuperscriptBox["t", "4"]}], SuperscriptBox["U", "3"]], "+", 
          InterpretationBox[SuperscriptBox[RowBox[{"O", "[", "t", "]"}], 
            "5"], SeriesData[t, 0, {}, 2, 5, 1], Editable -> False]}], 
        SeriesData[t, 0, {-4/U, 0, 16/U^3}, 2, 5, 1], Editable -> False], 
       "Out[10]="}}|>, <|"Code" -> "h0m = {{0, 0}, {0, 1.}}; vm = {{0, 1.}, \
{1., 0}};\n{heff, gen} = schriefferwolffmatrix[h0m, vm, {1}, 4, \"Generator\" \
-> True];\noff[lam_] := Abs[(MatrixExp[Sum[lam^m gen[[m]], {m, 4}]] . (h0m + \
lam vm) . MatrixExp[-Sum[lam^m gen[[m]], {m, 4}]])[[1, \
2]]];\noff[1/10]/off[1/20]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "31.79732668222751`", "Out[4]="}}|>, 
   <|"Code" -> 
     "schriefferwolffmatrix[{{0, 0}, {0, 0}}, {{0, 1}, {1, 0}}, {1}, 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "schriefferwolff::res: Resonance: the \
block-off-diagonal component 1 has a vanishing energy denominator, so P and Q \
are degenerate."}, {"Output", "$Failed", "Out[1]="}}|>, 
   <|"Code" -> 
     "schriefferwolffmatrix[{{0, 1}, {1, 2}}, {{0, 0}, {0, 0}}, {1}, 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "schriefferwolffmatrix::block: H0 couples the subspaces P and Q."}, 
      {"Output", "$Failed", "Out[1]="}}|>, 
   <|"Code" -> "snegrealconstants[t, U];\nschriefferwolffmatrix[{{U, 0, 0}, \
{0, 0, t}, {0, t, 0}}, IdentityMatrix[3], {1}, 2]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "schriefferwolffmatrix::diag: A symbolic H0 must be diagonal."}, 
      {"Output", "$Failed", "Out[2]="}}|>}|>

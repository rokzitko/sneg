<|"Source" -> "spinmatrixX.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "spinmatrixX[1] // MatrixForm", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"0", FractionBox["1", SqrtBox["2"]], "0"}, 
            {FractionBox["1", SqrtBox["2"]], "0", FractionBox["1", 
              SqrtBox["2"]]}, {"0", FractionBox["1", SqrtBox["2"]], "0"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[1]//MatrixForm="}}|>, <|"Code" -> "MatrixForm /@ \
{spinmatrixX[1/2], spinmatrixY[1/2], spinmatrixZ[1/2], spinmatrixP[1/2], \
spinmatrixM[1/2]}", "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {"0", FractionBox["1", "2"]}, {FractionBox["1", "2"], "0"}}, 
               RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", 
           TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{"0", RowBox[
                  {"-", FractionBox["\[ImaginaryI]", "2"]}]}, 
                {FractionBox["\[ImaginaryI]", "2"], "0"}}, RowSpacings -> 
                1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
               ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", 
           TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{FractionBox["1", "2"], 
                 "0"}, {"0", RowBox[{"-", FractionBox["1", "2"]}]}}, 
               RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", 
           TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{"0", "1"}, 
                {"0", "0"}}, RowSpacings -> 1, ColumnSpacings -> 1, 
               RowAlignments -> Baseline, ColumnAlignments -> Center], 
              "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
              BoxForm`e$]]], ",", TagBox[RowBox[{"(", "\[NoBreak]", 
              GridBox[{{"0", "0"}, {"1", "0"}}, RowSpacings -> 1, 
               ColumnSpacings -> 1, RowAlignments -> Baseline, 
               ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "spinmatrixX[3/2] // MatrixForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"0", FractionBox[SqrtBox["3"], "2"], "0", "0"}, 
            {FractionBox[SqrtBox["3"], "2"], "0", "1", "0"}, 
            {"0", "1", "0", FractionBox[SqrtBox["3"], "2"]}, 
            {"0", "0", FractionBox[SqrtBox["3"], "2"], "0"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[1]//MatrixForm="}}|>, <|"Code" -> "snegrealconstants[Dz, \
B];\nEigenvalues[Dz spinmatrixZ[1] . spinmatrixZ[1] + B spinmatrixX[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"Dz", ",", 
           RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                {"Dz", "-", SqrtBox[RowBox[{RowBox[{"4", " ", SuperscriptBox[
                       "B", "2"]}], "+", SuperscriptBox["Dz", "2"]}]]}], 
               ")"}]}], ",", RowBox[{FractionBox["1", "2"], " ", 
             RowBox[{"(", RowBox[{"Dz", "+", SqrtBox[RowBox[
                   {RowBox[{"4", " ", SuperscriptBox["B", "2"]}], "+", 
                    SuperscriptBox["Dz", "2"]}]]}], ")"}]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "{spinmatrixX[1/2], spinmatrixY[1/2], \
spinmatrixZ[1/2]} == {PauliX, PauliY, PauliZ}/2", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "{sx, sy, sz} = {spinmatrixX[2], spinmatrixY[2], \
spinmatrixZ[2]};\n{sx . sy - sy . sx == I sz, sx . sx + sy . sy + sz . sz == \
2 (2 + 1) IdentityMatrix[5]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinoperators[{Q, \
3/2}];\nspinclustermatrices[Q[1, 1], {Q[1]}, \"Blocks\" -> False] == \
spinmatrixX[3/2]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "spinmatrixX[1/3]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"spinmatrixX", "[", FractionBox["1", "3"], 
         "]"}], "Out[1]="}}|>}|>

<|"Source" -> "PauliX.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "MatrixForm /@ {PauliX, PauliY, PauliZ}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{"0", "1"}, 
                {"1", "0"}}, RowSpacings -> 1, ColumnSpacings -> 1, 
               RowAlignments -> Baseline, ColumnAlignments -> Center], 
              "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
              BoxForm`e$]]], ",", TagBox[RowBox[{"(", "\[NoBreak]", 
              GridBox[{{"0", RowBox[{"-", "\[ImaginaryI]"}]}, 
                {"\[ImaginaryI]", "0"}}, RowSpacings -> 1, ColumnSpacings -> 
                1, RowAlignments -> Baseline, ColumnAlignments -> Center], 
              "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
              BoxForm`e$]]], ",", TagBox[RowBox[{"(", "\[NoBreak]", 
              GridBox[{{"1", "0"}, {"0", RowBox[{"-", "1"}]}}, RowSpacings -> 
                1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
               ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "{PauliX . PauliY == I PauliZ, PauliY . \
PauliZ == I PauliX, PauliZ . PauliX == I PauliY, PauliX . PauliX == \
IdentityMatrix[2]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "True", ",", "True"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[1/2 VMV[{c[CR, UP], c[CR, \
DO]}, PauliX, {c[AN, UP], c[AN, DO]}] - spinx[c[]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "spinmatrixX[1/2] == PauliX/2", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "matrixhop[PauliX, c[1], c[2]] == spinfliphop[c[1], c[2]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

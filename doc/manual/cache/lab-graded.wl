<|"Source" -> "lab-graded.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\n{nc[X[2, \
\"UP\", \"0\"], X[1, \"0\", \"UP\"]], nc[X[2, \"UP\", \"DO\"], X[1, \"0\", \
\"UP\"]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"X", RowBox[{"1"}], RowBox[{"0", "UP"}]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"X", RowBox[{"2"}], RowBox[{"UP", "0"}]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], ")"}]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], 
                 RowBox[{"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                  {"UP", "DO"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]=", <|"Kind" -> "List", "Parts" -> {"-X^{0UP}_{1} X^{UP0}_{2}", 
          "X^{0UP}_{1} X^{UPDO}_{2}"}|>}}|>, 
   <|"Code" -> "antikomutator[X[i, \"0\", \"UP\"], X[j, \"UP\", \"0\"]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
          "KroneckerDeltaSeq"], " ", RowBox[
          {"(", RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"j"}], 
                 RowBox[{"0", "0"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"j"}], RowBox[
                  {"UP", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[3]=", <|"Kind" -> "Single", "Parts" -> 
         {"(X^{00}_{j}+X^{UPUP}_{j}) \\delta _{i,j}"}|>}}|>}|>

<|"Source" -> "snegspinlessfermionoperators.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f];\n{spinof[f], \
number[f[1]], hop[f[1], f[2]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", ""}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", ""}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{RowBox[{TagBox[StyleBox[
                 TemplateBox[{"f", RowBox[{"1", ""}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{"2", ""}], "\"\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"2", ""}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"1", ""}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "{antikomutator[f[AN, 1, 0], f[CR, 1, 0]], nc[f[CR, 1, 0], f[CR, 1, \
0]]}", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nbasis[f[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           TagBox[StyleBox[TemplateBox[{"f", RowBox[{""}], "\"\[Dagger]\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + \
nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, \
3}];\nFirst[bogoliubov[H /. {t -> 1, Dl -> 1, mu -> 0}, {f[1], f[2], f[3]}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.`", ",", "2.`", ",", 
           "2.`"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\n{f[CR, 1], f[CR, 1, 0], \
antikomutator[f[AN, 1], f[CR, 1, 0]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
              {"f", RowBox[{"1"}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", ""}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", "0"}], 
         "}"}], "Out[2]="}}|>}|>

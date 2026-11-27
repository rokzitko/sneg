<|"Source" -> "lab-custom.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[psi];\nsnegrealconst\
ants[x, y, z];\nsnegnonopQ[DiracDelta] = True;\npsi /: acmt[psi[AN, x_], \
psi[CR, y_]] := DiracDelta[x - y];\npsi /: acmt[psi[CR, x_], psi[AN, y_]] := \
DiracDelta[x - y];\nnc[psi[AN, x], psi[CR, y], psi[CR, z]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"psi", RowBox[
                {"y"}], "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"z"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"x"}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[{RowBox[{"DiracDelta", "[", 
             RowBox[{"x", "-", "z"}], "]"}], " ", 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"y"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "y"}], "]"}], 
           " ", TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"z"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "Out[6]=", 
       <|"Kind" -> "Sum", "Parts" -> {"\\mathrm{psi}^\\dagger_{y} \
\\mathrm{psi}^\\dagger_{z} \\mathrm{psi}_{x}", 
          "-\\mathrm{psi}^\\dagger_{y} \\delta (x-z)", 
          "+\\mathrm{psi}^\\dagger_{z} \\delta (x-y)"}|>}}|>, 
   <|"Code" -> "vevwick[nc[psi[AN, x], psi[AN, z], psi[CR, z], psi[CR, y]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"DiracDelta", "[", "0", "]"}], " ", 
           RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "y"}], "]"}]}], "-", 
         RowBox[{RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "z"}], "]"}], 
           " ", RowBox[{"DiracDelta", "[", RowBox[{RowBox[{"-", "y"}], "+", 
               "z"}], "]"}]}]}], "Out[7]=", <|"Kind" -> "Sum", 
        "Parts" -> {"\\delta (0) \\delta (x-y)", 
          "-\\delta (x-z) \\delta (z-y)"}|>}}|>}|>

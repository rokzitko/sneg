<|"Source" -> "spinminus.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nspinminus[c[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "nc[spinminus[c[1]], c[CR, 1, UP], VACUUM]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                 FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
           RGBColor[0, 1, 0]], TagBox[StyleBox[
           "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
           FontColor -> RGBColor[1, 0, 1]], 
          StyleForm[#1, FontColor -> RGBColor[1, 0, 1]] & ]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnc[spinminus[c[1]] + \
spinminus[c[2]], c[CR, 1, UP], c[CR, 2, UP], VACUUM]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ]}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[nc[spinplus[c[]], \
spinminus[c[]]] - (number[c[]]/2 + spinz[c[]] - hubbard[c[]])]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>}|>

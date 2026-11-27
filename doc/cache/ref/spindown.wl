<|"Source" -> "spindown.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\nspindown[c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                  1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[2]="}}|>, 
   <|"Code" -> "NestList[spindown, nc[c[CR, 1, UP], c[CR, 2, UP]], 3]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                 RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", FractionBox[RowBox[
             {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                      TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                         RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                 FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                        FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
              RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                      TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                          1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                          0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                       StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
            SqrtBox["2"]], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", "0"}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nspindow\
n[nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvc2ops[spindownvc[ops2vc[nc[c[CR, 1, UP], c[CR, 2, UP]]]]] == \
spindown[nc[c[CR, 1, UP], c[CR, 2, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[3]="}}|>}|>

<|"Source" -> "spinspinsymmetric.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nExpand[spinspinsymmetri\
c[{c[1], c[2], c[3]}] - (spinspin[c[1], c[2]] + spinspin[c[1], c[3]] + \
spinspin[c[2], c[3]])/3]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nstate = nc[c[CR, 1, UP], c[CR, 2, \
UP], c[CR, 3, UP], VACUUM];\nExpand[nc[spinspinsymmetric[{c[1], c[2], c[3]}], \
state]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["1", "4"], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
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
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"3", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nops = {c[1], c[2], c[3]};\nExpand[3 \
spinspinsymmetric[ops] - (manyspin[ops][[4]] - Total[spinss[ops]])/2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[3]="}}|>}|>

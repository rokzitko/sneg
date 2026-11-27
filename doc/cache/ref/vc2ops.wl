<|"Source" -> "vc2ops.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvc2ops[vc[1, 0, 0, 1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                 "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "vc2ops[vc[1, 1, 0, 0]]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"-", RowBox[{"(", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvc2ops[(vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", FractionBox[RowBox[
         {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                       StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
              ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                     RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                       1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
               SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm], 
            StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
            TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                    "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
        SqrtBox["2"]], "Out[3]="}}|>, <|"Code" -> "vc2ops[vacuum[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[1]="}}|>, 
   <|"Code" -> "vc2ops[vc[0, 1, 0, 0, ket[1/2]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                 FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
           RGBColor[0, 1, 0]], TagBox[RowBox[
           {TagBox[StyleBox["\"\[LeftBracketingBar]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ], RowBox[{FractionBox["1", "2"]}], 
            TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                 0]] & ]}], DisplayForm]}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nmakebasis[{d[]}];\nvc2ops[vc @@@ \
Tuples[{0, 1}, 2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
               ")"}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nv = vc[1, \
1, 0, 1];\nops2vc[vc2ops[v]] === v", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nvc2ops[vc[1, 0]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "vc2ops::incom: Incompatible lenghts of vc[] and BASIS."}, 
      {"Output", "\[Infinity]", "Out[3]="}}|>}|>

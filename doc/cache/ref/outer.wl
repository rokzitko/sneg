<|"Source" -> "outer.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "outer[{a1, a2}, {b1, b2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"a1", StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], "b1"}], ",", RowBox[
                {"a1", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                    0]], "b2"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"a2", StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], "b1"}], ",", RowBox[
                {"a2", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                    0]], "b2"}]}], "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nouter[{c[CR, 1, UP], c[CR, 2, UP]}, \
{c[AN, 1, UP], c[AN, 2, UP]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nFlatten[outer[{c[CR, 1, UP], c[CR, \
2, UP]}, {ket[0], ket[1]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                 RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
                RowBox[{"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], 
           ",", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
                RowBox[{"1"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], 
           ",", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
                RowBox[{"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], 
           ",", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
                RowBox[{"1"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                  FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                   FontColor -> RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}]}], 
         "}"}], "Out[2]="}}|>, 
   <|"Code" -> "Tr[outer[{a1, a2}, {b1, b2}]] == inner[{a1, a2}, {b1, b2}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

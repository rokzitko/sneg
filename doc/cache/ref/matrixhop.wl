<|"Source" -> "matrixhop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[alpha\
];\nmatrixhop[I alpha PauliY, c[1], c[2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{RowBox[{"-", "alpha"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"alpha", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"alpha", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "-", RowBox[{"alpha", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmatrixhop[{{t11, t12}, {t21, t22}}, \
c[1], c[2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"t22", " ", RowBox[{TagBox[StyleBox[TemplateBox[
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
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"t21", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"t12", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"t11", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{"Conjugate", "[", "t22", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{"Conjugate", "[", "t12", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{"Conjugate", "[", "t21", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{"Conjugate", "[", "t11", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[t];\nmatrixhop[SparseArray[{{1, 2} -> t}, \
{2, 2}], c[1], c[2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"t", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[t, theta];\nmatrixhop[t MatrixExp[I theta \
PauliY], c[1], c[2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"t", " ", RowBox[{"Cos", "[", "theta", "]"}], " ", 
           RowBox[{"(", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{"\[ImaginaryI]", " ", "t", " ", 
           RowBox[{"(", RowBox[{RowBox[{"\[ImaginaryI]", " ", RowBox[
                  {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
               RowBox[{"\[ImaginaryI]", " ", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
               RowBox[{"\[ImaginaryI]", " ", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
               RowBox[{"\[ImaginaryI]", " ", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
             ")"}], " ", RowBox[{"Sin", "[", "theta", "]"}]}]}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c, d];\nbond = (c[#1, \
1, #2] + c[#1, 2, #2])/Sqrt[2] &;\nmatrixhop[PauliZ, bond, d[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", FractionBox[RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                        "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                       StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                    "\"\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
              RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                      TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                         RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                 FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                   {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm]}]}], SqrtBox["2"]]}], "+", 
         FractionBox[RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                      FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                    TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          SqrtBox["2"]], "-", FractionBox[RowBox[
           {RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                   {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                       RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                 {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                        1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                         1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          SqrtBox["2"]], "+", FractionBox[RowBox[
           {RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                   {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                   {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          SqrtBox["2"]]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsnegrealconstants[t, \
alpha];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nH = \
sum[matrixhop[-t IdentityMatrix[2] + I alpha PauliY, c[R], c[R + 1]], \
{R}];\nhk = ExpToTrig[blochphaseexp[blochhamiltonian[fourier[H, c -> ck, R -> \
k], {ck}, k]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 "t", " ", RowBox[{"Cos", "[", "k", "]"}]}], ",", RowBox[
                {"2", " ", "\[ImaginaryI]", " ", "alpha", " ", RowBox[
                  {"Sin", "[", "k", "]"}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 "\[ImaginaryI]", " ", "alpha", " ", RowBox[{"Sin", "[", "k", 
                   "]"}]}], ",", RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
                 RowBox[{"Cos", "[", "k", "]"}]}]}], "}"}]}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "Eigenvalues[hk]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"2", " ", 
             RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "t"}], " ", 
                   RowBox[{"Cos", "[", "k", "]"}]}], "-", RowBox[{"alpha", 
                   " ", RowBox[{"Sin", "[", "k", "]"}]}]}], ")"}]}], ",", 
           RowBox[{"2", " ", RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "t"}], 
                   " ", RowBox[{"Cos", "[", "k", "]"}]}], "+", RowBox[
                  {"alpha", " ", RowBox[{"Sin", "[", "k", "]"}]}]}], 
               ")"}]}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, theta];\nH = \
Sum[matrixhop[-t MatrixExp[I theta PauliY], c[j], c[Mod[j, 3] + 1]], {j, \
3}];\nstates = Flatten[Table[c[CR, j, s], {j, 3}, {s, {UP, \
DO}}]];\nSimplify[Eigenvalues[matrixrepresentationop[H, states]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
             RowBox[{"Cos", "[", "theta", "]"}]}], ",", 
           RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
             RowBox[{"Cos", "[", "theta", "]"}]}], ",", 
           RowBox[{"t", " ", RowBox[{"(", RowBox[{RowBox[{"Cos", "[", 
                   "theta", "]"}], "-", RowBox[{SqrtBox["3"], " ", 
                   RowBox[{"Sin", "[", "theta", "]"}]}]}], ")"}]}], ",", 
           RowBox[{"t", " ", RowBox[{"(", RowBox[{RowBox[{"Cos", "[", 
                   "theta", "]"}], "-", RowBox[{SqrtBox["3"], " ", 
                   RowBox[{"Sin", "[", "theta", "]"}]}]}], ")"}]}], ",", 
           RowBox[{"t", " ", RowBox[{"(", RowBox[{RowBox[{"Cos", "[", 
                   "theta", "]"}], "+", RowBox[{SqrtBox["3"], " ", 
                   RowBox[{"Sin", "[", "theta", "]"}]}]}], ")"}]}], ",", 
           RowBox[{"t", " ", RowBox[{"(", RowBox[{RowBox[{"Cos", "[", 
                   "theta", "]"}], "+", RowBox[{SqrtBox["3"], " ", 
                   RowBox[{"Sin", "[", "theta", "]"}]}]}], ")"}]}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "H = Sum[matrixhop[-t MatrixExp[I theta \
PauliY], c[j], c[j + 1]], {j, \
2}];\nSimplify[Eigenvalues[matrixrepresentationop[H, states]]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}], ",", 
           RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}], ",", 
           RowBox[{SqrtBox["2"], " ", "t"}], ",", RowBox[{SqrtBox["2"], " ", 
             "t"}], ",", "0", ",", "0"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[a, b];\nsnegcomplexconstants[t11, t12, \
t21, t22];\nT = {{t11, t12}, {t21, \
t22}};\nmatrixrepresentationop[matrixhop[T, a[], b[]], {a[CR, UP], a[CR, DO], \
b[CR, UP], b[CR, DO]}] == ArrayFlatten[{{0, T}, {ConjugateTranspose[T], 0}}]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "Expand[matrixhop[T, a[], b[]] - \
matrixhop[ConjugateTranspose[T], b[], a[]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{matrixhop[IdentityMatrix[2], c[1], \
c[2]] == hop[c[1], c[2]], matrixhop[PauliX, c[1], c[2]] == spinfliphop[c[1], \
c[2]], Expand[matrixhop[PauliZ, c[1], c[2]] - hop[c[1], c[2], UP] + hop[c[1], \
c[2], DO]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[B];\nExpand[{matr\
ixhop[B/2 PauliZ, c[], c[]] - B spinz[c[]], matrixhop[B/4 PauliZ, c[], c[]] - \
B spinz[c[]]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{RowBox[{"-", FractionBox["1", "2"]}], 
               " ", "B", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{FractionBox["1", "2"], " ", "B", " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "matrixhop[{{1, 0, 0}}, c[1], c[2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "matrixhop::matrix: Expected a 2 by 2 hopping matrix, got {{1, 0, \
0}}."}, {"Output", "$Failed", "Out[1]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nmatrixhop[IdentityMatrix[2]\
, f[1], c[2]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "matrixhop::op: Expected a spin-1/2 fermion \
operator or an abstract-orbital Function, got f[1]."}, 
      {"Output", "$Failed", "Out[2]="}}|>}|>

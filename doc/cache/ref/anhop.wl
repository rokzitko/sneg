<|"Source" -> "anhop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[a, b];\nanhop[a[], b[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"b", RowBox[{TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
         "+", RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"b", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"b", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"b", RowBox[
                {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[2]="}}|>, <|"Code" -> "matrixrepresentationop[anhop[a[], b[]], \
{1, nc[a[CR, UP], b[CR, DO]], nc[a[CR, DO], b[CR, UP]]}]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", "1", ",", RowBox[{"-", "1"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "0", ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", "0", 
               ",", "0"}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nsnegrealconstants[kappa];\nkappa \
Total[anhop[{f[0], f[1]}, {f[1], f[2]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"kappa", " ", RowBox[
          {"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"f", RowBox[{"0", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], ")"}]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"0", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", 
                     TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                        RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                         RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"0", 
                     TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                        RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                         RGBColor[0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"0", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", 
                     TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                        RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                         RGBColor[0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"2", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ")"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[a, b];\n{Sx, Sy, Sz, \
S2} = manyspin[{a[], b[]}];\nH = anhop[a[], b[]];\nExpand[{komutator[H, Sx], \
komutator[H, Sy], komutator[H, Sz]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], 
       "Out[4]="}}|>, 
   <|"Code" -> "Expand[komutator[number[a[]] + number[b[]], H]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"b", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{"2", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"b", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{"2", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"b", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{"2", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"b", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[5]="}}|>, 
   <|"Code" -> "{Expand[anhop[a[], b[]] - anhop[b[], a[]]], Expand[anhop[a[], \
a[]] - 2 bcs[a[]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "anhop[a[], b[]] == genanhop[1, a[], b[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

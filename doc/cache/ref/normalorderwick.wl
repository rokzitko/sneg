<|"Source" -> "normalorderwick.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[CR, k1, UP], c[AN, k1, UP], c[CR, \
k2, DO], c[AN, k2, DO]];\nnormalorderwick[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
              {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                      StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                         0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
         "-", RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm], " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}]}], "Out[5]="}}|>, <|"Code" -> "vevwick[%]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[CR, k1, UP], c[AN, k1, UP], c[CR, \
k2, DO], c[AN, k2, DO]];\nnormalorder[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
              {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                      StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                         0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
         "+", RowBox[{"vev", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"k1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                    StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                       1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                    StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                       1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}]}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k];\nnormalorderwick[nc[c[AN, k, UP], c[CR, k, UP]]] == \
normalorder[nc[c[AN, k, UP], c[CR, k, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[4]="}}|>}|>

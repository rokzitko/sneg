<|"Source" -> "pow.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\npow[c[AN, UP] + c[CR, UP], 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\npow[a[AN] + a[CR], 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
              {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
               SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{"2", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nTable[pow[number[c[], UP], k], {k, 0, 3}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}, {T, 1}];\n{pow[S[1, 1], 2], \
Expand[pow[T[1, 1], 2] + pow[T[1, 2], 2] + pow[T[1, 3], 2]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "4"], ",", 
           "2"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Lambda]];\npow[\
\[Lambda] a[CR], 3]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{SuperscriptBox["\[Lambda]", "3"], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
              {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
               SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[d];\nExpand[pow[numbe\
r[d[]] - 1, 2] - isozsq[d[]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nExpand[pow[number[d[], UP] - \
number[d[], DO], 2] - (number[d[]] - 2 hubbard[d[]])]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a];\nvevwick[pow[(a[AN] + a[CR])/Sqrt[2], 4]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox["3", "4"], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nx = c[CR, 1, UP] + c[AN, 2, \
UP];\npow[x, 3] == nc[x, x, x]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegSeries[Exp, a[CR], 3]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "+", 
         TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
         FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"a", 
                "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                SuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm], 
            StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
            TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
               "Superscript", SyntaxForm -> SuperscriptBox], 
              ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
             DisplayForm]}], "2"], "+", RowBox[{FractionBox["1", "6"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{(c[CR, UP] + c[AN, UP])^2, \
pow[c[CR, UP] + c[AN, UP], 2]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{SuperscriptBox[RowBox[{"(", RowBox[{TagBox[
                 StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                        "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                       StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
                TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                        "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                       StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}], 
            "2"], ",", "1"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\npow[a[CR], -1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "pow::negativepower: Negative power detected: (a[0])^(-1)"}}|>, 
   <|"Code" -> "snegbosonoperators[a];\npow[{a[CR], a[AN]}, 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a", 
                 "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", "\"\""}, "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>}|>

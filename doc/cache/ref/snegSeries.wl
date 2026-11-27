<|"Source" -> "snegSeries.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\
\[Lambda]];\nsnegSeries[Exp, \[Lambda] a[CR], 3]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"1", "+", RowBox[{"\[Lambda]", " ", 
           TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", SuperscriptBox["\[Lambda]", 
            "2"], " ", RowBox[{TagBox[StyleBox[TemplateBox[{"a", 
                 "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{FractionBox["1", "6"], " ", 
           SuperscriptBox["\[Lambda]", "3"], " ", 
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
              DisplayForm]}]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\n{snegSeries[Sin[a[CR]], 3], \
snegSeries[Sqrt[1 + #] &, nc[a[CR], a[AN]], 2]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "-", RowBox[{FractionBox["1", "6"], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"1", "+", FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[
                   {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                    SuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                   SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "2"], "+", 
             RowBox[{FractionBox["1", "8"], " ", RowBox[{"(", RowBox[
                  {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                          TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                          SyntaxForm -> SuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                          FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"a", "\"\""}, "Superscript", 
                          SyntaxForm -> SuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "-", RowBox[
                    {TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                        "Superscript", SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                        SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"a", "\"\""}, "Superscript", 
                        SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"a", "\"\""}, "Superscript", 
                        SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm]}]}], ")"}]}]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[\[Theta]];\nExpand[snegSeries[Exp, I \[Theta] (c[AN, UP] + c[CR, UP]), 4]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "-", FractionBox[
          SuperscriptBox["\[Theta]", "2"], "2"], "+", 
         FractionBox[SuperscriptBox["\[Theta]", "4"], "24"], "+", 
         RowBox[{"\[ImaginaryI]", " ", "\[Theta]", " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{FractionBox["1", "6"], " ", "\[ImaginaryI]", " ", 
           SuperscriptBox["\[Theta]", "3"], " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{"\[ImaginaryI]", " ", "\[Theta]", " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[{FractionBox["1", "6"], " ", 
           "\[ImaginaryI]", " ", SuperscriptBox["\[Theta]", "3"], " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nsnegSeries[Log[1 + #] &, number[c[], UP], 3]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["5", "6"], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Lambda]];\nS = \
\[Lambda] (a[CR] - a[AN]);\nr = Expand[nc[snegSeries[Exp, S, 3], a[AN], \
snegSeries[Exp, -S, 3]]];\nr /. \[Lambda]^n_ /; n > 3 -> 0", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "\[Lambda]"}], "+", 
         TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[5]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Lambda]];\nvevwic\
k[nc[a[AN], snegSeries[Exp, \[Lambda] a[CR], 4]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\[Lambda]", 
       "Out[3]="}}|>, <|"Code" -> "snegbosonoperators[a];\nsnegSeries[Exp, \
a[CR] + a[AN], 2] == 1 + (a[CR] + a[AN]) + pow[a[CR] + a[AN], 2]/2", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Lambda]];\nSum[su\
perkomutator[\[Lambda] (a[CR] - a[AN]), a[AN], n]/n!, {n, 0, 3}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", "\[Lambda]"}], "+", 
         TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Lambda]];\nS = \
\[Lambda] (a[CR] - a[AN]);\nCollect[Expand[nc[snegSeries[Exp, S, 1], a[AN], \
snegSeries[Exp, -S, 1]]], \[Lambda]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "\[Lambda]"}], "+", 
         TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
         RowBox[{SuperscriptBox["\[Lambda]", "2"], " ", 
           RowBox[{"(", RowBox[{RowBox[{"-", TagBox[StyleBox[TemplateBox[
                    {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                     SuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{"2", " ", TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], "-", RowBox[
                {TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{"2", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                      {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                       SuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                      SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                      1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                      SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                      1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               "-", RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", "\"\""}, "Superscript", SyntaxForm -> 
                     SuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}]}], "Out[4]="}}|>}|>

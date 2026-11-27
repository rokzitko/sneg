<|"Source" -> "komutator.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\nkomutator[c[CR, 1, UP], c[AN, 2, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", 
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
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[2]="}}|>, 
   <|"Code" -> 
     "Expand[komutator[nc[c[CR, 1, UP], c[AN, 1, UP]], c[CR, 1, UP]]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\n{komutator[S[1, 4], S[1, 5]], \
komutator[S[1, 1], S[1, 2]], komutator[S[1, 1], S[2, 2]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"2", " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"\[ImaginaryI]", " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\n{sx, sy, sz} = \
spinxyz[d[]];\nExpand[komutator[sx, sy] - I sz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
p];\nsnegrealfunctions[eps];\nH = sum[eps[k] number[c[k], UP], \
{k}];\nsumSimplifyKD[sumExpand[komutator[c[AN, p, UP], H]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], " ", RowBox[{"eps", "[", "p", "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "komutator[{c[CR, 1, UP], c[CR, 2, UP]}, number[c[1]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", "0"}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrea\
lconstants[t, U];\nH = -t hop[c[1], c[2]] + U (hubbard[c[1]] + \
hubbard[c[2]]);\nExpand[komutator[number[c[1]] + number[c[2]], H]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "Expand[komutator[number[c[1]], H]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{RowBox[{"-", "t"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "-", RowBox[{"t", " ", 
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
              DisplayForm]}]}], "+", RowBox[{"t", " ", 
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
              DisplayForm]}]}], "+", RowBox[{"t", " ", 
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
              DisplayForm]}]}]}], "Out[5]="}}|>, 
   <|"Code" -> "Expand[komutator[c[AN, 1, UP], H]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{RowBox[{"-", "t"}], " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[{"U", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
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
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[1]="}}|>, <|"Code" -> "Expand[komutator[x, komutator[y, z]] + \
komutator[y, komutator[z, x]] + komutator[z, komutator[x, y]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[antikomutator[c[AN, 1, UP], \
c[CR, 1, UP]] - komutator[c[AN, 1, UP], c[CR, 1, UP]] - 2 nc[c[CR, 1, UP], \
c[AN, 1, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "{komutator[c[AN, 1, UP], c[CR, 1, UP]], antikomutator[c[AN, \
1, UP], c[CR, 1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"1", "-", RowBox[{"2", " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           "1"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nkomutator[spinz[c[1]] + \
spinz[c[2]], hop[c[1], c[2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"-", FractionBox["1", "2"]}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
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
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   ")"}]}], "-", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
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
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{"(", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
                    {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
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
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
                    {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[2]="}}|>, <|"Code" -> "Expand[%]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>}|>

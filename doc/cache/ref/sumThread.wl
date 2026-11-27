<|"Source" -> "sumThread.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsu\
m[{number[c[k], UP], number[c[k], DO]}, {k}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
           ",", RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "sumThread[%]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "k", "}"}]}], "]"}], ",", 
           RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"k", TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "k", "}"}]}], "]"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsumThread[sum[\
spinxyz[c[k]], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", 
             RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
                   RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                        "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                 ",", RowBox[{"{", "k", "}"}]}], "]"}]}], ",", 
           RowBox[{FractionBox["1", "2"], " ", RowBox[{"sum", "[", RowBox[
                {RowBox[{RowBox[{"\[ImaginaryI]", " ", RowBox[{TagBox[
                        StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                         ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                   "-", RowBox[{"\[ImaginaryI]", " ", RowBox[{TagBox[StyleBox[
                         TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                         ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
                 ",", RowBox[{"{", "k", "}"}]}], "]"}]}], ",", 
           RowBox[{FractionBox["1", "2"], " ", RowBox[{"sum", "[", RowBox[
                {RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                          FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                       ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                        {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                         "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                     StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                        0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                        "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm]}]}], ",", RowBox[{"{", "k", 
                   "}"}]}], "]"}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, \
g];\nThread[sum[{f[k], g[k]}, {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "Thread::tdlen: Objects \
of unequal length in sum[{f[k], g[k]}, {k}] cannot be combined."}, 
      {"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"f", "[", "k", "]"}], ",", RowBox[
                {"g", "[", "k", "]"}]}], "}"}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "sumThread[sum[{f[k], g[k]}, {k}]]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"sum", "[", 
             RowBox[{RowBox[{"f", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                 "}"}]}], "]"}], ",", RowBox[{"sum", "[", 
             RowBox[{RowBox[{"g", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                 "}"}]}], "]"}]}], "}"}], "Out[4]="}}|>}|>

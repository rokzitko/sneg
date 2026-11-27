<|"Source" -> "sumExpand.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsn\
egrealfunctions[f];\ns = sum[f[k] (number[c[k]] - 1), {k}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{"f", "[", "k", "]"}], " ", 
             RowBox[{"(", RowBox[{RowBox[{"-", "1"}], "+", RowBox[
                  {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
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
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "sumExpand[s]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", 
                 "]"}], ",", RowBox[{"{", "k", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
               " ", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                      {"k", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
               " ", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                      {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k, \
l];\nsnegrealfunctions[g];\nsnegrealconstants[x];\nsumExpand[sum[x + sum[g[k, \
l], {k}], {l}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"x", " ", RowBox[{"sum", "[", RowBox[{"1", ",", RowBox[
                {"{", "l", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", RowBox[{"k", ",", 
                 "l"}], "]"}], ",", RowBox[{"{", RowBox[{"k", ",", "l"}], 
               "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
p];\nsnegrealfunctions[eps];\nr = komutator[sum[eps[k] number[c[k], UP], \
{k}], c[CR, p, UP]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], 
               " ", RowBox[{"(", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                        {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                         "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
                     TemplateBox[{RowBox[{"k", ",", "p"}]}, 
                      "KroneckerDeltaSeq"]}], "-", RowBox[{TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
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
                 ")"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], 
               " ", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                      {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "{sumSimplifyKD[r] === r, sumSimplifyKD[sumExpand[r]]}", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"eps", "[", "p", "]"}]}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g, \
h];\nsumExpand[sum[(f[k] + g[k]) h[k], {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {RowBox[{RowBox[{"f", "[", "k", "]"}], " ", RowBox[{"h", "[", 
                 "k", "]"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"g", "[", "k", "]"}], 
               " ", RowBox[{"h", "[", "k", "]"}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "sumCollect[%]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], " ", RowBox[
                {"h", "[", "k", "]"}]}], "+", RowBox[{RowBox[{"g", "[", "k", 
                 "]"}], " ", RowBox[{"h", "[", "k", "]"}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g, h];\ns = \
sum[(f[k] + g[k])/(1 + h[k])^2, {k}];\nsumExpand[s]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {FractionBox[RowBox[{"f", "[", "k", "]"}], RowBox[{"1", "+", 
                RowBox[{"2", " ", RowBox[{"h", "[", "k", "]"}]}], "+", 
                SuperscriptBox[RowBox[{"h", "[", "k", "]"}], "2"]}]], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{FractionBox[RowBox[{"g", "[", "k", 
                "]"}], RowBox[{"1", "+", RowBox[{"2", " ", RowBox[{"h", "[", 
                    "k", "]"}]}], "+", SuperscriptBox[RowBox[{"h", "[", "k", 
                   "]"}], "2"]}]], ",", RowBox[{"{", "k", "}"}]}], "]"}]}], 
       "Out[4]="}}|>, <|"Code" -> "s /. ruleExpandUnderSum", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"f", "[", "k", "]"}], 
              SuperscriptBox[RowBox[{"(", RowBox[{"1", "+", RowBox[{"h", "[", 
                     "k", "]"}]}], ")"}], "2"]], "+", FractionBox[
              RowBox[{"g", "[", "k", "]"}], SuperscriptBox[RowBox[
                {"(", RowBox[{"1", "+", RowBox[{"h", "[", "k", "]"}]}], 
                 ")"}], "2"]]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[5]="}}|>}|>

<|"Source" -> "sumAutoRename.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "sumAutoRename", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nnc[sum[c[CR, \
k, UP], {k}], sum[c[AN, k, UP], {k}]]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, l];\nBlock[{sumAutoRename = False}, nc[sum[c[CR, k, UP], {k}], sum[c[AN, \
l, UP], {l}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"l", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "l"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfreeindexes[k, \
l];\nsnegrealfunctions[g];\nsnegrealconstants[x];\n{sum[sum[g[k, l], {k}] + \
x, {l}], Block[{sumAutoRename = False}, sum[sum[g[k, l], {k}] + x, {l}]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{"x", "+", 
                 RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", RowBox[
                        {"k", ",", "l"}], "]"}], ",", RowBox[{"{", "k", 
                       "}"}]}], "]"}]}], ",", RowBox[{"{", "l", "}"}]}], 
             "]"}], ",", RowBox[{RowBox[{"x", " ", RowBox[{"sum", "[", 
                 RowBox[{"1", ",", RowBox[{"{", "l", "}"}]}], "]"}]}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", RowBox[{"k", ",", 
                     "l"}], "]"}], ",", RowBox[{"{", RowBox[{"k", ",", "l"}], 
                   "}"}]}], "]"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nBlock[{sumAuto\
Rename = False}, nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "Sneg`Private`snegsumJoin::overlap: Overlapping \
indexes detected when joining{k} and {k}."}, 
      {"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"{", RowBox[{"k", ",", "k"}], 
             "}"}]}], "]"}], "Out[3]="}}|>}|>

<|"Source" -> "projector2.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nprojector2[c[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[2]="}}|>, <|"Code" -> "matrixrepresentationop[projector2[c[]], \
{1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", 
               "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
               "0", ",", "1"}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nprojector2[{c[1], c[2]}] == \
{projector2[c[1]], projector2[c[2]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nprojector2[c[]] == hubbard[c[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> 
     "Expand[nc[projector2[c[]], projector2[c[]]] - projector2[c[]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[1]="}}|>}|>

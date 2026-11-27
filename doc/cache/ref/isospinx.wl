<|"Source" -> "isospinx.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\n{isospinx[c[]], isospinx[c[], 1]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", 
             RowBox[{"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
                      {TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "+", 
                 RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                        {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}], ",", RowBox[{FractionBox["1", "2"], " ", 
             RowBox[{"(", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                      {"c", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "-", RowBox[
                  {TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{Expand[isospinx[c[]] - \
(isospinplus[c[]] + isospinminus[c[]])/2], Expand[bcs[c[]] - 2 \
isospinx[c[]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[2]="}}|>}|>

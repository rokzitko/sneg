<|"Source" -> "isospinplus.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\n{isospinplus[c[]], isospinplus[c[], 1]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
           ",", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> 
     "nc[isospinplus[c[]], VACUUM] == nc[c[CR, UP], c[CR, DO], VACUUM]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[komutator[isospinplus[c[]], \
isospinminus[c[]]] - 2 isospinz[c[]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nconj[isospinplus[c[], 1]] == \
isospinminus[c[], 1]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>}|>

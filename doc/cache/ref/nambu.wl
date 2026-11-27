<|"Source" -> "nambu.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nnambu[c[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "nambu[c[], 1]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{"-", TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnambu[{c[1], c[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], ",", 
           RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                       "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\neta = \
nambu[c[]];\n{Expand[komutator[spinz[c[]], #] & /@ eta], \
Expand[komutator[isospinz[c[]], #] & /@ eta]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{FractionBox[TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                        FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], "2"], ",", 
               FractionBox[TagBox[StyleBox[TemplateBox[{"c", RowBox[
                     {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                         RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], "2"]}], "}"}], ",", 
           RowBox[{"{", RowBox[{FractionBox[TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                        FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], "2"], ",", 
               RowBox[{"-", FractionBox[TagBox[StyleBox[TemplateBox[
                     {"c", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], "2"]}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "Expand[1/2 VMV[eta, PauliX, conj /@ eta] - isospinx[c[]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[1]="}}|>}|>

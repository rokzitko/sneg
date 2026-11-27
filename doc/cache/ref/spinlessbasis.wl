<|"Source" -> "spinlessbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[c];\nspinlessbasis[qbasis[{c[1], \
c[2]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "2"}], 
                 "}"}], ",", RowBox[{"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                       RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
                   TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                        {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, V];\nbz = spinlessbasis[qbasis[{c[1], c[2]}]];\nH = -t hop[c[1], c[2]] + \
V nc[number[c[1]], number[c[2]]];\nmakematricesbzop[H, bz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "2"}], "}"}], ",", 
               RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                        {"-", "t"}]}], "}"}], ",", RowBox[{"{", RowBox[
                      {RowBox[{"-", "t"}], ",", "0"}], "}"}]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{"{", "V", "}"}], "}"}]}], "}"}]}], "}"}], 
       "Out[5]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nspinlessbasis[qszbasis[{c[1], c[2]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                        {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], ",", TagBox[StyleBox[TemplateBox[
                      {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                        {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>}|>

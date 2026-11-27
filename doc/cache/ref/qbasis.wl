<|"Source" -> "qbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nqbasis[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                        {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], ",", TagBox[StyleBox[TemplateBox[
                      {"d", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", "1", "}"}], ",", RowBox[{"{", RowBox[
                  {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                          {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                         ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                       StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"d", RowBox[
                          {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\nbz = \
mergebasis[{{(-1)^(#[[1, 1]] + 1)}, #[[2]]} & /@ qbasis[{d[]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
                      {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
                   TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{"1", ",", RowBox[{"-", RowBox[{"(", RowBox[
                        {TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                          FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}]}], "}"}]}], 
             "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[Delta];\nmakematricesbzop[Delta (nc[d[CR, \
UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]), bz]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                     "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0"}], 
                     "}"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "Delta"}], 
                     "}"}], ",", RowBox[{"{", RowBox[{"Delta", ",", "0"}], 
                     "}"}]}], "}"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbzop2bzvc[qbasis[{c[1], c[2]}]] === \
qbasisvc[{c[1], c[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[2]="}}|>}|>

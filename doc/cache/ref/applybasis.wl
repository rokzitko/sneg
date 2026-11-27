<|"Source" -> "applybasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "applybasis[{{{0}, {a, b}}, {{1}, {c}}}, f]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"f", "[", "a", "]"}], ",", 
                   RowBox[{"f", "[", "b", "]"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{"f", "[", "c", "]"}], "}"}]}], "}"}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[d];\nbz = \
qszbasisvc[{d[]}];\napplybasis[bz, ap[number[d[]], #] &] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{{{-1, 0}, {0}}, {{0, \
-1/2}, {vc[0, 1]}}, {{0, 1/2}, {vc[1, 0]}}, {{1, 0}, {2*vc[1, 1]}}}", 
         ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1, 0}, {0}}, {{0, -1/2}, {Sneg`vc[0, 1]}}, 
          {{0, 1/2}, {Sneg`vc[1, 0]}}, {{1, 0}, {2*Sneg`vc[1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "applybasis[bz, vc2ops]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                      {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[StyleBox[
                   TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"-", RowBox[
                    {"(", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                          {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}]}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[d];\napplyba\
sis[qszbasis[{d[]}], nc[#, ket[0]] &]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", TagBox[RowBox[
                   {TagBox[StyleBox["\"\[LeftBracketingBar]\"", FontColor -> 
                       RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[1, 0.5, 0]] & ], RowBox[{"0"}], TagBox[
                     StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                       RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[1, 0.5, 0]] & ]}], DisplayForm], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                        {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[RowBox[
                     {TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                        FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0.5, 0]] & ], RowBox[
                       {"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                        FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
                    DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[RowBox[
                     {TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                        FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0.5, 0]] & ], RowBox[
                       {"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                        FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
                    DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"-", RowBox[{"(", RowBox[
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
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[RowBox[
                         {TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                          FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0.5, 0]] & ], RowBox[
                          {"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                          FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
                        DisplayForm]}], ")"}]}], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "applybasis[{{0, {a, b}}}, f]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"applybasis", "[", 
         RowBox[{RowBox[{"{", RowBox[{"{", RowBox[{"0", ",", RowBox[
                  {"{", RowBox[{"a", ",", "b"}], "}"}]}], "}"}], "}"}], ",", 
           "f"}], "]"}], "Out[1]="}}|>}|>

<|"Source" -> "UP.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\n{c[CR, 1, UP], UP, InputForm[c[CR, 1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", "1", ",", 
           InterpretationBox[StyleBox["c[0, 1, 1]", ShowStringCharacters -> 
              True, NumberMarks -> True], InputForm[c[0, 1, 1]], 
            Editable -> True, AutoDelete -> True]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "number[c[1], UP]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnc[c[CR, 1, UP], c[CR, 1, DO]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[{f, 1}];\n{number[f[]], f[CR, UP] === f[CR, 1]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{RowBox[{"-", "1"}]}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{RowBox[{"-", "1"}]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{"0"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"0"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"f", RowBox[{"1"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"f", RowBox[{"1"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}]}], ",", "True"}], "}"}], "Out[2]="}}|>}|>

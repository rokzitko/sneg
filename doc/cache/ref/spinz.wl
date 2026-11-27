<|"Source" -> "spinz.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nspinz[c[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["1", "2"], " ", 
         RowBox[{"(", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[
                  {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                 ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ")"}]}], 
       "Out[2]="}}|>, 
   <|"Code" -> "spinz[c[1]] == (number[c[1], UP] - number[c[1], DO])/2", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[{f, 3/2}];\nspinz[f[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"-", FractionBox["3", "2"]}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {RowBox[{"-", FractionBox["3", "2"]}]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {RowBox[{"-", FractionBox["3", "2"]}]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "-", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {RowBox[{"-", FractionBox["1", "2"]}]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {RowBox[{"-", FractionBox["1", "2"]}]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{FractionBox["1", 
                    "2"]}], "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{FractionBox["1", 
                    "2"]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["3", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"f", RowBox[{FractionBox["3", 
                    "2"]}], "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{FractionBox["3", 
                    "2"]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\nsnegrealconstant\
s[U, B];\nmakematricesbzvc[U hubbard[d[]] + B spinz[d[]], qszbasisvc[{d[]}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", RowBox[{"-", FractionBox["B", "2"]}], 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {"{", FractionBox["B", "2"], "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"{", "U", "}"}], "}"}]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nExpand[\
{komutator[spinz[c[]], spinplus[c[]]] - spinplus[c[]], komutator[spinz[c[]], \
spinminus[c[]]] + spinminus[c[]]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[2]="}}|>}|>

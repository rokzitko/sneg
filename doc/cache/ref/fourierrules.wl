<|"Source" -> "fourierrules.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, fk];\nf[AN, 1, 0] \
/. fourierrules[f -> fk, {3}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[
        RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], 
              "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
            ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
           DisplayForm], "+", RowBox[{SuperscriptBox["\[ExponentialE]", 
             FractionBox[RowBox[{"2", " ", "\[ImaginaryI]", " ", "\[Pi]"}], 
              "3"]], " ", TagBox[StyleBox[TemplateBox[{"fk", RowBox[
                 {"1", ""}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
          RowBox[{SuperscriptBox["\[ExponentialE]", RowBox[{"-", FractionBox[
                RowBox[{"2", " ", "\[ImaginaryI]", " ", "\[Pi]"}], "3"]}]], 
            " ", TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"2", ""}], 
                "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
              ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
             DisplayForm]}]}], SqrtBox["3"]], "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[t];\nSimplify[ExpToTrig[Expand[clusterhop[-\
t, f, f, 1, {3}] /. fourierrules[f -> fk, {3}]]]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"t", " ", RowBox[
          {"(", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"1", ""}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"fk", RowBox[{"1", ""}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"fk", RowBox[{"2", ""}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"fk", RowBox[{"2", ""}], "\"\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
           ")"}]}], "Out[4]="}}|>, <|"Code" -> "snegspinlessfermionoperators[\
f, fk];\nf[AN, {1, 0}, 0] /. fourierrules[f -> fk, {2, 2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{FractionBox["1", "2"], " ", 
         RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[{"fk", 
                 RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ""}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "+", TagBox[StyleBox[TemplateBox[{"fk", 
                 RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ""}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "-", TagBox[StyleBox[TemplateBox[{"fk", 
                 RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], ""}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "-", TagBox[StyleBox[TemplateBox[{"fk", 
                 RowBox[{RowBox[{"{", RowBox[{"1", ",", "1"}], "}"}], ""}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ")"}]}], "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c, ck];\nc[CR, 1, UP] /. fourierrules[c -> ck, \
{2}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[
        RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"0", 
                TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
              "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
              SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm], "-", 
          TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"1", TagBox[
                 StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
              "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
              SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm]}], SqrtBox["2"]], 
       "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nf[AN, \"x\", 1, 0] /. fourierrules[f -> fk, {2}, \"IndexPosition\" -> \
2]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[
        RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"x", "0", ""}], 
              "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
            ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
           DisplayForm], "-", TagBox[StyleBox[TemplateBox[
             {"fk", RowBox[{"x", "1", ""}], "\"\""}, "Subsuperscript", 
             SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm]}], SqrtBox["2"]], 
       "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nf[AN, 1, 0] /. fourierrules[f -> fk, {3}, \"Twist\" -> Pi]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[
        RowBox[{RowBox[{SuperscriptBox["\[ExponentialE]", FractionBox[
              RowBox[{"\[ImaginaryI]", " ", "\[Pi]"}], "3"]], " ", 
            TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], "\"\""}, 
               "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
              ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
             DisplayForm]}], "-", TagBox[StyleBox[TemplateBox[
             {"fk", RowBox[{"1", ""}], "\"\""}, "Subsuperscript", 
             SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
          RowBox[{SuperscriptBox["\[ExponentialE]", RowBox[{"-", FractionBox[
                RowBox[{"\[ImaginaryI]", " ", "\[Pi]"}], "3"]}]], " ", 
            TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"2", ""}], "\"\""}, 
               "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
              ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
             DisplayForm]}]}], SqrtBox["3"]], "Out[2]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[t, \
theta];\nH = clusterhop[-t, f, f, 1, {3}, \"Twist\" -> \
theta];\nSimplify[ExpToTrig[Expand[H /. fourierrules[f -> fk, {3}, \"Twist\" \
-> theta]]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"t", " ", 
         RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", RowBox[
                {"Cos", "[", FractionBox["theta", "3"], "]"}], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"0", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[
                      {"2", ""}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"2", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], " ", RowBox[{"(", RowBox[
                  {RowBox[{"Cos", "[", FractionBox["theta", "3"], "]"}], "-", 
                   RowBox[{SqrtBox["3"], " ", RowBox[{"Sin", "[", FractionBox[
                        "theta", "3"], "]"}]}]}], ")"}]}], "+", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[
                      {"1", ""}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"1", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], " ", RowBox[{"(", RowBox[
                  {RowBox[{"Cos", "[", FractionBox["theta", "3"], "]"}], "+", 
                   RowBox[{SqrtBox["3"], " ", RowBox[{"Sin", "[", FractionBox[
                        "theta", "3"], "]"}]}]}], ")"}]}]}], ")"}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t, U];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nHR = clusterhop[-t, c, c, 1, {4}] + U \
Sum[hubbard[c[n]], {n, 0, 3}];\nHK = sumCluster[fourier[-t sum[hop[c[R], c[R \
+ 1]], {R}] + U sum[hubbard[c[R]], {R}], c -> ck, R -> k], {4}];\nExpand[(HR \
/. fourierrules[c -> ck, {4}]) - HK]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nf[AN, 3, 0] /. \
fourierrules[f -> fk, {3}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"f", RowBox[{"3", ""}], "\"\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[2]="}}|>}|>

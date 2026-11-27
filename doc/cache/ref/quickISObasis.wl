<|"Source" -> "quickISObasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[d];\nnnop[d[]] = 0;\nquickISObasis[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {FractionBox["1", "2"], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
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
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[StyleBox[
                   TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], "}"}]}], 
             "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnnop[c[1]] = 0; nnop[c[2]] = 1;\nbz \
= quickISObasis[{c[1], c[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                         RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                         "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                     StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                        0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                         "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                     StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                        0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                         "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                   RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                        "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"-", RowBox[{"(", RowBox[
                      {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{RowBox[{FractionBox[
                      RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                        StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                      SqrtBox["2"]], "+", FractionBox[RowBox[{TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                         FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], SqrtBox["2"]]}], ",", 
                   RowBox[{FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[
                          {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                         FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], SqrtBox["2"]], "-", 
                     FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                          RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                        StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                      SqrtBox["2"]]}]}], "}"}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> 
     "Total[(2 #[[1, 1]] + 1) (2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "16", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\nnnop[c[1]] = 0; nnop[c[2]] = 1;\nbz = quickISObasis[{c[1], \
c[2]}];\nmakebasis[{c[1], c[2]}];\nH = -t hop[c[1], c[2]] + U/2 (isozsq[c[1]] \
+ isozsq[c[2]]);\n{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, \
bzop2bzvc[bz]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "U", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", RowBox[
                      {"(", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t"}], 
                         "+", "U"}], ")"}]}], ",", RowBox[{FractionBox["1", 
                      "2"], " ", RowBox[{"(", RowBox[{RowBox[{"2", " ", 
                          "t"}], "+", "U"}], ")"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", 
                     RowBox[{"(", RowBox[{"U", "-", SqrtBox[RowBox[
                          {RowBox[{"16", " ", SuperscriptBox["t", "2"]}], 
                          "+", SuperscriptBox["U", "2"]}]]}], ")"}]}], ",", 
                   RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                        {"U", "+", SqrtBox[RowBox[{RowBox[{"16", " ", 
                          SuperscriptBox["t", "2"]}], "+", SuperscriptBox[
                          "U", "2"]}]]}], ")"}]}]}], "}"}]}], "}"}]}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "Expand[komutator[H, isospinminus[c[1], \
nnop[c[1]]] + isospinminus[c[2], nnop[c[2]]]]]", "Reset" -> False, 
    "InLabel" -> "In[8]:=", "Cells" -> {{"Output", "0", "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnnop[c[1]] = 0; nnop[c[2]] = 1;\nbz \
= bzop2bzvc[quickISObasis[{c[1], c[2]}]];\nii = Last[manyisospin[{c[1], \
c[2]}, {0, 1}]];\nss = Last[manyspin[{c[1], c[2]}]];\nAnd @@ \
Flatten[Table[{Expand[ap[ii, v] - sub[[1, 1]] (sub[[1, 1]] + 1) v] === 0, \
Expand[ap[ss, v] - sub[[1, 2]] (sub[[1, 2]] + 1) v] === 0}, {sub, bz}, {v, \
sub[[2]]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[6]="}}|>, 
   <|"Code" -> "Tm = isospinminus[c[1], 0] + isospinminus[c[2], \
1];\nExpand[transformQStoIS[qsbasisvc[{c[1], c[2]}], Tm]] === \
Expand[bzop2bzvc[quickISObasis[{c[1], c[2]}]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnnop[c[1]] = 0; nnop[c[2]] = \
0;\nSelect[quickISObasis[{c[1], c[2]}], First[#] == {0, 0} &]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
               "}"}], ",", RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", 
                     FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                          RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                        StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                      SqrtBox["2"]]}], "+", FractionBox[RowBox[
                     {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}], SqrtBox["2"]]}], ",", 
                 RowBox[{FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[
                         {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}], SqrtBox["2"]], "-", 
                   FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                          RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    SqrtBox["2"]]}]}], "}"}]}], "}"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "nnop[c[2]] = 1;\nSelect[quickISObasis[{c[1], c[2]}], First[#] \
== {0, 0} &]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
               "}"}], ",", RowBox[{"{", RowBox[{RowBox[{FractionBox[
                    RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}], SqrtBox["2"]], "+", 
                   FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                          RowBox[{"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    SqrtBox["2"]]}], ",", RowBox[{FractionBox[RowBox[
                     {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}], SqrtBox["2"]], "-", 
                   FractionBox[RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                          RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    SqrtBox["2"]]}]}], "}"}]}], "}"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[e];\nFreeQ[quickISObasis[{e[]}], nnop]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "False", "Out[2]="}}|>, 
   <|"Code" -> "nnop[e[]] = 0;\nFreeQ[quickISObasis[{e[]}], nnop]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>}|>

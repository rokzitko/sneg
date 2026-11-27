<|"Source" -> "eigenoperators.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U];\n\
H0 = U (hubbard[c[1]] + hubbard[c[2]]);\ne = eigenoperators[nc[c[CR, 1, UP], \
c[AN, 2, UP]], H0];\ne[[All, 1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "U", ",", 
           RowBox[{"-", "U"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "Expand[e[[2, 2]] - nc[c[CR, 1, UP], c[AN, 2, UP], \
number[c[1], DO], 1 - number[c[2], DO]]]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "{Expand[Total[e[[All, 2]]] - nc[c[CR, 1, UP], c[AN, 2, UP]]], \
Union[Map[Expand[komutator[H0, #[[2]]] - #[[1]] #[[2]]] &, e]]}", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"{", "0", "}"}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[ed, \
U];\neigenoperators[d[CR, UP], ed number[d[]] + U hubbard[d[]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"ed", "+", "U"}], ",", 
               RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
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
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   ")"}]}]}], "}"}], ",", RowBox[
            {"{", RowBox[{"ed", ",", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"d", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
                 RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
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
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
             "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[a, \
d];\nsnegbosonoperators[b];\nsnegspinoperators[{S, \
1/2}];\nsnegrealconstants[ed, w, B, e1];\neigenoperators[nc[S[1, 4], a[CR, 1, \
DO], a[AN, 1, UP]] + nc[b[CR], d[AN, UP]], B S[1, 3] + w number[b[]] + ed \
number[d[]] + e1 number[a[1]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "ed"}], "+", 
                 "w"}], ",", RowBox[{TagBox[StyleBox[TemplateBox[{"b", 
                     "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                     SuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{"B", ",", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"S", RowBox[{"1"}], TagBox[StyleBox["\"+\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", RowBox[{"1", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, d];\nsnegrealconstants[ed, \
U];\nsnegrealfunctions[eps];\nsnegfreeindexes[k];\neigenoperators[nc[c[CR, k, \
UP], d[AN, UP]], eps[k] number[c[k]] + ed number[d[]] + U hubbard[d[]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "ed"}], "-", 
                 "U", "+", RowBox[{"eps", "[", "k", "]"}]}], ",", RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "ed"}], "+", 
                 RowBox[{"eps", "[", "k", "]"}]}], ",", RowBox[
                {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                         TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
                 RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                         TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nsnegrealconstants[ed, \
U];\neigenoperators[X[1, \"0\", \"UP\"] + X[1, \"UP\", \"2\"], U X[1, \"2\", \
\"2\"] + ed X[1, \"UP\", \"UP\"]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "ed"}], ",", TagBox[
                StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[{"0", 
                     "UP"}]}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"ed", "-", "U"}], ",", TagBox[
                StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[{"UP", 
                     "2"}]}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrea\
lconstants[t, U];\nH0 = U (hubbard[c[1]] + hubbard[c[2]]);\nV = -t hop[c[1], \
c[2]];\nS1 = Total[Map[If[#[[1]] === 0, 0, #[[2]]/#[[1]]] &, \
eigenoperators[V, H0]]];\ngen = Last[schriefferwolff[H0, V, \
nc[projector1[c[1]], projector1[c[2]]], 2, \"Generator\" -> \
True]];\nExpand[S1 - First[gen]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[ed, U];\nH0 = ed \
number[d[]] + U hubbard[d[]];\ne = eigenoperators[d[CR, UP], \
H0];\nExpand[komutator[H0, komutator[H0, d[CR, UP]]] - Total[Map[#[[1]]^2 \
#[[2]] &, e]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, \
U];\neigenoperators[c[CR, 1, UP], U hubbard[c[1]] + t hop[c[1], c[2]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "eigenoperators::diag: H0 is not diagonal in the \
occupation numbers: [H0, c[0, 1, 1]] is not proportional to c[0, 1, 1]."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\neigenoperators\
[sum[c[CR, k, UP], {k}], 0]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "eigenoperators::sum: Expressions with symbolic \
sums are not supported; decompose the summand."}, {"Output", "$Failed", 
       "Out[3]="}}|>}|>

<|"Source" -> "sumFullSimplify.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsn\
egrealfunctions[eps];\nH = sum[eps[k] number[c[k]], {k}];\nc1 = \
komutator[sum[number[c[k]], {k}], H];\nsumFullSimplify[c1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
p];\nsumFullSimplify[sum[KroneckerDelta[k, p] nc[c[CR, k, UP], c[AN, p, UP]], \
{k, p}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
sigma];\nsumFullSimplify[sum[number[c[k], sigma], {k, sigma}] - sum[1, {k}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"-", "1"}], "+", RowBox[{"sum", "[", RowBox[
                {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                         "sigma"}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"k", "sigma"}], "\"\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm]}], ",", RowBox[{"{", "sigma", 
                   "}"}]}], "]"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumind\
exes[{k, q}, Ns];\nsumFullSimplify[sum[number[ck[k + q]], {k}] - \
sum[number[ck[k]], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{p, q}, \
Ns];\nrho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], \
{p}];\nsumFullSimplify[komutator[rho[q], rho[-q]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{p, q}, \
Ns];\nrho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], \
{p}];\nsumSimplify[komutator[rho[q], rho[-q]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"sum", "[", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                      {"p", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"ck", RowBox[{"p", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "p", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"ck", RowBox[{"p1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"ck", RowBox[{"p1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "p1", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsnegrealfuncti\
ons[eps];\nc1 = komutator[sum[number[c[k]], {k}], sum[eps[k] number[c[k]], \
{k}]];\nsumCollect[sumSimplifyKD[sumExpand[c1]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[a];\nsnegfreeindexes[l, Q];\nrho[qq_] := \
sum[nc[a[CR, l + qq, UP], a[AN, l, UP]], \
{l}];\nsumFullSimplify[komutator[rho[Q], rho[-Q]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", RowBox[{"sum", "[", 
             RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                      {"l", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"a", RowBox[{"l", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
               RowBox[{"{", "l", "}"}]}], "]"}]}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", RowBox[{"l1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"a", RowBox[{"l1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", "l1", "}"}]}], "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "sumNameIndex[sumAbstractIndex[%, idx], idx, {l}]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>}|>

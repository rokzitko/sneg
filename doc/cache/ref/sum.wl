<|"Source" -> "sum.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
sigma];\nsnegrealfunctions[eps];\nH = sum[eps[k] number[c[k], sigma], {k, \
sigma}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                     "sigma"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", "sigma"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}]}], ",", RowBox[
            {"{", RowBox[{"k", ",", "sigma"}], "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, sigma];\nsnegrealconstants[x];\nsnegrealfunctions[eps];\n{sum[2 x eps[k] \
c[CR, k, UP], {k}], sum[x, {k, sigma}]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"2", " ", "x", " ", 
             RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                      {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
                   RowBox[{"eps", "[", "k", "]"}]}], ",", RowBox[{"{", "k", 
                   "}"}]}], "]"}]}], ",", RowBox[{"x", " ", 
             RowBox[{"sum", "[", RowBox[{"1", ",", RowBox[{"{", RowBox[
                    {"k", ",", "sigma"}], "}"}]}], "]"}]}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "{sum[eps[k], {sigma, k}] === sum[eps[k], \
{k, sigma}], sum[eps[k], {}]}", "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{"eps", "[", "k", "]"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[g];\nsum[sum[g[i, \
j], {i}], {j}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{"g", "[", RowBox[{"i", ",", "j"}], "]"}], ",", 
           RowBox[{"{", RowBox[{"i", ",", "j"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, sigma];\nsnegrealfunctions[eps];\nconj[sum[eps[k] c[CR, k, sigma], {k, \
sigma}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", "sigma"}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"eps", "[", "k", "]"}]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "sigma"}], "}"}]}], "]"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\nnc[c[CR, 1, UP], sum[c[AN, k, UP], {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\nr = komutator[sum[number[c[k], UP], {k}], sum[c[CR, k, UP], {k}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
                 TemplateBox[{RowBox[{"k", ",", "k1"}]}, 
                  "KroneckerDeltaSeq"]}], "-", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
             RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "sumSimplifyKD[sumExpand[r]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{"{", "k1", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nvev[nc[sum[c[A\
N, k, UP], {k}], sum[c[CR, k, UP], {k}]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {TemplateBox[{RowBox[{"k", ",", "k1"}]}, "KroneckerDeltaSeq"], ",", 
           RowBox[{"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "sumSimplifyKD[%]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[{"1", ",", 
           RowBox[{"{", "k1", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "sneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{q}, \
Ns];\nsnegrealfunctions[eps];\n{sum[1, {R}], sum[eps[q], {q, R}]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"Ns", ",", 
           RowBox[{"Ns", " ", RowBox[{"sum", "[", RowBox[
                {RowBox[{"eps", "[", "q", "]"}], ",", RowBox[{"{", "q", 
                   "}"}]}], "]"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nnc[sum[ck[CR, q, UP], {q}], \
sum[ck[AN, q, UP], {q}]]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"q", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                  {"q1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"{", RowBox[{"q", ",", "q1"}], "}"}]}], "]"}], 
       "Out[6]="}}|>, <|"Code" -> "{momentumindexQ[q1], latticesize[q1]}", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "Ns"}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, p, sigma];\nsnegrealfunctions[eps];\nH = sum[eps[k] number[c[k], sigma], \
{k, sigma}];\n{sumSimplifyKD[sumExpand[komutator[c[AN, p, UP], H]]], \
sumSimplifyKD[sumExpand[komutator[H, c[CR, p, UP]]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"eps", "[", "p", "]"}]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"eps", "[", "p", "]"}]}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "sumFullSimplify[komutator[sum[number[c[k]], {k}], H]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[cs];\nordering[cs] = \
SEA;\nsnegfreeindexes[k, sigma];\nsnegrealfunctions[eps];\nvev[sum[eps[k] \
number[cs[k], sigma], {k, sigma}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
              DisplayForm]}], ",", RowBox[{"{", RowBox[{"k", ",", "sigma"}], 
             "}"}]}], "]"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g];\ns = sum[f[k] + \
g[k], {k}];\n{s == sum[f[k], {k}] + sum[g[k], {k}], sumExpand[s], \
sumCollect[sumExpand[s]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"sum", "[", RowBox[
                {RowBox[{RowBox[{"f", "[", "k", "]"}], "+", RowBox[{"g", "[", 
                     "k", "]"}]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
             "\[Equal]", RowBox[{RowBox[{"sum", "[", RowBox[
                  {RowBox[{"f", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                     "}"}]}], "]"}], "+", RowBox[{"sum", "[", RowBox[
                  {RowBox[{"g", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                     "}"}]}], "]"}]}]}], ",", RowBox[
            {RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", "]"}], ",", 
                 RowBox[{"{", "k", "}"}]}], "]"}], "+", 
             RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "k", "]"}], ",", 
                 RowBox[{"{", "k", "}"}]}], "]"}]}], ",", 
           RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"f", "[", "k", "]"}], 
                 "+", RowBox[{"g", "[", "k", "]"}]}], ",", RowBox[
                {"{", "k", "}"}]}], "]"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, \
j];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[i, j] f[i], \
{i}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"f", "[", "j", "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g];\n{sum[{f[k], \
g[k]}, {k}], sumThread[sum[{f[k], g[k]}, {k}]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"sum", "[", 
             RowBox[{RowBox[{"{", RowBox[{RowBox[{"f", "[", "k", "]"}], ",", 
                   RowBox[{"g", "[", "k", "]"}]}], "}"}], ",", RowBox[
                {"{", "k", "}"}]}], "]"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"sum", "[", RowBox[
                  {RowBox[{"f", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                     "}"}]}], "]"}], ",", RowBox[{"sum", "[", RowBox[
                  {RowBox[{"g", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                     "}"}]}], "]"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> 
     "snegfreeindexes[k];\nsnegrealfunctions[f];\nisnumericQ[sum[f[k], {k}]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsnegrealfuncti\
ons[eps];\nH = sum[eps[k] number[c[k], UP], {k}];\nsum[a_, {k}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"a_", " ", 
         RowBox[{"sum", "[", RowBox[{"1", ",", RowBox[{"{", "k", "}"}]}], 
           "]"}]}], "Out[5]="}}|>, <|"Code" -> "H /. sum[a_, {k}] :> a", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                     TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                         1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "H /. HoldPattern[sum[a_, {k}]] :> a", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, p];\nnc[c[CR, k, \
UP], sum[c[AN, k, UP], {k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         RowBox[{"sum", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], ",", RowBox[{"{", "k", "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "nc[c[CR, p, UP], sum[c[AN, k, UP], {k}]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"p", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
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
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> 
     "snegfreeindexes[k];\nsnegrealfunctions[f];\nsum[sum[f[k], {k}], {k}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "Sneg`Private`snegsumJoin::overlap: Overlapping \
indexes detected when joining{k} and {k}."}, 
      {"Output", RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "k", "]"}], 
           ",", RowBox[{"{", RowBox[{"k", ",", "k"}], "}"}]}], "]"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\nBlock[{sumAutoRename = False}, nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, \
UP], {k}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "Sneg`Private`snegsumJoin::overlap: Overlapping \
indexes detected when joining{k} and {k}."}, 
      {"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"{", RowBox[{"k", ",", "k"}], 
             "}"}]}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k, \
sigma];\n{sum[number[c[k], sigma], {sigma}] == number[c[k]], sum[1, \
{sigma}]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"sum", "[", RowBox[
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
                   "}"}]}], "]"}], "\[Equal]", RowBox[{RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"sum", "[", RowBox[{"1", ",", RowBox[{"{", "sigma", 
                 "}"}]}], "]"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "Sum[number[c[k], s], {s, {UP, DO}}] == number[c[k]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>}|>

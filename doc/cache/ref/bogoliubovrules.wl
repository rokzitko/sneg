<|"Source" -> "bogoliubovrules.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegspinlessfermionoper\
ators[g];\nsnegrealconstants[xi, Dl];\nHS = xi number[c[1]] - Dl (nc[c[CR, 1, \
UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);\n{eps, T, Egs} = \
bogoliubov[HS, {c[1]}];\nrules = bogoliubovrules[T, {c[1]}, \
g];\nSimplify[Expand[HS /. rules] /. {xi -> 4/5, Dl -> 3/5}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", FractionBox["1", "5"]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", ""}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", ""}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[7]="}}|>, 
   <|"Code" -> "rules /. {xi -> 4/5, Dl -> 3/5}", "Reset" -> False, 
    "InLabel" -> "In[8]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             RowBox[{FractionBox[TagBox[StyleBox[TemplateBox[{"g", 
                    RowBox[{"2", ""}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                SqrtBox["10"]], "+", FractionBox[RowBox[{"3", " ", 
                  TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                SqrtBox["10"]]}]}], ",", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "\[Rule]", RowBox[{RowBox[{"-", FractionBox[
                  TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  SqrtBox["10"]]}], "+", FractionBox[RowBox[{"3", " ", 
                  TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", ""}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                SqrtBox["10"]]}]}], ",", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             RowBox[{FractionBox[RowBox[{"3", " ", TagBox[StyleBox[
                    TemplateBox[{"g", RowBox[{"1", ""}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}], SqrtBox["10"]], "+", 
               FractionBox[TagBox[StyleBox[TemplateBox[{"g", RowBox[
                     {"2", ""}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                SqrtBox["10"]]}]}], ",", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             RowBox[{FractionBox[RowBox[{"3", " ", TagBox[StyleBox[
                    TemplateBox[{"g", RowBox[{"2", ""}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}], SqrtBox["10"]], "-", 
               FractionBox[TagBox[StyleBox[TemplateBox[{"g", RowBox[
                     {"1", ""}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                SqrtBox["10"]]}]}]}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, gam];\nsnegrealconstants[xi, Dl];\nHS \
= xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], \
c[AN, 1, UP]]);\n{eps, T, Egs} = bogoliubov[HS, {c[1]}];\nSimplify[Expand[HS \
/. bogoliubovrules[T, {c[1]}, {gam[1]}]] /. {xi -> 4/5, Dl -> 3/5}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", FractionBox["1", "5"]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", 
                 TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                  StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                     1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                  StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegbosonoperators[b, q];\nH = 2 number[b[]] + 1/2 (nc[b[CR], \
b[CR]] + nc[b[AN], b[AN]]);\n{eps, T, Egs} = bogoliubov[H, \
{b}];\nChop[Expand[H /. bogoliubovrules[T, {b}, q]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "0.13397459621556143`"}], "+", 
         RowBox[{"1.7320508075688776`", " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"q", RowBox[{"1"}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"q", RowBox[{"1"}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegspinlessfermionoperators[g];\ns\
negrealconstants[xi, Dl];\nHS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, \
1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);\n{eps, T, Egs} = bogoliubov[HS, \
{c[1]}];\nrules = bogoliubovrules[T, {c[1]}, \
g];\nSimplify[vev[Expand[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, UP]]} \
/. rules]] /. {xi -> 4/5, Dl -> 3/5}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "10"], ",", 
           FractionBox["3", "10"]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "gaussianaverage[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, \
UP]]}, HS /. {xi -> 4/5, Dl -> 3/5}, {c[1]}, 0]", "Reset" -> False, 
    "InLabel" -> "In[8]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.09999999999999994`", ",", 
           "0.29999999999999993`"}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegspinlessfermionoperators[g];\ns\
negrealconstants[xi, Dl];\nHS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, \
1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);\nrules = \
bogoliubovrules[bogoliubov[HS, {c[1]}][[2]], {c[1]}, \
g];\nSimplify[Expand[{antikomutator[c[AN, 1, UP] /. rules, c[CR, 1, UP] /. \
rules], antikomutator[c[AN, 1, UP] /. rules, c[AN, 1, DO] /. rules]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "snegfermionoperators[c, \
gam];\nbogoliubovrules[IdentityMatrix[4], {c[1]}, gam]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "bogoliubovrules::qp: The \
quasiparticle operators gam do not give 2 modes."}, {"Output", "$Failed", 
       "Out[2]="}}|>, 
   <|"Code" -> "bogoliubovrules[IdentityMatrix[4], {c[1]}, {gam[1]}]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                 RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                    StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                       1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], "\[Rule]", 
             TagBox[StyleBox[TemplateBox[{"gam", RowBox[{"1", TagBox[
                    StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                       1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
g];\nbogoliubovrules[IdentityMatrix[2], {Function[x, (f[x, 1, 0] + f[x, 2, \
0])/Sqrt[2]]}, g]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bogoliubovrules::mode: The modes must be single \
operators to define replacement rules."}, {"Output", "$Failed", 
       "Out[2]="}}|>}|>

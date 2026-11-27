<|"Source" -> "quadraticform.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstan\
ts[t, mu, Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j \
+ 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], \
{j, 3}];\n{M, E0} = quadraticform[H, {f[1], f[2], f[3]}];\nMatrixForm[M]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"-", "mu"}], RowBox[{"-", "t"}], "0", "0", "Dl", 
             "0"}, {RowBox[{"-", "t"}], RowBox[{"-", "mu"}], 
             RowBox[{"-", "t"}], RowBox[{"-", "Dl"}], "0", "Dl"}, 
            {"0", RowBox[{"-", "t"}], RowBox[{"-", "mu"}], "0", 
             RowBox[{"-", "Dl"}], "0"}, {"0", RowBox[{"-", "Dl"}], "0", "mu", 
             "t", "0"}, {"Dl", "0", RowBox[{"-", "Dl"}], "t", "mu", "t"}, 
            {"0", "Dl", "0", "0", "t", "mu"}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[5]//MatrixForm="}}|>, <|"Code" -> "E0", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{"-", FractionBox[RowBox[{"3", " ", "mu"}], "2"]}], 
       "Out[6]="}}|>, 
   <|"Code" -> "Expand[quadraticoperator[M, {f[1], f[2], f[3]}, E0] - H]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", "0", "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t];\nH = \
-t Sum[hop[f[j], f[Mod[j, 4] + 1]], {j, 4}];\n{h, C0} = quadraticform[H, \
Table[f[j], {j, 4}], \"BdG\" -> False];\n{MatrixForm[h], C0, Eigenvalues[h]}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {"0", RowBox[{"-", "t"}], "0", RowBox[{"-", "t"}]}, 
                {RowBox[{"-", "t"}], "0", RowBox[{"-", "t"}], "0"}, 
                {"0", RowBox[{"-", "t"}], "0", RowBox[{"-", "t"}]}, 
                {RowBox[{"-", "t"}], "0", RowBox[{"-", "t"}], "0"}}, 
               RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", "0", ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t"}], ",", 
               RowBox[{"2", " ", "t"}], ",", "0", ",", "0"}], "}"}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, B];\nquadraticform[-t hop[c[1], c[2]] + B (spinz[c[1]] + spinz[c[2]]), \
{c[1], c[2]}, \"BdG\" -> False]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {FractionBox["B", "2"], ",", "0", ",", RowBox[{"-", "t"}], 
                   ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", FractionBox["B", "2"]}], ",", "0", ",", 
                   RowBox[{"-", "t"}]}], "}"}], ",", RowBox[{"{", 
                 RowBox[{RowBox[{"-", "t"}], ",", "0", ",", FractionBox["B", 
                    "2"], ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[
                  {"0", ",", RowBox[{"-", "t"}], ",", "0", ",", RowBox[
                    {"-", FractionBox["B", "2"]}]}], "}"}]}], "}"}], ",", 
           "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[xi, Dl];\nHS = xi \
number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, \
UP]]);\n{M, E0} = quadraticform[HS, {c[1]}];\n{MatrixForm[M], E0}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {"xi", "0", "0", RowBox[{"-", "Dl"}]}, {"0", "xi", "Dl", 
                 "0"}, {"0", "Dl", RowBox[{"-", "xi"}], "0"}, 
                {RowBox[{"-", "Dl"}], "0", "0", RowBox[{"-", "xi"}]}}, 
               RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
            Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], ",", "xi"}], 
         "}"}], "Out[5]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\
\nsnegrealconstants[t];\nbond = Function[x, (f[x, 1, 0] + f[x, 2, \
0])/Sqrt[2]];\nanti = Function[x, (f[x, 1, 0] - f[x, 2, \
0])/Sqrt[2]];\nquadraticform[-t hop[f[1], f[2]], {bond, anti}, \"BdG\" -> \
False]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "t"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "t"}], "}"}]}], "}"}], ",", "0"}], 
         "}"}], "Out[5]="}}|>, <|"Code" -> "snegbosonoperators[b];\nsnegrealc\
onstants[A, B];\nquadraticform[A number[b[]] + B/2 (nc[b[CR], b[CR]] + \
nc[b[AN], b[AN]]), {b}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"A", ",", 
                   "B"}], "}"}], ",", RowBox[{"{", RowBox[{"B", ",", "A"}], 
                 "}"}]}], "}"}], ",", RowBox[{"-", FractionBox["A", "2"]}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrea\
lconstants[eps];\n{quadraticform[eps number[c[1]], {c[1]}], quadraticform[eps \
number[c[1]], {c[1]}, \"BdG\" -> False]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                      {"eps", ",", "0", ",", "0", ",", "0"}], "}"}], ",", 
                   RowBox[{"{", RowBox[{"0", ",", "eps", ",", "0", ",", 
                       "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", 
                       ",", RowBox[{"-", "eps"}], ",", "0"}], "}"}], ",", 
                   RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", 
                       RowBox[{"-", "eps"}]}], "}"}]}], "}"}], ",", "eps"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"{", RowBox[{"eps", ",", "0"}], "}"}], ",", 
                   RowBox[{"{", RowBox[{"0", ",", "eps"}], "}"}]}], "}"}], 
               ",", "0"}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nsnegrealconstants[w, \
lam];\nquadraticform[w number[b[]] + lam (b[CR] + b[AN]), {b}, \"Linear\" -> \
True]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"w", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "w"}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", "lam", "}"}], ",", 
           RowBox[{"-", FractionBox["w", "2"]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "quadraticform[w number[b[]] + lam (b[CR] + b[AN]), {b}]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Message", "quadraticform::linear: The Hamiltonian contains \
terms linear in the boson operators; use \"Linear\" -> True."}, 
      {"Output", "$Failed", "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nquadraticform[nc[f[CR, 1, \
0], f[AN, 2, 0]], {f[1], f[2]}, \"BdG\" -> False]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "quadraticform::herm: The quadratic form is not Hermitian."}, 
      {"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
           ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "quadraticform[nc[f[CR, 1, 0], f[AN, 2, 0]], {f[1], f[2]}, \
\"BdG\" -> False, \"Check\" -> False]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
           ",", "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nL = 4;\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + \
1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu \
Sum[number[f[j]], {j, L}];\n{M, E0} = quadraticform[H, Table[f[j], {j, \
L}]];\nChop[First[bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, \"Fermion\"]]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           "1.9999999999999998`", ",", "2.`", ",", "2.`"}], "}"}], 
       "Out[6]="}}|>, 
   <|"Code" -> 
     "First[bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 3}, \"Fermion\"]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1.5664164432320113`", ",", 
           "2.780237230880795`", ",", "3.937475113894402`", ",", 
           "4.723654326245616`"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U, eps];\nHmf = \
meanfield[eps number[c[1]] + U hubbard[c[1]], {\"Hartree\", \
\"Pairing\"}];\n{M, E0} = quadraticform[Hmf, {c[1]}];\nMatrixForm[M]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"eps", "+", RowBox[{"U", " ", TagBox[
                  RowBox[{"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
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
                    "\[RightAngleBracket]"}], DisplayForm]}]}], "0", "0", 
             RowBox[{"U", " ", TagBox[RowBox[{"\[LeftAngleBracket]", 
                  RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                       "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                  "\[RightAngleBracket]"}], DisplayForm]}]}, 
            {"0", RowBox[{"eps", "+", RowBox[{"U", " ", TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm]}]}], 
             RowBox[{RowBox[{"-", "U"}], " ", TagBox[RowBox[
                 {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[TemplateBox[
                       {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                       "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                  "\[RightAngleBracket]"}], DisplayForm]}], "0"}, 
            {"0", RowBox[{"U", " ", TagBox[RowBox[{"\[LeftAngleBracket]", 
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
                       {1, 1}], DisplayForm]}], "\[RightAngleBracket]"}], 
                DisplayForm]}], RowBox[{RowBox[{"-", "eps"}], "-", RowBox[
                {"U", " ", TagBox[RowBox[{"\[LeftAngleBracket]", RowBox[
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
                    "\[RightAngleBracket]"}], DisplayForm]}]}], "0"}, 
            {RowBox[{RowBox[{"-", "U"}], " ", TagBox[RowBox[
                 {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[TemplateBox[
                       {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                        "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                    StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                    TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                       SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm]}], "\[RightAngleBracket]"}], 
                DisplayForm]}], "0", "0", RowBox[{RowBox[{"-", "eps"}], "-", 
               RowBox[{"U", " ", TagBox[RowBox[{"\[LeftAngleBracket]", 
                    RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm]}]}]}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[5]//MatrixForm="}}|>, <|"Code" -> "snegbosonoperators[b];\nsnegre\
alconstants[w, lam];\n{M, fl, E0} = quadraticform[w number[b[]] + lam (b[CR] \
+ b[AN]), {b}, \"Linear\" -> True, \"BdG\" -> False];\nE0 - First[fl]^2/M[[1, \
1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"-", FractionBox[SuperscriptBox["lam", 
           "2"], "w"]}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + \
nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];\nM \
= First[quadraticform[H, {f[1], f[2], f[3]}]];\ntaux = ArrayFlatten[{{0, \
IdentityMatrix[3]}, {IdentityMatrix[3], 0}}];\n{M === ConjugateTranspose[M], \
Expand[taux . Conjugate[M] . taux + M] === ConstantArray[0, {6, 6}]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True"}], "}"}], 
       "Out[6]="}}|>, 
   <|"Code" -> "Sort[Eigenvalues[M /. {t -> 1, Dl -> 1/2, mu -> 1/3}] // N]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", "1.8657903380229675`"}], ",", 
           RowBox[{"-", "1.3484430299299297`"}], ",", 
           RowBox[{"-", "0.1840139747597042`"}], ",", "0.1840139747597042`", 
           ",", "1.3484430299299297`", ",", "1.8657903380229675`"}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nsnegreal\
constants[Dl];\nquadraticform[Dl (nc[f[CR, 1, 0], f[CR, 2, 0]] + nc[f[AN, 2, \
0], f[AN, 1, 0]]), {f[1], f[2]}] === quadraticform[-Dl (nc[f[CR, 2, 0], f[CR, \
1, 0]] + nc[f[AN, 1, 0], f[AN, 2, 0]]), {f[1], f[2]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, \
mu];\nH = -t hop[f[1], f[2]] - mu (number[f[1]] + number[f[2]]) + 1;\n{{M, \
E0}, {h, C0}} = {quadraticform[H, {f[1], f[2]}], quadraticform[H, {f[1], \
f[2]}, \"BdG\" -> False]};\nE0 == C0 + Tr[h]/2", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nquadraticform[g hop[f[1], \
f[2]], {f[1], f[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "quadraticform::order: The Hamiltonian contains \
terms that are not quadratic in the modes (higher-order terms or products \
with other operators): -(g nc[f[0, 1, 0], f[1, 2, 0]]) - g nc[f[0, 2, 0], \
f[1, 1, 0]] + nc[f[0, 1, 0], f[1, 1, 0], g nc[f[0, 1, 0], f[1, 2, 0]]] + \
nc[f[0, 1, 0], f[1, 1, 0], g nc[f[0, 2, 0], f[1, 1, 0]]] - nc[f[0, 1, 0], g \
nc[f[0, 1, 0], f[1, 2, 0]], f[1, 1, 0]] - nc[f[0, 1, 0], g nc[f[0, 2, 0], \
f[1, 1, 0]], f[1, 1, 0]] + nc[f[1, 1, 0], g nc[f[0, 1, 0], f[1, 2, 0]], f[0, \
1, 0]] + nc[f[1, 1, 0], g nc[f[0, 2, 0], f[1, 1, 0]], f[0, 1, 0]] + nc[g \
nc[f[0, 1, 0], f[1, 2, 0]], f[0, 1, 0], f[1, 1, 0]] + nc[g nc[f[0, 2, 0], \
f[1, 1, 0]], f[0, 1, 0], f[1, 1, 0]]."}, {"Output", "$Failed", "Out[2]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t];\nquad\
raticform[-t hop[f[1], f[2]], {f[1]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "quadraticform::rest: The \
Hamiltonian contains terms that are not quadratic forms of the given modes: \
-(t nc[f[0, 1, 0], f[1, 2, 0]]) - t nc[f[0, 2, 0], f[1, 1, 0]]."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U];\nquadraticfor\
m[U hubbard[c[1]], {c[1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "quadraticform::order: The Hamiltonian contains \
terms that are not quadratic in the modes (higher-order terms or products \
with other operators): U nc[c[0, 1, 0], c[1, 1, 0]]."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegfreeindexes[j];\nquadra\
ticform[sum[number[f[j]], {j}], {f[1]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "quadraticform::sum: \
Symbolic sums are not supported; use bdghamiltonian for momentum-space \
Hamiltonians or sumCluster for finite clusters."}, {"Output", "$Failed", 
       "Out[3]="}}|>}|>

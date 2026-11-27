<|"Source" -> "fourier.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t, U];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = -t sum[hop[c[R], c[R + 1]], {R}] + U \
sum[hubbard[c[R]], {R}];\nfourier[H, c -> ck, R -> k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{RowBox[{"-", "t"}], " ", TagBox[SuperscriptBox[
                 "e", RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                   "\[CenterDot]", "1"}]], DisplayForm], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                DisplayForm], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"ck", RowBox[{"k", TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                   "1"}]], DisplayForm], " ", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                DisplayForm], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"ck", RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             FractionBox[RowBox[{"U", " ", RowBox[{"sum", "[", RowBox[
                   {RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {RowBox[{RowBox[{"-", "k"}], "+", "k2", "+", 
                          "k3"}], TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"ck", RowBox[{"k3", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                    RowBox[{"{", RowBox[{"k2", ",", "k3"}], "}"}]}], "]"}]}], 
              "Ns"]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[c[AN, R, UP], c -> ck, R -> k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"sum", "[", 
          RowBox[{RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                  "k", "\[CenterDot]", "R"}]], DisplayForm], " ", 
              TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
            RowBox[{"{", "k", "}"}]}], "]"}], SqrtBox["Ns"]], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\n{fourier[number[c[R]], c -> ck, R -> k], \
fourier[sum[number[c[R]], {R}], c -> ck, R -> k]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"sum", "[", 
             RowBox[{RowBox[{FractionBox[RowBox[{TagBox[SuperscriptBox["e", 
                      RowBox[{"\[ImaginaryI]", RowBox[{RowBox[{"-", "k"}], 
                          "+", "k1"}], "\[CenterDot]", "R"}]], DisplayForm], 
                    " ", RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}], "Ns"], "+", FractionBox[
                  RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                        RowBox[{RowBox[{"-", "k"}], "+", "k1"}], 
                        "\[CenterDot]", "R"}]], DisplayForm], " ", 
                    RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                      StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                         0]], TagBox[StyleBox[TemplateBox[{"ck", RowBox[
                          {"k1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}], "Ns"]}], ",", RowBox[
                {"{", RowBox[{"k", ",", "k1"}], "}"}]}], "]"}], ",", 
           RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{TagBox[StyleBox[
                     TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
                 RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", 
                         TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
               RowBox[{"{", "k", "}"}]}], "]"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegbosonoperators[b, \
bk];\nsnegrealconstants[w0];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{q}, Ns];\nfourier[w0 sum[nc[b[CR, R], b[AN, R]], \
{R}], b -> bk, R -> q]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"w0", " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[
                  {"bk", RowBox[{"q"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], ",", RowBox[{"{", "q", "}"}]}], "]"}]}], 
       "Out[5]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nfourier[-t \
sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}], f -> fk, R -> k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{RowBox[{"-", "t"}], " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "a1"}]], DisplayForm], " ", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                   "a2"}]], DisplayForm], " ", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", ""}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}], "-", RowBox[{"t", " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                   "\[CenterDot]", "a1"}]], DisplayForm], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "a2"}]], 
                DisplayForm], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"fk", RowBox[{"k", ""}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[v, \
w];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHssh = \
sum[v hop[f[R, \"A\"], f[R, \"B\"]] + w hop[f[R + 1, \"A\"], f[R, \"B\"]], \
{R}];\nfourier[Hssh, f -> fk, R -> k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{"v", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"fk", RowBox[{"k", "A", ""}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", "B", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}], "+", RowBox[{"w", " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "1"}]], DisplayForm], " ", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", "A", 
                       ""}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", "B", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}], "+", RowBox[{"v", " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", "B", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"k", "A", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{"w", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                DisplayForm], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"fk", RowBox[{"k", "B", ""}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"k", "A", ""}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[sum[hop[f[\"x\", R], f[\"x\", R \
+ 1]], {R}], f -> fk, R -> k, \"IndexPosition\" -> 2]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                   "1"}]], DisplayForm], " ", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"fk", RowBox[{"x", "k", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"x", "k", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                   "\[CenterDot]", "1"}]], DisplayForm], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"x", "k", ""}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"fk", RowBox[{"x", "k", ""}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nHk = sum[-t (blochphase[k, 1] + \
blochphase[-k, 1]) number[ck[k]], {k}];\nfourier[Hk, c -> ck, R -> k, \
\"Inverse\" -> True]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{RowBox[{"-", "t"}], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{RowBox[
                        {RowBox[{"-", "1"}], "+", "R"}], TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"R", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{RowBox[{RowBox[{"-", "1"}], "+", "R"}], 
                       TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"R", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{RowBox[{"1", "+", "R"}], TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"R", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
             RowBox[{"t", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{RowBox[{"1", "+", "R"}], TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"R", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"{", "R", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "sumFullSimplify[% - (-t sum[hop[c[R], c[R + 1]], {R}])]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", "0", "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[sum[number[c[R], UP], {R}], c -> \
ck, R -> k, \"Simplify\" -> False]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"sum", "[", 
          RowBox[{RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                  RowBox[{RowBox[{"-", "k"}], "+", "k1"}], "\[CenterDot]", 
                  "R"}]], DisplayForm], " ", RowBox[{TagBox[StyleBox[
                  TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                        "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                       StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k1", TagBox[
                       StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
            RowBox[{"{", RowBox[{"k", ",", "k1", ",", "R"}], "}"}]}], "]"}], 
        "Ns"], "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nh = blochhamiltonian[fourier[-t \
sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, \
k];\nExpToTrig[blochphaseexp[h]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
                 "t", " ", RowBox[{"Cos", "[", "k", "]"}]}], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[
                {RowBox[{"-", "2"}], " ", "t", " ", RowBox[{"Cos", "[", "k", 
                   "]"}]}]}], "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsnegbosonoperators[b, \
bk];\nsnegrealconstants[g];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k, q}, Ns];\nHep = g sum[nc[number[c[R]], b[CR, R] \
+ b[AN, R]], {R}];\nHq = fourier[fourier[Hep, c -> ck, R -> k], b -> bk, R -> \
q]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"g", " ", RowBox[
                 {TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k1", 
                          "-", "q"}], TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k1", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k1", "-", "q"}], 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]], "+", FractionBox[RowBox[{"g", " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k1", 
                          "+", "q"}], TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k1", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k1", "+", "q"}], 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]]}], ",", RowBox[{"{", RowBox[{"k1", ",", "q"}], 
             "}"}]}], "]"}], "Out[7]="}}|>, 
   <|"Code" -> "sumCollect[sumMomentumCanonical[sumExpand[Hq]]]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"g", " ", RowBox[
                 {TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k", "+", "q"}], 
                        TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", 
                          "+", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{RowBox[{"k", "-", "q"}], 
                        TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]], "+", 
             FractionBox[RowBox[{"g", " ", RowBox[{TagBox[StyleBox[
                    TemplateBox[{"bk", RowBox[{"q"}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", 
                          "-", "q"}], TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm]}]}], SqrtBox["Ns"]]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "q"}], "}"}]}], "]"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[c, ck, \
d];\nsnegrealconstants[V];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[V hop[d[], c[0]], c -> ck, R -> \
k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"V", " ", RowBox[
                 {TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                         StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                          0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                          0, 1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]], "+", FractionBox[RowBox[{"V", " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]], "+", FractionBox[RowBox[{"V", " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                       {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]], "+", FractionBox[RowBox[{"V", " ", 
                RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                       {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"ck", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
              SqrtBox["Ns"]]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsneglatticeindexes[{R, Rp}, Ns];\nsnegmomentumindexes[{k, p}, \
Ns];\nExpand[sumSimplifyKD[sumExpand[antikomutator[fourier[c[AN, R, UP], c -> \
ck, R -> k], fourier[c[CR, Rp, UP], c -> ck, R -> p]]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TemplateBox[{RowBox[{"R", ",", "Rp"}]}, 
        "KroneckerDeltaSeq"], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[number[ck[k]], c -> ck, R -> k]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "fourier::capture: The expression already \
contains the summation index k."}, {"Output", "$Failed", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsnegmomentumindexes[{k}, \
Ns];\nfourier[number[c[1]], c -> ck, n -> k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", "fourier::index: n must be a declared lattice index."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "snegmajoranaoperators[gm];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nfourier[gm[R], gm -> gk, R -> k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "fourier::op: Only fermionic and bosonic \
operators can be transformed: gm."}, {"Output", "$Failed", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, ck];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\n{conj[fourier[c[AN, R, UP], c -> ck, R \
-> k]], fourier[c[CR, R, UP], c -> ck, R -> k]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{"Conjugate", "[", FractionBox["1", SqrtBox["Ns"]], 
               "]"}], " ", RowBox[{"sum", "[", RowBox[
                {RowBox[{TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "R"}]], 
                    DisplayForm], " ", TagBox[StyleBox[TemplateBox[{"ck", 
                       RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                 RowBox[{"{", "k", "}"}]}], "]"}]}], ",", 
           FractionBox[RowBox[{"sum", "[", RowBox[{RowBox[
                 {TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                      RowBox[{"-", "k"}], "\[CenterDot]", "R"}]], 
                   DisplayForm], " ", TagBox[StyleBox[TemplateBox[{"ck", 
                      RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
                RowBox[{"{", "k", "}"}]}], "]"}], SqrtBox["Ns"]]}], "}"}], 
       "Out[4]="}}|>}|>

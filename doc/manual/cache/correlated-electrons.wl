<|"Source" -> "correlated-electrons.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "Needs[\"sneg`\"];\nsnegfermionoperators[c, \
d];\nsnegrealconstants[t, U, \\[Epsilon]];\natom = \\[Epsilon] number[d[]] + \
U hubbard[d[]];\n{number[d[]], hubbard[d[]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                    {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                        RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                         RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"d", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
               ")"}]}]}], "}"}], "Out[5]=", <|"Kind" -> "List", 
        "Parts" -> {"d^\\dagger_{\\downarrow} \
d_{\\downarrow}+d^\\dagger_{\\uparrow} d_{\\uparrow}", "-d^\\dagger_{\\downar\
row} d^\\dagger_{\\uparrow} d_{\\downarrow} d_{\\uparrow}"}|>}}|>, 
   <|"Code" -> "ba = qsbasisvc[{d[]}];\nrows = {Sequence @@ First[#], #[[2, \
1, 1]]} & /@ makematricesbzvc[atom, ba];\nTableForm[rows, TableHeadings -> \
{None, {\"Q\", \"S\", \"Energy\"}}]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"Q\"", HoldForm], 
            TagBox["\"S\"", HoldForm], TagBox["\"Energy\"", HoldForm]}, 
           {RowBox[{"-", "1"}], "0", "0"}, {"0", FractionBox["1", "2"], 
            "\[Epsilon]"}, {"1", "0", RowBox[{"U", "+", RowBox[{"2", " ", 
                "\[Epsilon]"}]}]}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Q", "S", 
             "Energy"}}]]], "Out[8]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{ccc}\n \\text{Q} & \\text{S} & \
\\text{Energy} \\\\\n\\hline\n -1 & 0 & 0 \\\\\n\\hline\n 0 & \\frac{1}{2} & \
\\epsilon  \\\\\n 1 & 0 & U+2 \\epsilon  \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "sz2 = pow[number[d[], UP] - number[d[], DO], 2];\nq2 = \
pow[number[d[]] - 1, 2];\nExpand[sz2 + q2]", "Reset" -> False, 
    "InLabel" -> "In[9]:=", "Cells" -> {{"Output", "1", "Out[11]=", 
       <|"Kind" -> "Single", "Parts" -> {"1"}|>}}|>, 
   <|"Code" -> "hat = atom /. {\\[Epsilon] -> -1, U -> 2};\nrows = \
Table[Prepend[Round[thermalaverage[#, hat, {d}, temp], 1/1000] & /@\n    \
{number[d[]], sz2, q2}, temp], {temp, {0, 1, 4}}];\nTableForm[N[rows], \
TableHeadings -> {None, {\"T\", \"Charge\", \"Moment\", \"Charge \
variance\"}}]", "Reset" -> False, "InLabel" -> "In[12]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[{{TagBox["\"T\"", HoldForm], 
            TagBox["\"Charge\"", HoldForm], TagBox["\"Moment\"", HoldForm], 
            TagBox["\"Charge variance\"", HoldForm]}, {"0.`", "1.`", "1.`", 
            "0.`"}, {"1.`", "1.`", "0.731`", "0.269`"}, {"4.`", "1.`", 
            "0.562`", "0.438`"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"T", "Charge", 
             "Moment", "Charge variance"}}]]], "Out[14]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cccc}\n \\text{T} & \
\\text{Charge} & \\text{Moment} & \\text{Charge variance} \\\\\n\\hline\n 0. \
& 1. & 1. & 0. \\\\\n\\hline\n 1. & 1. & 0.731 & 0.269 \\\\\n 4. & 1. & 0.562 \
& 0.438 \\\\\n\\end{array}"}|>}}|>, <|"Code" -> "sites = {c[1], c[2]};\nh0 = \
U Total[hubbard[sites]];\nv = -t hop[c[1], c[2]];\nhd = h0 + \
v;\nExpand[{komutator[hd, Total[number /@ sites]],\n  komutator[hd, \
Total[spinplus /@ sites]]}]", "Reset" -> False, "InLabel" -> "In[15]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[19]=", <|"Kind" -> "List", "Parts" -> {"0", "0"}|>}}|>, 
   <|"Code" -> "bd = qsbasisvc[sites];\nsector[s_] := Select[bd, First[#] == \
{0, s} &];\nmat[h_, b_] := First[makematricesbzvc[h, b]][[2]];\nhs = mat[hd, \
sector[0]];\n{Dimensions[hs], mat[hd, sector[1]]}", "Reset" -> False, 
    "InLabel" -> "In[20]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"3", ",", "3"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"{", "0", "}"}], "}"}]}], 
         "}"}], "Out[24]=", <|"Kind" -> "List", "Parts" -> 
         {"\\{3,3\\}", 
          "\\left(\n\\begin{array}{c}\n 0 \\\\\n\\end{array}\n\\right)"}|>}}|>\
, <|"Code" -> "hs // MatrixForm", "Reset" -> False, "InLabel" -> "In[25]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"U", RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}], 
             "0"}, {RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}], "0", 
             RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}]}, 
            {"0", RowBox[{RowBox[{"-", SqrtBox["2"]}], " ", "t"}], "U"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[25]//MatrixForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{ccc}\n U & -\\sqrt{2} t & 0 \
\\\\\n -\\sqrt{2} t & 0 & -\\sqrt{2} t \\\\\n 0 & -\\sqrt{2} t & U \
\\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "Factor[CharacteristicPolynomial[hs, x]]", "Reset" -> False, 
    "InLabel" -> "In[26]:=", "Cells" -> 
     {{"Output", RowBox[{"-", RowBox[{"(", RowBox[
            {RowBox[{"(", RowBox[{"U", "-", "x"}], ")"}], " ", 
             RowBox[{"(", RowBox[{RowBox[{"4", " ", SuperscriptBox["t", 
                    "2"]}], "+", RowBox[{"U", " ", "x"}], "-", SuperscriptBox[
                  "x", "2"]}], ")"}]}], ")"}]}], "Out[26]=", 
       <|"Kind" -> "Single", "Parts" -> 
         {"-\\left((U-x) \\left(4 t^2+U x-x^2\\right)\\right)"}|>}}|>, 
   <|"Code" -> "emin = (U - Sqrt[U^2 + 16 t^2])/2;\ndsite = Simplify[D[emin, \
U]/2];\n{dsite, dsite /. {U -> 0, t -> 1}}", "Reset" -> False, 
    "InLabel" -> "In[27]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{FractionBox["1", "4"], " ", 
             RowBox[{"(", RowBox[{"1", "-", FractionBox["U", SqrtBox[
                   RowBox[{RowBox[{"16", " ", SuperscriptBox["t", "2"]}], 
                     "+", SuperscriptBox["U", "2"]}]]]}], ")"}]}], ",", 
           FractionBox["1", "4"]}], "}"}], "Out[29]=", 
       <|"Kind" -> "List", "Parts" -> 
         {"\\frac{1}{4} \\left(1-\\frac{U}{\\sqrt{16 t^2+U^2}}\\right)", 
          "\\frac{1}{4}"}|>}}|>, <|"Code" -> "p = nc @@ (projector1 /@ \
sites);\nheff = schriefferwolff[h0, v, p, 2];\nsnegxoperators[X, \
\"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nExpand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]", 
    "Reset" -> False, "InLabel" -> "In[30]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", FractionBox[SuperscriptBox["t", "2"], "U"]}], "+", 
         FractionBox[RowBox[{"4", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"z\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"-\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"+\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"]}], 
       "Out[34]=", <|"Kind" -> "Sum", "Parts" -> {"-\\frac{t^2}{U}", 
          "+\\frac{4 S^{z}_{1} S^{z}_{2} t^2}{U}", 
          "+\\frac{2 S^{+}_{1} S^{-}_{2} t^2}{U}", 
          "+\\frac{2 S^{-}_{1} S^{+}_{2} t^2}{U}"}|>}}|>, 
   <|"Code" -> "Normal[Series[-emin, {t, 0, 4}, Assumptions -> U > 0]] //\n  \
Simplify[#, U > 0] &", "Reset" -> False, "InLabel" -> "In[35]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"4", " ", SuperscriptBox["t", 
           "2"], " ", RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", "4"}], " ", 
                SuperscriptBox["t", "2"]}], "+", SuperscriptBox["U", "2"]}], 
            ")"}]}], SuperscriptBox["U", "3"]], "Out[35]=", 
       <|"Kind" -> "Single", "Parts" -> 
         {"\\frac{4 t^2 \\left(U^2-4 t^2\\right)}{U^3}"}|>}}|>, 
   <|"Code" -> "line3 = c /@ Range[3];\nb3 = Select[qsbasisvc[line3], \
First[#] == {-1, 0} &];\nnd = Diagonal[mat[Total[hubbard[line3]], b3]];\nlo = \
Flatten[Position[nd, 0]]; hi = Flatten[Position[nd, 1]];\nv3 = mat[-hop[c[1], \
c[2]] - hop[c[2], c[3]], b3];\nk3 = -v3[[lo, hi]] . v3[[hi, lo]];\nk3 // \
MatrixForm", "Reset" -> False, "InLabel" -> "In[36]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"-", "4"}], "0", RowBox[{"-", "2"}]}, 
            {"0", "0", "0"}, {RowBox[{"-", "2"}], "0", RowBox[{"-", "4"}]}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[42]//MatrixForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\left(\n\\begin{array}{ccc}\n -4 & 0 & -2 \\\\\n 0 & 0 \
& 0 \\\\\n -2 & 0 & -4 \\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "{Sort[Eigenvalues[k3]], Sort[Diagonal[k3]]}", 
    "Reset" -> False, "InLabel" -> "In[43]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "6"}], ",", RowBox[
                {"-", "2"}], ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"-", "4"}], ",", RowBox[{"-", "4"}], 
               ",", "0"}], "}"}]}], "}"}], "Out[43]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\left(\n\\begin{array}{ccc}\n -6 & \
-2 & 0 \\\\\n -4 & -4 & 0 \\\\\n\\end{array}\n\\right)"}|>}}|>, 
   <|"Code" -> "ring = c /@ Range[4];\nhr = -Sum[hop[c[i], c[Mod[i, 4] + 1]], \
{i, 4}] +\n  U Total[hubbard[ring]];\nbr = qszbasisvc[ring];\nblocks = \
makematricesbzvc[hr, br];\n{Length[blocks], Total[Length /@ blocks[[All, \
2]]]}", "Reset" -> False, "InLabel" -> "In[44]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"25", ",", "256"}], "}"}], 
       "Out[48]=", <|"Kind" -> "List", "Parts" -> {"25", "256"}|>}}|>, 
   <|"Code" -> "e0[q_, u_] := Min[Flatten[Eigenvalues[N[#[[2]] /. U -> u]] & \
/@\n  Select[blocks, #[[1, 1]] == q &]]];\ngap[u_] := e0[1, u] + e0[-1, u] - \
2 e0[0, u];\nrows = Table[{u, N[Round[gap[u], 1/1000]]}, {u, {0, 2, 4, 8, \
16}}];\nTableForm[rows, TableHeadings -> {None, {\"U/t\", \"Charge gap / \
t\"}}]", "Reset" -> False, "InLabel" -> "In[49]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"U/t\"", HoldForm], TagBox["\"Charge gap / t\"", 
             HoldForm]}, {"0", "0.`"}, {"2", "1.238`"}, {"4", "2.701`"}, 
           {"8", "5.991`"}, {"16", "13.361`"}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"U/t", "Charge gap / t"}}]]], "Out[52]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{U/t} & \
\\text{Charge gap / t} \\\\\n\\hline\n 0 & 0. \\\\\n\\hline\n 2 & 1.238 \
\\\\\n 4 & 2.701 \\\\\n 8 & 5.991 \\\\\n 16 & 13.361 \\\\\n\\end{array}"}|>}}\
|>, <|"Code" -> "shell[l_] := With[{band = SortBy[Table[-2 Cos[2 Pi j/l], {j, \
0, l-1}], N]},\n  band[[l/2 + 1]] - band[[l/2]]];\nTableForm[Table[{l, \
N[shell[l]]}, {l, {4, 6, 8, 10}}],\n  TableHeadings -> {None, {\"Sites\", \
\"Charge gap / t\"}}]", "Reset" -> False, "InLabel" -> "In[53]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Sites\"", HoldForm], TagBox["\"Charge gap / t\"", 
             HoldForm]}, {"4", "0.`"}, {"6", "2.`"}, {"8", "0.`"}, 
           {"10", "1.2360679774997898`"}}, RowSpacings -> 1, 
          ColumnSpacings -> 3, RowAlignments -> Baseline, 
          ColumnAlignments -> Left, GridBoxDividers -> 
           {"Columns" -> {{False}}, "Rows" -> {False, True, {False}, False}}, 
          ColumnAlignments -> Left], {None, OutputFormsDump`HeadedColumns}], 
        Function[BoxForm`e$, TableForm[BoxForm`e$, TableHeadings -> 
           {None, {"Sites", "Charge gap / t"}}]]], "Out[54]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{cc}\n \\text{Sites} \
& \\text{Charge gap / t} \\\\\n\\hline\n 4 & 0. \\\\\n\\hline\n 6 & 2. \\\\\n \
8 & 0. \\\\\n 10 & 1.23607 \\\\\n\\end{array}"}|>}}|>, 
   <|"Code" -> "gatom = greensfunction[d[AN, UP], d[CR, UP], atom, z];\ng = \
gatom /. avg[number[d[], DO]] -> w;\nApart[g, z]", "Reset" -> False, 
    "InLabel" -> "In[55]:=", "Cells" -> 
     {{"Output", RowBox[{FractionBox[RowBox[{"1", "-", "w"}], 
          RowBox[{"z", "-", "\[Epsilon]"}]], "+", FractionBox["w", 
          RowBox[{RowBox[{"-", "U"}], "+", "z", "-", "\[Epsilon]"}]]}], 
       "Out[57]=", <|"Kind" -> "Sum", "Parts" -> 
         {"\\frac{1-w}{z-\\epsilon }", "+\\frac{w}{-U+z-\\epsilon }"}|>}}|>, 
   <|"Code" -> "ph = Factor[g /. {\\[Epsilon] -> -U/2, w -> 1/2}];\n{ph, \
Limit[z ph, z -> Infinity]}", "Reset" -> False, "InLabel" -> "In[58]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{FractionBox[RowBox[{"4", " ", "z"}], 
            RowBox[{RowBox[{"(", RowBox[{RowBox[{"-", "U"}], "+", 
                  RowBox[{"2", " ", "z"}]}], ")"}], " ", RowBox[{"(", 
                RowBox[{"U", "+", RowBox[{"2", " ", "z"}]}], ")"}]}]], ",", 
           "1"}], "}"}], "Out[59]=", <|"Kind" -> "List", 
        "Parts" -> {"\\frac{4 z}{(2 z-U) (U+2 z)}", "1"}|>}}|>, 
   <|"Code" -> "asym = atom /. {\\[Epsilon] -> -1/4, U -> 2};\npoles = \
lehmann[d[AN, UP], d[CR, UP], asym, {d}, 1/2];\nTableForm[N[Round[poles, \
1/1000]], TableHeadings -> {None, {\"Frequency\", \"Residue\"}}]", 
    "Reset" -> False, "InLabel" -> "In[60]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Frequency\"", HoldForm], TagBox["\"Residue\"", 
             HoldForm]}, {RowBox[{"-", "0.25`"}], "0.609`"}, 
           {"1.75`", "0.391`"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Frequency", 
             "Residue"}}]]], "Out[62]//TableForm=", <|"Kind" -> "Single", 
        "Parts" -> {"\\begin{array}{cc}\n \\text{Frequency} & \\text{Residue} \
\\\\\n\\hline\n -0.25 & 0.609 \\\\\n\\hline\n 1.75 & 0.391 \
\\\\\n\\end{array}"}|>}}|>, <|"Code" -> "wth = thermalaverage[number[d[], \
DO], asym, {d}, 1/2];\nz0 = 1/3 + I/4;\ngtrace = Total[poles[[All, 2]]/(z0 - \
poles[[All, 1]])];\n{Abs[1 - Total[poles[[All, 2]]]] < 10^-12,\n Abs[gtrace - \
(g /. {z -> z0, \\[Epsilon] -> -1/4, U -> 2, w -> wth})] < 10^-12}", 
    "Reset" -> False, "InLabel" -> "In[63]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True"}], "}"}], 
       "Out[66]=", <|"Kind" -> "List", "Parts" -> {"\\text{True}", 
          "\\text{True}"}|>}}|>, 
   <|"Code" -> "gk = Together[ph/(1 - ek ph)];\nFactor[Denominator[gk]]", 
    "Reset" -> False, "InLabel" -> "In[67]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", SuperscriptBox["U", "2"]}], 
         "-", RowBox[{"4", " ", "ek", " ", "z"}], "+", 
         RowBox[{"4", " ", SuperscriptBox["z", "2"]}]}], "Out[68]=", 
       <|"Kind" -> "Sum", "Parts" -> {"-U^2", "-4 \\text{ek} z", 
          "+4 z^2"}|>}}|>, <|"Code" -> "hf = meanfield[hd /. {t -> 1, U -> \
4}, \"Hartree\"];\nseed = {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], \
DO]] -> 0.9};\nsol = meanfieldsolve[hf, sites, seed, \"ParticleNumber\" -> \
2];\nm1 = avg[number[c[1], UP]] - avg[number[c[1], \
DO]];\n{sol[\"Converged\"], Round[sol[\"Energy\"], 1/1000],\n  Round[m1 /. \
sol[\"Averages\"], 1/1000] // N}", "Reset" -> False, "InLabel" -> "In[69]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{"-", FractionBox["1", "2"]}], ",", "0.866`"}], "}"}], 
       "Out[73]=", <|"Kind" -> "List", "Parts" -> {"\\text{True}", 
          "-\\frac{1}{2}", "0.866"}|>}}|>, 
   <|"Code" -> 
     "xi = xoperator[d[], \"0\", \"UP\"];\nExpand[antikomutator[xi, \
conj[xi]]]", "Reset" -> False, "InLabel" -> "In[74]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[75]=", <|"Kind" -> "Sum", 
        "Parts" -> {"1", "-d^\\dagger_{\\downarrow} d_{\\downarrow}"}|>}}|>, 
   <|"Code" -> "pg = 1 - hubbard[d[]];\nExpand[{nc[pg, pg] - pg, nc[pg, d[AN, \
UP], pg] - xi}]", "Reset" -> False, "InLabel" -> "In[76]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[77]=", <|"Kind" -> "List", "Parts" -> {"0", "0"}|>}}|>, 
   <|"Code" -> "gopt = 4 t/(U + Sqrt[U^2 + 16 t^2]);\nenergy[y_] := (-4 t y + \
U y^2)/(1 + y^2);\nFullSimplify[{energy[gopt] - emin,\n  gopt^2/(2 (1 + \
gopt^2)) - dsite}, U > 0 && t > 0]", "Reset" -> False, 
    "InLabel" -> "In[78]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], "Out[80]=", 
       <|"Kind" -> "List", "Parts" -> {"0", "0"}|>}}|>, 
   <|"Code" -> "snegfermionoperators[a];\nsnegrealconstants[J];\norb = a /@ \
Range[3];\nn[m_, s_] := number[a[m], s];\npair[m_] := nc[a[AN, m, DO], a[AN, \
m, UP]];\nhkpair[m_, k_] := (U - 2 J) (nc[n[m, UP], n[k, DO]] +\n    nc[n[m, \
DO], n[k, UP]]) +\n  (U - 3 J) Total[nc[n[m, #], n[k, #]] & /@ {UP, DO}] -\n  \
J (nc[spinplus[a[m]], spinminus[a[k]]] +\n     nc[spinminus[a[m]], \
spinplus[a[k]]]) +\n  J (nc[conj[pair[m]], pair[k]] + nc[conj[pair[k]], \
pair[m]]);\nhk = U Total[hubbard[orb]] + Total[hkpair @@@ Subsets[Range[3], \
{2}]];\nExpand[{hk - conj[hk], komutator[hk, Total[spinplus /@ orb]]}]", 
    "Reset" -> False, "InLabel" -> "In[81]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[88]=", <|"Kind" -> "List", "Parts" -> {"0", "0"}|>}}|>, 
   <|"Code" -> "bk = qsbasisvc[orb];\nk2 = makematricesbzvc[hk, Select[bk, \
#[[1, 1]] == -1 &]];\nlevels = Flatten[Table[{b[[1, 2]], #[[1]], #[[2]]} & \
/@\n    Tally[Expand /@ Eigenvalues[b[[2]]]], {b, k2}], \
1];\nTableForm[Sort[levels], TableHeadings -> {None, {\"Spin\", \"Energy\", \
\"Multiplets\"}}]", "Reset" -> False, "InLabel" -> "In[89]:=", 
    "Cells" -> {{"Output", TagBox[TagBox[GridBox[
          {{TagBox["\"Spin\"", HoldForm], TagBox["\"Energy\"", HoldForm], 
            TagBox["\"Multiplets\"", HoldForm]}, 
           {"0", RowBox[{RowBox[{"-", "J"}], "+", "U"}], "5"}, 
           {"0", RowBox[{RowBox[{"2", " ", "J"}], "+", "U"}], "1"}, 
           {"1", RowBox[{RowBox[{RowBox[{"-", "3"}], " ", "J"}], "+", "U"}], 
            "3"}}, RowSpacings -> 1, ColumnSpacings -> 3, 
          RowAlignments -> Baseline, ColumnAlignments -> Left, 
          GridBoxDividers -> {"Columns" -> {{False}}, "Rows" -> 
             {False, True, {False}, False}}, ColumnAlignments -> Left], 
         {None, OutputFormsDump`HeadedColumns}], Function[BoxForm`e$, 
         TableForm[BoxForm`e$, TableHeadings -> {None, {"Spin", "Energy", 
             "Multiplets"}}]]], "Out[92]//TableForm=", 
       <|"Kind" -> "Single", "Parts" -> {"\\begin{array}{ccc}\n \\text{Spin} \
& \\text{Energy} & \\text{Multiplets} \\\\\n\\hline\n 0 & U-J & 5 \
\\\\\n\\hline\n 0 & 2 J+U & 1 \\\\\n 1 & U-3 J & 3 \\\\\n\\end{array}"}|>}}|>\
, <|"Code" -> "Total[(2 #[[1]] + 1) #[[3]] & /@ levels] == Binomial[6, 2]", 
    "Reset" -> False, "InLabel" -> "In[93]:=", 
    "Cells" -> {{"Output", "True", "Out[93]=", <|"Kind" -> "Single", 
        "Parts" -> {"\\text{True}"}|>}}|>}|>

<|"Source" -> "sneglatticeindexes.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "sneglatticeindexes[{R}, Ns];\n{latticeindexQ[R], \
latticesize[R], Conjugate[R], sum[1, {R}]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "Ns", ",", "R", ",", 
           "Ns"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "sneglatticeindexes[{n}, 4];\nsneglatticeindexes[{Rs}, L1 \
L2];\n{sum[1, {n}], sum[1, {Rs}], Conjugate[L1]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"4", ",", RowBox[{"L1", " ", "L2"}], 
           ",", "L1"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = -t sum[hop[c[R], c[R + 1]], \
{R}];\nfourier[H, c -> ck, R -> k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{RowBox[{"-", "t"}], " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "1"}]], DisplayForm], " ", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", 
                       TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
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
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[6]="}}|>, 
   <|"Code" -> "sneglatticeindexes[{R, Rp}, \
Ns];\nsnegrealfunctions[f];\nsumSimplifyKD[sum[KroneckerDelta[Rp, R + 1] \
f[R], {R}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"f", "[", RowBox[{RowBox[{"-", "1"}], "+", 
           "Rp"}], "]"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsumSimplifyKD[sum[KroneckerDelta[j, i \
+ 1] f[i], {i}]]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], " ", 
             TemplateBox[{RowBox[{RowBox[{"1", "+", "i"}], ",", "j"}]}, 
              "KroneckerDeltaSeq"]}], ",", RowBox[{"{", "i", "}"}]}], "]"}], 
       "Out[5]="}}|>, <|"Code" -> "sneglatticeindexes[{Rb}, -3]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "sneglatticeindexes::size: The lattice size -3 \
must be a positive integer or an expression built from symbols."}, 
      {"Output", RowBox[{"sneglatticeindexes", "[", 
         RowBox[{RowBox[{"{", "Rb", "}"}], ",", RowBox[{"-", "3"}]}], "]"}], 
       "Out[1]="}}|>, <|"Code" -> "latticeindexQ[Rb]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> {{"Output", "False", "Out[2]="}}|>, 
   <|"Code" -> "sneglatticeindexes[R, Ns]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sneglatticeindexes", "[", RowBox[{"R", ",", "Ns"}], 
         "]"}], "Out[1]="}}|>, 
   <|"Code" -> "sneglatticeindexes[{R}, Ns];\nConjugate[1/Sqrt[Ns]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"Conjugate", "[", FractionBox["1", 
          SqrtBox["Ns"]], "]"}], "Out[2]="}}|>}|>

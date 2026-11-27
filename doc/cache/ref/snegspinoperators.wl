<|"Source" -> "snegspinoperators.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegspinoperators[{S, 1/2}];\n{S[1, 1], S[1, 3], S[1, 4], S[1, 5]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"1"}], TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"1"}], TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"1"}], TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "{komutator[S[1, 1], S[1, 2]], nc[S[1, 1], S[1, 2]], nc[S[1, \
4], S[1, 5]], spinspin[S[1], S[1]]}", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"\[ImaginaryI]", " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", FractionBox[RowBox[{"\[ImaginaryI]", " ", 
              TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                   StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm]}], "2"], ",", RowBox[{FractionBox["1", "2"], "+", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", FractionBox["3", "4"]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "komutator[S[1, 4], S[2, 5]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[{T, 1}];\n{nc[T[1, 4], T[1, 4], T[1, 4]], \
nc[T[1, 3], T[1, 3], T[1, 3]], spinspin[T[1], T[1]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           TagBox[StyleBox[TemplateBox[{"T", RowBox[{"1"}], TagBox[
                StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", "2"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{M, s}];\n{nc[M[1, 5], M[1, 4]], \
spinof[M]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"s", " ", RowBox[{"(", RowBox[
                  {"1", "+", "s"}], ")"}]}], "-", TagBox[StyleBox[TemplateBox[
                {"M", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "-", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"M", RowBox[{"1"}], 
                   TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"M", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           "s"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[G];\n{nc[G[1, 5], G[1, 4]], komutator[G[1, \
1], G[1, 2]], spinspin[G[1], G[1]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{RowBox[{"-", "2"}], " ", TagBox[StyleBox[
                 TemplateBox[{"G", RowBox[{"1"}], TagBox[StyleBox["\"z\"", 
                     FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                   TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"G", RowBox[{"1"}], TagBox[StyleBox["\"-\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"\[ImaginaryI]", " ", TagBox[StyleBox[TemplateBox[
                {"G", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                   TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"G", RowBox[{"1"}], TagBox[StyleBox["\"x\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                   TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"G", RowBox[{"1"}], TagBox[StyleBox["\"y\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                   TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"G", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "spincomponents[Expand[komutator[s\
pinspin[G[1], G[2]], G[1, 4] + G[2, 4]]]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nkomutator[S[i, 4], S[j, 5]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", " ", TemplateBox[
          {RowBox[{"i", ",", "j"}]}, "KroneckerDeltaSeq"], " ", 
         TagBox[StyleBox[TemplateBox[{"S", RowBox[{"j"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nsnegspincomponentindexes[a, \
b];\nkomutator[S[1, a], S[1, b]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", "\[ImaginaryI]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"levicivita", "[", 
                 RowBox[{"a", ",", "a1", ",", "b"}], "]"}], " ", TagBox[
                StyleBox[TemplateBox[{"S", RowBox[{"1"}], "a1"}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], ",", RowBox[{"{", "a1", "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegfermionoperators[c];\nnc[c[CR, 1, UP], S[3], c[AN, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox["\"z\"", 
               FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                 RGBColor[0, 0, 1]] & ]}, "Superscript", SyntaxForm -> 
             SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nsnegrealconstants[J];\nH = J \
Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];\n{First[#], \
Min[Eigenvalues[Last[#]]]} & /@ spinclustermatrices[H /. J -> 1, {S -> \
Range[4]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "2"}], 
                 "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"-", "1"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"-", "2"}]}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[{"-", "1"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "2", "}"}], ",", 
               "1"}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegfermionoperators[c];\nsnegrealconstants[J];\nHK = J (nc[S[3], \
spinz[c[]]] + (nc[S[4], spinminus[c[]]] + nc[S[5], \
spinplus[c[]]])/2);\nExpand[komutator[HK, S[3] + spinz[c[]]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegspinoperators[{T, 1}];\nsnegrealconstants[Dz];\nHani = Dz \
nc[T[1, 3], T[1, 3]];\n{pow[Hani, 2], spinclustermatrices[Hani, {T[1]}, \
\"Blocks\" -> False]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{SuperscriptBox["Dz", "2"], " ", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"T", RowBox[{"1"}], 
                   TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"T", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"Dz", ",", "0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", 
                   ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   "0", ",", "Dz"}], "}"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\n{operatorQ[S], spinQ[S], \
spinQ[S[1, 3]], spinof[S], MemberQ[listspinoperators, S]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", "True", ",", 
           FractionBox["1", "2"], ",", "True"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\n{conj[S[1, 4]], conj[S[1, 2]], \
conj[nc[S[1, 4], S[2, 3]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"1"}], TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"S", RowBox[{"1"}], TagBox[StyleBox["\"-\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{sx, sy, sz} = \
spinxyz[c[]];\nExpand[komutator[sx, sy] - I sz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\n{S[1], S[0], S[-3]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                 "\"x\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                  FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", "0", ",", 
           RowBox[{"-", TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                   "\"z\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[G];\n{spinof[G], nc[G[1, 4], G[1, 5]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                 TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"G", RowBox[{"1"}], 
                 TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{Q, -1}];\nspinQ[Q]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "snegspinoperators::size: \
The spin size -1 must be a positive half-integer or an expression built from \
symbols."}, {"Output", RowBox[{"spinQ", "[", "Q", "]"}], "Out[2]="}}|>}|>

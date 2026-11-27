<|"Source" -> "spintensor.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegspinoperators[S];\nTable[spintensor[1, q, S[1]], {q, -1, 1}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{FractionBox[TagBox[StyleBox[TemplateBox[{"S", 
                RowBox[{"1"}], TagBox[StyleBox["\"-\"", FontColor -> 
                   RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                     0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm], SqrtBox["2"]], 
           ",", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", RowBox[{"-", FractionBox[TagBox[StyleBox[
                TemplateBox[{"S", RowBox[{"1"}], TagBox[StyleBox["\"+\"", 
                    FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              SqrtBox["2"]]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{T, 1}];\n{spintensor[2, 0, S[1]], \
spintensor[2, 0, T[1]]}", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{SqrtBox[FractionBox["2", "3"]], " ", 
               RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                     TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                         1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                      StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                      StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}], "-", FractionBox[
              RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                    TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                 FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                   {"S", RowBox[{"1"}], TagBox[StyleBox["\"-\"", FontColor -> 
                       RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
              SqrtBox["6"]], "+", FractionBox[TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              SqrtBox["6"]]}], ",", RowBox[{RowBox[{"-", SqrtBox[
                FractionBox["2", "3"]]}], "+", RowBox[{SqrtBox[FractionBox[
                 "3", "2"]], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"T", RowBox[{"1"}], TagBox[StyleBox["\"z\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"T", RowBox[{"1"}], TagBox[StyleBox["\"z\"", 
                       FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                        FontColor -> RGBColor[0, 0, 1]] & ]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nspintensor[1, 0, S -> 0]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"0"}], 
           TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
            StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
        DisplayForm], "Out[2]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\n{spintensor[1, 1, S[1]], spintensor[2, 1, S[1]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"-", FractionBox[TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"1"}], TagBox[StyleBox["\"+\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              SqrtBox["2"]]}], ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[{Q, \
3/2}];\nspinclustermatrices[spintensor[2, 0, Q[1]], {Q[1]}, \"Blocks\" -> \
False] // MatrixForm", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{SqrtBox[FractionBox["3", "2"]], "0", "0", "0"}, 
            {"0", RowBox[{"-", SqrtBox[FractionBox["3", "2"]]}], "0", "0"}, 
            {"0", "0", RowBox[{"-", SqrtBox[FractionBox["3", "2"]]}], "0"}, 
            {"0", "0", "0", SqrtBox[FractionBox["3", "2"]]}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[2]//MatrixForm="}}|>, <|"Code" -> "snegspinoperators[{Q, \
3/2}];\nExpand[Table[{komutator[Q[1, 3], spintensor[3, q, Q[1]]] - q \
spintensor[3, q, Q[1]], If[q < 3, komutator[Q[1, 4], spintensor[3, q, Q[1]]] \
- Sqrt[12 - q (q + 1)] spintensor[3, q + 1, Q[1]], 0]}, {q, -3, 3}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinoperators[{T, \
1}];\nExpand[conj[spintensor[2, 1, T[1]]] + spintensor[2, -1, T[1]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nspintensor[1, 2, S[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "spintensor::range: The rank must satisfy k >= 0 and -k <= q <= k."}, 
      {"Output", "$Failed", "Out[2]="}}|>}|>

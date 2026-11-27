<|"Source" -> "dd.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nwick[nc[c[AN, k1, UP], c[CR, k2, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{InterpretationBox[
          RowBox[{"\"-\"", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                 FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], -Sneg`dd[Sneg`nc[c[0, k2, 1], c[1, k1, 1]]]], 
         "+", TemplateBox[{RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], 
         "-", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
            "KroneckerDeltaSeq"], " ", TagBox[RowBox[
             {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              "(", RowBox[{"-", "k2"}], ")"}], DisplayForm]}]}], 
       "Out[4]="}}|>, 
   <|"Code" -> "vev[dd[nc[c[CR, k1, UP], c[AN, k2, UP]]]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\ndd[t \
nc[c[CR, 1, UP], c[AN, 2, UP]] + 2 c[CR, 1, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{InterpretationBox[RowBox[
           {"2", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                  RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                    1, 0]] & ], TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], 2*Sneg`dd[c[0, 1, 1]]], "+", 
         InterpretationBox[RowBox[{"t", TagBox[RowBox[{TagBox[StyleBox[
                 "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], t*Sneg`dd[Sneg`nc[c[0, 1, 1], c[1, 2, 1]]]]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nwick[nc[c[AN, k1, UP], c[CR, k2, UP]]] /. \
dd[_] -> 0", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
          "KroneckerDeltaSeq"], "-", RowBox[
          {TemplateBox[{RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}]}], "Out[4]="}}|>, 
   <|"Code" -> "vevwick[nc[c[AN, k1, UP], c[CR, k2, UP]]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
          "KroneckerDeltaSeq"], "-", RowBox[
          {TemplateBox[{RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}]}], "Out[5]="}}|>}|>

<|"Source" -> "wick.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nwick[nc[c[CR, k1, UP], c[AN, k2, \
UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                    TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
            TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
          DisplayForm], "+", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
            "KroneckerDeltaSeq"], " ", TagBox[RowBox[
             {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              "(", RowBox[{"-", "k1"}], ")"}], DisplayForm]}]}], 
       "Out[4]="}}|>, 
   <|"Code" -> 
     "wick[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, UP]]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{InterpretationBox[
          RowBox[{"\"-\"", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                 FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k3", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k4", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], -Sneg`dd[Sneg`nc[c[0, k1, 1], c[0, k3, 1], 
             c[1, k2, 1], c[1, k4, 1]]]], "+", 
         RowBox[{RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k3", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k4", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "-", RowBox[
          {RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k3", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "+", RowBox[{TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           RowBox[{"(", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
                  RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                       "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                     FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                      TemplateBox[{"c", RowBox[{"k4", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                  TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                      0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                       0]] & ]}], DisplayForm], "+", RowBox[
                {TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
                  "KroneckerDeltaSeq"], " ", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                    RowBox[{"-", "k1"}], ")"}], DisplayForm]}]}], ")"}]}], 
         "-", RowBox[{RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
                RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                        TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                     "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k4", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}], "+", RowBox[
          {RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}], "-", RowBox[{TemplateBox[
            {RowBox[{"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
           TemplateBox[{RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}], "+", RowBox[{TemplateBox[
            {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
           TemplateBox[{RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegfreeindexes[i, j, k, \
l];\nwick[nc[d[AN, i, UP], d[CR, j, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{InterpretationBox[RowBox[{"\"-\"", 
            TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                  RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                    1, 0]] & ], RowBox[{TagBox[StyleBox[TemplateBox[
                    {"d", RowBox[{"j", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{"i", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], -Sneg`dd[Sneg`nc[d[0, j, 1], d[1, i, 1]]]], "+", 
         TemplateBox[{RowBox[{"i", ",", "j"}]}, "KroneckerDeltaSeq"]}], 
       "Out[3]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a];\nwick[nc[a[AN], a[AN], a[CR], a[CR]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"2", "+", InterpretationBox[
          RowBox[{"4", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                 FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                   RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                   TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
               TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                   0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], 4*Sneg`dd[Sneg`nc[a[0], a[1]]]], "+", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                 "Superscript", SyntaxForm -> SuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                  SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
            TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
          DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k, \[Sigma]];\nsnegrealfunctions[eps];\nwick[sum[eps[k] \
nc[c[CR, k, \[Sigma]], c[AN, k, \[Sigma]]], {k, \[Sigma]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             RowBox[{"(", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
                    RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                          "\[Sigma]"}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"k", "\[Sigma]"}], "\"\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}], TagBox[StyleBox[
                      "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
                  DisplayForm], "+", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                    RowBox[{"-", "k"}], ")"}], DisplayForm]}], ")"}]}], ",", 
           RowBox[{"{", RowBox[{"k", ",", "\[Sigma]"}], "}"}]}], "]"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nw = wick[nc[c[CR, k1, UP], c[AN, k2, \
UP], c[CR, k3, DO], c[AN, k4, DO]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{InterpretationBox[RowBox[{"\"-\"", 
            TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                  RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                    1, 0]] & ], RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"k1", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k3", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k4", TagBox[
                        StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}], TagBox[
                StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
             DisplayForm]}], -Sneg`dd[Sneg`nc[c[0, k1, 1], c[0, k3, 0], 
             c[1, k2, 1], c[1, k4, 0]]]], "+", 
         RowBox[{RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k3", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k4", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "+", RowBox[
          {RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}], "+", RowBox[{TemplateBox[
            {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
           TemplateBox[{RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}]}], "Out[4]="}}|>, 
   <|"Code" -> "Select[Expand[w], Count[#, _UnitStep, Infinity] == 1 &]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k3", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k4", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "+", RowBox[
          {RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                    TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                         StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                      "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                      SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm], ")"}], " ", TemplateBox[
            {RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
            DisplayForm]}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nx = nc[c[CR, k1, UP], c[AN, k2, UP], \
c[CR, k3, UP], c[AN, k4, UP]];\nSimplify[(wick[x] /. dd[_] -> 0) - \
vevwick[x]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nvev[wick[nc[c[AN, k1, UP], c[CR, k2, UP]]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
          "KroneckerDeltaSeq"], "-", RowBox[
          {TemplateBox[{RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[AN, k1, UP], c[CR, k2, \
UP]];\nExpand[(wick[x] /. dd -> normalorderwick) - x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nwick[c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"wick", "[", 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "]"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nnc[dd[nc[c[CR, k1, UP], c[AN, k1, UP]]], \
dd[nc[c[CR, k2, UP], c[AN, k2, UP]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"k1", TagBox[StyleBox["\"\[UpArrow]\"", 
                      FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
            TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
          DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
           RGBColor[0, 1, 0]], TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[TemplateBox[
                 {"c", RowBox[{"k2", TagBox[StyleBox["\"\[UpArrow]\"", 
                      FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                     StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
            TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
          DisplayForm]}], "Out[4]="}}|>}|>

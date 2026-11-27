<|"Source" -> "vevwick.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nvevwick[nc[c[CR, k1, UP], c[AN, k2, \
UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
          "KroneckerDeltaSeq"], " ", TagBox[RowBox[
           {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
            RowBox[{"-", "k1"}], ")"}], DisplayForm]}], "Out[4]="}}|>, 
   <|"Code" -> 
     "vevwick[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, \
UP]]]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
            "KroneckerDeltaSeq"], " ", TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
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
            DisplayForm]}]}], "Out[5]="}}|>, <|"Code" -> "SimplifyKD[%]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], " ", 
         RowBox[{"(", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", 
                   "k2"}]}, "KroneckerDeltaSeq"], " ", TemplateBox[
                {RowBox[{"k3", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
               TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                     RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                       0, 1, 0]] & ], "(", RowBox[{"-", "k3"}], ")"}], 
                DisplayForm]}], "+", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
               TemplateBox[{RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], 
               " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                    FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 1, 0]] & ], "(", "k3", ")"}], 
                DisplayForm]}]}], ")"}]}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegfreeindexes[i, j, k, l];\nx = \
nc[d[AN, i, UP], d[AN, j, UP], d[CR, k, UP], d[CR, l, UP]];\n{vevwick[x], \
vev[x]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{TemplateBox[{RowBox[{"i", ",", "l"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"j", ",", 
                   "k"}]}, "KroneckerDeltaSeq"]}], "-", 
             RowBox[{TemplateBox[{RowBox[{"i", ",", "k"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"j", ",", 
                   "l"}]}, "KroneckerDeltaSeq"]}]}], ",", 
           RowBox[{RowBox[{TemplateBox[{RowBox[{"i", ",", "l"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"j", ",", 
                   "k"}]}, "KroneckerDeltaSeq"]}], "-", 
             RowBox[{TemplateBox[{RowBox[{"i", ",", "k"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"j", ",", 
                   "l"}]}, "KroneckerDeltaSeq"]}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nvevwick[nc[a[AN], a[AN], a[AN], \
a[CR], a[CR], a[CR]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "6", "Out[2]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a, b];\nvevwick[nc[a[AN], b[AN], a[CR], b[CR]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[g];\nordering[g] = NONE;\nx = nc[g[AN, \
1, UP], g[AN, 2, UP], g[CR, 2, UP], g[CR, 1, UP]];\n{x, vevwick[x]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"g", 
                 RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"g", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"g", RowBox[
                  {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", "1"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k];\nsnegrealfunctions[eps];\nvevwick[sum[eps[k] \
nc[c[CR, k, UP], c[AN, k, UP]], {k}] + 1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"1", "+", RowBox[{"sum", "[", 
           RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", TagBox[
                RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                     RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                       0, 1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
                DisplayForm]}], ",", RowBox[{"{", "k", "}"}]}], "]"}]}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4, s1, s2];\nSimplifyKD[vevwick[nc[c[CR, \
k1, s1], c[CR, k2, s2], c[AN, k3, s2], c[AN, k4, s1]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"(", RowBox[{RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
               TemplateBox[{RowBox[{"k2", ",", "k3"}]}, 
                "KroneckerDeltaSeq"]}], "-", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
               TemplateBox[{RowBox[{"k2", ",", "k4"}]}, "KroneckerDeltaSeq"], 
               " ", TemplateBox[{RowBox[{"s1", ",", "s2"}]}, 
                "KroneckerDeltaSeq"]}]}], ")"}], " ", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], " ", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k2"}], ")"}], DisplayForm]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\n{SimplifyKD[vevwick[nc[c[CR, k1, UP], \
c[CR, k2, UP], c[AN, k3, UP], c[AN, k4, UP]]]],\n SimplifyKD[vevwick[nc[c[CR, \
k1, UP], c[CR, k2, DO], c[AN, k3, DO], c[AN, k4, UP]]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{"(", RowBox[{RowBox[{TemplateBox[{RowBox[
                      {"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
                   TemplateBox[{RowBox[{"k2", ",", "k3"}]}, 
                    "KroneckerDeltaSeq"]}], "-", RowBox[{TemplateBox[
                    {RowBox[{"k1", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
                   TemplateBox[{RowBox[{"k2", ",", "k4"}]}, 
                    "KroneckerDeltaSeq"]}]}], ")"}], " ", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
              DisplayForm], " ", TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k2"}], ")"}], DisplayForm]}], ",", 
           RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
              "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"k2", ",", 
                 "k3"}]}, "KroneckerDeltaSeq"], " ", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
              DisplayForm], " ", TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k2"}], ")"}], DisplayForm]}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k, kp];\nSimplifyKD[vevwick[nc[c[CR, k, UP], c[AN, kp, \
UP], c[CR, kp, UP], c[AN, k, UP]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
          DisplayForm], " ", RowBox[{"(", RowBox[
            {RowBox[{TemplateBox[{RowBox[{"k", ",", "kp"}]}, 
                "KroneckerDeltaSeq"], " ", TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                  RowBox[{"-", "kp"}], ")"}], DisplayForm]}], "+", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", "kp", ")"}], DisplayForm]}], ")"}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegbosonoperators[a];\nx = (a[AN] + \
a[CR])/Sqrt[2];\nTable[vevwick[pow[x, n]], {n, 2, 6, 2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           FractionBox["3", "4"], ",", FractionBox["15", "8"]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[AN, k1, UP], c[CR, k2, \
UP]];\n{wick[x], vevwick[x]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{InterpretationBox[RowBox[{"\"-\"", 
                TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                      RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                        SyntaxForm -> SubsuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                         SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                       ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                   TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 
                       1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                        0]] & ]}], DisplayForm]}], -Sneg`dd[Sneg`nc[
                 c[0, k2, 1], c[1, k1, 1]]]], "+", TemplateBox[
              {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], "-", 
             RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
                "KroneckerDeltaSeq"], " ", TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                  RowBox[{"-", "k2"}], ")"}], DisplayForm]}]}], ",", 
           RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
              "KroneckerDeltaSeq"], "-", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
               TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                     RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                       0, 1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
                DisplayForm]}]}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nx = nc[c[CR, k1, UP], c[AN, k2, UP], \
c[CR, k3, UP], c[AN, k4, UP]];\nvevwick[normalorderwick[x]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\n{vevwick[nc[c[CR, -1, UP], c[AN, -1, UP]]], vevwick[nc[c[AN, -1, UP], \
c[CR, -1, UP]]], vevwick[nc[c[AN, 1, UP], c[CR, 1, UP]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", "1"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4, s1, s2];\nr = vevwick[nc[c[CR, k1, s1], \
c[CR, k2, s2], c[AN, k3, s2], c[AN, k4, s1]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
            "KroneckerDeltaSeq"], " ", TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}], "-", RowBox[{TemplateBox[
            {RowBox[{"k1", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TemplateBox[{RowBox[{"k2", ",", "k4"}]}, "KroneckerDeltaSeq"], 
           " ", SuperscriptBox[TemplateBox[{RowBox[{"s1", ",", "s2"}]}, 
             "KroneckerDeltaSeq"], "2"], " ", TagBox[
            RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 
                  1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
            DisplayForm]}]}], "Out[4]="}}|>, <|"Code" -> "SimplifyKD[r]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"(", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", 
                   "k4"}]}, "KroneckerDeltaSeq"], " ", TemplateBox[
                {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"]}], "-", 
             RowBox[{TemplateBox[{RowBox[{"k1", ",", "k3"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"k2", ",", 
                   "k4"}]}, "KroneckerDeltaSeq"], " ", TemplateBox[
                {RowBox[{"s1", ",", "s2"}]}, "KroneckerDeltaSeq"]}]}], ")"}], 
         " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
          DisplayForm], " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
          DisplayForm]}], "Out[5]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegfermionoperators[c];\nordering[c] \
= SEA;\nsnegfreeindexes[k];\nvevwick[nc[a[AN], c[CR, k, UP], c[AN, k, UP], \
a[CR]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k"}], ")"}], DisplayForm], "-", 
         RowBox[{RowBox[{"vev", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], " ", 
           RowBox[{"vev", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", "\"\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}]}]}], 
       "Out[5]="}}|>, 
   <|"Code" -> 
     "vevwick[nc[a[AN], a[CR]]] vevwick[nc[c[CR, k, UP], c[AN, k, UP]]]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
          RowBox[{"-", "k"}], ")"}], DisplayForm], "Out[6]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nvevwick[nc[S[1, 4], S[1, 5]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["1", "2"], "+", 
         RowBox[{"vevwick", "[", TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"1"}], TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], "]"}]}], "Out[2]="}}|>}|>

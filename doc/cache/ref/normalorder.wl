<|"Source" -> "normalorder.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k];\nnormalorder[nc[c[CR, k, UP], c[AN, k, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", TagBox[RowBox[
           {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
            RowBox[{"-", "k"}], ")"}], DisplayForm]}], "Out[4]="}}|>, 
   <|"Code" -> "vev[%]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nnormalorder[nc[c[AN, k1, UP], c[CR, k2, \
UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"k2", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
         "+", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
            "KroneckerDeltaSeq"], " ", TagBox[RowBox[
             {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              "(", RowBox[{"-", "k2"}], ")"}], DisplayForm]}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d];\nnormalorder[nc[d\
[AN, 1, UP], d[CR, 1, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                {"d", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"d", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ")"}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nnormalorder[nc[a[AN], a[CR]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
            SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nnormalorder[{nc[a[AN], a[CR]], \
nc[a[CR], a[AN]]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a", 
                 "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nsnegrealconstants[\[Omega]];\nx = \
(a[AN] + a[CR])/Sqrt[2];\np = I (a[CR] - a[AN])/Sqrt[2];\nH = Expand[\[Omega] \
(pow[p, 2] + pow[x, 2])/2]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["\[Omega]", "2"], "+", 
         RowBox[{"\[Omega]", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[5]="}}|>, <|"Code" -> "normalorder[H]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{"\[Omega]", " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k];\nsnegrealfunctions[eps];\nnormalorder[sum[eps[k] \
nc[c[CR, k, UP], c[AN, k, UP]], {k}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", RowBox[
                {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "-", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], 
               " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                    FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 1, 0]] & ], "(", RowBox[
                   {"-", "k"}], ")"}], DisplayForm]}], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[CR, k1, UP], c[AN, k2, \
UP]];\nnormalorder[x] == x - vev[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[c[AN, k1, UP], c[CR, k2, \
UP]];\nnormalorder[x] - normalorderwick[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nx = nc[c[CR, k1, UP], c[AN, k2, UP], \
c[CR, k3, UP], c[AN, k4, UP]];\nnormalorder[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TemplateBox[{RowBox[{"k2", ",", "k3"}]}, 
            "KroneckerDeltaSeq"], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"k1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k4", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k3", TagBox[
                  StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                  StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"k4", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
            "KroneckerDeltaSeq"], " ", TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "+", RowBox[{"vev", "[", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k3", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k2", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k4", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}]}], 
       "Out[5]="}}|>, <|"Code" -> "snegbosonoperators[a];\nx = nc[a[AN], \
a[AN], a[CR], a[CR]];\n{normalorder[x], wick[x]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{RowBox[{"4", " ", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                     SuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
             "+", RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                  "Superscript", SyntaxForm -> SuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"2", "+", InterpretationBox[RowBox[{"4", TagBox[
                 RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                      RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 1, 0]] & ], RowBox[{TagBox[StyleBox[
                       TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                        SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                      FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                       TemplateBox[{"a", "\"\""}, "Superscript", 
                        SyntaxForm -> SuperscriptBox], 
                       ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                        {1, 1}], DisplayForm]}], TagBox[StyleBox[
                     "\"\[Colon]\"", FontColor -> RGBColor[0, 1, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
                 DisplayForm]}], 4*Sneg`dd[Sneg`nc[a[0], a[1]]]], "+", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], RowBox[{TagBox[StyleBox[TemplateBox[
                     {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                      SuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                     "Superscript", SyntaxForm -> SuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                   FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                    TemplateBox[{"a", "\"\""}, "Superscript", SyntaxForm -> 
                      SuperscriptBox], ScriptSizeMultipliers -> 1, 
                    ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                  StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                  TagBox[StyleBox[TemplateBox[{"a", "\"\""}, "Superscript", 
                     SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                     1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[0, 1, 
                    0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], 
              DisplayForm]}]}], "}"}], "Out[3]="}}|>}|>

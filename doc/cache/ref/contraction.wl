<|"Source" -> "contraction.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[i, \
j];\n{contraction[c[AN, i, UP], c[CR, j, UP]], contraction[c[CR, i, UP], \
c[AN, j, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
            "KroneckerDeltaSeq"], ",", "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\n{contraction[f[CR, k1, UP], f[AN, k2, UP]], \
contraction[f[AN, k1, UP], f[CR, k2, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
              "KroneckerDeltaSeq"], " ", TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k1"}], ")"}], DisplayForm]}], ",", 
           RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
              "KroneckerDeltaSeq"], "-", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
               TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                     RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                       0, 1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
                DisplayForm]}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[i, j, s1, \
s2];\ncontraction[c[AN, i, s1], c[CR, j, s2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
          "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"s1", ",", "s2"}]}, 
          "KroneckerDeltaSeq"]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{contraction[c[CR, 1, UP], c[CR, 2, \
UP]], contraction[c[AN, 1, UP], c[AN, 2, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\n{contraction[a[AN], a[CR]], \
contraction[a[CR], a[AN]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\n{contraction[f[CR, -2, UP], f[AN, -2, UP]], contraction[f[CR, 3, UP], \
f[AN, 3, UP]], contraction[f[AN, 3, UP], f[CR, 3, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", "1"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[g];\nordering[g] = \
NONE;\n{contraction[g[AN, 1, UP], g[CR, 1, UP]], contraction[g[CR, 1, UP], \
g[AN, 1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\ncontraction[f[AN, k1, UP], f[CR, k2, UP]] == \
vev[nc[f[AN, k1, UP], f[CR, k2, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\nSimplify[contraction[f[AN, k1, UP], f[CR, k2, \
UP]] + contraction[f[CR, k2, UP], f[AN, k1, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
        "KroneckerDeltaSeq"], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\nwick[nc[f[CR, k1, UP], f[AN, k2, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[Colon]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k1", 
                    TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k2", TagBox[
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
   <|"Code" -> "snegfermionoperators[c];\ncontraction[u, c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"contraction", "[", 
         RowBox[{"u", ",", TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], 
       "Out[2]="}}|>}|>

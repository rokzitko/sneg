<|"Source" -> "vev.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{vev[nc[c[AN, 1, UP], \
c[CR, 1, UP]]], vev[nc[c[CR, 1, UP], c[AN, 1, UP]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k, k1, k2];\n{vev[nc[f[CR, k, UP], f[AN, k, UP]]], \
vev[nc[f[AN, k, UP], f[CR, k, UP]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], DisplayForm], 
           ",", RowBox[{"1", "-", TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k"}], ")"}], DisplayForm]}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "vev[nc[f[CR, k1, UP], f[AN, k2, UP]]]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
          "KroneckerDeltaSeq"], " ", TagBox[RowBox[
           {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
            RowBox[{"-", "k1"}], ")"}], DisplayForm]}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, U];\nvev[U + t \
nc[c[AN, 1, UP], c[CR, 1, UP]] + t nc[c[AN, 2, DO], c[CR, 2, DO]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"2", " ", "t"}], "+", "U"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nvev[nc[c[AN, 1, \
UP], c[AN, 2, UP], c[CR, 2, UP], c[CR, 1, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "1", "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\nvev[nc[a[AN], a[AN], a[CR], a[CR]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "2", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k, \[Sigma]];\nsnegrealfunctions[eps];\nvev[sum[eps[k] \
nc[f[CR, k, \[Sigma]], f[AN, k, \[Sigma]]], {k, \[Sigma]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
              DisplayForm]}], ",", RowBox[{"{", RowBox[{"k", ",", 
               "\[Sigma]"}], "}"}]}], "]"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = SEA;\n{vev[nc[f[CR, \
-1, UP], f[AN, -1, UP]]], vev[nc[f[CR, 2, UP], f[AN, 2, UP]]], vev[nc[f[AN, \
-1, UP], f[CR, -1, UP]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", "0"}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nvev[{nu\
mber[c[1]], nc[c[AN, 1, UP], c[CR, 1, UP]]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, U];\nH = -t \
hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);\npsi = (nc[c[CR, 1, UP], \
c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];\nvev[nc[conj[psi], \
H, psi]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "d = (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[CR, 2, UP], c[CR, \
2, DO]])/Sqrt[2];\nvev[nc[conj[d], H, psi]]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "2"}], " ", "t"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = SEA;\nH = Sum[k \
number[f[k]], {k, -2, 2}];\nvev[H]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"-", "6"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\nvev[normalorder[nc[f[CR, k1, UP], f[AN, k2, \
UP]]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2];\nx = nc[f[AN, k1, UP], f[CR, k2, \
UP]];\n{vev[x], vevwick[x]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
              "KroneckerDeltaSeq"], "-", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], " ", 
               TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                     RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                       0, 1, 0]] & ], "(", RowBox[{"-", "k2"}], ")"}], 
                DisplayForm]}]}], ",", RowBox[
            {TemplateBox[{RowBox[{"k1", ",", "k2"}]}, "KroneckerDeltaSeq"], 
             "-", RowBox[{TemplateBox[{RowBox[{"k1", ",", "k2"}]}, 
                "KroneckerDeltaSeq"], " ", TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                  RowBox[{"-", "k2"}], ")"}], DisplayForm]}]}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{nc[conj[VACUUM]\
, c[AN, 1, UP], c[CR, 1, UP], VACUUM], vev[nc[c[AN, 1, UP], c[CR, 1, UP]]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "1"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[f];\nordering[f] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4];\nx = nc[f[CR, k1, UP], f[AN, k2, UP], \
f[CR, k3, UP], f[AN, k4, UP]];\nvev[x]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
            "KroneckerDeltaSeq"], " ", TemplateBox[
            {RowBox[{"k2", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k1"}], ")"}], 
            DisplayForm]}], "-", RowBox[{"vev", "[", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k1", 
                   TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k3", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k2", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{"k4", TagBox[
                    StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                       0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}]}], 
       "Out[5]="}}|>, <|"Code" -> "vevwick[x]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TemplateBox[{RowBox[{"k1", ",", "k4"}]}, 
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
            DisplayForm]}]}], "Out[6]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\n{vev[nc[a[CR], a[CR]]], \
vevwick[nc[a[CR], a[CR]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"vev", "[", RowBox[{TagBox[StyleBox[
                 TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], ",", 
           "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nvev[nc[c[AN, 1, UP], u, c[CR, 1, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"vev", "[", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], "u", 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[u];\nvev[nc[c[AN, 1, UP], u, c[CR, 1, \
UP]]]", "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", "u", "Out[4]="}}|>}|>

<|"Source" -> "acmt.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{acmt[c[AN, 1, UP], \
c[CR, 1, UP]], acmt[c[AN, 1, UP], c[CR, 2, UP]], acmt[c[CR, 1, UP], c[CR, 1, \
UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", "0"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "nc[c[AN, 1, UP], c[CR, 1, UP]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{acmt[c[AN, i, s], c[CR, j, t]], \
nc[c[AN, i, s], c[CR, j, t]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
              "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"s", ",", 
                 "t"}]}, "KroneckerDeltaSeq"]}], ",", 
           RowBox[{RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
                "KroneckerDeltaSeq"], " ", TemplateBox[{RowBox[{"s", ",", 
                   "t"}]}, "KroneckerDeltaSeq"]}], "-", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"j", "t"}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"i", "s"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, d];\n{acmt[c[AN, 1, UP], c[CR, UP]], \
acmt[c[AN, 1, UP], d[CR, 1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegmajoranaoperators[g];\n{acmt[g[1], \
g[1]], acmt[g[1], g[2]], acmt[g, g]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", "1"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[psi];\nsnegre\
alconstants[x, y, z];\nsnegnonopQ[DiracDelta] = True;\npsi /: acmt[psi[AN, \
x_], psi[CR, y_]] := DiracDelta[x - y];\npsi /: acmt[psi[CR, x_], psi[AN, \
y_]] := DiracDelta[x - y];\nnc[psi[AN, x], psi[CR, y], psi[CR, z]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"y"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"z"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"x"}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[{RowBox[{"DiracDelta", "[", 
             RowBox[{"x", "-", "z"}], "]"}], " ", 
           TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"y"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "y"}], "]"}], 
           " ", TagBox[StyleBox[TemplateBox[{"psi", RowBox[{"z"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[6]="}}|>, 
   <|"Code" -> "vevwick[nc[psi[AN, x], psi[AN, z], psi[CR, z], psi[CR, y]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"DiracDelta", "[", "0", "]"}], " ", 
           RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "y"}], "]"}]}], "-", 
         RowBox[{RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "z"}], "]"}], 
           " ", RowBox[{"DiracDelta", "[", RowBox[{RowBox[{"-", "y"}], "+", 
               "z"}], "]"}]}]}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[h];\nsnegrealconstants[S];\nh /: \
acmt[h[AN, i_, s1_], h[CR, j_, s2_]] := KroneckerDelta[s1, s2] If[i === j, 1, \
S];\nnc[h[AN, 1, UP], h[CR, 2, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"S", "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"h", RowBox[{"2", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"h", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[4]="}}|>, 
   <|"Code" -> "psiHL = nc[h[CR, 1, UP], h[CR, 2, DO]] - nc[h[CR, 1, DO], \
h[CR, 2, UP]];\nSimplify[vev[nc[conj[psiHL], psiHL]]]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{"2", " ", RowBox[
          {"(", RowBox[{"1", "+", SuperscriptBox["S", "2"]}], ")"}]}], 
       "Out[6]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{antikomutator[c\
[AN, k, UP], c[CR, q, UP]], acmt[c[AN, k, UP], c[CR, q, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TemplateBox[{RowBox[{"k", ",", "q"}]}, 
            "KroneckerDeltaSeq"], ",", TemplateBox[{RowBox[{"k", ",", "q"}]}, 
            "KroneckerDeltaSeq"]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\n{cmt[a[AN, 1], a[CR, 1]], acmt[a[AN, \
1], a[CR, 1]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           RowBox[{"acmt", "[", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"a", RowBox[{"1"}], "\"\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
               TagBox[StyleBox[TemplateBox[{"a", RowBox[{"1"}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[phi]\
;\nphi /: acmt[phi[AN, x_], phi[CR, y_]] := w[x - y];\nnc[phi[AN, 1], phi[CR, \
2], phi[CR, 3]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"phi", RowBox[{"2"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], RowBox[
                {"w", "[", RowBox[{"-", "2"}], "]"}]}], ")"}]}], "+", 
         RowBox[{RowBox[{"w", "[", RowBox[{"-", "1"}], "]"}], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"3"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"2"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"3"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"1"}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegrealfunctions[w];\nnc[phi[AN, 1], phi[CR, 2], phi[CR, \
3]]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"2"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"3"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"1"}], "\"\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"2"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
           RowBox[{"w", "[", RowBox[{"-", "2"}], "]"}]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"phi", RowBox[{"3"}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
           RowBox[{"w", "[", RowBox[{"-", "1"}], "]"}]}]}], "Out[5]="}}|>}|>

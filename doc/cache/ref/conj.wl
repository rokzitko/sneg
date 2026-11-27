<|"Source" -> "conj.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z]\
;\nconj[z nc[c[CR, 1, UP], c[AN, 2, DO]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"Conjugate", "[", "z", "]"}], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                 TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[conj[hop[c[1], c[2]]] - \
hop[c[1], c[2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[t];\nsnegcomplexconstants[z];\nconj[{t, z, \
2 + 3 I, Exp[I t]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"t", ",", 
           RowBox[{"Conjugate", "[", "z", "]"}], ",", 
           RowBox[{"2", "-", RowBox[{"3", " ", "\[ImaginaryI]"}]}], ",", 
           SuperscriptBox["\[ExponentialE]", RowBox[
             {RowBox[{"-", "\[ImaginaryI]"}], " ", "t"}]]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegbosonoperators[a];\nsnegmajoranaoperat\
ors[\[Gamma]];\nconj[{a[CR], nc[a[CR], a[AN], a[AN]], \[Gamma][1]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"a", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"\[Gamma]", "[", "1", "]"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nconj[{S[1, 1], S[1, 4], nc[S[1, 4], S[2, 5]]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
              {"S", RowBox[{"1"}], TagBox[StyleBox["\"x\"", FontColor -> 
                  RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                    0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
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
                  StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nconj[{ket[1], nc[c[CR, 1, UP], VACUUM]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"1"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm], ",", 
           RowBox[{TagBox[StyleBox[
               "\"\[LeftAngleBracket]\[Diameter]\[RightDoubleBracketingBar]\"\
", FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  1]] & ], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k];\nsnegcomplexfunctions[v];\nconj[sum[v[k] nc[c[CR, k, UP], c[AN, 0, UP]], \
{k}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{"Conjugate", "[", RowBox[{"v", "[", "k", 
                 "]"}], "]"}], " ", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"0", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[4]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[g];\nsnegfermionoperators[c];\n{conj[nc\
[g, c[CR, UP]]], conj[conj[g]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{OverscriptBox["g", 
                  "_"], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                    0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                      {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], ")"}]}], ",", "g"}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c, d];\nconj[c[CR, 1, UP] -> d[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "\[Rule]", TagBox[StyleBox[TemplateBox[
            {"d", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, d];\nsnegrealconstants[ed, \
U];\nsnegcomplexconstants[V];\nH = ed number[d[]] + U hubbard[d[]] + Sum[V \
nc[c[CR, \[Sigma]], d[AN, \[Sigma]]] + Conjugate[V] nc[d[CR, \[Sigma]], c[AN, \
\[Sigma]]], {\[Sigma], {UP, DO}}];\nExpand[conj[H] - H]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[\
\[CapitalDelta]];\npair = \[CapitalDelta] nc[c[CR, UP], c[CR, DO]];\npair + \
conj[pair]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{RowBox[{"-", "\[CapitalDelta]"}], 
           " ", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{RowBox[{"Conjugate", "[", "\[CapitalDelta]", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[\[Alpha], \
\[Beta]];\npsi = \[Alpha] c[CR, 1, UP] + \[Beta] nc[c[CR, 1, UP], c[CR, 1, \
DO]];\nvev[nc[conj[psi], psi]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{SuperscriptBox["\[Alpha]", "2"], "+", 
         SuperscriptBox["\[Beta]", "2"]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z];\nx = z \
nc[c[CR, 1, UP], c[AN, 2, DO]];\nconj[conj[x]] == x", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nA = c[CR, 1, UP] + c[AN, 2, UP];\nB \
= nc[c[CR, 3, DO], c[AN, 1, UP]];\nExpand[conj[nc[A, B]] - nc[conj[B], \
conj[A]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nconj[w c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"conj", "[", 
         RowBox[{"w", " ", TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "snegrealconstants[w];\nconj[w c[CR, 1, UP]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{"w", " ", 
         TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfreeindexes[k];\nconj[k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"Conjugate", "[", "k", "]"}], "Out[2]="}}|>}|>

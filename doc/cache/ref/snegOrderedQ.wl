<|"Source" -> "snegOrderedQ.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{snegOrderedQ[c[CR, 1, \
UP], c[AN, 1, UP]], snegOrderedQ[c[AN, 1, UP], c[CR, 1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], 
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
   <|"Code" -> "snegfermionoperators[c];\n{snegOrderedQ[c[CR, 1, UP], c[CR, \
2, DO]], snegOrderedQ[c[CR, 1, UP], c[CR, 1, DO]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[d];\nordering[d] = \
SEA;\n{snegOrderedQ[d[CR, 1, UP], d[AN, 1, UP]], snegOrderedQ[d[CR, -1, UP], \
d[AN, -1, UP]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegbosonoperators[a];\nsnegspino\
perators[{S, 1/2}];\n{snegOrderedQ[a[AN], a[CR]], snegOrderedQ[S[1, 3], S[1, \
4]], snegOrderedQ[S[2, 3], S[1, 4]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"False", ",", "True", ",", "False"}], 
         "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegOrderedQ[g[AN, 1], g[CR, 1]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"snegOrderedQ", "[", 
         RowBox[{RowBox[{"g", "[", RowBox[{"1", ",", "1"}], "]"}], ",", 
           RowBox[{"g", "[", RowBox[{"0", ",", "1"}], "]"}]}], "]"}], 
       "Out[1]="}}|>, <|"Code" -> "snegbosonoperators[b];\nb /: \
snegOrderedQ[b[t1_, j1___], b[t2_, j2___]] := OrderedQ[{{-t1, {j1}}, {-t2, \
{j2}}}];\n{nc[b[CR], b[AN]], nc[b[CR], b[CR], b[AN], b[AN]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "1"}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"b", "\"\""}, 
                  "Superscript", SyntaxForm -> SuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"b", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{"2", "-", RowBox[{"4", " ", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"b", "\"\""}, "Superscript", SyntaxForm -> 
                     SuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}]}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"b", "\"\""}, 
                  "Superscript", SyntaxForm -> SuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"b", "\"\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, "Superscript", 
                  SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "Expand[pow[number[b[]], 2]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "-", 
         RowBox[{"3", " ", RowBox[{TagBox[StyleBox[TemplateBox[{"b", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"b", "\"\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"b", "\"\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"b", "\"\[Dagger]\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[f];\nf /: \
snegOrderedQ[f[t1_, i1_, s1_], f[t2_, i2_, s2_]] := OrderedQ[{{i1, t1, s1}, \
{i2, t2, s2}}];\nnc[f[CR, 1, UP], f[CR, 2, UP], f[AN, 1, UP], f[AN, 2, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                {"f", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"f", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[3]="}}|>, <|"Code" -> "hop[f[1], f[2], UP]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"f", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"f", RowBox[{"2", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"f", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"f", RowBox[
                {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{snegOrderedQ[c[\
AN, 1, UP], c[CR, 2, UP]], OrderedQ[{c[AN, 1, UP], c[CR, 2, UP]}], {CR, AN}}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"False", ",", "False", ",", 
           RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c, \
d];\n{snegOrderedQ[d[CR, 1, UP], c[AN, 1, UP]], snegOrderedQ[d[CR, 1, UP], \
c[CR, 1, UP]], nc[d[CR, 1, UP], c[CR, 1, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "False", ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                         FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                    SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], ")"}]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[b2];\nb2 /: snegOrderedQ[b2[t1_, j1___], \
b2[t2_, j2___]] := OrderedQ[{{-t1, {j1}}, {-t2, {j2}}}];\n{nc[b2[CR], \
b2[AN]], vev[nc[b2[CR], b2[AN]]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "1"}], "+", 
             RowBox[{TagBox[StyleBox[TemplateBox[{"b2", "\"\""}, 
                  "Superscript", SyntaxForm -> SuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                 RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                  {"b2", "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                   SuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], ",", 
           RowBox[{RowBox[{"-", "1"}], "+", RowBox[{"vev", "[", RowBox[
                {TagBox[StyleBox[TemplateBox[{"b2", "\"\""}, "Superscript", 
                    SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 
                    1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"b2", "\"\[Dagger]\""}, 
                    "Superscript", SyntaxForm -> SuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], "]"}]}]}], "}"}], 
       "Out[3]="}}|>}|>

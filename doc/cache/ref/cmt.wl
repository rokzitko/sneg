<|"Source" -> "cmt.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegbosonoperators[a];\n{cmt[a[AN, 1], a[CR, 1]], \
cmt[a[CR, 1], a[AN, 1]], cmt[a[AN, 1], a[CR, 2]], cmt[a[AN, 1], a[AN, 2]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", RowBox[{"-", "1"}], 
           ",", "0", ",", "0"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "nc[a[AN], a[AN], a[CR]]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"2", " ", TagBox[StyleBox[
             TemplateBox[{"a", "\"\""}, "Superscript", SyntaxForm -> 
               SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"a", "\"\[Dagger]\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"a", "\"\""}, 
              "Superscript", SyntaxForm -> SuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\ncmt[a[AN, k], a[CR, q]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TemplateBox[{RowBox[{"k", ",", "q"}]}, 
        "KroneckerDeltaSeq"], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\n{cmt[S[1, 1], S[1, 2]], cmt[S[1, 4], \
S[1, 5]], cmt[S[1, 3], S[1, 4]], cmt[S[1, 1], S[2, 2]], cmt[S[i, 1], S[j, \
2]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"\[ImaginaryI]", " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"2", " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", TagBox[StyleBox[TemplateBox[
              {"S", RowBox[{"1"}], TagBox[StyleBox["\"+\"", FontColor -> 
                  RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                    0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", "0", ",", 
           RowBox[{"\[ImaginaryI]", " ", TemplateBox[{RowBox[{"i", ",", 
                 "j"}]}, "KroneckerDeltaSeq"], " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"i"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nb /: cmt[b[AN, i_], b[CR, j_]] := \
KroneckerDelta[i, j] (1 - 2 nc[b[CR, i], b[AN, i]]);\nb /: nc[x___, b[t_, \
i_], b[t_, i_], y___] := 0;\n{nc[b[AN, 1], b[CR, 1]], nc[b[CR, 1], b[CR, 1]], \
nc[b[AN, 1], b[CR, 2]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"1", "-", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"b", RowBox[{"1"}], "\"\[Dagger]\""}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"b", RowBox[{"1"}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}]}], ",", "0", ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"b", RowBox[{"2"}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"b", RowBox[{"1"}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> 
     "{Expand[pow[number[b[1]], 2]], Expand[komutator[b[AN, 1], b[CR, 2]]]}", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"b", 
                 RowBox[{"1"}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"b", RowBox[{"1"}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", "0"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "Expand[komutator[b[AN, 1], b[CR, 1]]]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{"1", "-", RowBox[{"2", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"b", RowBox[{"1"}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"b", RowBox[{"1"}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[6]="}}|>, 
   <|"Code" -> "snegbosonoperators[a];\n{komutator[a[AN, k], a[CR, q]], \
cmt[a[AN, k], a[CR, q]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TemplateBox[{RowBox[{"k", ",", "q"}]}, 
            "KroneckerDeltaSeq"], ",", TemplateBox[{RowBox[{"k", ",", "q"}]}, 
            "KroneckerDeltaSeq"]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegbosonoperators[a1, a2];\n{nc[a1[AN], a2[CR]], \
komutator[a1[AN], a2[CR]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"a2", 
                 "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                 SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"a1", "\"\""}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", "0"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{cmt[c[\
AN, 1, UP], c[CR, 1, UP]], komutator[c[AN, 1, UP], c[CR, 1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"cmt", "[", RowBox[{TagBox[StyleBox[
                 TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", TagBox[
                StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "]"}], ",", 
           RowBox[{"1", "-", RowBox[{"2", " ", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                        StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}]}], 
         "}"}], "Out[2]="}}|>}|>

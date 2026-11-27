<|"Source" -> "eomclosure.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[e, \
U];\nh = e number[d[]] + U hubbard[d[]];\n{ops, m, a} = eomclosure[d[AN, UP], \
h]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"d", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                    {"d", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                        StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 TagBox[StyleBox[TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"e", ",", "U"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[{"e", "+", 
                     "U"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "{Expand[a.ops - d[AN, UP]], \
Expand[(komutator[#, h] & /@ ops) - m.ops]}", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t];\nh = -t (hop[c[1], c[2]] + hop[c[2], c[3]]);\n{ops, m, a} = \
eomclosure[c[AN, 1, UP], h];\n{ops, Factor[CharacteristicPolynomial[m, z]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"3", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "}"}], ",", RowBox[{"z", " ", 
             RowBox[{"(", RowBox[{RowBox[{"2", " ", SuperscriptBox["t", 
                    "2"]}], "-", SuperscriptBox["z", "2"]}], ")"}]}]}], 
         "}"}], "Out[5]="}}|>, <|"Code" -> "snegfermionoperators[d];\neomclos\
ure[d[AN, UP], hubbard[d[]], \"MaxOperators\" -> 1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "eomclosure::open: The \
equations of motion do not close within 1 operators."}, 
      {"Output", "$Failed", "Out[2]="}}|>}|>

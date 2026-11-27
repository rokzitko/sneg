<|"Source" -> "isnumericQ.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegrealconstants[t];\nsnegcomplexconstants[z];\n\
snegfermionoperators[c];\n{isnumericQ[t], isnumericQ[2 t + I z^2], \
isnumericQ[u], isnumericQ[c[CR, 1, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", "False", ",", 
           "False"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> 
     "{nc[c[AN, 1, UP], t c[CR, 1, UP]], nc[c[AN, 1, UP], u c[CR, 1, UP]]}", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"t", " ", RowBox[{"(", RowBox[{"1", "-", 
                 RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                         TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                      "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                     FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                      FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], RowBox[{"u", " ", TagBox[StyleBox[TemplateBox[
                  {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegrealconstants[t];\n{isnumericQ[2], \
isnumericQ[Pi], isnumericQ[Sqrt[t]], isnumericQ[Exp[I t]], \
isnumericQ[Sin[u]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "True", ",", "True", ",", "False"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegrealfunctions[eps];\nsnegcomplexfunctions[V];\nsnegfreein\
dexes[k];\n{isnumericQ[eps[k]], isnumericQ[V[k] Conjugate[V[k]]], \
isnumericQ[KroneckerDelta[k, 1]], isnumericQ[UnitStep[k]], isnumericQ[g[k]]}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "True", ",", "True", ",", "False"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsneggrassmanconstants[x];\n{isnumer\
icQ[avg[nc[c[CR, 1, UP], c[AN, 1, UP]]]], isnumericQ[x]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "False"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[x, y];\nsnegnonopQ[DiracDelta] = True;\n{isnumericQ[DiracDelta[x - y]], \
nc[c[AN, 1, UP], DiracDelta[x - y] c[CR, 1, UP]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{RowBox[{"DiracDelta", "[", RowBox[{"x", "-", "y"}], "]"}], 
             " ", RowBox[{"(", RowBox[{"1", "-", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nw /: snegnonopQ[w] = \
True;\n{isnumericQ[3 w^2], nc[c[AN, 1, UP], w^2 c[CR, 1, UP]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{SuperscriptBox["w", "2"], " ", RowBox[{"(", RowBox[
                {"1", "-", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                       RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                       "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                   StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                   TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                          StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegrealconstants[t];\n{NumericQ[t], isnumericQ[t]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"False", ",", "True"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegrealconstants[t];\nsnegcomple\
xconstants[z];\nsnegfermionoperators[c];\nconj[(t + z) c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], " ", RowBox[{"(", RowBox[{"t", "+", 
             RowBox[{"Conjugate", "[", "z", "]"}]}], ")"}]}], "Out[4]="}}|>, 
   <|"Code" -> "isnumericQ[v] ^= True;\n{isnumericQ[v], isnumericQ[2 v], \
isnumericQ[v^2]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "False", ",", 
           "False"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnc[c[AN, 1, UP], U c[CR, 1, UP]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
           RGBColor[0, 1, 0]], RowBox[{"U", " ", 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[2]="}}|>}|>

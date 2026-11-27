<|"Source" -> "snegxoperators.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\n{X[1, \"0\", \
\"UP\"], nc[X[1, \"0\", \"UP\"], X[1, \"UP\", \"2\"]], nc[X[1, \"0\", \
\"UP\"], X[1, \"DO\", \"2\"]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"0", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", "0"}], 
         "}"}], "Out[2]="}}|>, 
   <|"Code" -> "komutator[X[1, \"UP\", \"DO\"], X[1, \"DO\", \"UP\"]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"DO", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], 
             RowBox[{"UP", "UP"}]}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\n{nc[X[2, \"UP\", \"0\"], \
X[1, \"0\", \"UP\"]], nc[X[2, \"UP\", \"DO\"], X[1, \"0\", \"UP\"]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                   TemplateBox[{"X", RowBox[{"1"}], RowBox[{"0", "UP"}]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                   TemplateBox[{"X", RowBox[{"2"}], RowBox[{"UP", "0"}]}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm]}], ")"}]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], 
                 RowBox[{"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                  {"UP", "DO"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "antikomutator[X[i, \"0\", \"UP\"], X[j, \"UP\", \"0\"]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"i", ",", "j"}]}, 
          "KroneckerDeltaSeq"], " ", RowBox[
          {"(", RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"j"}], 
                 RowBox[{"0", "0"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"j"}], RowBox[
                  {"UP", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegxoperators[Y, {1, 2, 3}, {0, 0, \
0}];\n{komutator[Y[1, 1, 2], Y[1, 2, 3]], komutator[Y[1, 1, 2], Y[1, 2, 1]]}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], RowBox[
                {"1", "3"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], 
                 RowBox[{"1", "1"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "-", 
             TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], RowBox[
                  {"2", "2"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegxoperators[X, \
\"HubbardAtom\"];\nsnegfermionoperators[c];\nxoperator[c[1], \"0\", \"UP\"]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "-", RowBox[{TagBox[StyleBox[TemplateBox[
              {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegxoperators[X, \
\"HubbardAtom\"];\nsnegfermionoperators[c];\nsnegrealconstants[t, J];\nsz[i_] \
:= (X[i, \"UP\", \"UP\"] - X[i, \"DO\", \"DO\"])/2;\nnn[i_] := X[i, \"UP\", \
\"UP\"] + X[i, \"DO\", \"DO\"];\nHtJ = -t Sum[nc[X[1, s, \"0\"], X[2, \"0\", \
s]] + nc[X[2, s, \"0\"], X[1, \"0\", s]], {s, {\"UP\", \"DO\"}}] + J \
(nc[sz[1], sz[2]] + (nc[X[1, \"UP\", \"DO\"], X[2, \"DO\", \"UP\"]] + nc[X[1, \
\"DO\", \"UP\"], X[2, \"UP\", \"DO\"]])/2 - nc[nn[1], nn[2]]/4);", 
    "Reset" -> True, "InLabel" -> "In[1]:=", "Cells" -> {}|>, 
   <|"Code" -> "Hf = xtofermions[HtJ, X -> c];\nsinglet = (nc[c[CR, 1, UP], \
c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]])/Sqrt[2];\ntriplet = nc[c[CR, \
1, UP], c[CR, 2, UP]];\nSimplify[{vev[nc[conj[singlet], Hf, singlet]], \
vev[nc[conj[triplet], Hf, triplet]]}]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"-", "J"}], ",", "0"}], "}"}], 
       "Out[10]="}}|>, <|"Code" -> "snegxoperators[X, \
\"HubbardAtom\"];\n{operatorQ[X], xoperatorQ[X], xoperatorQ[X[1, \"0\", \
\"UP\"]], MemberQ[listxoperators, X]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", "True", ",", 
           "True"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nconj[nc[X[1, \"UP\", \
\"0\"], X[2, \"DO\", \"UP\"]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], 
             RowBox[{"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], 
         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
         TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], 
             RowBox[{"UP", "DO"}]}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegxoperators[Y, {1, 2, 3}, {0, 0, 0}];\nSum[nc[Y[1, a, b], \
Y[1, b, a]], {a, 3}, {b, 3}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"3", " ", TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], 
               RowBox[{"1", "1"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{"3", " ", TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], 
               RowBox[{"2", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{"3", " ", TagBox[StyleBox[TemplateBox[{"Y", RowBox[{"1"}], 
               RowBox[{"3", "3"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[2]="}}|>, <|"Code" -> "snegxoperators[Z, {1, 2}, {0}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "snegxoperators::spec: The states {1, 2} and the \
parities {0} (0 or 1) must be lists of equal length with distinct states."}, 
      {"Output", "$Failed", "Out[1]="}}|>}|>

<|"Source" -> "xtofermions.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\nxtofermions[X[1, \"UP\", \"0\"] + X[1, \"2\", \"2\"], X -> \
c]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                 TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X\
, \"HubbardAtom\"];\nsnegrealconstants[t, J];\nsdot[i_, j_] := nc[X[i, \
\"UP\", \"UP\"] - X[i, \"DO\", \"DO\"], X[j, \"UP\", \"UP\"] - X[j, \"DO\", \
\"DO\"]]/4 + (nc[X[i, \"UP\", \"DO\"], X[j, \"DO\", \"UP\"]] + nc[X[i, \
\"DO\", \"UP\"], X[j, \"UP\", \"DO\"]])/2;\nnn[i_] := X[i, \"UP\", \"UP\"] + \
X[i, \"DO\", \"DO\"];\nhop2 = -t Sum[nc[X[1, s, \"0\"], X[2, \"0\", s]] + \
nc[X[2, s, \"0\"], X[1, \"0\", s]], {s, {\"UP\", \"DO\"}}];\nH = \
xtofermions[hop2 + J (sdot[1, 2] - nc[nn[1], nn[2]]/4), X -> c];\n{First[#], \
Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", "t"}], ",", "t"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", "t"}], ",", "t"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", "1"}]}], "}"}], ",", RowBox[{"{", "0", 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", "J"}], ",", "0", ",", "0", ",", 
                   "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}]}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\nst = {\"0\", \"UP\", \"DO\", \
\"2\"};\nUnion[Flatten[Table[Expand[xtofermions[nc[X[1, a, b], X[1, b2, d]], \
X -> c] - nc[xoperator[c[1], a, b], xoperator[c[1], b2, d]]], {a, st}, {b, \
st}, {b2, st}, {d, st}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", "0", "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\nxtofermions[fermionstox[hop[c[1], c[2]], c -> X], X -> c] \
== Expand[hop[c[1], c[2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[Y, {1, 2, 3}, {0, 0, \
0}];\nxtofermions[Y[1, 1, 2], Y -> c]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "xtofermions::states: Y \
must be declared with the states of the Hubbard atom, snegxoperators[Y, \
\"HubbardAtom\"]."}, {"Output", "$Failed", "Out[3]="}}|>}|>

<|"Source" -> "fermionstox.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\n{fermionstox[c[CR, 1, UP], c -> X], fermionstox[c[AN, 1, \
DO], c -> X]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"X", 
                 RowBox[{"1"}], RowBox[{"2", "DO"}]}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                  {"UP", "0"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], 
                 RowBox[{"0", "DO"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "-", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                  {"UP", "2"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X\
, \"HubbardAtom\"];\nfermionstox[{number[c[1]], hubbard[c[1]], spinz[c[1]], \
spinplus[c[1]]}, c -> X]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"2", " ", TagBox[StyleBox[TemplateBox[
                  {"X", RowBox[{"1"}], RowBox[{"2", "2"}]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                  {"DO", "DO"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "+", 
             TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                  {"UP", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"2", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{RowBox[{"-", FractionBox[TagBox[StyleBox[TemplateBox[
                   {"X", RowBox[{"1"}], RowBox[{"DO", "DO"}]}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm], "2"]}], "+", FractionBox[
              TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                   {"UP", "UP"}]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], "2"]}], ",", 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"UP", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X\
, \"HubbardAtom\"];\nfermionstox[hop[c[1], c[2]], c -> X]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[
                {"1"}], RowBox[{"0", "DO"}]}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"2", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"0", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"DO", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"2", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"UP", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"2", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"2", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"DO", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"2", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"0", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"2", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"UP", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"DO", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"0", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"DO", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"UP", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"DO", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"2", "DO"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"DO", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"UP", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"UP", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"0", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"UP", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"DO", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "-", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"UP", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"2", "UP"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "+", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"X", RowBox[{"1"}], RowBox[
                {"UP", "2"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"X", RowBox[{"2"}], RowBox[
                {"DO", "0"}]}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\nsnegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nheff = schriefferwolff[U (hubbard[c[1]] + hubbard[c[2]]), -t \
hop[c[1], c[2]], nc[projector1[c[1]], projector1[c[2]]], \
2];\nExpand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", FractionBox[SuperscriptBox["t", "2"], "U"]}], "+", 
         FractionBox[RowBox[{"4", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"z\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"-\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"], "+", 
         FractionBox[RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                  TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"S", RowBox[{"2"}], TagBox[StyleBox["\"+\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "U"]}], 
       "Out[6]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X\
, \"HubbardAtom\"];\nh = spinspin[c[1], c[2]] + hop[c[1], \
c[2]];\nExpand[xtofermions[fermionstox[h, c -> X], X -> c] - h]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nsnegxoperators[X, \
\"HubbardAtom\"];\nfermionstox[b[CR], b -> X]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "fermionstox::spin: b must be a spin-1/2 fermion operator."}, 
      {"Output", "$Failed", "Out[3]="}}|>}|>

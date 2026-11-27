<|"Source" -> "greensfunction.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[e, \
U];\nh = e number[d[]] + U hubbard[d[]];\ng = greensfunction[d[AN, UP], d[CR, \
UP], h, z];\nApart[g /. avg[number[d[], DO]] -> w, z]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{FractionBox[RowBox[{"1", "-", "w"}], 
          RowBox[{RowBox[{"-", "e"}], "+", "z"}]], "+", 
         FractionBox["w", RowBox[{RowBox[{"-", "e"}], "-", "U", "+", 
            "z"}]]}], "Out[5]="}}|>, <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegrealconstants[B];\ngreensfunction[S[1, 4], S[1, 5], B S[1, 3], \
z]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"2", " ", 
          TagBox[RowBox[{"\[LeftAngleBracket]", TagBox[StyleBox[TemplateBox[
                {"S", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             "\[RightAngleBracket]"}], DisplayForm]}], 
        RowBox[{"B", "+", "z"}]], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nh = -number[d[]]/2 + \
hubbard[d[]];\ng = greensfunction[d[AN, UP], d[CR, UP], h, z, \"Average\" -> \
(thermalaverage[#, h, {d}, 1/3] &)];\nAbs[(g - z/(z^2 - 1/4)) /. z -> 1/3 + \
I/2] < 10^-12", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[d, c];\nsnegrealconstants[ed, eb, v];\nh \
= ed number[d[]] + eb number[c[]] + v hop[d[], c[]];\ng = \
greensfunction[d[AN, UP], d[CR, UP], h, z];\nFactor[z - ed - 1/g]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[SuperscriptBox["v", "2"], 
        RowBox[{RowBox[{"-", "eb"}], "+", "z"}]], "Out[5]="}}|>}|>

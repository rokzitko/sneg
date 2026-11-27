<|"Source" -> "mcross.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "mcross[{a1, a2, a3}, {b1, b2, b3}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"a2", StyleBox["\[CenterDot]", 
                FontColor -> RGBColor[0, 1, 0]], "b3"}], "-", 
             RowBox[{"a3", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                  1, 0]], "b2"}]}], ",", RowBox[
            {RowBox[{"-", RowBox[{"(", RowBox[{"a1", StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], "b3"}], ")"}]}], "+", 
             RowBox[{"a3", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                  1, 0]], "b1"}]}], ",", RowBox[
            {RowBox[{"a1", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                  1, 0]], "b2"}], "-", RowBox[{"a2", StyleBox["\[CenterDot]", 
                FontColor -> RGBColor[0, 1, 0]], "b1"}]}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "snegspinoperators[{S, 1/2}];\ns = {S[1], \
S[2], S[3]};\nExpand[mcross[s, s] - I s]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsc = \
spinxyz[c[]];\nExpand[mcross[sc, sc] - I sc]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], 
       "Out[6]="}}|>, 
   <|"Code" -> "mcross[{1, 2, 3}, {4, 5, 6}] == Cross[{1, 2, 3}, {4, 5, 6}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nmcross[{S[1], S[2], S[3]}, \
{S[1], S[2], S[3]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"\[ImaginaryI]", " ", 
             TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox["\"x\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ",", 
           RowBox[{"\[ImaginaryI]", " ", TagBox[StyleBox[TemplateBox[
                {"S", TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 
                     1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{"\[ImaginaryI]", " ", 
             TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox["\"z\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "}"}], 
       "Out[2]="}}|>}|>

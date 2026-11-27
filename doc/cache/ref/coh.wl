<|"Source" -> "coh.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "sneggrassmanconstants[z];\nsnegspinlessfermionoperators[g];\ncoh[z, \
g[]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[
           "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
           FontColor -> RGBColor[1, 0, 1]], 
          StyleForm[#1, FontColor -> RGBColor[1, 0, 1]] & ], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {"z", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             OverscriptBox["z", "_"], StyleBox["\[CenterDot]", 
              FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
               "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"\
", FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  1]] & ]}]}], "-", RowBox[{"z", StyleBox["\[CenterDot]", 
            FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
             TemplateBox[{"g", "\"\[Dagger]\""}, "Superscript", 
              SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ]}]}], "Out[3]="}}|>, 
   <|"Code" -> "Expand[nc[g[AN], coh[z, g[]]] - nc[z, coh[z, g[]]]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[z];\nsnegspinlessfermionoperators[g];\n\
conj[coh[z, g[]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[
           "\"\[LeftAngleBracket]\[Diameter]\[RightDoubleBracketingBar]\"", 
           FontColor -> RGBColor[1, 0, 1]], 
          StyleForm[#1, FontColor -> RGBColor[1, 0, 1]] & ], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {"z", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             OverscriptBox["z", "_"], StyleBox["\[CenterDot]", 
              FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
               "\"\[LeftAngleBracket]\[Diameter]\[RightDoubleBracketingBar]\"\
", FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  1]] & ]}]}], "+", RowBox[{OverscriptBox["z", "_"], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftAngleBracket]\[Diameter]\[RightDoubleBracketingBar]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ], StyleBox["\[CenterDot]", 
            FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
             TemplateBox[{"g", "\"\""}, "Superscript", SyntaxForm -> 
               SuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
       "Out[3]="}}|>, <|"Code" -> "sneggrassmanconstants[z, \
w];\nsnegspinlessfermionoperators[g];\n{Expand[nc[conj[coh[z, g[]]], coh[z, \
g[]]]], Expand[nc[conj[coh[w, g[]]], coh[z, g[]]]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", RowBox[{"1", "+", 
             FractionBox[RowBox[{"w", StyleBox["\[CenterDot]", FontColor -> 
                  RGBColor[0, 1, 0]], OverscriptBox["w", "_"]}], "2"], "-", 
             RowBox[{"z", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                  1, 0]], OverscriptBox["w", "_"]}], "+", FractionBox[
              RowBox[{"z", StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                   1, 0]], OverscriptBox["z", "_"]}], "2"], "-", 
             RowBox[{FractionBox["1", "4"], " ", RowBox[{"w", StyleBox[
                  "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], "z", 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 OverscriptBox["w", "_"], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], OverscriptBox["z", 
                  "_"]}]}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[z];\nsnegfermionoperators[c];\nExpand[n\
c[c[AN, UP], coh[z, c[UP]]] - nc[z, coh[z, c[UP]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[3]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a];\nsnegcomplexconstants[x];\nst = coh[x, a[]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{SuperscriptBox["\[ExponentialE]", 
          RowBox[{RowBox[{"-", FractionBox["1", "2"]}], " ", "x", " ", 
            RowBox[{"Conjugate", "[", "x", "]"}]}]], " ", 
         RowBox[{SuperscriptBox["\[ExponentialE]", 
            RowBox[{"x", " ", TagBox[StyleBox[TemplateBox[{"a", 
                  "\"\[Dagger]\""}, "Superscript", SyntaxForm -> 
                  SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[
             "\"\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]\"", 
             FontColor -> RGBColor[1, 0, 1]], StyleForm[#1, FontColor -> 
               RGBColor[1, 0, 1]] & ]}]}], "Out[3]="}}|>, 
   <|"Code" -> "Simplify[nc[a[AN], st] - x st]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[z];\nsnegrealconstants[\[Alpha]];\nnc[i\
nt[conj[z], z], 1 - \[Alpha] nc[conj[z], z]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "\[Alpha]", "Out[3]="}}|>, 
   <|"Code" -> "sneggrassmanconstants[z];\nsnegspinlessfermionoperators[g];\n\
{antikomutator[z, conj[z]], antikomutator[z, g[CR]], nc[z, z]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], 
         "}"}], "Out[3]="}}|>}|>

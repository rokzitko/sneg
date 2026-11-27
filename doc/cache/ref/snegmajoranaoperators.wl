<|"Source" -> "snegmajoranaoperators.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegmajoranaoperators[g];\n{nc[g[1], g[1]], \
nc[g[2], g[1]], antikomutator[g[1], g[2]], conj[g[1]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{RowBox[{"g", "[", "1", "]"}], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 RowBox[{"g", "[", "2", "]"}]}], ")"}]}], ",", "0", ",", 
           RowBox[{"g", "[", "1", "]"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "snegmajoranaoperators[g, h];\n{nc[g, g], nc[h, g], nc[h[1], g[1]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{"g", StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], "h"}], ")"}]}], ",", 
           RowBox[{"-", RowBox[{"(", RowBox[{RowBox[{"g", "[", "1", "]"}], 
                 StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                 RowBox[{"h", "[", "1", "]"}]}], ")"}]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegmajoranaoperators[g];\nsnegfermionoper\
ators[c];\nnc[c[CR, 1, UP], g[1]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"-", RowBox[{"(", RowBox[{RowBox[{"g", "[", "1", "]"}], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}], ")"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegmajoranaoperators[g];\ncc = (g[1] + I \
g[2])/Sqrt[2];\ncd = conj[cc];\n{Expand[antikomutator[cc, cd]], Expand[nc[cc, \
cc]], Expand[nc[cd, cc]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", 
           RowBox[{FractionBox["1", "2"], "+", RowBox[{"\[ImaginaryI]", " ", 
               RowBox[{RowBox[{"g", "[", "1", "]"}], StyleBox["\[CenterDot]", 
                  FontColor -> RGBColor[0, 1, 0]], RowBox[{"g", "[", "2", 
                   "]"}]}]}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegmajoranaoperators[g];\npar = -2 I nc[g[1], \
g[2]];\n{Expand[pow[par, 2]], Expand[antikomutator[par, g[1]]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegmajoranaoperators[g];\n{operatorQ[g], \
majoranaQ[g], majoranaQ[g[1]], fermionQ[g]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", "True", ",", 
           RowBox[{"fermionQ", "[", "g", "]"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegmajoranaoperators[g];\npow[g[1], 2]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", FractionBox["1", "2"], 
       "Out[2]="}}|>}|>

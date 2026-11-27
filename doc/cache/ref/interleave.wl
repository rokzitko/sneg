<|"Source" -> "interleave.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "interleave[{a1, a2, a3}, {b1, b2, b3}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"a1", ",", "b1", ",", "a2", 
           ",", "b2", ",", "a3", ",", "b3"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "interleave[{a1, a2}, {b1, b2}, {c1, c2}]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"a1", ",", "b1", ",", "c1", ",", "a2", 
           ",", "b2", ",", "c2"}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], c[2], c[3], \
c[4]}];\nneel = vc @@ interleave[{1, 0, 1, 0}, {0, 1, 0, 1}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "expvvc[spinz /@ {c[1], c[2], c[3], c[4]}, neel]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
           RowBox[{"-", FractionBox["1", "2"]}], ",", FractionBox["1", "2"], 
           ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "interleave[{a1, a2, a3}, {b1, b2, b3}] === Riffle[{a1, a2, \
a3}, {b1, b2, b3}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

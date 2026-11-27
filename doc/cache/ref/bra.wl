<|"Source" -> "bra.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "{nc[bra[1], ket[1]], nc[bra[1], ket[2]], nc[bra[a], ket[b]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "0", ",", 
           TemplateBox[{RowBox[{"a", ",", "b"}]}, "KroneckerDeltaSeq"]}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "conj[ket[1/2]]", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftAngleBracket]\"", FontColor -> 
             RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
              RGBColor[1, 0.5, 0]] & ], RowBox[{FractionBox["1", "2"]}], 
          TagBox[StyleBox["\"\[RightBracketingBar]\"", FontColor -> 
             RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
              RGBColor[1, 0.5, 0]] & ]}], DisplayForm], "Out[2]="}}|>, 
   <|"Code" -> "nc[bra[x, Null], bra[Null, y]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox["\"\[LeftAngleBracket]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
          RowBox[{"x", "y"}], TagBox[StyleBox["\"\[RightBracketingBar]\"", 
            FontColor -> RGBColor[1, 0.5, 0]], 
           StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
        DisplayForm], "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nm\
akebasis[{c[]}];\nap[nc[c[CR, UP], bra[1/2]], vc[0, 0, ket[1/2]]]", 
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
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "nc[bra[1], nc[ket[0], bra[1]] + nc[ket[1], bra[1]], ket[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1", "Out[1]="}}|>}|>

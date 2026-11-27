<|"Source" -> "vccolor.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[c];\nmakebasis[{c[1], c[2]}];\nv = vc[1, 0, 1, 1]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "vccolor = vccolorSimple;\nv", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox[
            "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
              0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          "\[FilledRectangle]", "\[EmptyRectangle]", "\[FilledRectangle]", 
          "\[FilledRectangle]", TagBox[StyleBox["\"\[RightAngleBracket]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], DisplayForm], 
       "Out[5]="}}|>, <|"Code" -> "vccolor = vccolorDefault;\nv", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[7]="}}|>, 
   <|"Code" -> 
     "vccolor = Function[l, Riffle[ToString /@ l, \",\"]];\nvc[1, 0, 1, 1]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "1", ",", "0", 
          ",", "1", ",", "1", TagBox[StyleBox["\"\[RightAngleBracket]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ]}], DisplayForm], 
       "Out[2]="}}|>, <|"Code" -> "vccolor = vccolorDefault;", 
    "Reset" -> False, "InLabel" -> "In[3]:=", "Cells" -> {}|>}|>

<|"Source" -> "spinket.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "spinket[1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"1"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"0"}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{RowBox[{"-", "1"}]}], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0.5, 0]] & ]}], DisplayForm]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "spinket[1/2, {0, 1}]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"\[SmallCircle]", 
                FractionBox["1", "2"]}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], ",", TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ], 
              RowBox[{"\[SmallCircle]", RowBox[{"-", FractionBox["1", 
                   "2"]}]}], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ]}], DisplayForm]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "conj[spinket[3/2]] == spinbra[3/2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>, 
   <|"Code" -> "Outer[nc, spinbra[1], spinket[1]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"1", ",", "0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "1", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0", ",", "1"}], "}"}]}], "}"}], 
       "Out[1]="}}|>}|>

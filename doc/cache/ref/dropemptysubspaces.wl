<|"Source" -> "dropemptysubspaces.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "dropemptysubspaces[{{{0}, {}}, {{1}, {a, b}}, {{2}, {}}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", 
             RowBox[{"{", RowBox[{"a", ",", "b"}], "}"}]}], "}"}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[d];\nbz = \
qszbasisvc[{d[]}];\nbz2 = Map[{First[#], DeleteCases[Last[#], vc[1, 1]]} &, \
bz]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[
                  RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                      FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "dropemptysubspaces[bz2]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", TagBox[RowBox[
                   {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                      FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[
                  RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                      FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[4]="}}|>}|>

<|"Source" -> "spindownvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nv = ops2vc[nc[c[CR, 1, UP], c[CR, 2, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", TagBox[RowBox[{TagBox[StyleBox[
            "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
              0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
             RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
               0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
            FontColor -> RGBColor[0, 0, 1]], 
           StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
          TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
             RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
               0]] & ]}], DisplayForm], "Out[3]="}}|>, 
   <|"Code" -> "NestList[spindownvc, v, 3]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm], ",", FractionBox[
            RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                   "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 
                     1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                   "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
                 TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm], "+", TagBox[RowBox[
                {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                   FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                   "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                   "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
                 TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                    RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[
                      0, 1, 0]] & ]}], DisplayForm]}], SqrtBox["2"]], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm], ",", "0"}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\nvc2ops[spindownvc[vc[1, 0, 1, 0]]] == spindown[nc[c[CR, 1, UP], \
c[CR, 2, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>}|>

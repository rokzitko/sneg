<|"Source" -> "transformfunc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nstates = \
keyselect[qszbasisvc[{c[1], c[2]}], {0, 0}][[1, 2]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> 
     "transformfunc[states, nc[projector1[c[1]], projector1[c[2]]]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 1, 
                  0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                   0, 0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[RightAngleBracket]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ]}], DisplayForm], ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
               StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
              TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                 RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 
                   0, 1]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
               StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
              TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ]}], DisplayForm]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmakebasis[{c[1], \
c[2]}];\ntransformfunc[{vc[1, 0, 0, 0], vc[0, 0, 1, 0]}, 1 + nc[c[CR, 2, UP], \
c[AN, 1, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{FractionBox[TagBox[RowBox[
                {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                   FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                   "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm], SqrtBox["2"]], "+", 
             FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                   "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 
                     1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                   "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm], SqrtBox["2"]]}], ",", 
           RowBox[{FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                   "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 
                     1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                   "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
                 TagBox[StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm], SqrtBox["2"]], "-", 
             FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                   "\"\[LeftDoubleBracketingBar]\"", FontColor -> RGBColor[0, 
                     1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                   "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ], 
                 TagBox[StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                    RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                      1, 0, 0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                   FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                    FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                   "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                     0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                      0]] & ]}], DisplayForm], SqrtBox["2"]]}]}], "}"}], 
       "Out[3]="}}|>}|>

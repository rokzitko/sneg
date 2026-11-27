<|"Source" -> "spinbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "spinbasis[1]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ], RowBox[{"1"}], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ], RowBox[{"0"}], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ], RowBox[{RowBox[{"-", "1"}]}], TagBox[
                     StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                       RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[1, 0.5, 0]] & ]}], DisplayForm], "}"}]}], 
             "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "spinbasis[{1/2, 1/2}]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], "}"}], ",", 
               RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ], RowBox[{RowBox[{"-", FractionBox["1", 
                         "2"]}], RowBox[{"-", FractionBox["1", "2"]}]}], 
                    TagBox[StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                       RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[1, 0.5, 0]] & ]}], DisplayForm], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                        "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                          0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                          0.5, 0]] & ], RowBox[{FractionBox["1", "2"], 
                        RowBox[{"-", FractionBox["1", "2"]}]}], TagBox[
                       StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                         RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0.5, 0]] & ]}], DisplayForm], ",", 
                   TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                        FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, 
                         FontColor -> RGBColor[1, 0.5, 0]] & ], RowBox[
                       {RowBox[{"-", FractionBox["1", "2"]}], FractionBox[
                         "1", "2"]}], TagBox[StyleBox[
                        "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 
                          0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                          0.5, 0]] & ]}], DisplayForm]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ], RowBox[{FractionBox["1", "2"], FractionBox[
                       "1", "2"]}], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 
                        0.5, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "Length /@ Last /@ spinbasis[{1/2, 1, 1/2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", "3", ",", "4", ",", "3", ",", 
           "1"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}, {T, 1}];\nh = \
Expand[spinspin[S[1], T[1]] /. spinketbrarules[{S[1], T[1]}]];\n{First[#], \
Eigenvalues[Last[#]]} & /@ makematricesbzop[h, spinbasis[{1/2, 1}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", 
                   FractionBox["3", "2"]}], "}"}], ",", RowBox[
                {"{", FractionBox["1", "2"], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", FractionBox["1", 
                    "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", FractionBox["1", "2"]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", 
                  "2"], "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", FractionBox["1", "2"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["3", "2"], "}"}], 
               ",", RowBox[{"{", FractionBox["1", "2"], "}"}]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, 
   <|"Code" -> "Total[Length /@ Last /@ spinbasis[{1/2, 1, 3/2}]] == 2 3 4", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

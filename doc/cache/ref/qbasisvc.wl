<|"Source" -> "qbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nqbasisvc[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
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
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                        "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                        "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                       StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ]}], DisplayForm], ",", 
                   TagBox[RowBox[{TagBox[StyleBox[
                        "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                        "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                       StyleBox["\"\[RightAngleBracket]\"", FontColor -> 
                         RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ]}], DisplayForm]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", 
               RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "InputForm[%]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> 
     {{"Output", InterpretationBox[StyleBox[
         "{{{-1}, {vc[0, 0]}}, {{0}, {vc[0, 1], vc[1, 0]}}, {{1}, {vc[1, \
1]}}}", ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1}, {Sneg`vc[0, 0]}}, {{0}, {Sneg`vc[0, 1], 
            Sneg`vc[1, 0]}}, {{1}, {Sneg`vc[1, 1]}}}], Editable -> True, 
        AutoDelete -> True], "Out[3]//InputForm="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[a];\n{First[#], Length[Last[#]]} \
& /@ qbasisvc[{a[1], a[2], a[3]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", 
                   FractionBox["3", "2"]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", FractionBox["1", 
                    "2"]}], "}"}], ",", "3"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", FractionBox["1", "2"], "}"}], 
               ",", "3"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", FractionBox["3", "2"], "}"}], ",", 
               "1"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[U, \
B];\n{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[U hubbard[d[]] - \
B spinx[d[]], qbasisvc[{d[]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"-", "1"}], 
                 "}"}], ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", FractionBox["B", "2"]}], ",", 
                   FractionBox["B", "2"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", "U", "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{Length /@ Last /@ qbasisvc[{c[1], \
c[2]}], Table[Binomial[4, n], {n, 0, 4}]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"1", ",", "4", ",", "6", ",", "4", ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "4", ",", 
               "6", ",", "4", ",", "1"}], "}"}]}], "}"}], "Out[2]="}}|>}|>

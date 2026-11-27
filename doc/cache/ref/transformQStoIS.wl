<|"Source" -> "transformQStoIS.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\ntransformQStoIS[qsbasis\
vc[{c[1]}], isospinminus[c[1]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {FractionBox["1", "2"], ",", "0"}], "}"}], ",", RowBox[
                {"{", InterpretationBox[RowBox[{"\"-\"", TagBox[RowBox[
                      {TagBox[StyleBox["\"\[LeftDoubleBracketingBar]\"", 
                         FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 1, 0]] & ], TagBox[
                        StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                         "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                         "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm]}], -Sneg`vc[1, 1]], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", FractionBox["1", "2"]}], "}"}], 
               ",", RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nTminus = \
isospinminus[c[1], 0] + isospinminus[c[2], \
1];\ntransformQStoIS[qsbasisvc[{c[1], c[2]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", TagBox[RowBox[{TagBox[StyleBox[
                      "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                       RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 0, 
                        0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ], TagBox[StyleBox[
                      "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 1, 
                        0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ]}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{InterpretationBox[RowBox[{"\"-\"", TagBox[
                       RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], -Sneg`vc[1, 0, 1, 
                      1]], ",", InterpretationBox[RowBox[{"\"-\"", TagBox[
                       RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm]}], -Sneg`vc[1, 1, 1, 
                      0]]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", InterpretationBox[RowBox[{"\"-\"", 
                    TagBox[RowBox[{TagBox[StyleBox[
                         "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                         "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                        StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                         "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                         "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm]}], -Sneg`vc[1, 0, 1, 0]], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{RowBox[{"-", FractionBox[TagBox[
                         RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm], SqrtBox["2"]]}], "-", 
                     FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm], SqrtBox["2"]]}], ",", 
                   RowBox[{FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm], SqrtBox["2"]], "-", 
                     FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                          "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                          StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                          "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                          "\"\[RightAngleBracket]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ]}], DisplayForm], SqrtBox["2"]]}]}], 
                 "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "{First[#], Length[Last[#]]} & /@ \
transformQStoIS[qszbasisvc[{c[1], c[2]}], Tminus]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", "1"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", 
               "2"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"0", ",", "1"}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", "3"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[{"-", 
                     "1"}]}], "}"}], ",", "1"}], "}"}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t, U];\ntm = isospinminus[c[1], 0] + isospinminus[c[2], 1];\nH = -t \
hop[c[1], c[2]] + U/2 Total[isozsq[{c[1], c[2]}]];\nExpand[komutator[H, tm]]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[5]="}}|>, 
   <|"Code" -> "bz = transformQStoIS[qsbasisvc[{c[1], c[2]}], \
tm];\n{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bz]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "U", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", RowBox[
                      {"(", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t"}], 
                         "+", "U"}], ")"}]}], ",", RowBox[{FractionBox["1", 
                      "2"], " ", RowBox[{"(", RowBox[{RowBox[{"2", " ", 
                          "t"}], "+", "U"}], ")"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{FractionBox["1", "2"], " ", 
                     RowBox[{"(", RowBox[{"U", "-", SqrtBox[RowBox[
                          {RowBox[{"16", " ", SuperscriptBox["t", "2"]}], 
                          "+", SuperscriptBox["U", "2"]}]]}], ")"}]}], ",", 
                   RowBox[{FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                        {"U", "+", SqrtBox[RowBox[{RowBox[{"16", " ", 
                          SuperscriptBox["t", "2"]}], "+", SuperscriptBox[
                          "U", "2"]}]]}], ")"}]}]}], "}"}]}], "}"}]}], "}"}], 
       "Out[7]="}}|>, 
   <|"Code" -> 
     "Total[(2 #[[1, 1]] + 1) (2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", "16", "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\ntm = isospinminus[c[1], 0] + \
isospinminus[c[2], 1];\nbz = transformQStoIS[qsbasisvc[{c[1], c[2]}], \
tm];\nii = Expand[inner[isospinxyz[c[1], 0] + isospinxyz[c[2], 1], \
isospinxyz[c[1], 0] + isospinxyz[c[2], 1]]];\n{First[First[#]], \
Union[Diagonal[Last[#]]]} & /@ makematricesbzvc[ii, bz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"1", ",", RowBox[{"{", "2", "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{FractionBox["1", "2"], ",", RowBox[
                {"{", FractionBox["3", "4"], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", RowBox[{"{", "0", "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", RowBox[{"{", "0", "}"}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nClear[Tminus];\ntransformQStoIS[qsb\
asisvc[{c[1]}]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "transformQStoIS::Tminus: Operator Tminus is not defined."}, 
      {"Output", "$Failed", "Out[3]="}}|>}|>

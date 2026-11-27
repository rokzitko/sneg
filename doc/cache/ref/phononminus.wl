<|"Source" -> "phononminus.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "nc[phononminus[2],#] & /@ phononbasis[2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[LeftBracketingBar]\"", 
                FontColor -> RGBColor[1, 0.5, 0]], StyleForm[#1, FontColor -> 
                  RGBColor[1, 0.5, 0]] & ], RowBox[{"0"}], TagBox[StyleBox[
                "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                  0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 0]] & ]}], 
            DisplayForm], ",", RowBox[{SqrtBox["2"], " ", 
             RowBox[{"(", TagBox[RowBox[{TagBox[StyleBox[
                    "\"\[LeftBracketingBar]\"", FontColor -> RGBColor[1, 0.5, 
                      0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                       0]] & ], RowBox[{"1"}], TagBox[StyleBox[
                    "\"\[RightAngleBracket]\"", FontColor -> RGBColor[1, 0.5, 
                      0]], StyleForm[#1, FontColor -> RGBColor[1, 0.5, 
                       0]] & ]}], DisplayForm], ")"}]}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "p = phononplus[2]; m = \
phononminus[2];\nTable[nc[bra[n],komutator[m,p],ket[n]], {n,0,2}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "1", ",", 
           RowBox[{"-", "2"}]}], "}"}], "Out[2]="}}|>}|>

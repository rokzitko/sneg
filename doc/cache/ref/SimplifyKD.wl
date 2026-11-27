<|"Source" -> "SimplifyKD.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "{SimplifyKD[UnitStep[k] + UnitStep[-k]], \
SimplifyKD[1 - UnitStep[-k]], SimplifyKD[KroneckerDelta[a, b]^2]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", "k", ")"}], DisplayForm], ",", 
           TemplateBox[{RowBox[{"a", ",", "b"}]}, "KroneckerDeltaSeq"]}], 
         "}"}], "Out[1]="}}|>, 
   <|"Code" -> "SimplifyKD[KroneckerDelta[k, l] UnitStep[k] UnitStep[-l]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[1]="}}|>, 
   <|"Code" -> "Simplify[KroneckerDelta[k, l] UnitStep[k] UnitStep[-l]]", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", RowBox[{TemplateBox[{RowBox[{"k", ",", "l"}]}, 
          "KroneckerDeltaSeq"], " ", TagBox[RowBox[
           {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
             StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", "k", 
            ")"}], DisplayForm], " ", TagBox[
          RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
            RowBox[{"-", "l"}], ")"}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[cs];\nordering[cs] = \
SEA;\nsnegfreeindexes[k, l];\nvev[nc[cs[AN, k, UP], cs[CR, k, UP]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "-", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k"}], ")"}], DisplayForm]}], "Out[4]="}}|>, 
   <|"Code" -> "SimplifyKD[%]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
            FontColor -> RGBColor[0, 1, 0]], 
           StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", "k", 
          ")"}], DisplayForm], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[cs];\nordering[cs] = \
SEA;\nsnegfreeindexes[k, l];\nv = vevwick[nc[cs[CR, k, UP], cs[AN, k, UP], \
cs[CR, l, UP], cs[AN, l, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{SuperscriptBox[TemplateBox[{RowBox[{"k", ",", "l"}]}, 
             "KroneckerDeltaSeq"], "2"], " ", TagBox[
            RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 
                  1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
              "(", RowBox[{"-", "k"}], ")"}], DisplayForm]}], "+", 
         RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "l"}], ")"}], 
            DisplayForm]}], "-", RowBox[{SuperscriptBox[TemplateBox[
             {RowBox[{"k", ",", "l"}]}, "KroneckerDeltaSeq"], "2"], " ", 
           TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], DisplayForm], 
           " ", TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                 RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                   1, 0]] & ], "(", RowBox[{"-", "l"}], ")"}], 
            DisplayForm]}]}], "Out[4]="}}|>, <|"Code" -> "SimplifyKD[v]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k"}], ")"}], DisplayForm], " ", 
         RowBox[{"(", RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "l"}], ")"}], 
              DisplayForm], "+", RowBox[{TemplateBox[{RowBox[{"k", ",", 
                   "l"}]}, "KroneckerDeltaSeq"], " ", TagBox[RowBox[
                 {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                      0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
                  "(", "l", ")"}], DisplayForm]}]}], ")"}]}], "Out[5]="}}|>, 
   <|"Code" -> "SimplifyKD[{KroneckerDelta[a, b]^-1, UnitStep[k]^-1}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{FractionBox["1", TemplateBox[{RowBox[{"a", ",", "b"}]}, 
             "KroneckerDeltaSeq"]], ",", FractionBox["1", 
            TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                  RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                    1, 0]] & ], "(", "k", ")"}], DisplayForm]]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "snegfermionoperators[cs];\nordering[cs] = \
SEA;\nsnegfreeindexes[k, l];\nv = vevwick[nc[cs[CR, k, UP], cs[AN, k, UP], \
cs[CR, l, UP], cs[AN, l, UP]]];\n{SimplifyKD[v], FullSimplifyKD[v]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
                  FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                    RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
              DisplayForm], " ", RowBox[{"(", RowBox[{TagBox[RowBox[
                   {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 
                        1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                         0]] & ], "(", RowBox[{"-", "l"}], ")"}], 
                  DisplayForm], "+", RowBox[{TemplateBox[{RowBox[{"k", ",", 
                       "l"}]}, "KroneckerDeltaSeq"], " ", TagBox[RowBox[
                     {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                          0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                          1, 0]] & ], "(", "l", ")"}], DisplayForm]}]}], 
               ")"}]}], ",", RowBox[{"UnitStep", "[", 
             RowBox[{RowBox[{"-", "k"}], ",", RowBox[{"-", "l"}]}], "]"}]}], 
         "}"}], "Out[5]="}}|>}|>

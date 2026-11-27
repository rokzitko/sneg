<|"Source" -> "FullSimplifyKD.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "FullSimplifyKD[KroneckerDelta[i, j]^4 + UnitStep[k] + UnitStep[-k]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"1", "+", TemplateBox[
          {RowBox[{"i", ",", "j"}]}, "KroneckerDeltaSeq"]}], "Out[1]="}}|>, 
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
            DisplayForm]}]}], "Out[4]="}}|>, <|"Code" -> "FullSimplifyKD[v]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"UnitStep", "[", 
         RowBox[{RowBox[{"-", "k"}], ",", RowBox[{"-", "l"}]}], "]"}], 
       "Out[5]="}}|>, <|"Code" -> "SimplifyKD[v]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", 
              FontColor -> RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                RGBColor[0, 1, 0]] & ], "(", RowBox[{"-", "k"}], ")"}], 
          DisplayForm], " ", RowBox[{"(", RowBox[
            {TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> 
                   RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 
                     1, 0]] & ], "(", RowBox[{"-", "l"}], ")"}], 
              DisplayForm], "+", RowBox[{TemplateBox[{RowBox[{"k", ",", 
                   "l"}]}, "KroneckerDeltaSeq"], " ", TagBox[RowBox[
                 {TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[0, 1, 
                      0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
                  "(", "l", ")"}], DisplayForm]}]}], ")"}]}], "Out[6]="}}|>}|>

<|"Source" -> "lab-exchange.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nordering[c] = \
SEA;\nsnegfreeindexes[k1, k2, k3, k4, s1, s2];\nSimplifyKD[vevwick[nc[c[CR, \
k1, s1], c[CR, k2, s2], c[AN, k3, s2], c[AN, k4, s1]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"(", RowBox[{RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k4"}]}, "KroneckerDeltaSeq"], " ", 
               TemplateBox[{RowBox[{"k2", ",", "k3"}]}, 
                "KroneckerDeltaSeq"]}], "-", RowBox[{TemplateBox[
                {RowBox[{"k1", ",", "k3"}]}, "KroneckerDeltaSeq"], " ", 
               TemplateBox[{RowBox[{"k2", ",", "k4"}]}, "KroneckerDeltaSeq"], 
               " ", TemplateBox[{RowBox[{"s1", ",", "s2"}]}, 
                "KroneckerDeltaSeq"]}]}], ")"}], " ", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k1"}], ")"}], DisplayForm], " ", 
         TagBox[RowBox[{TagBox[StyleBox["\"\[Theta]\"", FontColor -> RGBColor[
                0, 1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], 
            "(", RowBox[{"-", "k2"}], ")"}], DisplayForm]}], "Out[4]=", 
       <|"Kind" -> "Single", "Parts" -> {"\\theta (-\\text{k1}) \\theta \
(-\\text{k2}) (\\delta _{\\text{k1},\\text{k4}} \\delta \
_{\\text{k2},\\text{k3}}-\\delta _{\\text{k1},\\text{k3}} \\delta \
_{\\text{k2},\\text{k4}} \\delta _{\\text{s1},\\text{s2}})"}|>}}|>}|>

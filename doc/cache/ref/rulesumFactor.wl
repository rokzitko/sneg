<|"Source" -> "rulesumFactor.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, \
g];\nsum[3 f[i] g[j], {i, j}] /. rulesumFactor", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"3", " ", RowBox[{"sum", "[", 
           RowBox[{RowBox[{"f", "[", "i", "]"}], ",", 
             RowBox[{"{", "i", "}"}]}], "]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "j", "]"}], ",", 
             RowBox[{"{", "j", "}"}]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfreeindexes[k, l];\nsum[UnitStep[-k] UnitStep[-l], {k, \
l}] /. rulesumFactor", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "k"}], ")"}], DisplayForm], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], " ", 
         RowBox[{"sum", "[", RowBox[{TagBox[RowBox[{TagBox[StyleBox[
                  "\"\[Theta]\"", FontColor -> RGBColor[0, 1, 0]], 
                 StyleForm[#1, FontColor -> RGBColor[0, 1, 0]] & ], "(", 
                RowBox[{"-", "l"}], ")"}], DisplayForm], ",", 
             RowBox[{"{", "l", "}"}]}], "]"}]}], "Out[2]="}}|>, 
   <|"Code" -> "snegfreeindexes[i, j];\nsnegrealfunctions[f, g, h];\ns = \
sum[f[i] g[j] + f[i] h[j], {i, j}];\ns /. rulesumFactor", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"sum", "[", RowBox[
          {RowBox[{RowBox[{RowBox[{"f", "[", "i", "]"}], " ", RowBox[
                {"g", "[", "j", "]"}]}], "+", RowBox[{RowBox[{"f", "[", "i", 
                 "]"}], " ", RowBox[{"h", "[", "j", "]"}]}]}], ",", 
           RowBox[{"{", RowBox[{"i", ",", "j"}], "}"}]}], "]"}], 
       "Out[4]="}}|>, <|"Code" -> "sumExpand[s] /. rulesumFactor", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "i", "]"}], 
               ",", RowBox[{"{", "i", "}"}]}], "]"}], " ", 
           RowBox[{"sum", "[", RowBox[{RowBox[{"g", "[", "j", "]"}], ",", 
               RowBox[{"{", "j", "}"}]}], "]"}]}], "+", 
         RowBox[{RowBox[{"sum", "[", RowBox[{RowBox[{"f", "[", "i", "]"}], 
               ",", RowBox[{"{", "i", "}"}]}], "]"}], " ", 
           RowBox[{"sum", "[", RowBox[{RowBox[{"h", "[", "j", "]"}], ",", 
               RowBox[{"{", "j", "}"}]}], "]"}]}]}], "Out[5]="}}|>}|>

<|"Source" -> "lab-ref-greensfunction.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[e, \
U];\nh = e number[d[]] + U hubbard[d[]];\ng = greensfunction[d[AN, UP], d[CR, \
UP], h, z];\nApart[g /. avg[number[d[], DO]] -> w, z]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{FractionBox[RowBox[{"1", "-", "w"}], 
          RowBox[{RowBox[{"-", "e"}], "+", "z"}]], "+", 
         FractionBox["w", RowBox[{RowBox[{"-", "e"}], "-", "U", "+", 
            "z"}]]}], "Out[5]=", <|"Kind" -> "Sum", 
        "Parts" -> {"\\frac{1-w}{z-e}", "+\\frac{w}{-e-U+z}"}|>}}|>}|>

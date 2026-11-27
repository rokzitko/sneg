<|"Source" -> "lab-projectors.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "Expand[spinss[d[]] - 3/4 projector1[d[]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{FractionBox["3", "4"], " ", 
           RowBox[{RowBox[{"number", "[", RowBox[{RowBox[{"d", "[", "]"}], 
                 ",", "0"}], "]"}], StyleBox["\[CenterDot]", 
              FontColor -> RGBColor[0, 1, 0]], RowBox[{"number", "[", RowBox[
                {RowBox[{"d", "[", "]"}], ",", "1"}], "]"}]}]}], "+", 
         RowBox[{FractionBox["3", "4"], " ", RowBox[
            {RowBox[{"number", "[", RowBox[{RowBox[{"d", "[", "]"}], ",", 
                 "1"}], "]"}], StyleBox["\[CenterDot]", FontColor -> RGBColor[
                0, 1, 0]], RowBox[{"number", "[", RowBox[
                {RowBox[{"d", "[", "]"}], ",", "0"}], "]"}]}]}], "-", 
         RowBox[{FractionBox["3", "4"], " ", RowBox[{"number", "[", 
             RowBox[{RowBox[{"d", "[", "]"}], ",", "0"}], "]"}]}], "-", 
         RowBox[{FractionBox["3", "4"], " ", RowBox[{"number", "[", 
             RowBox[{RowBox[{"d", "[", "]"}], ",", "1"}], "]"}]}], "+", 
         RowBox[{"spinss", "[", RowBox[{"d", "[", "]"}], "]"}]}], "Out[1]=", 
       <|"Kind" -> "Sum", "Parts" -> 
         {"\\frac{3 \\text{number}(d(),0) \\text{number}(d(),1)}{4}", 
          "+\\frac{3 \\text{number}(d(),1) \\text{number}(d(),0)}{4}", 
          "-\\frac{3}{4} \\text{number}(d(),0)", 
          "-\\frac{3}{4} \\text{number}(d(),1)", 
          "+\\text{spinss}(d())"}|>}}|>}|>

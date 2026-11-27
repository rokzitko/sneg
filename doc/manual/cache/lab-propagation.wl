<|"Source" -> "lab-propagation.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
h = -t (hop[c[1], c[2]] + hop[c[2], c[3]]);\n{ops, m, a} = eomclosure[c[AN, \
1, UP], h];\n{ops, Factor[CharacteristicPolynomial[m, z]]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"3", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], "}"}], ",", RowBox[{"z", " ", 
             RowBox[{"(", RowBox[{RowBox[{"2", " ", SuperscriptBox["t", 
                    "2"]}], "-", SuperscriptBox["z", "2"]}], ")"}]}]}], 
         "}"}], "Out[5]=", <|"Kind" -> "List", "Parts" -> 
         {"\\{c_{1\\uparrow},c_{2\\uparrow},c_{3\\uparrow}\\}", 
          "z \\left(2 t^2-z^2\\right)"}|>}}|>}|>

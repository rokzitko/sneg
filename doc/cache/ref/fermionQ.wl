<|"Source" -> "fermionQ.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegspinlessfermionoper\
ators[f];\n{fermionQ[c], fermionQ[c[CR, 1, UP]], fermionQ[f[AN, 1, 0]]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", "True", ",", 
           "True"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegbosonoperators[a];\nCases[{c[CR\
, 1, UP], a[AN], c[AN, 2, DO]}, _?fermionQ]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{TagBox[StyleBox[TemplateBox[
              {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                   FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                    FontColor -> RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", TagBox[StyleBox[TemplateBox[{"c", RowBox[
                {"2", TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}], "\"\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegbosonoperators[a];\n{fermionQ[a], \
fermionQ[u], TrueQ[fermionQ[a]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"fermionQ", "[", "a", "]"}], ",", 
           RowBox[{"fermionQ", "[", "u", "]"}], ",", "False"}], "}"}], 
       "Out[2]="}}|>}|>

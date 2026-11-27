<|"Source" -> "levicivita.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "{levicivita[1, 2, 3], levicivita[2, 1, 3], \
levicivita[1, 1, 3], levicivita[1, 2, 4]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1", ",", RowBox[{"-", "1"}], ",", "0", 
           ",", "0"}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> 
     "{levicivita[a, b, c], levicivita[b, a, c], levicivita[a, b, a]}", 
    "Reset" -> False, "InLabel" -> "In[2]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"levicivita", "[", RowBox[{"a", ",", "b", ",", 
               "c"}], "]"}], ",", RowBox[{"-", RowBox[{"levicivita", "[", 
               RowBox[{"a", ",", "b", ",", "c"}], "]"}]}], ",", "0"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinoperators[S];\nsnegspincomponentin\
dexes[a, b];\nkomutator[S[1, a], S[1, b]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"-", "\[ImaginaryI]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"levicivita", "[", 
                 RowBox[{"a", ",", "a1", ",", "b"}], "]"}], " ", TagBox[
                StyleBox[TemplateBox[{"S", RowBox[{"1"}], "a1"}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], ",", RowBox[{"{", "a1", "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinoperators[S];\nsnegspincomponentin\
dexes[a, b];\nspincomponents[sum[levicivita[a, b, 3] nc[S[1, a], S[2, b]], \
{a, b}], \"XYZ\"]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "Table[levicivita[i, j, k], {i, 3}, {j, 3}, {k, 3}] == \
Normal[LeviCivitaTensor[3]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[1]="}}|>}|>

<|"Source" -> "snegspincomponentindexes.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[S];\nsnegspincomponentindexes[a\
, b];\nkomutator[S[1, a], S[1, b]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", "\[ImaginaryI]"}], " ", 
         RowBox[{"sum", "[", RowBox[{RowBox[{RowBox[{"levicivita", "[", 
                 RowBox[{"a", ",", "a1", ",", "b"}], "]"}], " ", TagBox[
                StyleBox[TemplateBox[{"S", RowBox[{"1"}], "a1"}, 
                  "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                 ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
                DisplayForm]}], ",", RowBox[{"{", "a1", "}"}]}], "]"}]}], 
       "Out[3]="}}|>, <|"Code" -> "komutator[S[1, a], S[2, b]]", 
    "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nsnegspincomponentindexes[a];\nspincomp\
onents[sum[nc[S[1, a], S[2, a]], {a}]] == spincomponents[spinspin[S[1], \
S[2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nsnegspincomponentindexes[a, \
b];\nspincomponents[komutator[S[1, a], S[1, b]] /. {a -> 1, b -> 2}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"\[ImaginaryI]", " ", 
         TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> 
     "snegspinoperators[S];\nsnegspincomponentindexes[a];\nsum[S[1, 3], {a}]"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"3", " ", 
         TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegspincomponentindexes[a];\n{spincomponentindexQ[a], \
conj[S[1, a]], MemberQ[listspincomponentindexes, a]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], "a"}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], ",", "True"}], "}"}], "Out[2]="}}|>}|>

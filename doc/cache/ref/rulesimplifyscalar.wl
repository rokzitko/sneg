<|"Source" -> "rulesimplifyscalar.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\n\
e = scalar[Sin[t]^2 + Cos[t]^2] c[CR, 1, UP]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"scalar", "[", RowBox[{SuperscriptBox[RowBox[{"Cos", "[", 
                "t", "]"}], "2"], "+", SuperscriptBox[RowBox[{"Sin", "[", 
                "t", "]"}], "2"]}], "]"}]}], "Out[3]="}}|>, 
   <|"Code" -> "e /. rulesimplifyscalar", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{TagBox[StyleBox[TemplateBox[
            {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                  RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                    0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
            SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"scalar", "[", "1", "]"}]}], "Out[4]="}}|>, 
   <|"Code" -> "e /. rulesimplifyscalar /. rulestripscalar", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", TagBox[StyleBox[TemplateBox[
          {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                  0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
          SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
         ScriptBaselineShifts -> {1, 1}], DisplayForm], "Out[5]="}}|>}|>

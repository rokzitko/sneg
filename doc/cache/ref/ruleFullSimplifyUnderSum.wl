<|"Source" -> "ruleFullSimplifyUnderSum.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsn\
egrealfunctions[eps];\ns = sum[Sin[eps[k]]^2 c[CR, k, UP] + Cos[eps[k]]^2 \
c[CR, k, UP], {k}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                   RowBox[{"k", TagBox[StyleBox["\"\[UpArrow]\"", 
                       FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                        FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
               SuperscriptBox[RowBox[{"Cos", "[", RowBox[{"eps", "[", "k", 
                    "]"}], "]"}], "2"]}], "+", RowBox[{TagBox[StyleBox[
                 TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                       "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                      StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                   "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                   SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                 ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
               SuperscriptBox[RowBox[{"Sin", "[", RowBox[{"eps", "[", "k", 
                    "]"}], "]"}], "2"]}]}], ",", RowBox[{"{", "k", "}"}]}], 
         "]"}], "Out[4]="}}|>, <|"Code" -> "s /. ruleFullSimplifyUnderSum", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], ",", 
           RowBox[{"{", "k", "}"}]}], "]"}], "Out[5]="}}|>}|>

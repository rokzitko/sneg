<|"Source" -> "snegrealfunctions.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[k];\nsn\
egrealfunctions[eps];\n{nc[c[AN, k, UP], eps[k] c[CR, k, UP]], \
Conjugate[eps[k]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"eps", "[", "k", "]"}], " ", 
             RowBox[{"(", RowBox[{"1", "-", RowBox[{TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                      SyntaxForm -> SubsuperscriptBox], 
                     ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                      {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                    FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                     TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                       SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                     ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
               ")"}]}], ",", RowBox[{"eps", "[", "k", "]"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegfreeindexes[\
k, p];\nsnegrealfunctions[eps];\nH = sum[eps[k] number[c[k]], \
{k}];\nsumSimplifyKD[sumExpand[komutator[H, c[CR, p, UP]]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"p", TagBox[StyleBox[
                 "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
             "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
         RowBox[{"eps", "[", "p", "]"}]}], "Out[5]="}}|>, 
   <|"Code" -> "snegrealfunctions[eps];\nsnegcomplexfunctions[V];\n{Conjugate\
[eps[k]], Conjugate[V[k]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"eps", "[", "k", "]"}], ",", 
           RowBox[{"Conjugate", "[", RowBox[{"V", "[", "k", "]"}], "]"}]}], 
         "}"}], "Out[3]="}}|>}|>

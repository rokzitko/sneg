<|"Source" -> "snegcomplexfunctions.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, \
d];\nsnegfreeindexes[k];\nsnegcomplexfunctions[V];\nconj[V[k] nc[c[CR, k, \
UP], d[AN, UP]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"Conjugate", "[", 
           RowBox[{"V", "[", "k", "]"}], "]"}], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"k", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, \
d];\nsnegfreeindexes[k];\nsnegcomplexfunctions[V];\nHhyb = sum[V[k] nc[c[CR, \
k, UP], d[AN, UP]] + V[k] nc[c[CR, k, DO], d[AN, DO]], {k}];\nHhyb = Hhyb + \
conj[Hhyb];\nExpand[conj[Hhyb] - Hhyb]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0", "Out[6]="}}|>, 
   <|"Code" -> "sumSimplifyKD[sumExpand[komutator[d[AN, UP], Hhyb]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"k", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"Conjugate", "[", RowBox[{"V", "[", "k", "]"}], "]"}]}], 
           ",", RowBox[{"{", "k", "}"}]}], "]"}], "Out[7]="}}|>}|>

<|"Source" -> "snegcomplexconstants.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z]\
;\nconj[z nc[c[CR, 1, UP], c[AN, 2, UP]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"Conjugate", "[", "z", "]"}], " ", 
         RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                 TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                     0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                      0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z];\nH = z \
nc[c[CR, 1, UP], c[AN, 2, UP]] + conj[z nc[c[CR, 1, UP], c[AN, 2, \
UP]]];\nExpand[conj[H] - H]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegcomplexconstants[z, w];\npsi = \
nc[z c[CR, 1, UP] + w c[CR, 2, UP], VACUUM];\nnc[conj[psi], psi]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"w", " ", RowBox[{"Conjugate", "[", "w", "]"}]}], "+", 
         RowBox[{"z", " ", RowBox[{"Conjugate", "[", "z", "]"}]}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegcomplexconstants[z];\nsnegrealconstant\
s[t];\n{Conjugate[2 I z t], conj[z t]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
             "\[ImaginaryI]", " ", "t", " ", RowBox[{"Conjugate", "[", "z", 
               "]"}]}], ",", RowBox[{"t", " ", RowBox[{"Conjugate", "[", "z", 
               "]"}]}]}], "}"}], "Out[3]="}}|>}|>

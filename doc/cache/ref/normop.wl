<|"Source" -> "normop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\ns = nc[c[CR, 1, UP], \
c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];\nnormop[s]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", SqrtBox["2"], 
       "Out[3]="}}|>, <|"Code" -> "s/normop[s]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", FractionBox[RowBox[
         {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                   {"c", RowBox[{"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                        FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                         FontColor -> RGBColor[0, 0, 1]] & ]}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                       StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
              ")"}]}], "+", RowBox[{TagBox[StyleBox[TemplateBox[{"c", 
                RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                     RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[
                       1, 0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
               SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm], 
            StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
            TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                    "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                   StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
              ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
        SqrtBox["2"]], "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\n\
snegcomplexconstants[z];\nnormop[z c[CR, 1, UP]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", SqrtBox[RowBox[{"z", " ", RowBox[{"Conjugate", "[", "z", 
            "]"}]}]], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nn\
ormop[{nc[c[CR, 1, UP], c[CR, 1, UP]], nc[c[CR, 1, UP], c[CR, 1, DO]]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> 
     "snegbosonoperators[a];\nTable[normop[pow[a[CR], n]], {n, 0, 4}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1", ",", "1", ",", 
           SqrtBox["2"], ",", SqrtBox["6"], ",", 
           RowBox[{"2", " ", SqrtBox["6"]}]}], "}"}], "Out[2]="}}|>}|>

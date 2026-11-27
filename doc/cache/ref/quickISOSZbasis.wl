<|"Source" -> "quickISOSZbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "snegfermionoperators[d];\nnnop[d[]] = 0;\nquickISOSZbasis[{d[]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {FractionBox["1", "2"], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[
                         TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                      {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                      {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], "}"}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[d];\nsnegrealconstant\
s[U, B];\nnnop[d[]] = 0;\nbz = \
quickISOSZbasis[{d[]}];\nmakebasis[{d[]}];\nmakematricesbzvc[U/2 isozsq[d[]] \
- B spinz[d[]], bzop2bzvc[bz]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {FractionBox["1", "2"], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", FractionBox["U", "2"], "}"}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", RowBox[{"-", FractionBox["B", "2"]}], 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", FractionBox["B", "2"], "}"}], "}"}]}], 
             "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nnnop[c[1]] = 0; nnop[c[2]] = \
1;\ndims = {First[#], Length[Last[#]]} & /@ quickISOSZbasis[{c[1], c[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", "2"], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", "2"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{FractionBox["1", 
                    "2"], ",", RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], 
               ",", "2"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"0", ",", "0"}], "}"}], ",", "3"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", "1"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, 
   <|"Code" -> "Total[(2 #[[1, 1]] + 1) #[[2]] & /@ dims]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> {{"Output", "16", "Out[4]="}}|>}|>

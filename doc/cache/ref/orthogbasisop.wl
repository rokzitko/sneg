<|"Source" -> "orthogbasisop.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nbz = \
qszbasis[{d[]}];\northogbasisop[applybasis[bz, 3 # &], bz]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "1", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", TagBox[StyleBox[TemplateBox[{"d", RowBox[
                      {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                    "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                   ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                    {1, 1}], DisplayForm], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", TagBox[StyleBox[
                   TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                         "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                        StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                     "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                     SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                   ScriptBaselineShifts -> {1, 1}], DisplayForm], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"-", RowBox[
                    {"(", RowBox[{TagBox[StyleBox[TemplateBox[{"d", RowBox[
                          {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                        FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                         TemplateBox[{"d", RowBox[{TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                         ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}], "}"}]}], "}"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nbz = \
qszbasis[{c[1], c[2]}];\nswapped = bz /. {c[t_, 1, s_] :> c[t, 2, s], c[t_, \
2, s_] :> c[t, 1, s]};\nodd = dropemptysubspaces[orthogbasisop[mapbasis[bz, \
swapped, Subtract], bz]];\n{First[#], Length[Last[#]]} & /@ odd", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", "2"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"1", ",", RowBox[{"-", FractionBox["1", 
                      "2"]}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", "1"}], "}"}]}], "}"}], 
       "Out[5]="}}|>}|>

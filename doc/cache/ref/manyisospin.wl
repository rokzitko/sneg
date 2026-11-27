<|"Source" -> "manyisospin.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\n{ix, iy, iz, ii} = \
manyisospin[{c[1], c[2]}, {0, 1}];\nix", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{RowBox[{"-", FractionBox["1", "2"]}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                   TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"1", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                  {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                      RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                       RGBColor[1, 0, 0]] & ]}], "\"\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[3]="}}|>, <|"Code" -> "Expand[{komutator[ix, iy] - I iz, \
komutator[ii, ix], komutator[ii, iz]}]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c];\nii = \
manyisospin[{c[1], c[2]}, {0, 1}][[4]];\n{First[#], Eigenvalues[Last[#]]} & \
/@ makematricesbzvc[ii, qszbasisvc[{c[1], c[2]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "2"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "2", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{FractionBox["3", "4"], ",", FractionBox["3", 
                    "4"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{FractionBox["3", "4"], ",", FractionBox["3", 
                    "4"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[
                    {"-", "1"}]}], "}"}], ",", RowBox[{"{", "0", "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"2", ",", "0", 
                   ",", "0", ",", "0"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", RowBox[
                {"{", RowBox[{FractionBox["3", "4"], ",", FractionBox["3", 
                    "4"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {FractionBox["3", "4"], ",", FractionBox["3", "4"]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], ",", RowBox[
                {"{", "2", "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[Outer[komutator, \
Most[manyisospin[{c[1], c[2]}, {0, 1}]], Most[manyspin[{c[1], c[2]}]]]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0", ",", "0"}], "}"}]}], "}"}], 
       "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nmanyisospin[{c[1], c[2]}, {0}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "manyisospin::unequallength: Different lengths: ops={c[1], c[2]} \
ns={0}."}}|>}|>

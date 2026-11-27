<|"Source" -> "mapbasis.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> 
     "mapbasis[{{{0}, {a, b}}, {{1}, {c}}}, {{{0}, {x, y}}, {{1}, {z}}}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", 
               RowBox[{"{", RowBox[{FractionBox[RowBox[{"a", "+", "x"}], 
                    "2"], ",", FractionBox[RowBox[{"b", "+", "y"}], "2"]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", "1", "}"}], ",", RowBox[{"{", FractionBox[
                  RowBox[{"c", "+", "z"}], "2"], "}"}]}], "}"}]}], "}"}], 
       "Out[1]="}}|>, <|"Code" -> "mapbasis[{{{0}, {a, b}}, {{1}, {c}}}, \
{{{0}, {x, y}}, {{1}, {z}}}, Subtract]", "Reset" -> False, 
    "InLabel" -> "In[2]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", "0", "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"a", "-", "x"}], ",", RowBox[{"b", "-", 
                     "y"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", "1", "}"}], ",", RowBox[
                {"{", RowBox[{"c", "-", "z"}], "}"}]}], "}"}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\nbz = \
qszbasis[{c[1], c[2]}];\nswapped = bz /. {c[t_, 1, s_] :> c[t, 2, s], c[t_, \
2, s_] :> c[t, 1, s]};\neven = dropemptysubspaces[orthogbasisop[mapbasis[bz, \
swapped], bz]];\nSelect[even, First[#] == {-1, 1/2} &]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"{", RowBox[
            {RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox["1", 
                  "2"]}], "}"}], ",", RowBox[{"{", RowBox[{FractionBox[
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], SqrtBox["2"]], "+", FractionBox[
                  TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", TagBox[
                         StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                     SyntaxForm -> SubsuperscriptBox], 
                    ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                     {1, 1}], DisplayForm], SqrtBox["2"]]}], "}"}]}], "}"}], 
         "}"}], "Out[5]="}}|>, 
   <|"Code" -> "mapbasis[{{{0}, {a}}}, {{{1}, {x}}}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", "mapbasis::compat: Incompatible subspaces"}}|>}|>

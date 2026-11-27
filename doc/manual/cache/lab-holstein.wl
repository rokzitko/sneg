<|"Source" -> "lab-holstein.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, ck]; \
snegbosonoperators[b, bk];\nsnegrealconstants[g]; sneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k,q}, Ns];\nemit = g sum[nc[number[c[R],UP], \
b[CR,R]], {R}];\nemk = fourier[fourier[emit,c -> ck,R -> k], b -> bk,R -> \
q];\nsumMomentumCanonical[sumExpand[emk]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", FractionBox[RowBox[{"g", " ", RowBox[{"sum", "[", 
            RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"bk", RowBox[{"q"}], 
                    "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                    SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                  ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"ck", RowBox[{"k", TagBox[
                       StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                   SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                   1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
                TagBox[StyleBox[TemplateBox[{"ck", RowBox[{RowBox[{"k", "+", 
                        "q"}], TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                         RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\""}, 
                   "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                  ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                   {1, 1}], DisplayForm]}], ",", RowBox[{"{", RowBox[
                 {"k", ",", "q"}], "}"}]}], "]"}]}], SqrtBox["Ns"]], 
       "Out[6]=", <|"Kind" -> "Single", "Parts" -> {"\\frac{g \\sum_{k,q} \
\\mathrm{bk}^\\dagger_{q} \\mathrm{ck}^\\dagger_{k\\uparrow} \
\\mathrm{ck}_{k+q\\uparrow}}{\\sqrt{\\text{Ns}}}"}|>}}|>}|>

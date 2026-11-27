<|"Source" -> "showbzmat.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps, \
U, B];\nshowbzmat[makematricesbzvc[eps number[d[]] + U hubbard[d[]] + B \
spinz[d[]], qszbasisvc[{d[]}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", "0"}], 
               "}"}], TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{{"0"}}, 
                 RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                  Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
              Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}, 
            {RowBox[{"{", RowBox[{"0", ",", RowBox[{"-", FractionBox["1", 
                    "2"]}]}], "}"}], TagBox[RowBox[{"(", "\[NoBreak]", 
                GridBox[{{RowBox[{RowBox[{"-", FractionBox["B", "2"]}], "+", 
                     "eps"}]}}, RowSpacings -> 1, ColumnSpacings -> 1, 
                 RowAlignments -> Baseline, ColumnAlignments -> Center], 
                "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
                BoxForm`e$]]]}, {RowBox[{"{", RowBox[{"0", ",", FractionBox[
                  "1", "2"]}], "}"}], TagBox[RowBox[{"(", "\[NoBreak]", 
                GridBox[{{RowBox[{FractionBox["B", "2"], "+", "eps"}]}}, 
                 RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                  Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
              Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}, 
            {RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
             TagBox[RowBox[{"(", "\[NoBreak]", GridBox[
                 {{RowBox[{RowBox[{"2", " ", "eps"}], "+", "U"}]}}, 
                 RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> 
                  Baseline, ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
              Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[3]//MatrixForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nshowbzmat[qsbasis[{c[1], c[2]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{"{", RowBox[{RowBox[{"-", "2"}], ",", "0"}], 
               "}"}], TagBox[RowBox[{"(", "\[NoBreak]", TagBox[GridBox[
                  {{"1"}}, ColumnAlignments -> Center, RowSpacings -> 1, 
                  ColumnAlignments -> Left], Column], "\[NoBreak]", ")"}], 
              Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}, 
            {RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", FractionBox["1", 
                  "2"]}], "}"}], TagBox[RowBox[{"(", "\[NoBreak]", 
                TagBox[GridBox[{{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                         {"2", TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                       "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                       {1, 1}], DisplayForm]}, {TagBox[StyleBox[TemplateBox[
                       {"c", RowBox[{"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                        "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                        SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                      ScriptBaselineShifts -> {1, 1}], DisplayForm]}}, 
                  RowSpacings -> 1, ColumnAlignments -> Center, 
                  ColumnAlignments -> Left], Column], "\[NoBreak]", ")"}], 
              Function[BoxForm`e$, MatrixForm[BoxForm`e$]]]}, 
            {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
             TagBox[RowBox[{"(", "\[NoBreak]", TagBox[GridBox[
                  {{RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"2", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}, {FractionBox[RowBox[
                      {RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"1", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                       "-", RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"1", TagBox[StyleBox["\"\[UpArrow]\"", 
                          FontColor -> RGBColor[1, 0, 0]], StyleForm[#1, 
                          FontColor -> RGBColor[1, 0, 0]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm], 
                         StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 
                          0]], TagBox[StyleBox[TemplateBox[{"c", RowBox[
                          {"2", TagBox[StyleBox["\"\[DownArrow]\"", 
                          FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ]}], 
                          "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                          ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
                     SqrtBox["2"]]}, {RowBox[{TagBox[StyleBox[TemplateBox[
                         {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}}, RowSpacings -> 1, 
                  ColumnAlignments -> Center, ColumnAlignments -> Left], 
                 Column], "\[NoBreak]", ")"}], Function[BoxForm`e$, 
               MatrixForm[BoxForm`e$]]]}, {RowBox[{"{", RowBox[{"0", ",", 
                 "1"}], "}"}], TagBox[RowBox[{"(", "\[NoBreak]", 
                TagBox[GridBox[{{RowBox[{"-", RowBox[{"(", RowBox[
                         {TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ]}], "\"\[Dagger]\""}, 
                          "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                          FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                          TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                          SyntaxForm -> SubsuperscriptBox], 
                          ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                          {1, 1}], DisplayForm]}], ")"}]}]}}, RowSpacings -> 
                   1, ColumnAlignments -> Center, ColumnAlignments -> Left], 
                 Column], "\[NoBreak]", ")"}], Function[BoxForm`e$, 
               MatrixForm[BoxForm`e$]]]}, {RowBox[{"{", RowBox[{"1", ",", 
                 FractionBox["1", "2"]}], "}"}], TagBox[RowBox[{"(", 
                "\[NoBreak]", TagBox[GridBox[{{RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}, {RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}}, RowSpacings -> 1, 
                  ColumnAlignments -> Center, ColumnAlignments -> Left], 
                 Column], "\[NoBreak]", ")"}], Function[BoxForm`e$, 
               MatrixForm[BoxForm`e$]]]}, {RowBox[{"{", RowBox[{"2", ",", 
                 "0"}], "}"}], TagBox[RowBox[{"(", "\[NoBreak]", 
                TagBox[GridBox[{{RowBox[{TagBox[StyleBox[TemplateBox[
                         {"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm]}]}}, RowSpacings -> 1, 
                  ColumnAlignments -> Center, ColumnAlignments -> Left], 
                 Column], "\[NoBreak]", ")"}], Function[BoxForm`e$, 
               MatrixForm[BoxForm`e$]]]}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[2]//MatrixForm="}}|>}|>

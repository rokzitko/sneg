<|"Source" -> "xtospin.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegxoperators[X, \
\"HubbardAtom\"];\nsnegspinoperators[{S, 1/2}];\nxtospin[X[1, \"UP\", \"DO\"] \
+ X[1, \"DO\", \"DO\"], X -> S]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox["1", "2"], "-", 
         TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "+", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nxtospin[fermionstox[spinspin[c[1], c[2]], c -> X], X -> S]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[a, d];\nsnegrealconstants[ed, U, \
v];\nsnegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, 1/2}];\nheff \
= schriefferwolff[ed number[d[]] + U hubbard[d[]], v hop[a[], d[]], \
projector1[d[]], 2];\nhk = Expand[xtospin[fermionstox[heff[[3]], d -> X], X \
-> S]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{FractionBox[SuperscriptBox["v", "2"], 
          "ed"], "-", FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                   {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                       RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"a", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm]}]}], RowBox[{"2", " ", "ed"}]], "-", 
         FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                   {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                       RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                        RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
                RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[
                 {"a", RowBox[{TagBox[StyleBox["\"\[DownArrow]\"", 
                      FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                       FontColor -> RGBColor[0, 0, 1]] & ]}], "\"\""}, 
                 "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
               DisplayForm]}]}], RowBox[{"2", " ", 
            RowBox[{"(", RowBox[{"ed", "+", "U"}], ")"}]}]], "-", 
         FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"a", RowBox[
                   {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[
                        1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                         0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                 SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                 1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{"2", " ", "ed"}]], "-", FractionBox[
          RowBox[{SuperscriptBox["v", "2"], " ", RowBox[
             {TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{"2", " ", RowBox[{"(", RowBox[{"ed", "+", "U"}], ")"}]}]], 
         "+", FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                    "\"z\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "ed"], 
         "-", FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                    "\"z\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{"ed", "+", "U"}]], "-", FractionBox[
          RowBox[{SuperscriptBox["v", "2"], " ", RowBox[
             {TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox["\"z\"", 
                    FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "ed"], 
         "+", FractionBox[RowBox[{SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                    "\"z\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{"ed", "+", "U"}]], "-", FractionBox[
          RowBox[{"U", " ", SuperscriptBox["v", "2"], " ", 
            RowBox[{TagBox[StyleBox[TemplateBox[{"S", TagBox[StyleBox[
                    "\"+\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                     FontColor -> RGBColor[0, 0, 1]] & ]}, "Superscript", 
                 SyntaxForm -> SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{SuperscriptBox["ed", "2"], "+", RowBox[{"ed", " ", 
              "U"}]}]], "-", FractionBox[RowBox[{"U", " ", 
            SuperscriptBox["v", "2"], " ", RowBox[{TagBox[StyleBox[
                TemplateBox[{"S", TagBox[StyleBox["\"-\"", FontColor -> 
                     RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                       0, 0, 1]] & ]}, "Superscript", SyntaxForm -> 
                  SuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                     StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                  "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm], 
              StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
              TagBox[StyleBox[TemplateBox[{"a", RowBox[{TagBox[StyleBox[
                      "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                     StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                  "\"\""}, "Subsuperscript", SyntaxForm -> 
                  SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], 
          RowBox[{SuperscriptBox["ed", "2"], "+", RowBox[{"ed", " ", 
              "U"}]}]]}], "Out[6]="}}|>, 
   <|"Code" -> "Simplify[Coefficient[hk, nc[S[3], a[CR, UP], a[AN, UP]]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"-", FractionBox[
          RowBox[{"U", " ", SuperscriptBox["v", "2"]}], 
          RowBox[{SuperscriptBox["ed", "2"], "+", RowBox[{"ed", " ", 
              "U"}]}]]}], "Out[7]="}}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\n{xtospin[nc[X[1, \"UP\", \"DO\"], X[1, \"DO\", \"UP\"]], X -> S], \
xtospin[X[1, \"UP\", \"UP\"], X -> S]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{FractionBox["1", "2"], "+", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{FractionBox["1", "2"], "+", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegxoperators[X, \"HubbardAtom\"];\nsnegspinoperators[{S, \
1/2}];\nxtospin[X[1, \"2\", \"UP\"] + X[1, \"UP\", \"UP\"], X -> S]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "xtospin::out: Terms with X-operators outside the \
singly occupied subspace were dropped: X[1, 2, UP]."}, 
      {"Output", RowBox[{FractionBox["1", "2"], "+", 
         TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegxoperators[X, \
\"HubbardAtom\"];\nxtospin[X[1, \"UP\", \"UP\"], X -> c]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "xtospin::spinop: c must be declared with snegspinoperators."}, 
      {"Output", "$Failed", "Out[3]="}}|>}|>

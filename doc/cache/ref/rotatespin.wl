<|"Source" -> "rotatespin.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[S];\nrotatespin[{S[1, 3], S[1, \
4]}, S, RotationMatrix[Pi, {1, 0, 0}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"-", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], ",", RowBox[{TagBox[StyleBox[TemplateBox[
                {"S", RowBox[{"1"}], TagBox[StyleBox["\"x\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], "-", 
             RowBox[{"\[ImaginaryI]", " ", TagBox[StyleBox[TemplateBox[
                  {"S", RowBox[{"1"}], TagBox[StyleBox["\"y\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegrealconstants[\[CurlyPhi]];\n\
rotatespin[S[1, 3], S, RotationMatrix[\[CurlyPhi], {0, 1, 0}]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"Cos", "[", "\[CurlyPhi]", "]"}], " ", 
           TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], " ", RowBox[{"Sin", "[", "\[CurlyPhi]", "]"}]}]}], 
       "Out[4]="}}|>, <|"Code" -> "snegspinoperators[S];\nrotatespin[S[1, 3] \
+ S[2, 3], S, If[# == 2, RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] \
&]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "-", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nrotatespin[S[R, \"A\", 3] + S[R, \
\"B\", 3], S, If[Last[{##}] === \"B\", RotationMatrix[Pi, {1, 0, 0}], \
IdentityMatrix[3]] &]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"R", "A"}], 
             TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
              StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
            "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
           ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
          DisplayForm], "-", TagBox[StyleBox[TemplateBox[
            {"S", RowBox[{"R", "B"}], TagBox[StyleBox["\"z\"", FontColor -> 
                RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                  1]] & ]}, "Subsuperscript", SyntaxForm -> 
             SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
           ScriptBaselineShifts -> {1, 1}], DisplayForm]}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nrotatespin[spinspin[S[1], S[2]], S, \
If[# == 2, RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "-", RowBox[
          {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
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
            DisplayForm]}]}], "Out[2]="}}|>, <|"Code" -> "spincomponents[%]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"-", RowBox[{"(", RowBox[{TagBox[StyleBox[TemplateBox[
                  {"S", RowBox[{"1"}], TagBox[StyleBox["\"z\"", FontColor -> 
                      RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                       RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm], StyleBox[
                "\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], TagBox[
                StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[StyleBox[
                     "\"z\"", FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                      FontColor -> RGBColor[0, 0, 1]] & ]}, "Subsuperscript", 
                  SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                  1, ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
             ")"}]}], "+", RowBox[{FractionBox["1", "2"], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"+\"", FontColor -> RGBColor[0, 0, 1]], 
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
                 TagBox[StyleBox["\"-\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nsnegrealconstants[Q];\nSimplify[spinco\
mponents[rotatespin[spinspin[S[1], S[2]], S, RotationMatrix[(# - 1) Q, {0, 1, \
0}] &], \"XYZ\"]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{RowBox[{"Cos", "[", "Q", "]"}], 
           " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 1]], 
                StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[
                {"2"}], TagBox[StyleBox["\"y\"", FontColor -> RGBColor[0, 0, 
                   1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
              "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[{RowBox[{"Cos", "[", "Q", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], " ", RowBox[{"Sin", "[", "Q", "]"}]}], "-", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], " ", RowBox[{"Sin", "[", "Q", "]"}]}]}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinoperators[S];\nsnegrealconstants[\
\[Theta], \[CurlyPhi]];\nrot = RotationMatrix[\[Theta], {0, 0, 1}] . \
RotationMatrix[\[CurlyPhi], {1, 0, \
0}];\nSimplify[spincomponents[rotatespin[spinspin[S[1], S[2]], S, rot] - \
spinspin[S[1], S[2]]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nsnegrealconstants[\[CurlyPhi]];\nr = \
rotatespin[{S[1, 1], S[1, 2], S[1, 3]}, S, RotationMatrix[\[CurlyPhi], {1, 1, \
0}/Sqrt[2]]];\nSimplify[spincomponents[komutator[r[[1]], r[[2]]] - I \
r[[3]]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[S];\nrotatespin[nc[S[1, 3], S[2, 3]], S, \
RotationMatrix[\[Alpha], {0, 1, 0}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{RowBox[{RowBox[{"Cos", "[", "\[Alpha]", 
               "]"}], " ", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], RowBox[{RowBox[{"Cos", "[", "\[Alpha]", 
               "]"}], " ", TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "-", RowBox[
          {RowBox[{RowBox[{"Cos", "[", "\[Alpha]", "]"}], " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], StyleBox["\[CenterDot]", FontColor -> 
             RGBColor[0, 1, 0]], RowBox[{TagBox[StyleBox[TemplateBox[
                {"S", RowBox[{"2"}], TagBox[StyleBox["\"x\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], " ", 
             RowBox[{"Sin", "[", "\[Alpha]", "]"}]}]}], "-", 
         RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], " ", RowBox[{"Sin", "[", "\[Alpha]", "]"}]}], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           RowBox[{RowBox[{"Cos", "[", "\[Alpha]", "]"}], " ", 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                  StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[
          {RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], " ", RowBox[{"Sin", "[", "\[Alpha]", "]"}]}], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], " ", RowBox[{"Sin", "[", "\[Alpha]", "]"}]}]}]}], 
       "Out[2]="}}|>, <|"Code" -> "snegrealconstants[\[Alpha]];\nrotatespin[n\
c[S[1, 3], S[2, 3]], S, RotationMatrix[\[Alpha], {0, 1, 0}]]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{SuperscriptBox[RowBox[{"Cos", "[", "\[Alpha]", "]"}], "2"], 
           " ", RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "-", RowBox[
          {RowBox[{"Cos", "[", "\[Alpha]", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], " ", RowBox[{"Sin", "[", "\[Alpha]", "]"}]}], 
         "-", RowBox[{RowBox[{"Cos", "[", "\[Alpha]", "]"}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"S", RowBox[{"1"}], 
                 TagBox[StyleBox["\"z\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], 
                 TagBox[StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], " ", RowBox[{"Sin", "[", "\[Alpha]", "]"}]}], 
         "+", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"S", 
                 RowBox[{"1"}], TagBox[StyleBox["\"x\"", FontColor -> 
                    RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[
                      0, 0, 1]] & ]}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"S", RowBox[{"2"}], TagBox[
                  StyleBox["\"x\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}], " ", SuperscriptBox[RowBox[{"Sin", "[", 
              "\[Alpha]", "]"}], "2"]}]}], "Out[4]="}}|>, 
   <|"Code" -> 
     "snegspinoperators[S];\nrotatespin[S[1, 3], S, IdentityMatrix[2]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "rotatespin::matrix: The rotation {{1, 0}, {0, \
1}} must be a 3 by 3 matrix."}, {"Output", "$Failed", "Out[2]="}}|>}|>

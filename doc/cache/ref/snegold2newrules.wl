<|"Source" -> "snegold2newrules.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, e, o];\nrules = \
snegold2newrules[{c[1], c[2]}, {e[], o[]}, {(c[1] + c[2])/Sqrt[2], (c[1] - \
c[2])/Sqrt[2]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "\[Rule]", RowBox[{FractionBox[RowBox[
                 {"e", "[", "]"}], SqrtBox["2"]], "+", FractionBox[
                RowBox[{"o", "[", "]"}], SqrtBox["2"]]}]}], ",", 
           RowBox[{TagBox[RowBox[{"c", "[", RowBox[{"2"}], "]"}], 
              DisplayForm], "\[Rule]", RowBox[{FractionBox[RowBox[
                 {"e", "[", "]"}], SqrtBox["2"]], "-", FractionBox[
                RowBox[{"o", "[", "]"}], SqrtBox["2"]]}]}]}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "oprules = c[t_, i_, s_] :> ((c[i] /. \
rules) /. op_?operatorQ[] :> op[t, s]);\nc[CR, 1, UP] /. oprules", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {FractionBox[TagBox[StyleBox[TemplateBox[{"e", RowBox[{
                TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 
                    0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
              "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
              SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm], SqrtBox["2"]], 
         "+", FractionBox[TagBox[StyleBox[TemplateBox[
             {"o", RowBox[{TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> 
                   RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 
                     0, 0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
             SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
            ScriptBaselineShifts -> {1, 1}], DisplayForm], SqrtBox["2"]]}], 
       "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[c, e, o];\nrules = \
snegold2newrules[{c[1], c[2]}, {e[], o[]}, {(c[1] + c[2])/Sqrt[2], (c[1] - \
c[2])/Sqrt[2]}];\noprules = c[t_, i_, s_] :> ((c[i] /. rules) /. \
op_?operatorQ[] :> op[t, s]);\nExpand[hop[c[1], c[2]] /. oprules] == \
Expand[number[e[]] - number[o[]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "Expand[(number[c[1]] + number[c[2]]) /. oprules]", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{TagBox[StyleBox[TemplateBox[{"e", RowBox[
                {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[0, 
                     0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                      1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
              SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                   "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                  StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}], "+", RowBox[
          {TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
               SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
             ScriptBaselineShifts -> {1, 1}], DisplayForm], 
           StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
           TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                   "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                  StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
               "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
             ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
            DisplayForm]}]}], "Out[5]="}}|>, 
   <|"Code" -> "Expand[(hubbard[c[1]] + hubbard[c[2]]) /. oprules]", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{RowBox[{"-", FractionBox["1", "2"]}], " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"e", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                SyntaxForm -> SubsuperscriptBox], ScriptSizeMultipliers -> 
                1, ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"e", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "+", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> RGBColor[
                       0, 0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                        1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"e", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"e", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}], "-", 
         RowBox[{FractionBox["1", "2"], " ", RowBox[
            {TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                    StyleForm[#1, FontColor -> RGBColor[1, 0, 0]] & ]}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"o", RowBox[{TagBox[StyleBox[
                     "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 1]], 
                    StyleForm[#1, FontColor -> RGBColor[0, 0, 1]] & ]}], 
                 "\"\""}, "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 
                1, 0]], TagBox[StyleBox[TemplateBox[{"o", RowBox[
                  {TagBox[StyleBox["\"\[UpArrow]\"", FontColor -> RGBColor[1, 
                       0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                        0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm]}]}]}], 
       "Out[6]="}}|>, <|"Code" -> "snegfermionoperators[c, e, \
o];\nsnegrealconstants[\[Theta]];\nsnegold2newrules[{c[1], c[2]}, {e[], o[]}, \
{Cos[\[Theta]] c[1] + Sin[\[Theta]] c[2], -Sin[\[Theta]] c[1] + Cos[\[Theta]] \
c[2]}] // Simplify", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{TagBox[StyleBox[TemplateBox[{"c", "\"\""}, 
                "Superscript", SyntaxForm -> SuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm], "\[Rule]", RowBox[{RowBox[{RowBox[{"Cos", "[", 
                   "\[Theta]", "]"}], " ", RowBox[{"e", "[", "]"}]}], "-", 
               RowBox[{RowBox[{"o", "[", "]"}], " ", RowBox[{"Sin", "[", 
                   "\[Theta]", "]"}]}]}]}], ",", 
           RowBox[{TagBox[RowBox[{"c", "[", RowBox[{"2"}], "]"}], 
              DisplayForm], "\[Rule]", RowBox[{RowBox[{RowBox[{"Cos", "[", 
                   "\[Theta]", "]"}], " ", RowBox[{"o", "[", "]"}]}], "+", 
               RowBox[{RowBox[{"e", "[", "]"}], " ", RowBox[{"Sin", "[", 
                   "\[Theta]", "]"}]}]}]}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, e, o];\nsnegold2newrules[{c[1], \
c[2]}, {e[], o[]}, {c[1] + c[2], c[1] - c[2]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Print", TemplateBox[{"\"The transformation rule is not unitary.\""}, 
        "RowDefault"]}}|>}|>

<|"Source" -> "qsbasisvc.md", "SnegVersion" -> "2.2.0", 
 "Results" -> 
  {<|"Code" -> "snegfermionoperators[d];\nqsbasisvc[{d[]}] // InputForm", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        StyleBox["{{{-1, 0}, {vc[0, 0]}}, {{0, 1/2}, {vc[1, 0]}}, {{1, 0}, \
{-vc[1, 1]}}}", ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-1, 0}, {Sneg`vc[0, 0]}}, {{0, 1/2}, {Sneg`vc[1, 0]}}, 
          {{1, 0}, {-Sneg`vc[1, 1]}}}], Editable -> True, 
        AutoDelete -> True], "Out[2]//InputForm="}}|>, 
   <|"Code" -> 
     "snegfermionoperators[c];\nqsbasisvc[{c[1], c[2]}] // InputForm", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", InterpretationBox[StyleBox["{{{-2, 0}, {vc[0, 0, \
0, 0]}}, {{-1, 1/2}, {vc[0, 0, 1, 0], vc[1, 0, 0, 0]}}, {{0, 0}, {-vc[0, 0, \
1, 1], (vc[0, 1, 1, 0] - vc[1, 0, 0, 1])/Sqrt[2], -vc[1, 1, 0, 0]}}, {{0, 1}, \
{-vc[1, 0, 1, 0]}}, {{1, 1/2}, {-vc[1, 0, 1, 1], -vc[1, 1, 1, 0]}}, {{2, 0}, \
{vc[1, 1, 1, 1]}}}", ShowStringCharacters -> True, NumberMarks -> True], 
        InputForm[{{{-2, 0}, {Sneg`vc[0, 0, 0, 0]}}, {{-1, 1/2}, 
           {Sneg`vc[0, 0, 1, 0], Sneg`vc[1, 0, 0, 0]}}, 
          {{0, 0}, {-Sneg`vc[0, 0, 1, 1], (Sneg`vc[0, 1, 1, 0] - 
              Sneg`vc[1, 0, 0, 1])/Sqrt[2], -Sneg`vc[1, 1, 0, 0]}}, 
          {{0, 1}, {-Sneg`vc[1, 0, 1, 0]}}, {{1, 1/2}, {-Sneg`vc[1, 0, 1, 1], 
            -Sneg`vc[1, 1, 1, 0]}}, {{2, 0}, {Sneg`vc[1, 1, 1, 1]}}}], 
        Editable -> True, AutoDelete -> True], "Out[4]//InputForm="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\ndims = {First[#], Length[Last[#]]} \
& /@ qsbasisvc[{c[1], c[2], c[3]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "3"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "2"}], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", "3"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", "0"}], "}"}], ",", "6"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "1"}], "}"}], ",", "3"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", "8"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", FractionBox[
                    "3", "2"]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", "6"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "1"}], "}"}], ",", 
               "3"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"2", ",", FractionBox["1", "2"]}], "}"}], ",", 
               "3"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"3", ",", "0"}], "}"}], ",", "1"}], "}"}]}], 
         "}"}], "Out[2]="}}|>, 
   <|"Code" -> "Total[(2 #[[1, 2]] + 1) #[[2]] & /@ dims]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> {{"Output", "64", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d, c];\n{First[#], Length[Last[#]]} & /@ \
qsbasisvc[{d[], c[1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", FractionBox["1", "2"]}], "}"}], ",", "2"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", "3"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", "1"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox["1", 
                    "2"]}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", "1"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t, U];\nH = -t \
hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];\n{First[#], \
Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, qsbasisvc[{c[1], c[2]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"-", "t"}], ",", "t"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"U", ",", RowBox[
                    {FractionBox["1", "2"], " ", RowBox[{"(", RowBox[
                        {"U", "-", SqrtBox[RowBox[{RowBox[{"16", " ", 
                          SuperscriptBox["t", "2"]}], "+", SuperscriptBox[
                          "U", "2"]}]]}], ")"}]}], ",", RowBox[{FractionBox[
                      "1", "2"], " ", RowBox[{"(", RowBox[{"U", "+", SqrtBox[
                          RowBox[{RowBox[{"16", " ", SuperscriptBox["t", 
                          "2"]}], "+", SuperscriptBox["U", "2"]}]]}], 
                       ")"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{RowBox[{"-", "t"}], "+", "U"}], ",", RowBox[
                    {"t", "+", "U"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"2", " ", "U"}], "}"}]}], "}"}]}], 
         "}"}], "Out[4]="}}|>, <|"Code" -> "snegfermionoperators[d, \
c];\nsnegrealconstants[U, V];\nH = -U/2 number[d[]] + U hubbard[d[]] + V \
hop[d[], c[]];\neig = {First[#], Eigenvalues[Last[#]]} & /@ \
makematricesbzvc[H, qsbasisvc[{d[], c[]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "2"}], ",", 
                   "0"}], "}"}], ",", RowBox[{"{", "0", "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{FractionBox["1", "4"], " ", RowBox[
                      {"(", RowBox[{RowBox[{"-", "U"}], "-", SqrtBox[RowBox[
                          {SuperscriptBox["U", "2"], "+", RowBox[{"16", " ", 
                          SuperscriptBox["V", "2"]}]}]]}], ")"}]}], ",", 
                   RowBox[{FractionBox["1", "4"], " ", RowBox[{"(", RowBox[
                        {RowBox[{"-", "U"}], "+", SqrtBox[RowBox[
                          {SuperscriptBox["U", "2"], "+", RowBox[{"16", " ", 
                          SuperscriptBox["V", "2"]}]}]]}], ")"}]}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", RowBox[{FractionBox["1", "4"], " ", 
                     RowBox[{"(", RowBox[{RowBox[{"-", "U"}], "-", SqrtBox[
                          RowBox[{SuperscriptBox["U", "2"], "+", RowBox[
                          {"64", " ", SuperscriptBox["V", "2"]}]}]]}], 
                       ")"}]}], ",", RowBox[{FractionBox["1", "4"], " ", 
                     RowBox[{"(", RowBox[{RowBox[{"-", "U"}], "+", SqrtBox[
                          RowBox[{SuperscriptBox["U", "2"], "+", RowBox[
                          {"64", " ", SuperscriptBox["V", "2"]}]}]]}], 
                       ")"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"-", FractionBox["U", "2"]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"1", ",", FractionBox["1", "2"]}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{FractionBox["1", "4"], " ", 
                     RowBox[{"(", RowBox[{RowBox[{"-", "U"}], "-", SqrtBox[
                          RowBox[{SuperscriptBox["U", "2"], "+", RowBox[
                          {"16", " ", SuperscriptBox["V", "2"]}]}]]}], 
                       ")"}]}], ",", RowBox[{FractionBox["1", "4"], " ", 
                     RowBox[{"(", RowBox[{RowBox[{"-", "U"}], "+", SqrtBox[
                          RowBox[{SuperscriptBox["U", "2"], "+", RowBox[
                          {"16", " ", SuperscriptBox["V", "2"]}]}]]}], 
                       ")"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], 
               ",", RowBox[{"{", "0", "}"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "Series[-U/2 - (-U - Sqrt[U^2 + 64 V^2])/4, {V, 0, 2}, \
Assumptions -> U > 0]", "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", InterpretationBox[
        RowBox[{FractionBox[RowBox[{"8", " ", SuperscriptBox["V", "2"]}], 
           "U"], "+", InterpretationBox[SuperscriptBox[
            RowBox[{"O", "[", "V", "]"}], "3"], SeriesData[V, 0, {}, 2, 3, 
            1], Editable -> False]}], SeriesData[V, 0, {8/U}, 2, 3, 1], 
        Editable -> False], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = \
transformbasis[qsbasisvc[{c[1], c[2]}], nc[projector1[c[1]], \
projector1[c[2]]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{FractionBox[
                    TagBox[RowBox[{TagBox[StyleBox[
                         "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                         "\"\[EmptyRectangle]\"", FontColor -> RGBColor[1, 0, 
                          0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[FilledRectangle]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                        StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                         "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                         "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm], SqrtBox["2"]], "-", 
                   FractionBox[TagBox[RowBox[{TagBox[StyleBox[
                         "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                         "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                        StyleBox["\"\[EmptyRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                         "\"\[FilledRectangle]\"", FontColor -> RGBColor[0, 
                          0, 1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                         "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm], SqrtBox["2"]]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "1"}], "}"}], ",", RowBox[{"{", InterpretationBox[
                  RowBox[{"\"-\"", TagBox[RowBox[{TagBox[StyleBox[
                         "\"\[LeftDoubleBracketingBar]\"", FontColor -> 
                          RGBColor[0, 1, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 1, 0]] & ], TagBox[StyleBox[
                         "\"\[FilledRectangle]\"", FontColor -> RGBColor[1, 
                          0, 0]], StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ], TagBox[StyleBox["\"\[EmptyRectangle]\"", 
                         FontColor -> RGBColor[0, 0, 1]], StyleForm[#1, 
                          FontColor -> RGBColor[0, 0, 1]] & ], TagBox[
                        StyleBox["\"\[FilledRectangle]\"", FontColor -> 
                          RGBColor[1, 0, 0]], StyleForm[#1, FontColor -> 
                          RGBColor[1, 0, 0]] & ], TagBox[StyleBox[
                         "\"\[EmptyRectangle]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ], TagBox[StyleBox[
                         "\"\[RightAngleBracket]\"", FontColor -> RGBColor[0, 
                          1, 0]], StyleForm[#1, FontColor -> RGBColor[0, 1, 
                          0]] & ]}], DisplayForm]}], -Sneg`vc[1, 0, 1, 0]], 
                 "}"}]}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "makematricesbzvc[spinspin[c[1], c[2]], bz]", 
    "Reset" -> False, "InLabel" -> "In[3]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0"}], "}"}], ",", RowBox[{"{", RowBox[{"{", RowBox[
                    {"-", FractionBox["3", "4"]}], "}"}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"{", FractionBox["1", "4"], 
                   "}"}], "}"}]}], "}"}]}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = qsbasisvc[{c[1], c[2], \
c[3]}];\n{sx, sy, sz, ss} = manyspin[{c[1], c[2], c[3]}];\nAnd @@ \
Flatten[Table[{Expand[ap[ss, v] - sub[[1, 2]] (sub[[1, 2]] + 1) v] === 0, \
Expand[ap[sz, v] - sub[[1, 2]] v] === 0}, {sub, bz}, {v, sub[[2]]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nExpand[bzop2bzvc[qsbasis[{c[1], \
c[2]}]]] === Expand[qsbasisvc[{c[1], c[2]}]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "True", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\n{Length /@ Last /@ qsbasisvc[{c[1], \
c[2], c[3]}], Length /@ Last /@ qszbasisvc[{c[1], c[2], c[3]}]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "3", ",", "6", ",", "3", 
               ",", "8", ",", "1", ",", "6", ",", "3", ",", "3", ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"1", ",", "3", ",", "3", ",", 
               "3", ",", "9", ",", "3", ",", "1", ",", "9", ",", "9", ",", 
               "1", ",", "3", ",", "9", ",", "3", ",", "3", ",", "3", ",", 
               "1"}], "}"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[a];\nqsbasisvc[{a[1], a[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "qsbasisvc::spin: qsbasisvc requires spin-1/2 \
fermion operators. Unsupported operators: {a[1], a[2]}. Use qbasisvc, \
nonebasisvc, or qszbasisvc for spinless operators."}, 
      {"Output", "$Failed", "Out[2]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[B];\nmakematrices\
bzvc[-B (spinz[c[1]] + spinz[c[2]]), qsbasisvc[{c[1], c[2]}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "2"}], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   FractionBox["1", "2"]}], "}"}], ",", RowBox[
                {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", FractionBox[
                          "B", "2"]}], ",", "0"}], "}"}], ",", RowBox[
                    {"{", RowBox[{"0", ",", RowBox[{"-", FractionBox["B", 
                          "2"]}]}], "}"}]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", 
                       ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                       "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[
                      {"0", ",", "0", ",", "0"}], "}"}]}], "}"}]}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"{", RowBox[{"-", "B"}], 
                   "}"}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", FractionBox[
                    "1", "2"]}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"{", RowBox[{RowBox[{"-", FractionBox["B", "2"]}], 
                       ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", 
                       RowBox[{"-", FractionBox["B", "2"]}]}], "}"}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[
              {RowBox[{"{", RowBox[{"2", ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"{", "0", "}"}], "}"}]}], "}"}]}], "}"}], 
       "Out[3]="}}|>}|>

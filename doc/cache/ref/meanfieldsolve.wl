<|"Source" -> "meanfieldsolve.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], c[2]] + \
4 (hubbard[c[1]] + hubbard[c[2]]);\nsol = meanfieldsolve[meanfield[H], {c[1], \
c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, \
\"ParticleNumber\" -> 2];\nKeys[sol]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"\"Averages\"", ",", "\"Energy\"", ",", 
           "\"QuasiparticleEnergies\"", ",", "\"ChemicalPotential\"", ",", 
           "\"Iterations\"", ",", "\"Converged\""}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "{sol[\"Energy\"], (avg[number[c[1], UP]] - avg[number[c[1], \
DO]]) /. sol[\"Averages\"], sol[\"Converged\"]}", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"-", "0.5000000000000004`"}], 
           ",", "0.8660254039264407`", ",", "True"}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], c[2]] + \
(hubbard[c[1]] + hubbard[c[2]]);\nsol = meanfieldsolve[meanfield[H, \
\"Hartree\"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], \
DO]] -> 0.9}, \"ParticleNumber\" -> 2];\n{sol[\"Energy\"], \
sol[\"Averages\"]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"-", "1.5`"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{TagBox[RowBox[{"\[LeftAngleBracket]", 
                    RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.4999999998656033`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.5000000001343972`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.5000000001343969`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
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
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.4999999998656031`"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -Sum[hop[c[i], c[Mod[i, 4] + \
1]], {i, 4}] + 1/2 Sum[number[c[i]], {i, 4}] - 3 Sum[hubbard[c[i]], {i, \
4}];\nF = Table[avg[nc[c[AN, i, DO], c[AN, i, UP]]], {i, 4}];\nsol = \
meanfieldsolve[meanfield[H, \"Pairing\"], Table[c[i], {i, 4}], Thread[F -> \
0.3], \"Temperature\" -> 1/10];\n{sol[\"Converged\"], F /. \
sol[\"Averages\"]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"True", ",", 
           RowBox[{"{", RowBox[{"0.34323302058509697`", ",", 
               "0.34323302058509686`", ",", "0.34323302058509686`", ",", 
               "0.34323302058509686`"}], "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "{3 First[F /. sol[\"Averages\"]], d /. FindRoot[1 == 3/4 \
Sum[Tanh[Sqrt[(-2 Cos[k] + 1/2)^2 + d^2]/(2/10)]/(2 Sqrt[(-2 Cos[k] + 1/2)^2 \
+ d^2]), {k, {0, Pi/2, Pi, 3 Pi/2}}], {d, 1}]}", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"1.029699061755291`", ",", 
           "1.0296990620785578`"}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nsol = \
meanfieldsolve[meanfield[number[b[]] + 1/4 nc[b[CR], b[CR], b[AN], b[AN]], \
\"Normal\"], {b}, {}, \"Temperature\" -> 1];\nx = avg[number[b[]]] /. \
sol[\"Averages\"];\n{x, 1/(Exp[1 + x] - 1)}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.3499764853931092`", ",", 
           "0.34997648540491283`"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], c[2]] + 4 \
(hubbard[c[1]] + hubbard[c[2]]);\nsol = meanfieldsolve[meanfield[H, \
\"Hartree\"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], \
DO]] -> 0.9}, \"ParticleNumber\" -> 2];\n{sol[\"ChemicalPotential\"], \
sol[\"QuasiparticleEnergies\"]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1.9999999999999103`", ",", 
           RowBox[{"{", RowBox[{"2.000000000245953`", ",", 
               "2.0000000002459535`", ",", "2.0000000002459557`", ",", 
               "2.000000000245956`"}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "sol = meanfieldsolve[meanfield[H, \"Hartree\"], {c[1], c[2]}, \
{avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, \
\"ParticleNumber\" -> 2, \"Temperature\" -> 1/2];\n(avg[number[c[1], UP]] - \
avg[number[c[1], DO]]) /. sol[\"Averages\"]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Output", "0.8165867721295612`", 
       "Out[2]="}}|>, <|"Code" -> "Map[meanfieldsolve[meanfield[H, \
\"Hartree\"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], \
DO]] -> 0.9}, \"ParticleNumber\" -> 2, \"Mixing\" -> #][\"Iterations\"] &, \
{1/4, 1/2, 1}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"91", ",", "41", ",", "15"}], 
         "}"}], "Out[1]="}}|>, <|"Code" -> "sol = meanfieldsolve[meanfield[H, \
\"Hartree\"], {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], \
DO]] -> 0.9}, \"ParticleNumber\" -> 2, \"MaxIterations\" -> \
5];\n{sol[\"Converged\"], sol[\"QuasiparticleEnergies\"]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "meanfieldsolve::conv: No \
convergence after 5 iterations; the last change was 0.0050359."}, 
      {"Output", RowBox[{"{", RowBox[{"False", ",", "$Failed"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "L = 6; T0 = 1/10; Ne = 4;\nek = Table[-2 \
Cos[2 Pi j/L], {j, 0, L - 1}];\nmu0 = mu /. FindRoot[2 Total[1/(Exp[(ek - \
mu)/T0] + 1)] == Ne, {mu, -1}];\nUc = 1/(Total[1/(4 T0 Cosh[(ek - mu0)/(2 \
T0)]^2)]/L)", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "1.1998910624184778`", "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nring[U_] := -Sum[hop[c[i], c[Mod[i, \
L] + 1]], {i, L}] + U Sum[hubbard[c[i]], {i, L}];\nmagnetization[U_] := \
Module[{s = meanfieldsolve[meanfield[ring[U], \"Hartree\"], Table[c[i], {i, \
L}], Flatten[Table[{avg[number[c[i], UP]] -> 0.5, avg[number[c[i], DO]] -> \
0.2}, {i, L}]], \"ParticleNumber\" -> Ne, \"Temperature\" -> T0]},\n  \
Chop[Sum[avg[number[c[i], UP]] - avg[number[c[i], DO]], {i, L}]/L /. \
s[\"Averages\"], 10^-6]];\n{magnetization[0.9 Uc], magnetization[1.3 Uc]}", 
    "Reset" -> False, "InLabel" -> "In[5]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           "0.2507766176803345`"}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -Sum[hop[c[i], c[Mod[i, 4] + \
1]], {i, 4}] + 4 Sum[hubbard[c[i]], {i, 4}];\nmf = meanfield[H, \
\"Hartree\"];\nneel = Flatten[Table[{avg[number[c[i], UP]] -> 0.5 + 0.4 \
(-1)^i, avg[number[c[i], DO]] -> 0.5 - 0.4 (-1)^i}, {i, 4}]];\nafm = \
meanfieldsolve[mf, Table[c[i], {i, 4}], neel, \"ParticleNumber\" -> \
4];\n{Table[avg[number[c[i], UP]] - avg[number[c[i], DO]], {i, 4}] /. \
afm[\"Averages\"], afm[\"Energy\"]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"-", "0.8161463613042114`"}], ",", 
               "0.8161463613042113`", ",", RowBox[{"-", 
                 "0.8161463613042114`"}], ",", "0.8161463613042119`"}], 
             "}"}], ",", RowBox[{"-", "1.7632978285545962`"}]}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], \
c[2]] + 4 (hubbard[c[1]] + hubbard[c[2]]);\nmf = meanfield[H];\nsol = \
meanfieldsolve[mf, {c[1], c[2]}, {avg[number[c[1], UP]] -> 0.9, \
avg[number[c[2], DO]] -> 0.9}, \"ParticleNumber\" -> 2];\nM = \
First[quadraticform[mf /. sol[\"Averages\"], {c[1], c[2]}]] - \
sol[\"ChemicalPotential\"] DiagonalMatrix[{1, 1, 1, 1, -1, -1, -1, \
-1}];\n{sol[\"Energy\"], gaussianaverage[H, M, {c[1], c[2]}, 0]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", "0.5000000000000004`"}], ",", 
           RowBox[{"-", "0.49999999999999994`"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "N[(4 - Sqrt[16 + 16])/2]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{"-", "0.8284271247461903`"}], "Out[7]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], c[2]] + 4 \
(hubbard[c[1]] + hubbard[c[2]]);\nsol = meanfieldsolve[meanfield[H, \
\"Hartree\"], {c[1], c[2]}, {}, \"ParticleNumber\" -> 2];\n{sol[\"Energy\"], \
sol[\"Averages\"]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", 
           RowBox[{"{", RowBox[{RowBox[{TagBox[RowBox[{"\[LeftAngleBracket]", 
                    RowBox[{TagBox[StyleBox[TemplateBox[{"c", RowBox[{"1", 
                          TagBox[StyleBox["\"\[DownArrow]\"", FontColor -> 
                          RGBColor[0, 0, 1]], StyleForm[#1, FontColor -> 
                          RGBColor[0, 0, 1]] & ]}], "\"\[Dagger]\""}, 
                         "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.49999999998373906`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"1", TagBox[StyleBox[
                          "\"\[UpArrow]\"", FontColor -> RGBColor[1, 0, 0]], 
                          StyleForm[#1, FontColor -> RGBColor[1, 0, 
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.4999999999580535`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\[Dagger]\""}, "Subsuperscript", 
                         SyntaxForm -> SubsuperscriptBox], 
                        ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> 
                         {1, 1}], DisplayForm], StyleBox["\[CenterDot]", 
                       FontColor -> RGBColor[0, 1, 0]], TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
                          "\"\[DownArrow]\"", FontColor -> RGBColor[0, 0, 
                          1]], StyleForm[#1, FontColor -> RGBColor[0, 0, 
                          1]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.49999999995805344`"}], ",", RowBox[{TagBox[RowBox[
                   {"\[LeftAngleBracket]", RowBox[{TagBox[StyleBox[
                        TemplateBox[{"c", RowBox[{"2", TagBox[StyleBox[
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
                          0]] & ]}], "\"\""}, "Subsuperscript", SyntaxForm -> 
                          SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
                        ScriptBaselineShifts -> {1, 1}], DisplayForm]}], 
                    "\[RightAngleBracket]"}], DisplayForm], "\[Rule]", 
                 "0.49999999998373884`"}]}], "}"}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsol = meanfieldsolve[-Sum[hop[c[i], \
c[Mod[i, 4] + 1]], {i, 4}], Table[c[i], {i, 4}], {}, \"ParticleNumber\" -> \
3];\nsol[\"Energy\"]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "meanfieldsolve::open: The level at the Fermi \
energy is degenerate and partially filled; its states are occupied equally \
(ensemble average)."}, {"Message", "meanfieldsolve::open: The level at the \
Fermi energy is degenerate and partially filled; its states are occupied \
equally (ensemble average)."}, {"Output", 
       RowBox[{"-", "3.9999999999999996`"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[U];\nmeanfieldsol\
ve[meanfield[U hubbard[c[1]]], {c[1]}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "meanfieldsolve::numeric: \
The mean-field Hamiltonian contains symbols other than the averages: {U}."}, 
      {"Output", "$Failed", "Out[3]="}}|>}|>

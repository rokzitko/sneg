<|"Source" -> "bogoliubov.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[xi, \
Dl];\nHS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, \
DO], c[AN, 1, UP]]);\n{eps, T, Egs} = bogoliubov[HS, {c[1]}];\n{eps, Egs}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{SqrtBox[RowBox[{SuperscriptBox[
                   "Dl", "2"], "+", SuperscriptBox["xi", "2"]}]], ",", 
               SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", SuperscriptBox[
                   "xi", "2"]}]]}], "}"}], ",", RowBox[{"xi", "-", 
             SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", SuperscriptBox[
                 "xi", "2"]}]]}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "E1 = Sqrt[xi^2 + Dl^2];\nSimplify[{T[[1, 1]]^2 - (1 + \
xi/E1)/2, T[[4, 1]]^2 - (1 - xi/E1)/2}]", "Reset" -> False, 
    "InLabel" -> "In[6]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + \
nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];\nM \
= First[quadraticform[H, {f[1], f[2], f[3]}]];\nFirst[bogoliubov[M /. {t -> \
1, Dl -> 1, mu -> 0}, \"Fermion\"]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.`", ",", "2.`", ",", "2.`"}], "}"}], 
       "Out[5]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nsnegreal\
constants[t, mu, Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], \
f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu \
Sum[number[f[j]], {j, 3}];\nM = First[quadraticform[H, {f[1], f[2], f[3]}]] \
/. {t -> 1, Dl -> 1/2, mu -> 1/3};\n{eps, T} = bogoliubov[M, \
\"Fermion\"];\neps", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.18401397475970294`", ",", 
           "1.3484430299299297`", ",", "1.8657903380229675`"}], "}"}], 
       "Out[6]="}}|>, <|"Code" -> "Chop[{ConjugateTranspose[T] . T - \
IdentityMatrix[6], ConjugateTranspose[T] . M . T - DiagonalMatrix[Join[eps, \
-eps]]}] // Flatten // Union", "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[{"{", "0", "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nsnegrealconstants[A, B];\n{eps, T, \
Egs} = bogoliubov[A number[b[]] + B/2 (nc[b[CR], b[CR]] + nc[b[AN], b[AN]]), \
{b}];\n{eps, Egs}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", SqrtBox[RowBox[{SuperscriptBox["A", "2"], 
                "-", SuperscriptBox["B", "2"]}]], "}"}], ",", 
           RowBox[{RowBox[{"-", FractionBox["A", "2"]}], "+", 
             RowBox[{FractionBox["1", "2"], " ", SqrtBox[RowBox[
                 {SuperscriptBox["A", "2"], "-", SuperscriptBox["B", 
                   "2"]}]]}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nH = 2 number[b[1]] + 3 number[b[2]] + \
(nc[b[CR, 1], b[AN, 2]] + nc[b[CR, 2], b[AN, 1]]) + (nc[b[CR, 1], b[CR, 2]] + \
nc[b[AN, 2], b[AN, 1]]);\n{eps, T, Egs} = bogoliubov[H, {b[1], b[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"1.0000000000000044`", ",", 
               "3.464101615137755`"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", 
                     "0.9045340337332928`"}], ",", "0.5420542414630479`", 
                   ",", RowBox[{"-", "0.30151134457776585`"}], ",", 
                   RowBox[{"-", "0.14524299625388826`"}]}], "}"}], ",", 
               RowBox[{"{", RowBox[{"0.6030226891555289`", ",", 
                   "0.8550094036871162`", ",", "0.30151134457776435`", ",", 
                   RowBox[{"-", "0.06138691326879819`"}]}], "}"}], ",", 
               RowBox[{"{", RowBox[{RowBox[{"-", "0.30151134457776585`"}], 
                   ",", RowBox[{"-", "0.14524299625388826`"}], ",", 
                   RowBox[{"-", "0.9045340337332928`"}], ",", 
                   "0.5420542414630479`"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0.30151134457776435`", ",", RowBox[
                    {"-", "0.06138691326879819`"}], ",", 
                   "0.6030226891555289`", ",", "0.8550094036871162`"}], 
                 "}"}]}], "}"}], ",", RowBox[{"-", "0.2679491924311206`"}]}], 
         "}"}], "Out[3]="}}|>, <|"Code" -> "eta = DiagonalMatrix[{1, 1, -1, \
-1}];\nChop[ConjugateTranspose[T] . eta . T - eta]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0"}], "}"}], 
           ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", "0", ",", 
               "0"}], "}"}], ",", RowBox[{"{", RowBox[{"0", ",", "0", ",", 
               "0", ",", "0"}], "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegrealconstants[Dl];\nsnegrealfu\
nctions[xi];\nsnegmomentumindexes[{k}, Ns];\nhk = \
First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, \
-k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, \
k]];\nFirst[bogoliubov[hk /. xi[-k] -> xi[k], \"Fermion\", \"Momentum\" -> \
True]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
              SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]], ",", 
           SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", SuperscriptBox[
               RowBox[{"xi", "[", "k", "]"}], "2"]}]], ",", 
           RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
                SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]]}], ",", 
           RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
                SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]]}]}], 
         "}"}], "Out[6]="}}|>, <|"Code" -> "hk = {{-2 Cos[0.7] + 0.5 Sin[0.7] \
- 0.3, 0.4 I Sin[0.7]}, {-0.4 I Sin[0.7], 2 Cos[0.7] + 0.5 Sin[0.7] + \
0.3}};\nbogoliubov[hk, \"Fermion\", \"Momentum\" -> True][[1]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"2.1698500427887533`", ",", 
           RowBox[{"-", "1.5256323555510625`"}]}], "}"}], "Out[2]="}}|>, 
   <|"Code" -> "bogoliubov[hk, \"Fermion\"]", "Reset" -> False, 
    "InLabel" -> "In[3]:=", "Cells" -> {{"Message", "bogoliubov::phs: The \
matrix does not have the particle-hole symmetry of the BdG matrix of the \
modes (a, a^dagger); for Bloch BdG matrices H(k) use \"Momentum\" -> True."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "{First[bogoliubov[{{10^-6, 0}, {0, -10^-6}}, \"Fermion\"]], \
First[bogoliubov[{{10^-6, 0}, {0, -10^-6}}, \"Fermion\", \"Tolerance\" -> \
10^-3]]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", "1.`*^-6", "}"}], ",", 
           RowBox[{"{", "0.`", "}"}]}], "}"}], "Out[1]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nL = 8;\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + \
1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu \
Sum[number[f[j]], {j, L}];\nM = First[quadraticform[H, Table[f[j], {j, \
L}]]];\n{eps, T} = bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 1/2}, \
\"Fermion\"];\neps", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.000028610229541446884`", 
           ",", "1.5539452804547689`", ",", "1.695416190611353`", ",", 
           "1.8822859828963001`", ",", "2.076238114994449`", ",", 
           "2.249965118443107`", ",", "2.385327548011508`", ",", 
           "2.4708140820526765`"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "Table[Abs[T[[j, 1]]]^2 + Abs[T[[L + j, 1]]]^2, {j, L}]", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.46875000305551007`", ",", 
           "0.02929690262502451`", ",", "0.0018315012941993816`", ",", 
           "0.00012159303939917124`", ",", "0.00012159303939310722`", ",", 
           "0.0018315012940958715`", ",", "0.02929690262336763`", ",", 
           "0.46875000302901026`"}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "{eps, T} = bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 3}, \
\"Fermion\"];\n{First[eps], Table[Abs[T[[j, 1]]]^2 + Abs[T[[L + j, 1]]]^2, \
{j, L}]}", "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1.2315475884907126`", ",", 
           RowBox[{"{", RowBox[{"0.057574057471224974`", ",", 
               "0.10705736651281637`", ",", "0.15366937616461906`", ",", 
               "0.1816991998513375`", ",", "0.1816991998513386`", ",", 
               "0.15366937616462029`", ",", "0.10705736651281711`", ",", 
               "0.05757405747122596`"}], "}"}]}], "}"}], "Out[10]="}}|>, 
   <|"Code" -> "snegbosonoperators[a, b];\nsnegrealconstants[w, lam];\n{eps, \
T, Egs} = bogoliubov[w (number[a[]] + number[b[]]) + lam (nc[a[CR], b[CR]] + \
nc[b[AN], a[AN]]), {a, b}];\n{eps, Egs}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{SqrtBox[RowBox[{RowBox[{"-", SuperscriptBox[
                     "lam", "2"]}], "+", SuperscriptBox["w", "2"]}]], ",", 
               SqrtBox[RowBox[{RowBox[{"-", SuperscriptBox["lam", "2"]}], 
                  "+", SuperscriptBox["w", "2"]}]]}], "}"}], ",", 
           RowBox[{RowBox[{"-", "w"}], "+", SqrtBox[RowBox[{RowBox[
                 {"-", SuperscriptBox["lam", "2"]}], "+", SuperscriptBox["w", 
                 "2"]}]]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegbosonoperators[b];\nsnegrealconstants[hs];\nh = \
Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}] - hs Sum[(-1)^(i + 1) S[i, 3], \
{i, 4}];\nh = rotatespin[h, S, If[EvenQ[First[{##}]], RotationMatrix[Pi, {1, \
0, 0}], IdentityMatrix[3]] &];\nhb = holsteinprimakoff[h, S -> b, 2];\n{eps, \
T, Egs} = bogoliubov[hb /. hs -> 1/10, Table[b[i], {i, 4}]];\n{eps, Egs}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0.45825756949558444`", ",", 
               "0.4582575694955846`", ",", "1.1`", ",", 
               "1.1000000000000003`"}], "}"}], ",", 
           RowBox[{"-", "1.8417424305044152`"}]}], "}"}], "Out[8]="}}|>, 
   <|"Code" -> "N[Sqrt[(1 + 1/10)^2 - Cos[{0, Pi/2, Pi/2, Pi}]^2]]", 
    "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.458257569495584`", ",", 
           "1.1`", ",", "1.1`", ",", "0.458257569495584`"}], "}"}], 
       "Out[9]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nH = \
Sum[-hop[f[j], f[j + 1]] + 1/2 (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j \
+ 1, 0], f[AN, j, 0]]), {j, 3}] - 1/3 Sum[number[f[j]], {j, 4}];\nmodes = \
Table[f[j], {j, 4}];\n{eps, T, Egs} = bogoliubov[H, modes];\n{Egs, \
thermalaverage[H, H, modes, 0]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", "3.1049695431527033`"}], ",", 
           RowBox[{"-", "3.1049695431527033`"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[g];\nChop[Expand[H /. \
bogoliubovrules[T, modes, g]] - Sum[eps[[m]] number[g[m]], {m, 4}] - Egs]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[2]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[g];\nsnegrealconstants[e1, \
e2];\nquadraticoperator[DiagonalMatrix[{e1, e2, -e1, -e2}], {g[1], g[2]}]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{RowBox[{"-", FractionBox["e1", "2"]}], 
         "-", FractionBox["e2", "2"], "+", RowBox[{"e1", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"g", RowBox[{"1", ""}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}], "+", RowBox[{"e2", " ", 
           RowBox[{TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", ""}], 
                 "\"\[Dagger]\""}, "Subsuperscript", SyntaxForm -> 
                 SubsuperscriptBox], ScriptSizeMultipliers -> 1, 
               ScriptBaselineShifts -> {1, 1}], DisplayForm], 
             StyleBox["\[CenterDot]", FontColor -> RGBColor[0, 1, 0]], 
             TagBox[StyleBox[TemplateBox[{"g", RowBox[{"2", ""}], "\"\""}, 
                "Subsuperscript", SyntaxForm -> SubsuperscriptBox], 
               ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}], 
              DisplayForm]}]}]}], "Out[3]="}}|>, 
   <|"Code" -> "snegspinoperators[{S, 1/2}];\nsnegbosonoperators[b];\nh = \
Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];\nh = rotatespin[h, S, \
If[EvenQ[First[{##}]], RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] \
&];\nbogoliubov[holsteinprimakoff[h, S -> b, 2], Table[b[i], {i, 4}]]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bogoliubov::posdef: The bosonic BdG matrix is \
not positive definite (dynamical instability or zero modes)."}, 
      {"Output", "$Failed", "Out[5]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[t, mu, \
Dl];\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + \
nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, \
3}];\nbogoliubov[First[quadraticform[H, {f[1], f[2], f[3]}]], \"Fermion\"]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bogoliubov::symbolic: Symbolic BdG matrices are \
supported only if they decompose into blocks of at most two modes; the block \
{1, 2, 3, 4, 5, 6} is larger."}, {"Output", "$Failed", "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f];\nsnegrealconstants[e];\nbogo\
liubov[e number[f[1]], {f[1]}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{RowBox[{"{", "e", "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0"}], "}"}], 
               ",", RowBox[{"{", RowBox[{"0", ",", "1"}], "}"}]}], "}"}], 
           ",", "0"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "bogoliubov[-number[f[1]], {f[1]}]", "Reset" -> False, 
    "InLabel" -> "In[4]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{RowBox[{"{", "1.`", "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0.`", ",", "1.`"}], 
                 "}"}], ",", RowBox[{"{", RowBox[{"1.`", ",", "0.`"}], 
                 "}"}]}], "}"}], ",", RowBox[{"-", "1.`"}]}], "}"}], 
       "Out[4]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nsnegreal\
constants[t, mu, Dl];\nL = 5;\nH = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, \
j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu \
Sum[number[f[j]], {j, L}];\nM = First[quadraticform[H, Table[f[j], {j, \
L}]]];\nbogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, \"Fermion\"]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bogoliubov::phs: The matrix does not have the \
particle-hole symmetry of the BdG matrix of the modes (a, a^dagger); for \
Bloch BdG matrices H(k) use \"Momentum\" -> True."}, 
      {"Output", "$Failed", "Out[6]="}}|>, 
   <|"Code" -> "Sort[Chop[Eigenvalues[N[M /. {t -> 1, Dl -> 1, mu -> 0}]]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", "2.0000000000000004`"}], ",", 
           RowBox[{"-", "2.`"}], ",", RowBox[{"-", "2.`"}], ",", 
           RowBox[{"-", "1.9999999999999996`"}], ",", "0", ",", "0", ",", 
           "1.9999999999999996`", ",", "1.9999999999999998`", ",", 
           "1.9999999999999998`", ",", "2.`"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "gaussianaverage[{number[f[1]], number[f[3]]}, M /. {t -> 1, \
Dl -> 1, mu -> 0}, Table[f[j], {j, L}], 0]", "Reset" -> False, 
    "InLabel" -> "In[8]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.5`", ",", "0.5`"}], "}"}], 
       "Out[8]="}}|>}|>

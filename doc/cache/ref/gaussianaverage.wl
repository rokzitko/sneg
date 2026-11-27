<|"Source" -> "gaussianaverage.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], \
c[2]];\ngaussianaverage[{number[c[1]], nc[c[CR, 1, UP], c[AN, 2, UP]]}, H, \
{c[1], c[2]}, 0]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.9999999999999998`", ",", 
           "0.5000000000000001`"}], "}"}], "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps, \
T];\ngaussianaverage[number[d[], UP], eps number[d[]], {d}, T]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox["1", 
        RowBox[{"1", "+", SuperscriptBox["\[ExponentialE]", 
           RowBox[{"eps", "/", "T"}]]}]], "Out[3]="}}|>, 
   <|"Code" -> "snegbosonoperators[b];\nsnegrealconstants[w, A, B, \
T];\n{gaussianaverage[number[b[]], w number[b[]], {b}, T], \
gaussianaverage[number[b[]], {{A, B}, {B, A}}, {b}, 0]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{FractionBox["1", 
            RowBox[{RowBox[{"-", "1"}], "+", SuperscriptBox[
               "\[ExponentialE]", FractionBox["w", "T"]]}]], ",", 
           FractionBox[SuperscriptBox["B", "2"], RowBox[{"2", " ", 
              SqrtBox[RowBox[{SuperscriptBox["A", "2"], "-", SuperscriptBox[
                  "B", "2"]}]], " ", RowBox[{"(", RowBox[{"A", "+", 
                  SqrtBox[RowBox[{SuperscriptBox["A", "2"], "-", 
                     SuperscriptBox["B", "2"]}]]}], ")"}]}]]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nH = 4/5 \
number[c[1]] - 3/5 (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, \
1, UP]]);\ngaussianaverage[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, UP]], \
hubbard[c[1]]}, H, {c[1]}, 0]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.09999999999999994`", ",", 
           "0.29999999999999993`", ",", "0.09999999999999999`"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nH = \
Sum[-hop[f[j], f[j + 1]] + 1/2 (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j \
+ 1, 0], f[AN, j, 0]]), {j, 2}];\nM = First[quadraticform[H, {f[1], f[2], \
f[3]}]];\ngaussianaverage[{number[f[1]], nc[f[AN, 2, 0], f[AN, 1, 0]], \
nc[number[f[1]], number[f[3]]]}, M, {f[1], f[2], f[3]}, 1/2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.5000000000000001`", ",", 
           RowBox[{"-", "0.14527180364742667`"}], ",", 
           "0.25000000000000044`"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegrealconstants[Dl];\nsnegrealfu\
nctions[xi];\nsnegmomentumindexes[{k}, Ns];\nhk = \
First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, \
-k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> \
xi[k];\nSimplify[gaussianaverage[{nc[ck[AN, -k, DO], ck[AN, k, UP]], \
number[ck[k], UP]}, hk, {ck}, 0, \"Momentum\" -> k] /. {xi[k] -> 2/5, Dl -> \
3/10}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{FractionBox["3", "10"], ",", 
           FractionBox["1", "10"]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[fk];\nsnegmomentumindexes[{k}, \
Ns];\ngaussianaverage[{number[fk[k]], number[fk[-k]]}, sum[-number[fk[k]], \
{k}], {fk}, 0, \"Momentum\" -> k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1.`", ",", "1.`"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\n{gaussia\
naverage[number[f[1]], {{10^-6, 0}, {0, -10^-6}}, {f[1]}, 0], \
gaussianaverage[number[f[1]], {{10^-6, 0}, {0, -10^-6}}, {f[1]}, 0, \
\"Tolerance\" -> 10^-3]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.`", ",", "0.5`"}], "}"}], 
       "Out[2]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f];\nL = \
12;\nH = -Sum[hop[f[j], f[j + 1]], {j, L - 1}] + Sum[number[f[j]], {j, \
L}];\ndens = gaussianaverage[Table[number[f[j]], {j, L}], H, Table[f[j], {j, \
L}], 0];\nNumberForm[dens, 3]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[
         {"{", RowBox[{InterpretationBox[StyleBox["\"0.214\"", 
              ShowStringCharacters -> False], 0.21388826418483378, 
             AutoDelete -> True], ",", InterpretationBox[
             StyleBox["\"0.424\"", ShowStringCharacters -> False], 
             0.4235379004545026, AutoDelete -> True], ",", InterpretationBox[
             StyleBox["\"0.332\"", ShowStringCharacters -> False], 
             0.3322733845046201, AutoDelete -> True], ",", InterpretationBox[
             StyleBox["\"0.315\"", ShowStringCharacters -> False], 
             0.31516330225423794, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.387\"", ShowStringCharacters -> 
               False], 0.38698854912389846, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.328\"", ShowStringCharacters -> 
               False], 0.3281485994779068, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.328\"", ShowStringCharacters -> 
               False], 0.32814859947790614, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.387\"", ShowStringCharacters -> 
               False], 0.38698854912389824, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.315\"", ShowStringCharacters -> 
               False], 0.3151633022542388, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.332\"", ShowStringCharacters -> 
               False], 0.33227338450461924, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.424\"", ShowStringCharacters -> 
               False], 0.42353790045450207, AutoDelete -> True], ",", 
            InterpretationBox[StyleBox["\"0.214\"", ShowStringCharacters -> 
               False], 0.21388826418483547, AutoDelete -> True]}], "}"}], 
        NumberForm[#1, 3] & ], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegrealconstants[Dl, x, \
T];\nsnegrealfunctions[xi];\nsnegmomentumindexes[{k}, Ns];\nhk = \
First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, \
-k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> \
xi[k];\nF = FullSimplify[ExpToTrig[gaussianaverage[nc[ck[AN, -k, DO], ck[AN, \
k, UP]], hk, {ck}, T, \"Momentum\" -> k] /. xi[k] -> x], Dl > 0 && T > 0 && \
Element[x, Reals]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"Dl", " ", 
          RowBox[{"Tanh", "[", FractionBox[SqrtBox[RowBox[{SuperscriptBox[
                 "Dl", "2"], "+", SuperscriptBox["x", "2"]}]], 
             RowBox[{"2", " ", "T"}]], "]"}]}], 
        RowBox[{"2", " ", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
             SuperscriptBox["x", "2"]}]]}]], "Out[6]="}}|>, 
   <|"Code" -> "FindRoot[Dl == 3/4 Sum[F /. {x -> -2 Cos[q] + 1/2, T -> \
1/10}, {q, {0, Pi/2, Pi, 3 Pi/2}}], {Dl, 1}]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"Dl", "\[Rule]", 
           "1.0296990620785584`"}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegbosonoperators[bk];\nsnegrealconstants[n0U];\nsnegrealfun\
ctions[ek];\nsnegmomentumindexes[{k}, Ns];\nhb = sum[(ek[k] + n0U) \
number[bk[k]] + n0U/2 (nc[bk[CR, k], bk[CR, -k]] + nc[bk[AN, -k], bk[AN, \
k]]), {k}];\nhk = First[bdghamiltonian[hb, {bk}, k]] /. ek[-k] -> ek[k];\nnk \
= gaussianaverage[number[bk[k]], hk, {bk}, 0, \"Momentum\" -> \
k];\nSimplify[nk - ((ek[k] + n0U)/Sqrt[ek[k] (ek[k] + 2 n0U)] - 1)/2, ek[k] > \
0 && n0U > 0]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[8]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH = -hop[c[1], c[2]] + 1/3 \
number[c[1]];\nobs = nc[number[c[1], UP], number[c[2], DO]] + nc[c[CR, 1, \
UP], c[AN, 2, UP]];\n{gaussianaverage[obs, H, {c[1], c[2]}, 1/2], \
thermalaverage[obs, H, {c[1], c[2]}, 1/2]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"0.5869247923244236`", ",", 
           "0.5869247923244238`"}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nH0 = -hop[c[1], c[2]] + 1/5 \
number[c[1]];\nHint = 2 (hubbard[c[1]] + hubbard[c[2]]);\nmf = \
meanfield[Hint];\nrules = Map[# -> gaussianaverage[First[#], H0, {c[1], \
c[2]}, 1/3] &, Union[Cases[mf, _avg, Infinity]]];\n{gaussianaverage[mf /. \
rules, H0, {c[1], c[2]}, 1/3], gaussianaverage[Hint, H0, {c[1], c[2]}, 1/3]}"\
, "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.9547318937243758`", ",", 
           "0.9547318937243758`"}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[d];\nsnegrealconstants[eps];\n{gaussiana\
verage[number[d[], UP], eps number[d[]], {d}, 0], gaussianaverage[number[d[], \
UP], -number[d[]], {d}, 0]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "1.`"}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\n{gaussianaverage\
[number[c[1]] + number[c[2]], -hop[c[1], c[2]], {c[1], c[2]}, 0], \
gaussianaverage[number[c[1]] + number[c[2]], -hop[c[1], c[2]] - 3/2 \
(number[c[1]] + number[c[2]]), {c[1], c[2]}, 0]}", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{"2.`", ",", "4.000000000000001`"}], 
         "}"}], "Out[2]="}}|>, <|"Code" -> "snegfermionoperators[c];\ngaussia\
naverage[number[c[3]], -hop[c[1], c[2]], {c[1], c[2]}, 0]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "gaussianaverage::op: The operator c[0, 3, 0] is not one of the \
modes."}, {"Output", "$Failed", "Out[2]="}}|>}|>

<|"Source" -> "blochhamiltonian.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nHk = fourier[-t sum[hop[c[R], c[R + 1]], \
{R}], c -> ck, R -> k];\nh = blochhamiltonian[Hk, {ck}, k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{RowBox[{RowBox[{"-", "t"}], " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                    DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[
                {RowBox[{RowBox[{"-", "t"}], " ", TagBox[SuperscriptBox["e", 
                     RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                       "\[CenterDot]", "1"}]], DisplayForm]}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                    DisplayForm]}]}]}], "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "ExpToTrig[blochphaseexp[h]]", "Reset" -> False, 
    "InLabel" -> "In[7]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
                 RowBox[{"Cos", "[", "k", "]"}]}], ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", RowBox[{RowBox[{"-", "2"}], " ", 
                 "t", " ", RowBox[{"Cos", "[", "k", "]"}]}]}], "}"}]}], 
         "}"}], "Out[7]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nH = -t \
sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}];\nh = \
blochhamiltonian[fourier[H, f -> fk, R -> k], {fk}, \
k];\nSimplify[ExpToTrig[blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, 0}, a2 -> \
{0, 1}}]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
             RowBox[{"(", RowBox[{RowBox[{"Cos", "[", "kx", "]"}], "+", 
                 RowBox[{"Cos", "[", "ky", "]"}]}], ")"}]}], "}"}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t, B];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = -t sum[hop[c[R], c[R + 1]], {R}] + B \
sum[spinz[c[R]], {R}];\nblochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, \
k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{FractionBox["B", "2"], 
                 "-", RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}], ",", RowBox[{"{", RowBox[{"0", ",", RowBox[
                {RowBox[{"-", FractionBox["B", "2"]}], "-", RowBox[
                  {"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}]}], "}"}]}], 
         "}"}], "Out[6]="}}|>, <|"Code" -> "snegbosonoperators[b, \
bk];\nsnegrealconstants[w0, J];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[w0 nc[b[CR, R], b[AN, R]] + J \
(nc[b[CR, R], b[AN, R + 1]] + nc[b[CR, R + 1], b[AN, R]]), \
{R}];\nblochhamiltonian[fourier[H, b -> bk, R -> k], {bk}, k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{"w0", "+", RowBox[{"J", " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "1"}]], DisplayForm]}], "+", 
             RowBox[{"J", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                DisplayForm]}]}], "}"}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nblochhamiltonian[fourier[-t \
sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, k, \"Check\" -> \
False]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{RowBox[
                  {RowBox[{"-", "t"}], " ", TagBox[SuperscriptBox["e", 
                     RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                       "\[CenterDot]", "1"}]], DisplayForm]}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                    DisplayForm]}]}], ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", RowBox[{RowBox[{RowBox[{"-", "t"}], 
                   " ", TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                    DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}]}], "}"}]}], 
         "}"}], "Out[5]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nHx = fourier[-t sum[hop[f[\"x\", R], \
f[\"x\", R + 1]], {R}], f -> fk, R -> k, \"IndexPosition\" -> \
2];\nblochhamiltonian[Hx, {fk[\"x\"]}, k, \"IndexPosition\" -> 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "t"}], " ", TagBox[
                SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                    {"-", "k"}], "\[CenterDot]", "1"}]], DisplayForm]}], "-", 
             RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                  {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                DisplayForm]}]}], "}"}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[v, \
w];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHssh = \
fourier[sum[v hop[f[R, \"A\"], f[R, \"B\"]] + w hop[f[R + 1, \"A\"], f[R, \
\"B\"]], {R}], f -> fk, R -> k];\nblochhamiltonian[Hssh, {fk[\"A\"], \
fk[\"B\"]}, k, \"OrbitalPositions\" -> {0, 1/2}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", RowBox[{RowBox[{"(", RowBox[
                    {"v", "+", RowBox[{"w", " ", TagBox[SuperscriptBox["e", 
                         RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                          "\[CenterDot]", "1"}]], DisplayForm]}]}], ")"}], 
                 " ", TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                     "k", "\[CenterDot]", FractionBox["1", "2"]}]], 
                  DisplayForm]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{TagBox[SuperscriptBox["e", 
                   RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                     "\[CenterDot]", FractionBox["1", "2"]}]], DisplayForm], 
                 " ", RowBox[{"(", RowBox[{"v", "+", RowBox[{"w", " ", 
                       TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                          "k", "\[CenterDot]", "1"}]], DisplayForm]}]}], 
                   ")"}]}], ",", "0"}], "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[v, \
w];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHssh = \
fourier[sum[v hop[f[R, \"A\"], f[R, \"B\"]] + w hop[f[R + 1, \"A\"], f[R, \
\"B\"]], {R}], f -> fk, R -> k];\nh = blochhamiltonian[Hssh, {fk[\"A\"], \
fk[\"B\"]}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", RowBox[{"v", "+", 
                 RowBox[{"w", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "1"}]], DisplayForm]}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"v", "+", RowBox[{"w", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "Simplify[Eigenvalues[ExpToTrig[blochphaseexp[h]]]]", 
    "Reset" -> False, "InLabel" -> "In[7]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["v", "2"], 
                "+", SuperscriptBox["w", "2"], "+", RowBox[{"2", " ", "v", 
                  " ", "w", " ", RowBox[{"Cos", "[", "k", "]"}]}]}]]}], ",", 
           SqrtBox[RowBox[{SuperscriptBox["v", "2"], "+", SuperscriptBox["w", 
               "2"], "+", RowBox[{"2", " ", "v", " ", "w", " ", 
                RowBox[{"Cos", "[", "k", "]"}]}]}]]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nsneglatticevectors[a1, a2];\nHg = -t \
sum[hop[f[R, \"A\"], f[R, \"B\"]] + hop[f[R, \"A\"], f[R - a1, \"B\"]] + \
hop[f[R, \"A\"], f[R - a2, \"B\"]], {R}];\nh = blochhamiltonian[fourier[Hg, f \
-> fk, R -> k], {fk[\"A\"], fk[\"B\"]}, k]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", RowBox[{RowBox[{"-", "t"}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                       "a1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "a2"}]], 
                    DisplayForm]}]}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "t"}], "-", 
                 RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                      {"\[ImaginaryI]", "k", "\[CenterDot]", "a1"}]], 
                    DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "a2"}]], DisplayForm]}]}], ",", "0"}], 
             "}"}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "Simplify[blochphaseexp[h, {k -> {4 Pi/3, 0}, a1 -> {1, 0}, a2 \
-> {1/2, Sqrt[3]/2}}]]", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t, alpha];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[matrixhop[-t IdentityMatrix[2] + \
I alpha PauliMatrix[2], c[R], c[R + 1]], {R}];\nh = \
blochhamiltonian[fourier[H, c -> ck, R -> k], {ck}, \
k];\nSimplify[ExpToTrig[blochphaseexp[h]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "t", " ", 
                 RowBox[{"Cos", "[", "k", "]"}]}], ",", RowBox[{"2", " ", 
                 "\[ImaginaryI]", " ", "alpha", " ", RowBox[{"Sin", "[", "k", 
                   "]"}]}]}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", "\[ImaginaryI]", 
                 " ", "alpha", " ", RowBox[{"Sin", "[", "k", "]"}]}], ",", 
               RowBox[{RowBox[{"-", "2"}], " ", "t", " ", RowBox[{"Cos", "[", 
                   "k", "]"}]}]}], "}"}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[t, \
Delta];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHcdw = \
sum[-t hop[f[R, \"A\"], f[R, \"B\"]] - t hop[f[R, \"B\"], f[R + 1, \"A\"]] + \
Delta (number[f[R, \"A\"]] - number[f[R, \"B\"]]), {R}];\nh = \
blochhamiltonian[fourier[Hcdw, f -> fk, R -> k], {fk[\"A\"], fk[\"B\"]}, \
k];\nSimplify[Eigenvalues[ExpToTrig[blochphaseexp[h]]]]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["Delta", "2"], "+", 
                RowBox[{"2", " ", SuperscriptBox["t", "2"]}], "+", 
                RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
                  RowBox[{"Cos", "[", "k", "]"}]}]}]]}], ",", 
           SqrtBox[RowBox[{SuperscriptBox["Delta", "2"], "+", 
              RowBox[{"2", " ", SuperscriptBox["t", "2"]}], "+", 
              RowBox[{"2", " ", SuperscriptBox["t", "2"], " ", 
                RowBox[{"Cos", "[", "k", "]"}]}]}]]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[v, \
w];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHssh = \
fourier[sum[v hop[f[R, \"A\"], f[R, \"B\"]] + w hop[f[R + 1, \"A\"], f[R, \
\"B\"]], {R}], f -> fk, R -> k];\nh1 = blochhamiltonian[Hssh, {fk[\"A\"], \
fk[\"B\"]}, k];\nh2 = blochhamiltonian[Hssh, {fk[\"A\"], fk[\"B\"]}, k, \
\"OrbitalPositions\" -> {0, 1/2}];\nSimplify[blochphaseexp[Det[h1] - \
Det[h2]]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "0", "Out[8]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[Delta];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nblochhamiltonian[fourier[Delta \
sum[blochphase[Pi, R] number[f[R]], {R}], f -> fk, R -> k], {fk}, k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "blochhamiltonian::transl: The Hamiltonian is not \
translation invariant for the given orbitals; the residual off-diagonal part \
is {{Delta KroneckerDelta[k, k28 + Pi]}}."}, {"Output", "$Failed", 
       "Out[5]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[Delta];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nblochhamiltonian[fourier[Delta \
sum[nc[f[CR, R], f[CR, R + 1]] + nc[f[AN, R + 1], f[AN, R]], {R}], f -> fk, R \
-> k], {fk}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "blochhamiltonian::anom: The Hamiltonian contains \
terms that do not conserve the particle number (anomalous or linear terms), \
which are not described by h(k); use bdghamiltonian."}, 
      {"Output", "$Failed", "Out[5]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[fk];\nsnegmomentumindexes[{k}, \
Ns];\nblochhamiltonian[sum[I nc[fk[CR, k, \"A\", 0], fk[AN, k, \"B\", 0]], \
{k}], {fk[\"A\"], fk[\"B\"]}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", 
       "blochhamiltonian::herm: The Bloch Hamiltonian is not Hermitian."}, 
      {"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{"0", ",", "\[ImaginaryI]"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c, \
ck];\nsnegrealconstants[t];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nblochhamiltonian[-t sum[hop[c[R], c[R + \
1]], {R}], {ck}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], ",", 
           RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}]}], "}"}], 
       "Out[5]="}}|>}|>

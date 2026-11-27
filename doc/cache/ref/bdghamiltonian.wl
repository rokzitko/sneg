<|"Source" -> "bdghamiltonian.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t, mu, Dl];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[-t hop[f[R], f[R + 1]] - mu \
number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], \
f[AN, R, 0]]), {R}];\nHk = fourier[H, f -> fk, R -> k];\n{hk, E0} = \
bdghamiltonian[Hk, {fk}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{RowBox[{"-", "mu"}], "-", RowBox[{"t", " ", 
                       TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                          RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                        DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                          "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", 
                   RowBox[{RowBox[{RowBox[{"-", "Dl"}], " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                          {"-", "k"}], "\[CenterDot]", "1"}]], 
                        DisplayForm]}], "+", RowBox[{"Dl", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                          "\[CenterDot]", "1"}]], DisplayForm]}]}]}], "}"}], 
               ",", RowBox[{"{", RowBox[{RowBox[{RowBox[{"Dl", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                          {"-", "k"}], "\[CenterDot]", "1"}]], 
                        DisplayForm]}], "-", RowBox[{"Dl", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                          "\[CenterDot]", "1"}]], DisplayForm]}]}], ",", 
                   RowBox[{"mu", "+", RowBox[{"t", " ", TagBox[SuperscriptBox[
                         "e", RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                          "\[CenterDot]", "1"}]], DisplayForm]}], "+", 
                     RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                          {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                        DisplayForm]}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{"-", FractionBox[RowBox[{"mu", " ", "Ns"}], "2"]}]}], 
         "}"}], "Out[7]="}}|>, 
   <|"Code" -> 
     "Simplify[ExpToTrig[blochphaseexp[hk, {k -> q}]]] // MatrixForm", 
    "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{RowBox[{RowBox[{"-", "mu"}], "-", RowBox[{"2", " ", "t", 
                 " ", RowBox[{"Cos", "[", "q", "]"}]}]}], 
             RowBox[{"2", " ", "\[ImaginaryI]", " ", "Dl", " ", RowBox[
                {"Sin", "[", "q", "]"}]}]}, {RowBox[{RowBox[{"-", "2"}], " ", 
               "\[ImaginaryI]", " ", "Dl", " ", RowBox[{"Sin", "[", "q", 
                 "]"}]}], RowBox[{"mu", "+", RowBox[{"2", " ", "t", " ", 
                 RowBox[{"Cos", "[", "q", "]"}]}]}]}}, RowSpacings -> 1, 
           ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[8]//MatrixForm="}}|>, <|"Code" -> "Simplify[ExpToTrig[blochphasee\
xp[Eigenvalues[hk]^2, {k -> q}]] - ((2 t Cos[q] + mu)^2 + 4 Dl^2 Sin[q]^2)]", 
    "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0", ",", "0"}], "}"}], 
       "Out[9]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegrealconstan\
ts[Dl];\nsnegrealfunctions[xi];\nsnegmomentumindexes[{k}, Ns];\nHB = \
sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, \
-k, DO], ck[AN, k, UP]]), {k}];\n{hk, E0} = bdghamiltonian[HB, {ck}, \
k];\n{MatrixForm[hk], E0}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {RowBox[{"xi", "[", "k", "]"}], "0", "0", RowBox[
                  {"-", "Dl"}]}, {"0", RowBox[{"xi", "[", "k", "]"}], "Dl", 
                 "0"}, {"0", "Dl", RowBox[{"-", RowBox[{"xi", "[", RowBox[
                      {"-", "k"}], "]"}]}], "0"}, {RowBox[{"-", "Dl"}], "0", 
                 "0", RowBox[{"-", RowBox[{"xi", "[", RowBox[{"-", "k"}], 
                     "]"}]}]}}, RowSpacings -> 1, ColumnSpacings -> 1, 
               RowAlignments -> Baseline, ColumnAlignments -> Center], 
              "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
              BoxForm`e$]]], ",", RowBox[{"sum", "[", 
             RowBox[{RowBox[{"xi", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                 "}"}]}], "]"}]}], "}"}], "Out[7]="}}|>, 
   <|"Code" -> "bogoliubov[hk /. xi[-k] -> xi[k], \"Fermion\", \"Momentum\" \
-> True][[1]]", "Reset" -> False, "InLabel" -> "In[8]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
              SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]], ",", 
           SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", SuperscriptBox[
               RowBox[{"xi", "[", "k", "]"}], "2"]}]], ",", 
           RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
                SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]]}], ",", 
           RowBox[{"-", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
                SuperscriptBox[RowBox[{"xi", "[", "k", "]"}], "2"]}]]}]}], 
         "}"}], "Out[8]="}}|>, <|"Code" -> "snegspinlessfermionoperators[f, \
fk];\nsnegrealconstants[t, m, Dl];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nH = sum[-t (hop[f[R, \"A\"], f[R, \
\"B\"]] + hop[f[R, \"B\"], f[R + 1, \"A\"]]) + m (number[f[R, \"A\"]] - \
number[f[R, \"B\"]]) + Dl (nc[f[CR, R, \"A\", 0], f[CR, R + 1, \"A\", 0]] + \
nc[f[AN, R + 1, \"A\", 0], f[AN, R, \"A\", 0]]), {R}];\n{hk, E0} = \
bdghamiltonian[fourier[H, f -> fk, R -> k], {fk[\"A\"], fk[\"B\"]}, \
k];\nMatrixForm[hk]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", TagBox[RowBox[{"(", "\[NoBreak]", 
          GridBox[{{"m", RowBox[{RowBox[{"-", "t"}], "-", RowBox[
                {"t", " ", TagBox[SuperscriptBox["e", RowBox[
                    {"\[ImaginaryI]", RowBox[{"-", "k"}], "\[CenterDot]", 
                     "1"}]], DisplayForm]}]}], RowBox[{RowBox[
                {RowBox[{"-", "Dl"}], " ", TagBox[SuperscriptBox["e", 
                   RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                     "\[CenterDot]", "1"}]], DisplayForm]}], "+", RowBox[
                {"Dl", " ", TagBox[SuperscriptBox["e", RowBox[
                    {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                  DisplayForm]}]}], "0"}, {RowBox[{RowBox[{"-", "t"}], "-", 
               RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                    {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                  DisplayForm]}]}], RowBox[{"-", "m"}], "0", "0"}, 
            {RowBox[{RowBox[{"Dl", " ", TagBox[SuperscriptBox["e", 
                   RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                     "\[CenterDot]", "1"}]], DisplayForm]}], "-", RowBox[
                {"Dl", " ", TagBox[SuperscriptBox["e", RowBox[
                    {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                  DisplayForm]}]}], "0", RowBox[{"-", "m"}], 
             RowBox[{"t", "+", RowBox[{"t", " ", TagBox[SuperscriptBox["e", 
                   RowBox[{"\[ImaginaryI]", RowBox[{"-", "k"}], 
                     "\[CenterDot]", "1"}]], DisplayForm]}]}]}, 
            {"0", "0", RowBox[{"t", "+", RowBox[{"t", " ", TagBox[
                  SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                     "\[CenterDot]", "1"}]], DisplayForm]}]}], "m"}}, 
           RowSpacings -> 1, ColumnSpacings -> 1, RowAlignments -> Baseline, 
           ColumnAlignments -> Center], "\[NoBreak]", ")"}], 
        Function[BoxForm`e$, MatrixForm[BoxForm`e$]]], 
       "Out[7]//MatrixForm="}}|>, <|"Code" -> "snegspinoperators[{S, \
s}];\nsnegbosonoperators[b, \
bk];\nsnegrealconstants[J];\nsneglatticeindexes[{R}, \
Ns];\nsnegmomentumindexes[{k}, Ns];\nh = J sum[spinspin[S[R, \"A\"], S[R, \
\"B\"]] + spinspin[S[R, \"B\"], S[R + 1, \"A\"]], {R}];\nh = rotatespin[h, S, \
If[Last[{##}] === \"B\", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] \
&];\nhk = fourier[holsteinprimakoff[h, S -> b, 2], b -> bk, R -> k];\n{Mk, \
E0} = bdghamiltonian[hk, {bk[\"A\"], bk[\"B\"]}, k];\n{MatrixForm[Mk], E0}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{TagBox[RowBox[{"(", "\[NoBreak]", GridBox[{
                {RowBox[{"2", " ", "J", " ", "s"}], "0", "0", RowBox[
                  {RowBox[{"J", " ", "s"}], "+", RowBox[{"J", " ", "s", " ", 
                     TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                         RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                      DisplayForm]}]}]}, {"0", RowBox[{"2", " ", "J", " ", 
                   "s"}], RowBox[{RowBox[{"J", " ", "s"}], "+", RowBox[
                    {"J", " ", "s", " ", TagBox[SuperscriptBox["e", RowBox[
                        {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                      DisplayForm]}]}], "0"}, {"0", RowBox[
                  {RowBox[{"J", " ", "s"}], "+", RowBox[{"J", " ", "s", " ", 
                     TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                         RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                      DisplayForm]}]}], RowBox[{"2", " ", "J", " ", "s"}], 
                 "0"}, {RowBox[{RowBox[{"J", " ", "s"}], "+", RowBox[
                    {"J", " ", "s", " ", TagBox[SuperscriptBox["e", RowBox[
                        {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                      DisplayForm]}]}], "0", "0", RowBox[{"2", " ", "J", " ", 
                   "s"}]}}, RowSpacings -> 1, ColumnSpacings -> 1, 
               RowAlignments -> Baseline, ColumnAlignments -> Center], 
              "\[NoBreak]", ")"}], Function[BoxForm`e$, MatrixForm[
              BoxForm`e$]]], ",", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", 
               "J", " ", "Ns", " ", "s"}], "-", RowBox[{"2", " ", "J", " ", 
               "Ns", " ", SuperscriptBox["s", "2"]}]}]}], "}"}], 
       "Out[10]="}}|>, <|"Code" -> "w = bogoliubov[Mk, \"Boson\", \
\"Momentum\" -> True][[1]];\nSimplify[ExpToTrig[blochphaseexp[w^2, {k -> \
q}]]]", "Reset" -> False, "InLabel" -> "In[11]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{RowBox[{"-", "2"}], " ", SuperscriptBox["J", 
              "2"], " ", SuperscriptBox["s", "2"], " ", 
             RowBox[{"(", RowBox[{RowBox[{"-", "1"}], "+", RowBox[
                  {"Cos", "[", "q", "]"}]}], ")"}]}], ",", 
           RowBox[{RowBox[{"-", "2"}], " ", SuperscriptBox["J", "2"], " ", 
             SuperscriptBox["s", "2"], " ", RowBox[{"(", RowBox[
                {RowBox[{"-", "1"}], "+", RowBox[{"Cos", "[", "q", "]"}]}], 
               ")"}]}], ",", RowBox[{RowBox[{"-", "2"}], " ", 
             SuperscriptBox["J", "2"], " ", SuperscriptBox["s", "2"], " ", 
             RowBox[{"(", RowBox[{RowBox[{"-", "1"}], "+", RowBox[
                  {"Cos", "[", "q", "]"}]}], ")"}]}], ",", 
           RowBox[{RowBox[{"-", "2"}], " ", SuperscriptBox["J", "2"], " ", 
             SuperscriptBox["s", "2"], " ", RowBox[{"(", RowBox[
                {RowBox[{"-", "1"}], "+", RowBox[{"Cos", "[", "q", "]"}]}], 
               ")"}]}]}], "}"}], "Out[12]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[fk];\nsnegrealconstants[t];\nsne\
gmomentumindexes[{k}, Ns];\nbdghamiltonian[sum[-2 t Cos[k] nc[fk[CR, \"x\", \
k, 0], fk[AN, \"x\", k, 0]], {k}], {fk[\"x\"]}, k, \"IndexPosition\" -> 2]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{RowBox[{"-", "2"}], " ", "t", " ", RowBox[
                      {"Cos", "[", "k", "]"}]}], ",", "0"}], "}"}], ",", 
               RowBox[{"{", RowBox[{"0", ",", RowBox[{"2", " ", "t", " ", 
                     RowBox[{"Cos", "[", "k", "]"}]}]}], "}"}]}], "}"}], ",", 
           RowBox[{RowBox[{"-", "t"}], " ", RowBox[{"sum", "[", RowBox[
                {RowBox[{"Cos", "[", "k", "]"}], ",", RowBox[{"{", "k", 
                   "}"}]}], "]"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[fk];\nsnegmomentumindexes[{k}, \
Ns];\nbdghamiltonian[sum[nc[fk[CR, k, 0], fk[AN, -k, 0]], {k}], {fk}, k]", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bdghamiltonian::transl: The Hamiltonian is not \
translation invariant for the given orbitals; the residual off-diagonal part \
is {{KroneckerDelta[-k, k13], 0}, {0, -KroneckerDelta[-k, k13]}}."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "bdghamiltonian[sum[nc[fk[CR, k, 0], fk[AN, -k, 0]], {k}], \
{fk}, k, \"Check\" -> False]", "Reset" -> False, "InLabel" -> "In[4]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {TemplateBox[{RowBox[{RowBox[{"-", "k"}], ",", "k"}]}, 
                    "KroneckerDeltaSeq"], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", RowBox[{"-", TemplateBox[
                      {RowBox[{RowBox[{"-", "k"}], ",", "k"}]}, 
                      "KroneckerDeltaSeq"]}]}], "}"}]}], "}"}], ",", 
           RowBox[{FractionBox["1", "2"], " ", RowBox[{"sum", "[", RowBox[
                {TemplateBox[{RowBox[{RowBox[{"-", "k"}], ",", "k"}]}, 
                  "KroneckerDeltaSeq"], ",", RowBox[{"{", "k", "}"}]}], 
               "]"}]}]}], "}"}], "Out[4]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[t, \
mu, Dl];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nH = \
sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + \
1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}];\nhk = \
First[bdghamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]];\ne2 = \
Simplify[ExpToTrig[blochphaseexp[Max[Eigenvalues[hk]]^2, {k -> \
q}]]];\nFactor[{e2 /. q -> 0, e2 /. q -> Pi}]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[{SuperscriptBox[
            RowBox[{"(", RowBox[{"mu", "+", RowBox[{"2", " ", "t"}]}], ")"}], 
            "2"], ",", SuperscriptBox[RowBox[{"(", RowBox[{"mu", "-", 
                RowBox[{"2", " ", "t"}]}], ")"}], "2"]}], "}"}], 
       "Out[8]="}}|>, <|"Code" -> "snegfermionoperators[ck];\nsnegrealconstan\
ts[Dl, x, T];\nsnegrealfunctions[xi];\nsnegmomentumindexes[{k}, Ns];\nhk = \
First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, \
-k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> \
xi[k];\nF = gaussianaverage[nc[ck[AN, -k, DO], ck[AN, k, UP]], hk, {ck}, T, \
\"Momentum\" -> k];\nFullSimplify[ExpToTrig[F /. xi[k] -> x], Dl > 0 && T > 0 \
&& Element[x, Reals]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", FractionBox[RowBox[{"Dl", " ", 
          RowBox[{"Tanh", "[", FractionBox[SqrtBox[RowBox[{SuperscriptBox[
                 "Dl", "2"], "+", SuperscriptBox["x", "2"]}]], 
             RowBox[{"2", " ", "T"}]], "]"}]}], 
        RowBox[{"2", " ", SqrtBox[RowBox[{SuperscriptBox["Dl", "2"], "+", 
             SuperscriptBox["x", "2"]}]]}]], "Out[7]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[t, \
t2];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nHk = \
fourier[sum[-t hop[f[R], f[R + 1]] + I t2 (nc[f[CR, R + 2, 0], f[AN, R, 0]] - \
nc[f[CR, R, 0], f[AN, R + 2, 0]]), {R}], f -> fk, R -> \
k];\n{First[bdghamiltonian[Hk, {fk}, k]], blochhamiltonian[Hk, {fk}, k]}", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{RowBox[{"\[ImaginaryI]", " ", "t2", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                          {RowBox[{"-", "2"}], " ", "k"}], "\[CenterDot]", 
                          "1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                       TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                          RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                        DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                          "\[CenterDot]", "1"}]], DisplayForm]}], "-", 
                     RowBox[{"\[ImaginaryI]", " ", "t2", " ", TagBox[
                        SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                          {"2", " ", "k"}], "\[CenterDot]", "1"}]], 
                        DisplayForm]}]}], ",", "0"}], "}"}], ",", RowBox[
                {"{", RowBox[{"0", ",", RowBox[{RowBox[{"\[ImaginaryI]", " ", 
                       "t2", " ", TagBox[SuperscriptBox["e", RowBox[
                          {"\[ImaginaryI]", RowBox[{RowBox[{"-", "2"}], " ", 
                          "k"}], "\[CenterDot]", "1"}]], DisplayForm]}], "+", 
                     RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                          {"\[ImaginaryI]", RowBox[{"-", "k"}], 
                          "\[CenterDot]", "1"}]], DisplayForm]}], "+", 
                     RowBox[{"t", " ", TagBox[SuperscriptBox["e", RowBox[
                          {"\[ImaginaryI]", "k", "\[CenterDot]", "1"}]], 
                        DisplayForm]}], "-", RowBox[{"\[ImaginaryI]", " ", 
                       "t2", " ", TagBox[SuperscriptBox["e", RowBox[
                          {"\[ImaginaryI]", RowBox[{"2", " ", "k"}], 
                          "\[CenterDot]", "1"}]], DisplayForm]}]}]}], 
                 "}"}]}], "}"}], ",", RowBox[{"{", RowBox[{"{", RowBox[
                {RowBox[{"\[ImaginaryI]", " ", "t2", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                        {RowBox[{"-", "2"}], " ", "k"}], "\[CenterDot]", 
                       "1"}]], DisplayForm]}], "-", RowBox[{"t", " ", 
                   TagBox[SuperscriptBox["e", RowBox[{"\[ImaginaryI]", 
                       RowBox[{"-", "k"}], "\[CenterDot]", "1"}]], 
                    DisplayForm]}], "-", RowBox[{"t", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", "k", 
                       "\[CenterDot]", "1"}]], DisplayForm]}], "-", 
                 RowBox[{"\[ImaginaryI]", " ", "t2", " ", TagBox[
                    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", RowBox[
                        {"2", " ", "k"}], "\[CenterDot]", "1"}]], 
                    DisplayForm]}]}], "}"}], "}"}]}], "}"}], "Out[6]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[f, fk];\nsnegrealconstants[t, \
mu, Dl];\nsneglatticeindexes[{R}, Ns];\nsnegmomentumindexes[{k}, Ns];\nH = \
sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + \
1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}];\nhk = \
First[bdghamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]];\nnum = \
blochphaseexp[hk /. {t -> 1, Dl -> 3/5, mu -> 3/10}, {k -> \
9/10}];\nbogoliubov[num, \"Fermion\"]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> {{"Message", "bogoliubov::phs: The \
matrix does not have the particle-hole symmetry of the BdG matrix of the \
modes (a, a^dagger); for Bloch BdG matrices H(k) use \"Momentum\" -> True."}, 
      {"Output", "$Failed", "Out[8]="}}|>, 
   <|"Code" -> "bogoliubov[num, \"Fermion\", \"Momentum\" -> True][[1]]", 
    "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"1.8069624458515028`", ",", 
           RowBox[{"-", "1.8069624458515028`"}]}], "}"}], "Out[9]="}}|>, 
   <|"Code" -> "snegspinlessfermionoperators[fk];\nsneglatticeindexes[{R}, \
Ns];\nbdghamiltonian[sum[number[fk[R]], {R}], {fk}, R]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Message", 
       "bdghamiltonian::index: R must be a declared momentum index."}, 
      {"Output", "$Failed", "Out[3]="}}|>, 
   <|"Code" -> "snegfermionoperators[ck];\nsnegmomentumindexes[{k}, \
Ns];\nbdghamiltonian[sum[nc[number[ck[k], UP], number[ck[k], DO]], {k}], \
{ck}, k]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Message", "bdghamiltonian::rest: The matrix element \
nc[ck[0, k25, 0], ck[1, k25, 0]] still contains operators or unevaluated \
sums."}, {"Output", "$Failed", "Out[3]="}}|>}|>

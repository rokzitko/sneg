(*
  Regression tests for examples/diagonalize.m.

  Run from the repository root with:
    wolframscript -file examples/test-diagonalize.m
*)

If[Check[Get["sneg.m"], $Failed] === $Failed,
  Print["Loading sneg.m failed."];
  Exit[1]
];
If[Check[Get["examples/diagonalize.m"], $Failed] === $Failed,
  Print["Loading examples/diagonalize.m failed."];
  Exit[1]
];

SetAttributes[test, HoldAll];
testpassed = 0;
testfailed = 0;

test[a_, b_] := Module[{aa, bb},
  aa = a;
  bb = b;
  If[aa =!= bb,
    Print["test[] failed."];
    Print[HoldForm[a], " ==> ", aa];
    Print["*******************"];
    Print[HoldForm[b], " ==> ", bb];
    testfailed++,
    testpassed++
  ];
];

Print["* diagonalize trace helpers *"];
snegfermionoperators[diagonalizeTestOp];

Block[{deg, beta = 0, vac, eigTest, expr, spectralBasis, spectralEig,
    spectralEigFast, spectralOps, tinyOp, tinySpectra, shiftedEig,
    shiftedEigFast, partition, shiftedSlow, shiftedFast},
  deg[_] = 1;
  makebasis[{diagonalizeTestOp[]}];
  vac = vacuum[];
  eigTest = {{{0}, {{0, 1},
    {vac, ap[diagonalizeTestOp[CR, UP], vac]}}}};
  expr = diagonalizeTestOp[CR, UP] + diagonalizeTestOp[AN, UP];
  test[
    {tdtr1expr[eigTest, expr], tdtr2expr[eigTest, expr],
      tdtr2exprALT[eigTest, expr]},
    {0, 0, 0}
  ];

  spectralBasis = qszbasisvc[{diagonalizeTestOp[]}];
  spectralEig = Map[
    {#[[1]], {ConstantArray[0, Length[#[[2]]]], #[[2]]}} &,
    spectralBasis];
  spectralEigFast = Map[
    {#[[1]], {ConstantArray[0, Length[#[[2]]]],
      IdentityMatrix[Length[#[[2]]]]}} &,
    spectralBasis];
  spectralOps = {
    diagonalizeTestOp[CR, UP],
    diagonalizeTestOp[AN, UP],
    diagonalizeTestOp[CR, DO],
    diagonalizeTestOp[AN, DO],
    number[diagonalizeTestOp[]],
    diagonalizeTestOp[CR, UP] + diagonalizeTestOp[AN, UP]
  };
  test[
    Map[
      Function[op, Expand[
        spectral2expr[spectralEig, op] -
          spectral2FAST[spectralEigFast,
            makeallmatricesbzvc[op, spectralBasis]]]],
      spectralOps],
    ConstantArray[0, Length[spectralOps]]
  ];

  tinyOp = 1.*^-6 diagonalizeTestOp[CR, UP];
  tinySpectra = {
    spectral2expr[spectralEig, tinyOp],
    spectral2FAST[spectralEigFast,
      makeallmatricesbzvc[tinyOp, spectralBasis]]
  };
  test[Map[FreeQ[#, Delta[_]]&, tinySpectra], {False, False}];

  shiftedEig[shift_] := Map[
    {#[[1]], {{N[#[[1, 1]] + 1 + shift]}, #[[2]]}} &,
    spectralBasis];
  shiftedEigFast[shift_] := Map[
    {#[[1]], {{N[#[[1, 1]] + 1 + shift]},
      IdentityMatrix[Length[#[[2]]]]}} &,
    spectralBasis];
  partition[e_] := Total[Exp[-beta Flatten[e[[All, 2, 1]]]]];

  beta = 100.;
  shiftedSlow = spectral2expr[shiftedEig[0.3],
      diagonalizeTestOp[CR, UP]] / partition[shiftedEig[0.3]];
  shiftedFast = spectral2FAST[shiftedEigFast[0.3],
      makeallmatricesbzvc[diagonalizeTestOp[CR, UP], spectralBasis]] /
    partition[shiftedEigFast[0.3]];
  test[
    Chop[Expand /@ ({shiftedSlow, shiftedFast} -
      ConstantArray[Delta[omega + 1.], 2])],
    {0, 0}
  ];
];

Print["* diagonalize small complex mixing *"];
Block[{Hmat, eig, eig2, bazavc, smallComponents},
  Hmat = {{{0, 0}, {{0., I 1.*^-11}, {-I 1.*^-11, 1.}}}};
  bazavc[{0, 0}] = IdentityMatrix[2];
  diagonalize[{}];
  smallComponents = Select[Abs[Flatten[eig[[1, 2, 2]]]],
    0 < # < 1.*^-10 &];
  test[Length[smallComponents], 2];
  test[
    Max[Abs[Map[Norm, eig[[All, 2, 2]], {2}] - 1]] < 1.*^-14,
    True
  ];
];

Print["*** DONE. ***"];
Print["Passed: ", testpassed, " Failed: ", testfailed];
If[testfailed == 0,
  Print["### PASSED! ###"];
  Exit[0],
  Exit[1]
];

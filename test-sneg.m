(* 
  Regression tests for SNEG library.

  Copyright 2006-2011 Rok Zitko, rok.zitko@ijs.si


   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA


  When new functionality is added, do two things:
   1. run the regression tests to see if it breaks old functionality
   2. add tests for the new functionality for future regression tests.
  Likewise, run the regression tests after each refactoring of the code.

  $Id: test-sneg.m,v 1.9 2007/12/03 15:46:57 rok Exp rok $
*)

(* CHANGE LOG:
  14. 7. 2011 - subtraction (with Expand) instead of comparison
*)

Check[reslt=Get[FileNameJoin[{DirectoryName[$InputFileName], "sneg.m"}]],
  Print["Messages detected. Aborting."];
  Exit[1];
];
If[reslt == $Failed,
  Print["Loading failed. Aborting."];
  Exit[1];
];

Print["test-sneg.m $Id: test-sneg.m,v 1.9 2007/12/03 15:46:57 rok Exp rok $"];

SetAttributes[test, HoldAll];

(* 'a' and 'b' are two expressions that will be tested for equality after
being explicitly evaluated. 'workingversion' is the last version where the
test succeeded. *)

testpassed = 0;
testfailed = 0;
testlabel = "";
testtimeout = 60;

(* If negate == True, we test for inequality! *)

test[a_, b_, workingversion_:0, negate_:False] := Module[
  {aa, bb, failed, evaluationFailed = Unique["testEvaluationFailed"]},
  aa = CheckAbort[TimeConstrained[
    Check[ReleaseHold[a], evaluationFailed], testtimeout, evaluationFailed],
    evaluationFailed];
  bb = CheckAbort[TimeConstrained[
    Check[ReleaseHold[b], evaluationFailed], testtimeout, evaluationFailed],
    evaluationFailed];
  failed = aa === evaluationFailed || bb === evaluationFailed ||
    If[negate === False, aa =!= bb, aa === bb];
  If[failed,
    Print["test[] failed.", If[testlabel === "", "", " " <> testlabel]];
    Print[HoldForm[a], " ==> ", aa];
    Print["*******************"];
    Print[HoldForm[b], " ==> ", bb];
    If[workingversion != 0, Print["Worked in version ", workingversion]];
    testfailed ++;
    Return[False],

    (* else *)

    testpassed ++;
    Return[True];
  ];
];

SetAttributes[testcase, HoldAll];
testcase[name_, actual_, expected_] :=
  Block[{testlabel = name}, test[actual, expected]];

(* Also catch failures in setup and in calculations performed before an
   assertion. A group must execute at least one assertion. *)
SetAttributes[testgroup, HoldRest];
testgroup[name_, body_] := Module[
  {before = testpassed + testfailed, result, failure = Unique["testGroupFailed"]},
  Print["** ", name, " **"];
  result = CheckAbort[TimeConstrained[Check[body, failure], 180, failure], failure];
  If[result === failure || before === testpassed + testfailed,
    Print["Test group failed or did not complete: ", name];
    testfailed++
  ];
];

(* Trap missing right sides in the tests *)
test[a_] := (
  Print["sneg-test error. Missing right side. Left=", a];
  testfailed++;
  False
);

(* Self-test: test if test[] really tests! *)
If[Block[{Print = Function[Null]}, test[ "self", "test" ]] === False,
  Print["*** Self-test OK. ***"];
  testfailed--,
  (* else *)
  Print["*** Self-test FAILED. ***"];
  Exit[1];
];

test::probe = "Intentional test-harness message.";
harnessProbes = Block[{$Messages = {}, Print = Function[Null],
    testpassed = 0, testfailed = 0, testtimeout = 1/10},
  {test[Message[test::probe]; 1, 1],
   test[Abort[], 1],
   test[Pause[1]; 1, 1],
   test[Message[test::probe]; 1, Message[test::probe]; 1],
   test[Abort[], 0, 0, True]}
];
If[harnessProbes =!= ConstantArray[False, 5],
  Print["*** Failure-handling self-test FAILED. ***"];
  Exit[1]
];

Print["** Package contexts **"];
test[$SnegVersion, "2.2.0"];
test[MemberQ[$Packages, "sneg`"], True];
test[MemberQ[$ContextPath, "Sneg`"], True];
test[Context[number], "Sneg`"];
test[Check[Needs["sneg`"]; True, False, Needs::nocont], True];

Print["** Listable functions **"];
test[nc[{a, b}, c], {nc[a, c], nc[b, c]}];
test[nc[{a, b}, {c, d}], {nc[a, c], nc[b, d]}];
test[nc[{a, b}, c, d], {nc[a, c, d], nc[b, c, d]}];
test[nc[a, {b, c}, d], {nc[a, b, d], nc[a, c, d]}];
test[nc[a, {b, c}, d, {e, f}, g], {nc[a, b, d, e, g], nc[a, c, d, f, g]}];
test[komutator[{a, b}, c], {nc[a, c] - nc[c, a], nc[b, c] - nc[c, b]}];
test[komutator[a, {b, c}], {nc[a, b] - nc[b, a], nc[a, c] - nc[c, a]}];
test[komutator[{a, b}, {c, d}], {nc[a, c] - nc[c, a], nc[b, d] - nc[d, b]}];
test[antikomutator[{a, b}, c], {nc[a, c] + nc[c, a], nc[b, c] + nc[c, b]}];
test[antikomutator[a, {b, c}], {nc[a, b] + nc[b, a], nc[a, c] + nc[c, a]}];
test[antikomutator[{a, b}, {c, d}], 
  {nc[a, c] + nc[c, a], nc[b, d] + nc[d, b]}];
test[normalorder[{a, b}], {a - vev[a], b - vev[b]}];
test[normalorderwick[{a, b}], {a - vevwick[a], b - vevwick[b]}];
test[conj[{a, b}], {conj[a], conj[b]}];
test[conj[{a, b, c}], {conj[a], conj[b], conj[c]}];
test[vev[{a, b}], {vev[a], vev[b]}];
test[vevwick[{a, b}], {vevwick[a], vevwick[b]}];
test[number[{a[], b[]}], {number[a[]], number[b[]]}];
test[isozsq[{a[], b[]}], {isozsq[a[]], isozsq[b[]]}];
test[hubbard[{a[], b[]}], {hubbard[a[]], hubbard[b[]]}];
test[spinxyz[{a[], b[]}], {spinxyz[a[]], spinxyz[b[]]}];
test[spinx[{a[], b[]}], {spinx[a[]], spinx[b[]]}];
test[spiny[{a[], b[]}], {spiny[a[]], spiny[b[]]}];
test[spinz[{a[], b[]}], {spinz[a[]], spinz[b[]]}];
test[spinss[{a[], b[]}], {spinss[a[]], spinss[b[]]}];
test[spinplus[{a[], b[]}], {spinplus[a[]], spinplus[b[]]}];
test[spinminus[{a[], b[]}], {spinminus[a[]], spinminus[b[]]}];
test[nambu[{a[], b[]}], {nambu[a[]], nambu[b[]]}];
test[isospinxyz[{a[], b[]}], {isospinxyz[a[]], isospinxyz[b[]]}];
test[isospinx[{a[], b[]}], {isospinx[a[]], isospinx[b[]]}];
test[isospiny[{a[], b[]}], {isospiny[a[]], isospiny[b[]]}];
test[isospinz[{a[], b[]}], {isospinz[a[]], isospinz[b[]]}];
test[basis[{a[], b[]}], {basis[a[]], basis[b[]]}];
test[pow[{a[], b[]}, 0], {1, 1}];
test[pow[{a[], b[]}, 1], {a[], b[]}];
test[pow[{a[], b[]}, 2], {nc[a[], a[]], nc[b[], b[]]}];
test[hop[{a[1], a[2]}, {b[1], b[2]}], {hop[a[1], b[1]], hop[a[2], b[2]]}];
test[hop[{a[1], a[3]}, a[2]], {hop[a[1], a[2]], hop[a[3], a[2]]}];
test[spinfliphop[{a[1], a[2]}, {b[1], b[2]}], {spinfliphop[a[1], b[1]], 
  spinfliphop[a[2], b[2]]}];
test[spinfliphop[{a[1], a[3]}, a[2]], 
  {spinfliphop[a[1], a[2]], spinfliphop[a[3], a[2]]}];
test[holehop[{a[1], a[2]}, {b[1], b[2]}], 
  {holehop[a[1], b[1]], holehop[a[2], b[2]]}];
test[holehop[{a[1], a[3]}, a[2]], 
  {holehop[a[1], a[2]], holehop[a[3], a[2]]}];
test[twohop[{a[1], a[2]}, {b[1], b[2]}], 
  {twohop[a[1], b[1]], twohop[a[2], b[2]]}];
test[twohop[{a[1], a[3]}, a[2]], {twohop[a[1], a[2]], twohop[a[3], a[2]]}];
test[invertspin[{a, b}], {invertspin[a], invertspin[b]}];
test[mbfunc[{a, b}], {mbfunc[a], mbfunc[b]}];
test[op2ndx[{a, b}], {op2ndx[a], op2ndx[b]}];
test[ndx2op[{n1, n2}], {ndx2op[n1], ndx2op[n2]}];
test[ap[{a, b}, {c, d}], {ap[a, c], ap[b, d]}];
test[ap[a, {b, c}], {ap[a, b], ap[a, c]}];
test[ap[{a, b}, c], {ap[a, c], ap[b, c]}];
test[vc2ops[{a, b}], {vc2ops[a], vc2ops[b]}];
test[scalarproduct[{a, b}, c], {scalarproduct[a, c], scalarproduct[b, c]}];
test[scalarproductvc[{a, b}, c], {scalarproduct[a, c], scalarproduct[b, c]}];
test[expvvc[{a, b}, c], 
  {scalarproduct[c, ap[a, c]], scalarproduct[c, ap[b, c]]}];
test[expvvc[a, {b, c}], 
    {scalarproduct[b, ap[a, b]], scalarproduct[c, ap[a, c]]}];
test[expvop[{a, b}, c], {vev[nc[conj[c], a, c]], vev[nc[conj[c], b, c]]}];
test[expvop[a, {b, c}], {vev[nc[conj[b], a, b]], vev[nc[conj[c], a, c]]}];
test[braket[a, {b, c}, d], 
  {scalarproduct[a, ap[b, d]], scalarproduct[a, ap[c, d]]}];
test[braket[{a, b}, c, d], 
  {scalarproduct[a, ap[c, d]], scalarproduct[b, ap[c, d]]}];
test[braket[a, b, {c, d}], 
  {scalarproduct[a, ap[b, c]], scalarproduct[a, ap[b, d]]}];
test[zeroonvac[{a, b}], {zeroonvac[a], zeroonvac[b]}];
test[normvc[{a, b}], {Sqrt[vev[scalarproduct[a, a]]],
  Sqrt[vev[scalarproduct[b, b]]]}];
test[wick[{a, b}], {wick[a], wick[b]}];  
  
(* Some definitions for later use *)
snegfermionoperators[c, d, e];
snegmajoranaoperators[m1, m2];
snegrealconstants[x];  

(** Tests **)
Sneg`nambar = "sentinel";
test[isospinxyz[c[]]; Sneg`nambar, "sentinel"];
Clear[Sneg`nambar];

test[operatorQ[nc], False ];

Print["** Operators **"];
test[ operatorQ[c], True ];
test[ operatorQ[c[CR]], True ];
test[ fermionQ[c], True ];
test[ c[CR, 1], N[c[CR, 1]], 1.56 ];

(* Default ordering *)
test[ ordering[c] == EMPTY, True];
test[ ordering[d] == EMPTY, True];
test[ ordering[e] == EMPTY, True];

Print["** Constants **"];
test[ Conjugate[x], x ];

Print["** Non-commutative multiplication nc[] **"];
test[ nc[], 1 ];
test[ nc[nc[]], 1 ];
test[ nc[nc[], nc[]], 1 ];
test[ nc[nc[nc[]]], 1 ];
test[ nc[nc[nc[], nc[]], nc[nc[], nc[]]], 1];

test[ nc[1], 1 ];
test[ nc[x], x ];
test[ nc[c], c ];

test[ nc[1+1], 2 ];
test[ nc[nc[1], nc[nc[2], nc[3]], nc[nc[4]]], 24 ];
test[ nc[nc[1], nc[4], nc[], nc[6]], 24 ];

test[ nc[2, 3], 6 ];
test[ nc[2, x], 2x ];
test[ nc[2, x^2], 2x^2 ];
test[ nc[2, Sin[x]], 2Sin[x] ];

test[ nc[2, a], 2 a ];
test[ nc[a, 2], 2 a ];

test[ nc[x, a], x a ];
test[ nc[a, x], x a ];

test[ nc[x^2, a], x^2 a ];
test[ nc[Sqrt[x], a], Sqrt[x] a ];
test[ nc[a, x^2], x^2 a ];

test[ nc[Sin[x], a], Sin[x] a ];
test[ nc[a, Sin[x]], Sin[x] a ];

test[ nc[2, b, c], 2 nc[b, c] ];
test[ nc[a, b, 2], 2 nc[a, b] ];
test[ nc[a, 2, b], 2 nc[a, b] ];
test[ nc[a, 2 b, c], 2 nc[a, b, c] ];

test[ nc[x, b, c], x nc[b, c] ];
test[ nc[a, b, x], x nc[a, b] ];
test[ nc[a, x, b], x nc[a, b] ];
test[ nc[a, x b, c], x nc[a, b, c] ];

test[ nc[x^2, b, c], x^2 nc[b, c] ];
test[ nc[a, b, x^2], x^2 nc[a, b] ];
test[ nc[a, x^2, b], x^2 nc[a, b] ];
test[ nc[a, x^2 b, c], x^2 nc[a, b, c] ];

test[ nc[a, nc[b, c], d], nc[a, b, c, d] ];

test[ nc[a+b], a+b ];
test[ nc[a, b+c, d], nc[a,b,d] + nc[a,c,d] ];
test[ nc[a, b+c], nc[a,b] + nc[a,c] ];
test[ nc[a+b, c], nc[a,c] + nc[b,c] ];
test[ nc[a+b+c], a+b+c];

test[ Expand @ nc[a, b, 3c, 4d, e + 2f, g, nc[h, nc[i]]],
  12 nc[a, b, c, d, e, g, h, i] + 24 nc[a, b, c, d, f, g, h, i] ];

Print["** Canonical sorting **"];
test[ nc[c[AN], c[CR]], 1 - nc[c[CR], c[AN]] ];
test[ nc[c[AN, UP], c[CR, UP]], 1 - nc[c[CR, UP], c[AN, UP]] ];
test[ nc[c[AN, UP], c[CR, DO]], -nc[c[CR, DO], c[AN, UP]] ];
test[ nc[c[CR, UP], c[CR, DO]], -nc[c[CR, DO], c[CR, UP]] ];

test[ nc[c[AN], d[CR]], -nc[d[CR], c[AN]] ];

Print["** Pauli rule **"];
test[ nc[c[CR], c[CR]], 0];
test[ nc[c[AN], c[AN]], 0];

Print["** Commutators **"];
test[ komutator[c, d], nc[c, d] - nc[d, c] ];
test[ antikomutator[c, d], nc[c, d] + nc[d, c] ];
test[ MatchQ[cmt[g[1, 2], g[1, 3]],
  HoldPattern[cmt[g[1, 2], g[1, 3]]]], True ];

snegspinoperators[spinTestA, spinTestB];
test[ cmt[spinTestA[1], spinTestB[2]], 0 ];
test[ komutator[spinTestA[1], spinTestB[2]], 0 ];
test[ commutator[
    spinTestA[spinSiteA, spinComponentA],
    spinTestB[spinSiteB, spinComponentB]], 0 ];
test[ komutator[spinTestA[1], spinTestA[2]], I spinTestA[3] ];

spinTestA /: cmt[
  spinTestA[spinHook], spinTestB[spinHook]] = customSpinCommutator;
test[ commutator[spinTestA[spinHook], spinTestB[spinHook]],
  customSpinCommutator ];

Print["** Canonical anti-commutators **"];
test[ antikomutator[c[CR], c[CR]], 0 ];
test[ antikomutator[c[AN], c[AN]], 0 ];
test[ antikomutator[c[CR], c[AN]], 1 ];

test[ antikomutator[c[CR], d[CR]], 0 ];
test[ antikomutator[c[CR], d[AN]], 0 ];
test[ antikomutator[c[AN], d[AN]], 0 ];

Print["** Exhaustive fermion ordering regression **"];

Module[
  {
    savedOrdering = DownValues[ordering], stateRows, modes,
    effectiveRank, expectedCrossOrderedQ, expectedSameOrderedQ,
    orderingSignature, expectedSignature, failureSummary,
    rowsC, rowsD, modeC, modeD, rowC, rowD, left, right,
    leftMode, rightMode, expected, actual, label,
    orientation,
    classificationFailures = {}, crossComparatorFailures = {},
    crossProductFailures = {}, sameComparatorFailures = {},
    crossAcmtFailures = {}, crossAnticommutatorFailures = {},
    orderingTestK1, orderingTestK2
  },

  (* Each row contains a label, an operator, its raw EMPTY rank, its
     effective SEA rank, and whether the SEA role is known. *)
  stateRows[op_, symbolicMomentum_] := {
    {"CR+", op[CR, 1], CR, CR, True},
    {"CR-", op[CR, -1], CR, AN, True},
    {"AN+", op[AN, 1], AN, AN, True},
    {"AN-", op[AN, -1], AN, CR, True},
    {"CR0", op[CR, 0], CR, CR, False},
    {"AN0", op[AN, 0], AN, AN, False},
    {"CRsym", op[CR, symbolicMomentum], CR, CR, False},
    {"ANsym", op[AN, symbolicMomentum], AN, AN, False},
    {"CRbare", op[CR], CR, CR, False},
    {"ANbare", op[AN], AN, AN, False}
  };

  modes = {EMPTY, SEA, NONE};
  rowsC = stateRows[c, orderingTestK1];
  rowsD = stateRows[d, orderingTestK2];

  effectiveRank[mode_, row_] := If[mode === SEA, row[[4]], row[[3]]];

  (* Different heads use an independent effective rank for each operand.
     NONE is an absolute barrier. *)
  expectedCrossOrderedQ[mode1_, state1_, mode2_, state2_] :=
    Module[{rank1, rank2},
      If[mode1 === NONE || mode2 === NONE, Return[True]];
      rank1 = effectiveRank[mode1, state1];
      rank2 = effectiveRank[mode2, state2];
      Which[
        rank1 === CR && rank2 === AN, True,
        rank1 === AN && rank2 === CR, False,
        rank1 === rank2, OrderedQ[{state1[[2]], state2[[2]]}],
        True, True
      ]
    ];

  (* This mirrors the existing same-head contract and is strictly a
     preservation check; the cross-head fix must not alter it. *)
  expectedSameOrderedQ[mode_, state1_, state2_] := Which[
    mode === NONE, True,
    mode === EMPTY, OrderedQ[{state1[[2]], state2[[2]]}],
    mode === SEA && state1[[5]] && state2[[5]] &&
      state1[[4]] === CR && state2[[4]] === AN, True,
    mode === SEA && state1[[5]] && state2[[5]] &&
      state1[[4]] === AN && state2[[4]] === CR, False,
    True, OrderedQ[{state1[[2]], state2[[2]]}]
  ];

  (* Convert an evaluated product to inert data. This prevents the expected
     expression from being canonicalized by the implementation under test. *)
  orderingSignature[expr_] := Module[{value = expr},
    Which[
      Head[value] === nc, {1, List @@ value},
      MatchQ[value, Times[-1, _nc]], {-1, List @@ (-value)},
      True, {"unexpected", value}
    ]
  ];

  expectedSignature[isOrdered_, op1_, op2_] :=
    If[isOrdered, {1, {op1, op2}}, {-1, {op2, op1}}];

  failureSummary[failures_List] := {
    Length[failures],
    If[failures === {}, {}, Take[failures, Min[Length[failures], 12]]]
  };

  Block[{ordering},
    DownValues[ordering] = savedOrdering;

    (* Verify all built-in creation/annihilation classifications, including
       the zero, symbolic, and missing-momentum fallback classes. *)
    Do[
      ordering[c] = modeC;
      Do[
        expected = Which[
          modeC === SEA && rowC[[5]],
            {rowC[[4]] === CR, rowC[[4]] === AN},
          modeC === SEA,
            {False, False},
          True,
            {rowC[[3]] === CR, rowC[[3]] === AN}
        ];
        actual = {
          TrueQ[iscreation[rowC[[2]]]],
          TrueQ[isannihilation[rowC[[2]]]]
        };
        If[actual =!= expected,
          AppendTo[classificationFailures,
            {{modeC, rowC[[1]]}, actual, expected}]
        ],
        {rowC, rowsC}
      ],
      {modeC, modes}
    ];

    (* Exhaustive 3x3 ordering-mode matrix, ten state classes per head,
       and both head orientations: 1800 distinct pair situations. *)
    Do[
      ordering[c] = modeC;
      ordering[d] = modeD;
      Do[
        Do[
          left = If[orientation === 1, rowC, rowD];
          right = If[orientation === 1, rowD, rowC];
          leftMode = If[orientation === 1, modeC, modeD];
          rightMode = If[orientation === 1, modeD, modeC];
          label = {
            modeC, modeD, rowC[[1]], rowD[[1]],
            If[orientation === 1, "c,d", "d,c"]
          };

          expected = expectedCrossOrderedQ[
            leftMode, left, rightMode, right];
          actual = snegOrderedQ[left[[2]], right[[2]]];
          If[actual =!= expected,
            AppendTo[crossComparatorFailures, {label, actual, expected}]
          ];

          expected = expectedSignature[expected, left[[2]], right[[2]]];
          actual = orderingSignature[nc[left[[2]], right[[2]]]];
          If[actual =!= expected,
            AppendTo[crossProductFailures, {label, actual, expected}]
          ],
          {orientation, 2}
        ],
        {rowC, rowsC}, {rowD, rowsD}
      ],
      {modeC, modes}, {modeD, modes}
    ];

    (* Same-head comparator preservation matrix: 300 situations. *)
    Do[
      ordering[c] = modeC;
      Do[
        expected = expectedSameOrderedQ[modeC, rowC, rowD /. d -> c];
        actual = snegOrderedQ[rowC[[2]], rowD[[2]] /. d -> c];
        If[actual =!= expected,
          AppendTo[sameComparatorFailures,
            {{modeC, rowC[[1]], rowD[[1]]}, actual, expected}]
        ],
        {rowC, rowsC}, {rowD, rowsD}
      ],
      {modeC, modes}
    ];

    (* Distinct declared fermion heads have a zero canonical
       anticommutator for every index and momentum class. *)
    ordering[c] = EMPTY;
    ordering[d] = EMPTY;
    Do[
      actual = acmt[rowC[[2]], rowD[[2]]];
      If[actual =!= 0,
        AppendTo[crossAcmtFailures,
          {{rowC[[1]], rowD[[1]], "c,d"}, actual, 0}]
      ];
      actual = acmt[rowD[[2]], rowC[[2]]];
      If[actual =!= 0,
        AppendTo[crossAcmtFailures,
          {{rowC[[1]], rowD[[1]], "d,c"}, actual, 0}]
      ];

      actual = anticommutator[rowC[[2]], rowD[[2]]];
      If[actual =!= 0,
        AppendTo[crossAnticommutatorFailures,
          {{rowC[[1]], rowD[[1]], "c,d"}, actual, 0}]
      ],
      {rowC, rowsC}, {rowD, rowsD}
    ];
  ];

  test[failureSummary[classificationFailures], {0, {}}];
  test[failureSummary[sameComparatorFailures], {0, {}}];
  test[failureSummary[crossComparatorFailures], {0, {}}];
  test[failureSummary[crossProductFailures], {0, {}}];
  test[failureSummary[crossAcmtFailures], {0, {}}];
  test[failureSummary[crossAnticommutatorFailures], {0, {}}];
  test[DownValues[ordering], savedOrdering];
];

Module[
  {
    savedOrdering = DownValues[ordering],
    savedSnegOrderedQ = DownValues[snegOrderedQ],
    orderingSignature, expectedSignature, expectedByRank,
    physicalRank, failureSummary, orderingAssumeK1,
    orderingAssumeK2, type1, type2, sign1, sign2, rank1, rank2,
    op1, op2, actual, expected, permutation, permuted,
    case, canonical, canonicalIndex, vacuumCases,
    assumptionFailures = {}, extraIndexFailures = {},
    symbolicTypeFailures = {}, customHookFailures = {},
    multiOperatorFailures = {}, vacuumFailures = {},
    noneBarrierFailures = {}, dispatchFailures = {},
    restorationFailures = {}, extraIndexCases, symbolicTypeCases,
    canonicalTriples, fourthHead, fourStates, canonicalFour,
    repeatedCanonical
  },

  orderingSignature[expr_] := Module[{value = expr},
    Which[
      Head[value] === nc, {1, List @@ value},
      MatchQ[value, Times[-1, _nc]], {-1, List @@ (-value)},
      True, {"unexpected", value}
    ]
  ];

  expectedSignature[isOrdered_, left_, right_] :=
    If[isOrdered, {1, {left, right}}, {-1, {right, left}}];

  expectedByRank[leftRank_, left_, rightRank_, right_] := Which[
    leftRank === CR && rightRank === AN, True,
    leftRank === AN && rightRank === CR, False,
    leftRank === rightRank, OrderedQ[{left, right}],
    True, True
  ];

  physicalRank[type_, sign_] := If[
    sign > 0,
    type,
    If[type === CR, AN, CR]
  ];

  failureSummary[failures_List] := {
    Length[failures],
    If[failures === {}, {}, Take[failures, Min[Length[failures], 12]]]
  };

  Block[{ordering},
    DownValues[ordering] = savedOrdering;
    ordering[c] = SEA;
    ordering[d] = SEA;
    ordering[e] = SEA;

    (* Resolve symbolic SEA momenta through all 16 raw-type/sign classes
       and check both head orientations. *)
    Do[
      UpValues[orderingAssumeK1] = {};
      UpValues[orderingAssumeK2] = {};
      If[sign1 > 0,
        orderingAssumeK1 /: orderingAssumeK1 > 0 = True,
        orderingAssumeK1 /: orderingAssumeK1 < 0 = True
      ];
      If[sign2 > 0,
        orderingAssumeK2 /: orderingAssumeK2 > 0 = True,
        orderingAssumeK2 /: orderingAssumeK2 < 0 = True
      ];

      op1 = c[type1, orderingAssumeK1];
      op2 = d[type2, orderingAssumeK2];
      rank1 = physicalRank[type1, sign1];
      rank2 = physicalRank[type2, sign2];

      expected = expectedByRank[rank1, op1, rank2, op2];
      actual = snegOrderedQ[op1, op2];
      If[actual =!= expected,
        AppendTo[assumptionFailures,
          {{type1, sign1, type2, sign2, "c,d", "orderedQ"},
            actual, expected}]
      ];
      actual = orderingSignature[nc[op1, op2]];
      expected = expectedSignature[expected, op1, op2];
      If[actual =!= expected,
        AppendTo[assumptionFailures,
          {{type1, sign1, type2, sign2, "c,d", "product"},
            actual, expected}]
      ];

      expected = expectedByRank[rank2, op2, rank1, op1];
      actual = snegOrderedQ[op2, op1];
      If[actual =!= expected,
        AppendTo[assumptionFailures,
          {{type1, sign1, type2, sign2, "d,c", "orderedQ"},
            actual, expected}]
      ];
      actual = orderingSignature[nc[op2, op1]];
      expected = expectedSignature[expected, op2, op1];
      If[actual =!= expected,
        AppendTo[assumptionFailures,
          {{type1, sign1, type2, sign2, "d,c", "product"},
            actual, expected}]
      ],
      {type1, {CR, AN}}, {sign1, {-1, 1}},
      {type2, {CR, AN}}, {sign2, {-1, 1}}
    ];
    UpValues[orderingAssumeK1] = {};
    UpValues[orderingAssumeK2] = {};

    (* Trailing indexes and unequal arities must not affect the physical
       classification or introduce a cross-head delta term. *)
    extraIndexCases = {
      {"opposite roles", c[CR, -1, 7, UP], AN,
        d[AN, -2, 9, DO], CR},
      {"same creators reverse heads", d[CR, 2, 4], CR,
        c[AN, -1, 3, UP], CR},
      {"same annihilators reverse heads", d[AN, 2, 4, DO], AN,
        c[CR, -1, 3], AN},
      {"different arity", c[CR, -1, 1], AN,
        d[AN, -1, 1, 2, 3, UP], CR},
      {"same trailing indexes", c[CR, -1, 1, UP], AN,
        d[AN, -1, 1, UP], CR},
      {"symbolic fallback", c[AN, orderingAssumeK1, 1, UP], AN,
        d[CR, orderingAssumeK2, 1, UP], CR}
    };
    Do[
      expected = expectedByRank[case[[3]], case[[2]], case[[5]], case[[4]]];
      actual = snegOrderedQ[case[[2]], case[[4]]];
      If[actual =!= expected,
        AppendTo[extraIndexFailures,
          {{case[[1]], "orderedQ"}, actual, expected}]
      ];
      actual = orderingSignature[nc[case[[2]], case[[4]]]];
      expected = expectedSignature[expected, case[[2]], case[[4]]];
      If[actual =!= expected,
        AppendTo[extraIndexFailures,
          {{case[[1]], "product"}, actual, expected}]
      ];
      If[acmt[case[[2]], case[[4]]] =!= 0,
        AppendTo[extraIndexFailures,
          {{case[[1]], "acmt"}, acmt[case[[2]], case[[4]]], 0}]
      ],
      {case, extraIndexCases}
    ];

    (* Unsupported first-index values retain legacy fail-closed behavior;
       equal symbolic types are still sorted by head. *)
    symbolicTypeCases = {
      {"unknown-left", c[tip, 1], d[CR, 1], True},
      {"unknown-right", d[CR, 1], c[tip, 1], True},
      {"both-unknown-ordered", c[tip, 2], d[tip, 1], True},
      {"both-unknown-reversed", d[tip, 1], c[tip, 2], False}
    };
    Do[
      actual = snegOrderedQ[case[[2]], case[[3]]];
      If[actual =!= case[[4]],
        AppendTo[symbolicTypeFailures,
          {{case[[1]], "orderedQ"}, actual, case[[4]]}]
      ];
      actual = orderingSignature[nc[case[[2]], case[[3]]]];
      expected = expectedSignature[case[[4]], case[[2]], case[[3]]];
      If[actual =!= expected,
        AppendTo[symbolicTypeFailures,
          {{case[[1]], "product"}, actual, expected}]
      ],
      {case, symbolicTypeCases}
    ];

    (* Every permutation of representative three-head canonical strings
       must produce the parity sign and the same final order. *)
    canonicalTriples = {
      {c[AN, -1, 1], d[AN, -2, 1], e[CR, -1, 1]},
      {c[CR, 1, 1], d[CR, 2, 1], e[AN, 1, 1]},
      {c[AN, -1, 1], d[CR, orderingAssumeK1, 1], e[AN, 1, 1]},
      {c[CR, -1, 1], d[AN, 1, 1], e[CR, -2, 1]}
    };
    Do[
      permuted = canonical[[permutation]];
      actual = TimeConstrained[
        orderingSignature[Apply[nc, permuted]], 5, $TimedOut];
      expected = {Signature[permutation], canonical};
      If[actual =!= expected,
        AppendTo[multiOperatorFailures,
          {{"three heads", canonicalIndex, permutation}, actual, expected}]
      ],
      {canonicalIndex, Length[canonicalTriples]},
      {canonical, {canonicalTriples[[canonicalIndex]]}},
      {permutation, Permutations[Range[3]]}
    ];

    (* Four-head and repeated-head permutations exercise parity,
       termination, and composition with the unchanged same-head rules. *)
    Block[{listfermionoperators = listfermionoperators},
      snegfermionoperators[fourthHead];
      ordering[fourthHead] = SEA;
      fourStates = {
        {c[AN, -1, 1], CR},
        {d[CR, 1, 1], CR},
        {e[AN, 1, 1], AN},
        {fourthHead[CR, -1, 1], AN}
      };
      canonicalFour = First /@ Sort[fourStates,
        expectedByRank[#1[[2]], #1[[1]], #2[[2]], #2[[1]]] &];
      Do[
        permuted = canonicalFour[[permutation]];
        actual = TimeConstrained[
          orderingSignature[Apply[nc, permuted]], 5, $TimedOut];
        expected = {Signature[permutation], canonicalFour};
        If[actual =!= expected,
          AppendTo[multiOperatorFailures,
            {{"four heads", permutation}, actual, expected}]
        ],
        {permutation, Permutations[Range[4]]}
      ];
    ];

    repeatedCanonical = {
      c[CR, 1, 11], c[AN, -1, 22],
      d[CR, 1, 11], d[AN, -1, 22]
    };
    Do[
      permuted = repeatedCanonical[[permutation]];
      actual = TimeConstrained[
        orderingSignature[Apply[nc, permuted]], 5, $TimedOut];
      expected = {Signature[permutation], repeatedCanonical};
      If[actual =!= expected,
        AppendTo[multiOperatorFailures,
          {{"repeated heads", permutation}, actual, expected}]
      ],
      {permutation, Permutations[Range[4]]}
    ];

    (* A NONE head is an absolute barrier, while active operators on one
       side of that barrier may still sort among themselves. *)
    ordering[d] = NONE;
    actual = orderingSignature[
      nc[e[CR, 1], d[CR, 1], c[CR, 1]]];
    expected = {1, {e[CR, 1], d[CR, 1], c[CR, 1]}};
    If[actual =!= expected,
      AppendTo[noneBarrierFailures,
        {{"both sides blocked"}, actual, expected}]
    ];
    actual = orderingSignature[
      nc[d[CR, 1], e[CR, 1], c[CR, 1]]];
    expected = {-1, {d[CR, 1], c[CR, 1], e[CR, 1]}};
    If[actual =!= expected,
      AppendTo[noneBarrierFailures,
        {{"active suffix"}, actual, expected}]
    ];

    ordering[d] = SEA;

    (* Cross-head ordering must expose the correct physical annihilator to
       the vacuum rules and agree with Wick evaluation. *)
    vacuumCases = {
      {"right vacuum reordered",
        nc[c[CR, -1], d[AN, -1], VACUUM], 0},
      {"left vacuum reordered",
        nc[conj[VACUUM], c[CR, -1], d[AN, -1]], 0},
      {"right vacuum already physical",
        nc[c[AN, -1], d[CR, -1], VACUUM], 0},
      {"zeroonvac reordered",
        zeroonvac[nc[c[CR, -1], d[AN, -1]]], 0},
      {"negative four-point vev",
        vev[nc[c[CR, -1], d[CR, -1],
          c[AN, -1], d[AN, -1]]], -1},
      {"negative four-point wick",
        vevwick[nc[c[CR, -1], d[CR, -1],
          c[AN, -1], d[AN, -1]]], -1},
      {"negative four-point vacuum",
        nc[conj[VACUUM], c[CR, -1], d[CR, -1],
          c[AN, -1], d[AN, -1], VACUUM], -1},
      {"positive four-point vev",
        vev[nc[c[AN, 1], d[AN, 1], c[CR, 1], d[CR, 1]]], -1},
      {"positive four-point vacuum",
        nc[conj[VACUUM], c[AN, 1], d[AN, 1],
          c[CR, 1], d[CR, 1], VACUUM], -1}
    };
    Do[
      If[case[[2]] =!= case[[3]],
        AppendTo[vacuumFailures,
          {case[[1]], case[[2]], case[[3]]}]
      ],
      {case, vacuumCases}
    ];

    (* Exercise supported customization paths on isolated operator heads. *)
    Module[
      {
        hookC, hookD, customCreator, customAnnihilator,
        customBoth, customNeither, customSymbolic, customExact,
        customUpValue, customNonBoolean, customNone,
        customOrderingResult, customOrdering, customCases,
        savedFermionList = listfermionoperators
      },
      Block[{listfermionoperators = savedFermionList, snegOrderedQ},
        DownValues[snegOrderedQ] = savedSnegOrderedQ;
        snegfermionoperators[hookC, hookD];
        ordering[hookC] = customOrdering;
        ordering[hookD] = customOrdering;

        hookC /: iscreation[hookC[_, customCreator]] = True;
        hookC /: isannihilation[hookC[_, customCreator]] = False;
        hookD /: iscreation[hookD[_, customCreator]] = True;
        hookD /: isannihilation[hookD[_, customCreator]] = False;
        hookC /: iscreation[hookC[_, customAnnihilator]] = False;
        hookC /: isannihilation[hookC[_, customAnnihilator]] = True;
        hookD /: iscreation[hookD[_, customAnnihilator]] = False;
        hookD /: isannihilation[hookD[_, customAnnihilator]] = True;

        hookC /: iscreation[hookC[_, customBoth]] = True;
        hookC /: isannihilation[hookC[_, customBoth]] = True;
        hookD /: iscreation[hookD[_, customBoth]] = True;
        hookD /: isannihilation[hookD[_, customBoth]] = True;
        hookC /: iscreation[hookC[_, customNeither]] = False;
        hookC /: isannihilation[hookC[_, customNeither]] = False;
        hookD /: iscreation[hookD[_, customNeither]] = False;
        hookD /: isannihilation[hookD[_, customNeither]] = False;
        hookC /: iscreation[hookC[_, customSymbolic]] = customOrderingResult;
        hookC /: isannihilation[hookC[_, customSymbolic]] = customOrderingResult;
        hookD /: iscreation[hookD[_, customSymbolic]] = customOrderingResult;
        hookD /: isannihilation[hookD[_, customSymbolic]] = customOrderingResult;

        customCases = {
          {"custom raw AN creator", hookC[AN, customCreator],
            hookD[CR, customAnnihilator], True},
          {"custom raw CR annihilator", hookC[CR, customAnnihilator],
            hookD[AN, customCreator], False},
          {"contradictory hooks", hookC[AN, customBoth],
            hookD[CR, customBoth], False},
          {"false hooks", hookC[AN, customNeither],
            hookD[CR, customNeither], False},
          {"non-Boolean hooks", hookC[AN, customSymbolic],
            hookD[CR, customSymbolic], False}
        };
        Do[
          actual = snegOrderedQ[case[[2]], case[[3]]];
          If[actual =!= case[[4]],
            AppendTo[customHookFailures,
              {{case[[1]], "orderedQ"}, actual, case[[4]]}]
          ];
          actual = orderingSignature[nc[case[[2]], case[[3]]]];
          expected = expectedSignature[case[[4]], case[[2]], case[[3]]];
          If[actual =!= expected,
            AppendTo[customHookFailures,
              {{case[[1]], "product"}, actual, expected}]
          ],
          {case, customCases}
        ];

        snegOrderedQ[
          hookC[CR, customExact], hookD[AN, customExact]] = False;
        snegOrderedQ[
          hookD[AN, customExact], hookC[CR, customExact]] = True;
        actual = orderingSignature[
          nc[hookC[CR, customExact], hookD[AN, customExact]]];
        expected = {-1,
          {hookD[AN, customExact], hookC[CR, customExact]}};
        If[actual =!= expected,
          AppendTo[customHookFailures,
            {{"exact comparator", "product"}, actual, expected}]
        ];

        hookC /: snegOrderedQ[
          hookC[CR, customUpValue], hookD[AN, customUpValue]] = False;
        hookD /: snegOrderedQ[
          hookD[AN, customUpValue], hookC[CR, customUpValue]] = True;
        actual = orderingSignature[
          nc[hookC[CR, customUpValue], hookD[AN, customUpValue]]];
        expected = {-1,
          {hookD[AN, customUpValue], hookC[CR, customUpValue]}};
        If[actual =!= expected,
          AppendTo[customHookFailures,
            {{"UpValue comparator", "product"}, actual, expected}]
        ];

        snegOrderedQ[
          hookC[AN, customNonBoolean],
          hookD[CR, customNonBoolean]] = customOrderingResult;
        actual = orderingSignature[
          nc[hookC[AN, customNonBoolean],
            hookD[CR, customNonBoolean]]];
        expected = {1,
          {hookC[AN, customNonBoolean],
            hookD[CR, customNonBoolean]}};
        If[actual =!= expected,
          AppendTo[customHookFailures,
            {{"non-Boolean comparator", "product"}, actual, expected}]
        ];

        ordering[hookC] = NONE;
        ordering[hookD] = EMPTY;
        snegOrderedQ[
          hookC[CR, customNone], hookD[AN, customNone]] = False;
        actual = orderingSignature[
          nc[hookC[CR, customNone], hookD[AN, customNone]]];
        expected = {1, {hookC[CR, customNone], hookD[AN, customNone]}};
        If[actual =!= expected,
          AppendTo[customHookFailures,
            {{"NONE overrides comparator", "product"}, actual, expected}]
        ];
      ];
    ];

    (* The new cross-head fermion rule must not capture other operator
       families or alter their established signs and NONE behavior. *)
    Module[
      {bosonC, bosonD, savedBosonList = listbosonoperators},
      Block[{listbosonoperators = savedBosonList},
        snegbosonoperators[bosonC, bosonD];

        actual = orderingSignature[
          nc[bosonC[AN, 1], bosonD[CR, 1]]];
        expected = {1, {bosonD[CR, 1], bosonC[AN, 1]}};
        If[actual =!= expected,
          AppendTo[dispatchFailures,
            {{"cross-head bosons"}, actual, expected}]
        ];

        ordering[bosonC] = NONE;
        actual = orderingSignature[
          nc[bosonC[AN, 1], bosonD[CR, 1]]];
        expected = {1, {bosonC[AN, 1], bosonD[CR, 1]}};
        If[actual =!= expected,
          AppendTo[dispatchFailures,
            {{"boson NONE"}, actual, expected}]
        ];

        ordering[c] = EMPTY;
        actual = orderingSignature[nc[c[AN, 1], bosonD[CR, 1]]];
        expected = {1, {bosonD[CR, 1], c[AN, 1]}};
        If[actual =!= expected,
          AppendTo[dispatchFailures,
            {{"fermion-boson"}, actual, expected}]
        ];

        actual = orderingSignature[nc[c[CR, 1], m1[1]]];
        expected = {-1, {m1[1], c[CR, 1]}};
        If[actual =!= expected,
          AppendTo[dispatchFailures,
            {{"Dirac-Majorana"}, actual, expected}]
        ];
      ];
    ];
  ];

  test[failureSummary[assumptionFailures], {0, {}}];
  test[failureSummary[extraIndexFailures], {0, {}}];
  test[failureSummary[symbolicTypeFailures], {0, {}}];
  test[failureSummary[customHookFailures], {0, {}}];
  test[failureSummary[multiOperatorFailures], {0, {}}];
  test[failureSummary[vacuumFailures], {0, {}}];
  test[failureSummary[noneBarrierFailures], {0, {}}];
  test[failureSummary[dispatchFailures], {0, {}}];

  If[DownValues[ordering] =!= savedOrdering,
    AppendTo[restorationFailures,
      {"ordering", DownValues[ordering], savedOrdering}]
  ];
  If[DownValues[snegOrderedQ] =!= savedSnegOrderedQ,
    AppendTo[restorationFailures,
      {"snegOrderedQ", DownValues[snegOrderedQ], savedSnegOrderedQ}]
  ];
  test[failureSummary[restorationFailures], {0, {}}];
];

Print["** Majorana anti-commutators **"];
test[ acmt[m2[1], m1[1]], 0 ];
test[ acmt[m1, m1], 1 ];
test[ nc[m2[1], m1[1]], -nc[m1[1], m2[1]] ];

Print["** Conjugation **"];
test[ conj[1], 1 ];
test[ conj[I], -I ];
test[ conj[x], x ];
test[ conj[x c[]], x conj[c[]] ];
test[ conj[c[CR]], c[AN] ];
test[ conj[c[CR, UP]], c[AN, UP] ];
test[ conj[c[AN]], c[CR] ];
test[ conj[c[AN, UP]], c[CR, UP] ];
test[ conj[conj[c[symbolicType, 1]]], c[symbolicType, 1] ];
snegbosonoperators[conjBoson];
test[ conj[conj[conjBoson[symbolicType, 1]]],
  conjBoson[symbolicType, 1] ];
test[ conj[a+b], conj[a] + conj[b] ];
test[ conj[nc[a, b]], nc[conj[b], conj[a]] ];
test[ conj[nc[c[CR], d[CR]]], -nc[c[AN], d[AN]] ];
test[ conj[scalar[z]], scalar[conj[z]] ];
test[ conj[{a,b}], {conj[a], conj[b]} ];

Print["** Vacuum expectation value **"];
test[ vev[1], 1 ];
test[ vev[scalar[2]], 2 ];
test[ vev[a+b], vev[a] + vev[b] ];
test[ vev[2a], 2 vev[a] ];
test[ vev[c[CR]], 0 ];
test[ vev[nc[c[AN], c[CR]]], 1 ];
test[ vev[nc[c[CR], c[AN]]], 0 ];

snegbosonoperators[noneBosonA, noneBosonB];
snegfreeindexes[noneBosonI, noneBosonJ, noneBosonK, noneBosonL];
ordering[noneBosonA] = NONE;
ordering[noneBosonB] = NONE;

test[ contraction[noneBosonA[AN, noneBosonI],
    noneBosonA[CR, noneBosonJ]],
  KroneckerDelta[noneBosonI, noneBosonJ] ];
test[ vev[nc[noneBosonA[AN], noneBosonA[CR]]], 1 ];
test[ vevwick[nc[noneBosonA[AN, noneBosonI],
    noneBosonA[CR, noneBosonJ]]],
  KroneckerDelta[noneBosonI, noneBosonJ] ];
test[ vev[nc[noneBosonA[AN, noneBosonI],
    noneBosonA[CR, noneBosonJ]]],
  KroneckerDelta[noneBosonI, noneBosonJ] ];
test[ vev[nc[noneBosonA[CR], noneBosonA[AN]]], 0 ];
test[ vev[nc[noneBosonA[AN], noneBosonB[CR]]], 0 ];
test[ vev[nc[noneBosonA[AN], noneBosonA[AN],
    noneBosonA[CR], noneBosonA[CR]]], 2 ];
test[ vev[nc[noneBosonA[AN], noneBosonB[AN],
    noneBosonA[CR], noneBosonB[CR]]], 1 ];
test[ vev[nc[noneBosonA[AN, noneBosonI],
    noneBosonA[AN, noneBosonJ],
    noneBosonA[CR, noneBosonK],
    noneBosonA[CR, noneBosonL]]],
  KroneckerDelta[noneBosonI, noneBosonK]
      KroneckerDelta[noneBosonJ, noneBosonL] +
    KroneckerDelta[noneBosonI, noneBosonL]
      KroneckerDelta[noneBosonJ, noneBosonK] ];
test[ normalorder[nc[noneBosonA[AN], noneBosonA[CR]]],
  -1 + nc[noneBosonA[AN], noneBosonA[CR]] ];

Print["** zeroonvac[] **"];
test[ zeroonvac[1], 1 ];
test[ zeroonvac[a+b], zeroonvac[a] + zeroonvac[b] ];
test[ zeroonvac[2 a], 2 zeroonvac[a] ];
test[ zeroonvac[x a], x zeroonvac[a] ];

Print["** Inner and outer products **"];
test[ inner[{2,3}, {a,b}], 2a+3b ];
test[ outer[{2,3}, {a,b}], {{2 a, 2 b}, {3 a, 3 b}} ];
  
Print["** number[] **"];
test[ number[c[], UP], nc[c[0, 1], c[1, 1]] ];
test[ number[c[], DO], nc[c[0, 0], c[1, 0]] ];
test[ number[c[]], nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]]];

snegbosonoperators[simplifyNumberBoson];
test[
  MatchQ[
    SnegSimplifyNumber[
      nc[simplifyNumberBoson[CR], simplifyNumberBoson[AN]]],
    HoldPattern[HoldForm[number[simplifyNumberBoson[]]]]],
  True
];
test[
  MatchQ[
    SnegSimplifyNumber[
      nc[simplifyNumberBoson[CR, 2, 3],
        simplifyNumberBoson[AN, 2, 3]]],
    HoldPattern[HoldForm[number[simplifyNumberBoson[2, 3]]]]],
  True
];
test[
  MatchQ[
    SnegSimplify[
      nc[simplifyNumberBoson[CR, 7], simplifyNumberBoson[AN, 7]]],
    HoldPattern[HoldForm[number[simplifyNumberBoson[7]]]]],
  True
];
test[
  MatchQ[
    SnegSimplifyNumber[nc[c[CR, 4, UP], c[AN, 4, UP]]],
    HoldPattern[HoldForm[number[c[4], 1]]]],
  True
];

Print["** hubbard[] **"];
test[ hubbard[c[]], -nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];

Print["** hamiltonian[] **"];
test[ hamiltonian[Hubbard, c[], 2, U, t, eps] // FullSimplify,
t (nc[c[0, 1, 0], c[1, 2, 0]] + nc[c[0, 1, 1], c[1, 2, 1]] + 
nc[c[0, 2, 0], c[1, 1, 0]] + nc[c[0, 2, 1], c[1, 1, 1]]) + 
eps (nc[c[0, 1, 0], c[1, 1, 0]] + nc[c[0, 1, 1], c[1, 1, 1]] + 
nc[c[0, 2, 0], c[1, 2, 0]] + nc[c[0, 2, 1], c[1, 2, 1]]) - 
U (nc[c[0, 1, 0], c[0, 1, 1], c[1, 1, 0], c[1, 1, 1]] + 
nc[c[0, 2, 0], c[0, 2, 1], c[1, 2, 0], c[1, 2, 1]])
];

Print["** chargecharge[] **"];
test[ chargecharge[c[], d[]], 
-nc[c[0, 0], d[0, 0], c[1, 0], d[1, 0]] - 
nc[c[0, 0], d[0, 1], c[1, 0], d[1, 1]] - 
nc[c[0, 1], d[0, 0], c[1, 1], d[1, 0]] - 
nc[c[0, 1], d[0, 1], c[1, 1], d[1, 1]]
];

Print["** Spin operators **"];
test[ PauliX, {{0, 1}, {1, 0}} ];
test[ PauliY, {{0, -I}, {I, 0}} ];
test[ PauliZ, {{1, 0}, {0, -1}} ];
test[ PauliPlus, {{0, 2}, {0, 0}} ];
test[ PauliMinus, {{0, 0}, {2, 0}} ];

test[ spinall[c[]],
{(nc[c[0, 0], c[1, 1]] + nc[c[0, 1], c[1, 0]])/2, 
(I*nc[c[0, 0], c[1, 1]] - I*nc[c[0, 1], c[1, 0]])/2, 
(-nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]])/2, 
(3*(nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]] + 
2*nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]]))/4}
];

test[ spinxyz[c[]],
{(nc[c[0, 0], c[1, 1]] + nc[c[0, 1], c[1, 0]])/2, 
(I*nc[c[0, 0], c[1, 1]] - I*nc[c[0, 1], c[1, 0]])/2, 
(-nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]])/2}
];

test[ spinxyzgen[d[CR, #]&], spinxyz[d[]] ];

test[ spinss[c[]], (3*(nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]] + 
2*nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]]))/4
];
test[ spinx[c[]], (nc[c[0, 0], c[1, 1]] + nc[c[0, 1], c[1, 0]])/2 ];
test[ spiny[c[]], (I*nc[c[0, 0], c[1, 1]] - I*nc[c[0, 1], c[1, 0]])/2 ];
test[ spinz[c[]], (nc[c[0, 1], c[1, 1]] - nc[c[0, 0], c[1, 0]])/2 ];
test[ spinplus[c[]], nc[c[0, 1], c[1, 0]] ];
test[ spinminus[c[]], nc[c[0, 0], c[1, 1]] ];
test[
  MatchQ[SnegSimplify[Expand[spinx[c[]]]],
    HoldPattern[HoldForm[spinx[c[]]]]],
  True
];
test[
  MatchQ[SnegSimplifySpin[Expand[spiny[c[3]]]],
    HoldPattern[HoldForm[spiny[c[3]]]]],
  True
];
test[
  MatchQ[SnegSimplifySpin[Expand[spinz[c[3]]]],
    HoldPattern[HoldForm[spinz[c[3]]]]],
  True
];
test[
  MatchQ[SnegSimplifySpin[Expand[2 spinx[c[3]]]],
    HoldPattern[2 HoldForm[spinx[c[3]]]]],
  True
];
test[
  SnegSimplifySpin[Expand[I spiny[c[3]]]] /.
    HoldPattern[HoldForm[spiny[c[3]]]] -> 1,
  I
];
test[
  SnegSimplifySpin[Simplify[2 I spinz[c[3]]]] /.
    HoldPattern[HoldForm[spinz[c[3]]]] -> 1,
  2 I
];
test[
  SnegSimplifySpin[Simplify[-2 I spinx[c[3]]]] /.
    HoldPattern[HoldForm[spinx[c[3]]]] -> 1,
  -2 I
];
test[
  MatchQ[SnegSimplifySpin[
      Expand[4 nc[spinx[c[3]], spiny[d[4]]]]],
    HoldPattern[
      4 nc[HoldForm[spinx[c[3]]], HoldForm[spiny[d[4]]]]]],
  True
];
test[
  MatchQ[SnegSimplifySpin[
      Expand[nc[spinx[c[3]], spinz[d[4]]]]],
    HoldPattern[
      nc[HoldForm[spinx[c[3]]], HoldForm[spinz[d[4]]]]]],
  True
];
test[
  FreeQ[Sneg`ruleSnegSimplifySpin,
    Alternatives[Sneg`l1, Sneg`l2, Sneg`ir2]],
  True
];

Print["** spinspin[] **"];
test[ spinspin[c[], c[]], (3*(nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]] + 
2*nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]]))/4
];
test[ spinspin[c[], d[]], (-nc[c[0, 0], d[0, 0], c[1, 0], d[1, 0]] + 
nc[c[0, 0], d[0, 1], c[1, 0], d[1, 1]] - 
2*nc[c[0, 0], d[0, 1], c[1, 1], d[1, 0]] - 
2*nc[c[0, 1], d[0, 0], c[1, 0], d[1, 1]] + 
nc[c[0, 1], d[0, 0], c[1, 1], d[1, 0]] - 
nc[c[0, 1], d[0, 1], c[1, 1], d[1, 1]])/4
];

Print["** spinspin transverse and longitudinal parts **"];
test[ spinspinpm[c[], d[]], -nc[c[0, 1], d[0, 0], c[1, 0], d[1, 1]] ];
test[ spinspinmp[c[], d[]], -nc[c[0, 0], d[0, 1], c[1, 1], d[1, 0]] ];
test[ spinspinxy[c[], d[]], 
(-nc[c[0, 0], d[0, 1], c[1, 1], d[1, 0]] - 
  nc[c[0, 1], d[0, 0], c[1, 0], d[1, 1]])/2
];
test[ spinspinz[c[], d[]], (-nc[c[0, 0], d[0, 0], c[1, 0], d[1, 0]] + 
  nc[c[0, 0], d[0, 1], c[1, 0], d[1, 1]] + 
  nc[c[0, 1], d[0, 0], c[1, 1], d[1, 0]] - 
  nc[c[0, 1], d[0, 1], c[1, 1], d[1, 1]])/4
];

Print["** manyspin[] **"];
test[ manyspin[{c[], d[]}], 
{nc[c[0, 0], c[1, 1]]/2 + nc[c[0, 1], c[1, 0]]/2 + 
  nc[d[0, 0], d[1, 1]]/2 + nc[d[0, 1], d[1, 0]]/2, 
 (I/2)*nc[c[0, 0], c[1, 1]] - 
  (I/2)*nc[c[0, 1], c[1, 0]] + 
  (I/2)*nc[d[0, 0], d[1, 1]] - 
  (I/2)*nc[d[0, 1], d[1, 0]], -nc[c[0, 0], c[1, 0]]/2 + 
  nc[c[0, 1], c[1, 1]]/2 - nc[d[0, 0], d[1, 0]]/2 + 
  nc[d[0, 1], d[1, 1]]/2, (3*nc[c[0, 0], c[1, 0]])/4 + 
  (3*nc[c[0, 1], c[1, 1]])/4 + (3*nc[d[0, 0], d[1, 0]])/
   4 + (3*nc[d[0, 1], d[1, 1]])/4 + 
  (3*nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]])/2 - 
  nc[c[0, 0], d[0, 0], c[1, 0], d[1, 0]]/2 + 
  nc[c[0, 0], d[0, 1], c[1, 0], d[1, 1]]/2 - 
  nc[c[0, 0], d[0, 1], c[1, 1], d[1, 0]] - 
  nc[c[0, 1], d[0, 0], c[1, 0], d[1, 1]] + 
  nc[c[0, 1], d[0, 0], c[1, 1], d[1, 0]]/2 - 
  nc[c[0, 1], d[0, 1], c[1, 1], d[1, 1]]/2 + 
  (3*nc[d[0, 0], d[0, 1], d[1, 0], d[1, 1]])/2}
];

Print["** halfintegerQ[] **"];
test[ halfintegerQ[10], True ];
test[ halfintegerQ[21/2], True ];
test[ halfintegerQ[Pi], False ];
test[ halfintegerQ[2.3], False ];

Print["** Construction of matrix representations of spin matrices **"];
test[ spinmatrixZ[1], {{1, 0, 0}, {0, 0, 0}, {0, 0, -1}} ];
test[ spinmatrixP[1], {{0, Sqrt[2], 0}, {0, 0, Sqrt[2]}, {0, 0, 0}} ];
test[ spinmatrixM[1], {{0, 0, 0}, {Sqrt[2], 0, 0}, {0, Sqrt[2], 0}} ];
test[ spinmatrixX[1], {{0, 1/Sqrt[2], 0}, {1/Sqrt[2], 0, 1/Sqrt[2]}, 
 {0, 1/Sqrt[2], 0}} ];
test[ spinmatrixY[1], {{0, (-I)/Sqrt[2], 0}, {I/Sqrt[2], 0, (-I)/Sqrt[2]}, 
 {0, I/Sqrt[2], 0}} ];

test[ spinmatrixZ[1/2], 1/2 PauliZ ];
test[ spinmatrixP[1/2], 1/2 PauliPlus ];
test[ spinmatrixM[1/2], 1/2 PauliMinus ];
test[ spinmatrixX[1/2], 1/2 PauliX ];
test[ spinmatrixY[1/2], 1/2 PauliY ];

Print["** direct[] **"];
test[ direct[{2,3}, {a,b}], {2 a, 2 b, 3 a, 3 b} ];

Print["** nambu[] **"];
test[ nambu[c[]], {c[0, 1], c[1, 0]} ];
test[ nambu[c[], -1], {c[0, 1], -c[1, 0]} ];

Print["** isospin[] **"];
test[ isospin[c[]], 
{(-nc[c[0, 0], c[0, 1]] + nc[c[1, 0], c[1, 1]])/2, 
 (I*nc[c[0, 0], c[0, 1]] + I*nc[c[1, 0], c[1, 1]])/2, 
 (-1 + nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]])/2}
];
test[ isospin[c[], -1],
{(nc[c[0, 0], c[0, 1]] - nc[c[1, 0], c[1, 1]])/2, 
 ((-I)*nc[c[0, 0], c[0, 1]] - I*nc[c[1, 0], c[1, 1]])/2, 
 (-1 + nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]])/2}
];

Print["** manyisospin[] **"];
test[ manyisospin[{c[], d[]}, {1, -1}],
{nc[c[0, 0], c[0, 1]]/2 - nc[c[1, 0], c[1, 1]]/2 + 
  nc[d[0, 0], d[0, 1]]/2 - nc[d[1, 0], d[1, 1]]/2, 
 (-I/2)*nc[c[0, 0], c[0, 1]] - 
  (I/2)*nc[c[1, 0], c[1, 1]] - 
  (I/2)*nc[d[0, 0], d[0, 1]] - 
  (I/2)*nc[d[1, 0], d[1, 1]], 
 -1 + nc[c[0, 0], c[1, 0]]/2 + nc[c[0, 1], c[1, 1]]/2 + 
  nc[d[0, 0], d[1, 0]]/2 + nc[d[0, 1], d[1, 1]]/2, 
 2 - (5*nc[c[0, 0], c[1, 0]])/4 - 
  (5*nc[c[0, 1], c[1, 1]])/4 - (5*nc[d[0, 0], d[1, 0]])/
   4 - (5*nc[d[0, 1], d[1, 1]])/4 - 
  (3*nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]])/2 - 
  nc[c[0, 0], c[0, 1], d[1, 0], d[1, 1]] - 
  nc[c[0, 0], d[0, 0], c[1, 0], d[1, 0]]/2 - 
  nc[c[0, 0], d[0, 1], c[1, 0], d[1, 1]]/2 - 
  nc[c[0, 1], d[0, 0], c[1, 1], d[1, 0]]/2 - 
  nc[c[0, 1], d[0, 1], c[1, 1], d[1, 1]]/2 - 
  nc[d[0, 0], d[0, 1], c[1, 0], c[1, 1]] - 
  (3*nc[d[0, 0], d[0, 1], d[1, 0], d[1, 1]])/2}
];

Print["** Functions of operators **"];
test[ pow[c[], 0], 1 ];
test[ pow[c[], 1], c[] ];
test[ pow[c[], 2], nc[c[], c[]] ];
test[ pow[c[], 3], nc[c[], c[], c[]] ];
test[ pow[c[], 4], nc[c[], c[], c[], c[]] ];
test[ snegSeries[Exp[A], 2], 1 + A + nc[A, A]/2 ];
test[ snegSeries[Sin[A], 3], A - nc[A, A, A]/6 ];
test[ snegSeries[Exp[A]], snegSeries[Exp, A, 5] ];

Print["** hop[] **"];
test[ hop[c[], d[], UP], nc[c[0, 1], d[1, 1]] + nc[d[0, 1], c[1, 1]] ];
test[ hop[c[], d[], DO], nc[c[0, 0], d[1, 0]] + nc[d[0, 0], c[1, 0]] ];
test[ hop[c[], d[]],
 nc[c[0, 0], d[1, 0]] + nc[c[0, 1], d[1, 1]] + nc[d[0, 0], c[1, 0]] + 
  nc[d[0, 1], c[1, 1]]
];
test[
  MatchQ[SnegSimplify[Expand[hop[c[], d[]]]],
    HoldPattern[HoldForm[hop[c[], d[]]]]],
  True
];
test[
  MatchQ[SnegSimplifyHop[Expand[3 hop[c[3], d[4]]]],
    HoldPattern[3 HoldForm[hop[c[3], d[4]]]]],
  True
];
test[
  MatchQ[SnegSimplifyHop[Expand[-2 twohop[c[3], d[4]]]],
    HoldPattern[-2 HoldForm[twohop[c[3], d[4]]]]],
  True
];
test[
  FreeQ[Sneg`ruleSnegSimplifyHop,
    Alternatives[Sneg`op1, Sneg`op2, Sneg`ir2]],
  True
];

Print["** matrixhop[] **"];
test[ matrixhop[IdentityMatrix[2], c[], d[]], hop[c[], d[]] ];
test[ matrixhop[ConstantArray[0, {2, 2}], c[], d[]], 0 ];
test[ matrixhop[PauliX, c[], d[]], spinfliphop[c[], d[]] ];
test[ matrixhop[PauliY, c[], d[]],
  -I nc[c[CR, UP], d[AN, DO]] + I nc[c[CR, DO], d[AN, UP]] +
   I nc[d[CR, DO], c[AN, UP]] - I nc[d[CR, UP], c[AN, DO]]
];
test[ matrixhop[PauliZ, c[], d[]],
  Expand[hop[c[], d[], UP] - hop[c[], d[], DO]] ];
test[ matrixhop[I IdentityMatrix[2], c[], c[]], 0 ];

Module[{z},
  (* Matrix coefficients are scalars without requiring a declaration. *)
  test[ matrixhop[z IdentityMatrix[2], c[], d[]], genhop[z, c[], d[]] ];
  test[ matrixhop[{{0, z}, {0, 0}}, c[3, 5], d[4, 6]],
    z nc[c[CR, 3, 5, UP], d[AN, 4, 6, DO]] +
      Conjugate[z] nc[d[CR, 4, 6, DO], c[AN, 3, 5, UP]] ];
];

Module[{t11, t12, t21, t22, mat, h, states, zero},
  snegcomplexconstants[t11, t12, t21, t22];
  mat = {{t11, t12}, {t21, t22}};
  h = matrixhop[mat, c[3], d[4]];
  states = {c[CR, 3, UP], c[CR, 3, DO], d[CR, 4, UP], d[CR, 4, DO]};
  zero = ConstantArray[0, {2, 2}];
  (* Independent check of spin ordering, off-diagonal signs, and H.c. *)
  test[ matrixrepresentationop[h, states],
    ArrayFlatten[{{zero, mat}, {ConjugateTranspose[mat], zero}}] ];
  test[ Expand[conj[h] - h], 0 ];
  test[ Expand[matrixhop[ConjugateTranspose[mat], d[4], c[3]] - h], 0 ];
  test[ matrixhop[SparseArray[mat], c[3], d[4]], h ];
];

(* Pauli decomposition must not lose a small floating-point entry through
   cancellation between large identity and Pauli-Z coefficients. *)
test[ Coefficient[
    matrixhop[{{1.*^20, 0}, {0, 1.}}, c[], d[]],
    nc[c[CR, DO], d[AN, DO]]], 1. ];
test[ Coefficient[Expand[
    matrixhop[{{1.*^20, Sin[x]}, {Sin[x], 1.}}, c[], d[]]],
    nc[c[CR, DO], d[AN, DO]]], 1. ];

Module[{theta, complexAngle, assumedAngle, axis, rotation, h, seed,
    states, zero, expected},
  snegrealconstants[theta];
  snegcomplexconstants[complexAngle];
  states = {c[CR, UP], c[CR, DO], d[CR, UP], d[CR, DO]};
  zero = ConstantArray[0, {2, 2}];
  Do[
    (* Pass a precomputed MatrixExp result, not just an inline call. *)
    rotation = MatrixExp[I theta axis];
    h = matrixhop[rotation, c[], d[]];
    seed = VMV[{c[CR, UP], c[CR, DO]}, I axis, {d[AN, UP], d[AN, DO]}];
    expected = Cos[theta] hop[c[], d[]] + Sin[theta] (seed + conj[seed]);
    test[ Expand[h - expected], 0 ];
    test[ Expand[conj[h] - h], 0 ];
    test[ h /. theta -> 0, hop[c[], d[]] ];
    (* Check simplification itself, in particular for the diagonal rotation
       whose raw MatrixExp entries are complex exponentials. *)
    test[ !FreeQ[h, Cos[theta]] && !FreeQ[h, Sin[theta]] &&
      FreeQ[h, Power[E, _]], True ],
    {axis, {PauliX, PauliY, PauliZ, (PauliX + 2 PauliY + 3 PauliZ)/Sqrt[14]}}
  ];
  test[ Expand[matrixhop[MatrixExp[theta PauliX], c[], d[]] -
      Cosh[theta] hop[c[], d[]] - Sinh[theta] spinfliphop[c[], d[]]], 0 ];
  test[ Assuming[Element[assumedAngle, Reals],
      matrixhop[MatrixExp[I assumedAngle PauliZ], c[], d[]]] /.
      assumedAngle -> theta,
    matrixhop[MatrixExp[I theta PauliZ], c[], d[]] ];

  (* Do not assume that a rotation parameter is real. Compare a complex
     substitution against the independently evaluated one-particle matrix. *)
  h = matrixhop[MatrixExp[I complexAngle PauliZ], c[], d[]];
  test[ Expand[conj[h] - h], 0 ];
  rotation = MatrixExp[I ((1 + I)/3) PauliZ];
  expected = ArrayFlatten[{{zero, rotation}, {ConjugateTranspose[rotation], zero}}];
  test[ Max[Abs[Flatten[N[
      matrixrepresentationop[h /. complexAngle -> (1 + I)/3, states] -
        expected]]]] < 10^-12, True ];
];

Module[{spinless, spinone, boson, invalidMatrices},
  snegspinlessfermionoperators[spinless];
  snegfermionoperators[{spinone, 1}];
  snegbosonoperators[boson];
  invalidMatrices = {1, {1, 2}, {{1, 0}}, {{1, 0}, {0}}, IdentityMatrix[3]};
  test[ Quiet[matrixhop[#, c[], d[]] & /@ invalidMatrices, matrixhop::matrix],
    ConstantArray[$Failed, Length[invalidMatrices]] ];
  test[ Quiet[Check[matrixhop[IdentityMatrix[3], c[], d[]], matrixHopMessage,
      matrixhop::matrix], matrixhop::matrix], matrixHopMessage ];
  test[ Quiet[matrixhop[IdentityMatrix[2], #, d[]] & /@
      {spinless[], spinone[], boson[]}, matrixhop::op],
    {$Failed, $Failed, $Failed} ];
  test[ Quiet[Check[matrixhop[IdentityMatrix[2], c[], spinone[]],
      matrixHopMessage, matrixhop::op], matrixhop::op], matrixHopMessage ];
];

Print["** holehop[] **"];
Module[{holeHops, pairUP, pairDO},
  holeHops = {
    holehop[c[], d[], UP],
    holehop[c[], d[], DO],
    holehop[c[], d[], holeSpin]
  };
  test[
    holeHops,
    {anomaloushop[d[], c[], UP],
      anomaloushop[d[], c[], DO],
      anomaloushop[d[], c[], holeSpin]}
  ];
  test[
    Expand /@ (conj /@ holeHops - holeHops),
    {0, 0, 0}
  ];

  pairUP = nc[d[CR, UP], c[CR, DO]];
  pairDO = nc[d[CR, DO], c[CR, UP]];
  test[
    {braketop[pairUP, holeHops[[1]], 1],
      braketop[1, holeHops[[1]], pairUP],
      braketop[pairDO, holeHops[[2]], 1],
      braketop[1, holeHops[[2]], pairDO]},
    {1, 1, 1, 1}
  ];
  test[
    holehop[c[], d[]],
    anomaloushop[d[], c[]]
  ];
];

Print["** Abstract orbital functions **"];
complexAbstractOrbital =
  (c[#1, #2] + I d[#1, #2])/Sqrt[2]&;
orthogonalAbstractOrbital =
  (c[#1, #2] - I d[#1, #2])/Sqrt[2]&;

test[
  Module[{anCalls = 0, fn},
    fn = Function[{type, sigma},
      If[type === AN, anCalls++];
      (c[type, sigma] + I d[type, sigma])/Sqrt[2]
    ];
    number[fn, UP];
    hop[fn, fn, UP];
    matrixhop[PauliY, fn, fn];
    anomaloushop[fn, fn, UP];
    anhop[fn, fn, UP];
    spinxyz[fn];
    spinspin[fn, fn];
    anCalls
  ],
  0
];
test[
  expvop[number[complexAbstractOrbital, UP],
    complexAbstractOrbital[CR, UP]],
  1
];
test[
  {braketop[complexAbstractOrbital[CR, UP],
      hop[complexAbstractOrbital, orthogonalAbstractOrbital, UP],
      orthogonalAbstractOrbital[CR, UP]],
    braketop[orthogonalAbstractOrbital[CR, UP],
      hop[complexAbstractOrbital, orthogonalAbstractOrbital, UP],
      complexAbstractOrbital[CR, UP]]},
  {1, 1}
];
test[
  Expand /@ {
    conj[anomaloushop[complexAbstractOrbital,
        orthogonalAbstractOrbital, UP]] -
      anomaloushop[complexAbstractOrbital,
        orthogonalAbstractOrbital, UP],
    conj[anhop[complexAbstractOrbital,
        orthogonalAbstractOrbital]] -
      anhop[complexAbstractOrbital, orthogonalAbstractOrbital]},
  {0, 0}
];
test[
  Expand /@ {
    conj[hop[complexAbstractOrbital, c[]]] -
      hop[complexAbstractOrbital, c[]],
    conj[anomaloushop[complexAbstractOrbital, c[]]] -
      anomaloushop[complexAbstractOrbital, c[]],
    conj[anhop[complexAbstractOrbital, c[]]] -
      anhop[complexAbstractOrbital, c[]]},
  {0, 0, 0}
];
test[
  Expand /@ (conj /@ spinxyz[complexAbstractOrbital] -
    spinxyz[complexAbstractOrbital]),
  {0, 0, 0}
];

Module[{mat, h, states, zero},
  mat = {{1 + I, 2 - I}, {3 I, -2}};
  h = matrixhop[mat, complexAbstractOrbital, orthogonalAbstractOrbital];
  states = {complexAbstractOrbital[CR, UP], complexAbstractOrbital[CR, DO],
    orthogonalAbstractOrbital[CR, UP], orthogonalAbstractOrbital[CR, DO]};
  zero = ConstantArray[0, {2, 2}];
  test[ matrixrepresentationop[h, states],
    ArrayFlatten[{{zero, mat}, {ConjugateTranspose[mat], zero}}] ];
  test[ Expand[conj[h] - h], 0 ];
  test[ Expand[matrixhop[mat, complexAbstractOrbital, c[]] -
      matrixhop[mat, complexAbstractOrbital, c[#1, #2]&]], 0 ];
  test[ Expand[matrixhop[mat, c[], complexAbstractOrbital] -
      matrixhop[mat, c[#1, #2]&, complexAbstractOrbital]], 0 ];
];

Print["** Iterator capture regression **"];
snegfermionoperators[{captureSpinA, 1}, {captureSpinB, 1}];
snegspinoperators[captureJ, captureK];

Module[{alphaQ},
  alphaQ[make_Function, q_Symbol] := Module[
    {u = Unique["Global`alphaIndex$"]},
    SameQ[make[q], make[u] /. u -> q]
  ];

  test[ alphaQ[number[captureSpinA[#]] &, Sneg`s], True ];
  test[ alphaQ[hubbard[captureSpinA[#]] &, Sneg`s1], True ];
  test[ alphaQ[hubbard[captureSpinA[#]] &, Sneg`s2], True ];
  test[ alphaQ[
      hamiltonian[Hubbard, c[#], 2, U, t, eps] &, Sneg`i], True ];
  test[ alphaQ[spinxyz[captureSpinA[#]] &, Sneg`s], True ];
  test[ alphaQ[spinspin[captureJ[#], captureK[captureFixed]] &,
    Sneg`i], True ];
  test[ alphaQ[manyisospin[{c[#], d[captureFixed]}] &, Sneg`i], True ];
  test[ alphaQ[hop[captureSpinA[#], captureSpinB[captureFixed]] &,
    Sneg`s], True ];

  test[ alphaQ[basis[c[#]] &, Sneg`n], True ];
  test[ alphaQ[basis[c[#]] &, Sneg`i], True ];
  test[ alphaQ[basis[c[#]] &, Sneg`j], True ];
  test[ alphaQ[basis[captureSpinA[#]] &, Sneg`s], True ];
  test[ alphaQ[mbfunc[captureSpinA[#]] &, Sneg`s], True ];

  test[ alphaQ[spinfliphop[c[#], d[captureFixed]] &,
    Sneg`sigma], True ];
  test[ alphaQ[holehop[c[#], d[captureFixed]] &, Sneg`sigma], True ];
  test[ alphaQ[spinfliphopphi[c[#], d[captureFixed], phi] &,
    Sneg`sigma], True ];
];

Print["** twohop[] **"];
test[ twohop[c[], d[]],
nc[c[0, 0], c[0, 1], d[1, 0], d[1, 1]] + 
  nc[d[0, 0], d[0, 1], c[1, 0], c[1, 1]]
];

Print["** invertspin[] **"];
test[ invertspin[1], 1 ];
test[ invertspin[x], x ];
test[ invertspin[a, b+c, d], invertspin[a,b,d] + invertspin[a,c,d] ];
test[ invertspin[a, 2 b, c], 2 invertspin[a,b,c] ];
test[ invertspin[a, x b, c], x invertspin[a,b,c] ];

test[ invertspin[c[CR,UP]], c[CR,DO] ];
test[ invertspin[c[CR,DO]], c[CR,UP] ];

test[ invertspin[c[CR,i,UP]], c[CR,i,DO] ];
test[ invertspin[c[CR,i,DO]], c[CR,i,UP] ];
test[ invertspin[c[CR,i,j,UP]], c[CR,i,j,DO] ];
test[ invertspin[c[CR,i,j,DO]], c[CR,i,j,UP] ];

test[ invertspin[ nc[c[CR,DO], c[CR,UP]] ], -nc[c[CR,DO], c[CR,UP]], 1.56 ];
test[ invertspin[ nc[c[CR,DO], d[CR,UP]] ], nc[c[CR,UP], d[CR,DO]], 1.56 ];

Print["** Basis for a single site **"];
test[ basis[c[]], {1, c[0, 1], c[0, 0], nc[c[0, 0], c[0, 1]]} ];

Print["** makebasis[] **"];
test[ makebasis[{c[]}], {c[0, 1], c[0, 0]} ];
test[ makebasis[{c[], d[]}], {c[0, 1], c[0, 0], d[0, 1], d[0, 0]} ];

Print["** goodopQ[], op2ndx[] **"];
makebasis[{c[], d[]}];
test[ goodopQ[c[CR, UP]], True ];
test[ goodopQ[b[CR, UP]], False ];
test[ op2ndx[c[CR, UP]], 1 ];
test[ op2ndx[d[CR, DO]], 4 ];
test[ ndx2op[1], c[CR, UP] ];
test[ ndx2op[4], d[CR, DO] ];
test[ vacuum[], vc[0, 0, 0, 0] ];

Print["*** Evaluations of occupation number expressions ***"];
  
test[vc[1], N[vc[1]], 1.56];

Print["** ap **"];
test[ ap[a+b], ap[a] + ap[b] ];
test[ ap[2a], 2 ap[a] ];
test[ ap[x a], x ap[a] ];
test[ ap[a,2,b], 2 ap[a,b] ];

makebasis[{c[], d[]}];
vac = vacuum[];

test[ ap[vac], vac ];

test[ ap[c[AN, UP], vac], 0 ];
test[ ap[c[AN, DO], vac], 0 ];
test[ ap[d[AN, UP], vac], 0 ];
test[ ap[d[AN, DO], vac], 0 ];

test[ ap[c[CR, UP], vac], vc[1, 0, 0, 0] ];
test[ ap[c[CR, DO], vac], vc[0, 1, 0, 0] ];
test[ ap[d[CR, UP], vac], vc[0, 0, 1, 0] ];
test[ ap[d[CR, DO], vac], vc[0, 0, 0, 1] ];
test[ ap[nc[c[CR,UP], c[CR,DO]], vac], vc[1, 1, 0, 0] ];
test[ ap[nc[c[CR,UP], d[CR,UP], c[CR,DO]], vac], -vc[1, 1, 1, 0] ];  
test[ ap[nc[ c[CR, UP], d[CR, UP], c[CR, DO], d[CR, DO]], vac],
  -vc[1, 1, 1, 1] ];
test[ ap[sum[c[CR, UP], {i}], vac], sum[1, {i}] vc[1, 0, 0, 0] ];

test[ ap[{c[CR, UP], c[CR, DO], d[CR, UP], d[AN, UP]}, vac],
{vc[1, 0, 0, 0], vc[0, 1, 0, 0], vc[0, 0, 1, 0], 0}];

(* vc2ops[], vc2opsmap[] *)
test[ vc2ops[vac], 1 ];
test[ vc2ops[ap[c[CR, UP], vac]], c[CR,UP] ];

test[ vc2ops[a+b], vc2ops[a] + vc2ops[b] ];
test[ vc2ops[2a], 2 vc2ops[a] ];
test[ vc2ops[x a], x vc2ops[a] ];
test[ vc2ops[a, 2, b], 2 vc2ops[a, b] ];

makebasis[{}];
test[ vc2ops[vacuum[]], 1 ];
test[ vc2ops[vc[ket[0]]], ket[0] ];

makebasis[{c[], d[]}];
test[ Quiet[vc2ops[vc[]], vc2ops::incom], Infinity ];

(* OLD test[ vc2opsmap[{vac, ap[c[CR, UP], vac]}],
{1, c[CR,UP]} ]; *)

Print["* scalarproduct[] *"];
test[ scalarproduct[a+b,c], scalarproduct[a,c] + scalarproduct[b,c] ];
test[ scalarproduct[a, 2 b], 2 scalarproduct[a, b] ];
test[ scalarproduct[a, x b], x scalarproduct[a, b] ];

test[ scalarproduct[ 1/2 vc[1,0,0], 1/3 vc[1,0,0] ], 1/6 ];
test[ scalarproductvc[ scalar[z] vc[1], vc[1] ], scalar[conj[z]] ];
test[ scalarproductvc[
    vc[0, ket[Null, tensorI]], vc[0, ket[Null, tensorJ]]],
  KroneckerDelta[tensorI, tensorJ] ];
test[ scalarproductvc[vc[1, ket[tensorI]], vc[0, ket[tensorJ]]], 0 ];

Print["* scalarproductop[] *"];
test[ scalarproductop[ c[CR,UP], c[CR,UP] ] , 1 ];
test[ scalarproductop[ scalar[z] c[CR,UP], c[CR,UP] ], scalar[conj[z]] ];
test[ scalarproductop[ c[CR,UP], scalar[z] c[CR,UP] ], scalar[z] ];

test[ normvc[scalar[2] vc[1]], 2 ];
test[ normop[scalar[2] c[CR,UP]], 2 ];
test[ normvc[scalar[1+I] vc[1]], Sqrt[2] ];
test[ normop[scalar[1+I] c[CR,UP]], Sqrt[2] ];

Print["* expvop[] *"];
test[ expvop[ number[c[]], c[CR,UP] ], 1 ];

Print["* matrixrepresentationop[] *"];
bas = basis[c[]];
test[ matrixrepresentationop[number[c[]], bas, bas],
{{0, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 2}}
];

snegrealconstants[t, U];
H = t^2/U number[c[]]
bas = {1, c[CR,UP]};
test[ matrixrepresentationop[H, bas, bas], {{0, 0}, {0, t^2/U}} ];

test[ (Clear[Sneg`tot];
       mambpair[{{0}, {a}}, {{1}, {b}}, op,
         Function[{oparg, vecs1, vecs2}, {{1}}]];
       ValueQ[Sneg`tot]), False ];

Print["* matrixrepresentationvcsparse[] *"];
makebasis[{c[], d[]}];
basvc = {vc[0, 0, 0, 0], vc[1, 0, 0, 0]};
test[ Normal[matrixrepresentationvcsparse[I c[CR, UP], basvc]],
  {{0, 0}, {I, 0}} ];
test[ Normal[matrixrepresentationvcsparse[(1+I) c[AN, UP], basvc]],
  {{0, 1+I}, {0, 0}} ];
test[ Normal[matrixrepresentationvcsparse[(1-I) d[CR, UP], basvc]],
  {{0, 0}, {0, 0}} ];
test[ Normal[matrixrepresentationvcsparse[I c[CR, UP], basvc]],
  matrixrepresentationvc[I c[CR, UP], basvc] ];
test[ Normal[matrixrepresentationvcsparse[(1+I) c[AN, UP], basvc]],
  matrixrepresentationvc[(1+I) c[AN, UP], basvc] ];
test[ Normal[matrixrepresentationvcsparse[(1-I) d[CR, UP], basvc]],
  matrixrepresentationvc[(1-I) d[CR, UP], basvc] ];

makebasis[{c[]}];
test[ Quiet[Check[matrixrepresentationvcfast[c[CR, UP], {vc[0, 0]}], $Failed]],
  {{0}} ];

basvc = {-vc[1, 0]};
test[ matrixrepresentationvcfast[1, basvc],
  matrixrepresentationvc[1, basvc] ];
test[ Normal[matrixrepresentationvcsparse[1, basvc]],
  matrixrepresentationvc[1, basvc] ];

basvc = {I vc[1, 0]};
test[ matrixrepresentationvcfast[1, basvc],
  matrixrepresentationvc[1, basvc] ];
test[ Normal[matrixrepresentationvcsparse[1, basvc]],
  matrixrepresentationvc[1, basvc] ];

basvc = {(vc[1, 0] + I vc[0, 1])/Sqrt[2]};
test[ matrixrepresentationvcfast[1, basvc],
  matrixrepresentationvc[1, basvc] ];
test[ Normal[matrixrepresentationvcsparse[1, basvc]],
  matrixrepresentationvc[1, basvc] ];

test[ matrixrepresentationvcfast[1, {I vc[1, 0]}, {-vc[1, 0]}],
  matrixrepresentationvc[1, {I vc[1, 0]}, {-vc[1, 0]}] ];

basvc = {vc[0, 0], vc[1, 0]};
test[ matrixrepresentationvcfast[1.*^-12, basvc][[1, 1]], 1.*^-12 ];

Module[{auxBasis, auxGram},
  auxBasis = {vc[ket[matrixKetA]], vc[ket[matrixKetB]]};
  auxGram = {
    {1, KroneckerDelta[matrixKetA, matrixKetB]},
    {KroneckerDelta[matrixKetA, matrixKetB], 1}
  };
  test[ matrixrepresentationvcfast[1, auxBasis], auxGram ];
  test[ Normal[matrixrepresentationvcsparse[1, auxBasis]], auxGram ];
  test[
    matrixrepresentationvcfast[1, {First[auxBasis]}, {Last[auxBasis]}],
    {{KroneckerDelta[matrixKetA, matrixKetB]}}
  ];
];

Print["*** Orthogonalisation ***"];

test[ snegorthog[{{0, 1}}] =!= {}, True ];
test[ snegorthog[{{0, 1}, {0, 0}}] =!= {}, True ];
test[ snegorthog[{{Sin[x]^2 + Cos[x]^2 - 1}}], {} ];
test[ snegzeroarrayQ[{orthogUnknown, 0}], False ];
test[ snegzeroarrayQ[{Sin[x]^2 + Cos[x]^2 - 1, 0}], True ];

snegrealconstants[orthogPhase];
Module[{phaseVC, phaseOP},
  phaseVC = vc[1, 0] + Exp[I orthogPhase] vc[0, 1];
  test[
    orthogvc[{phaseVC}, {vc[1, 0], vc[0, 1]}],
    {vc[1, 0]/Sqrt[2] + Exp[I orthogPhase] vc[0, 1]/Sqrt[2]}
  ];
  test[
    normvc[First[orthogvc[{phaseVC}, {vc[1, 0], vc[0, 1]}]]],
    1
  ];

  phaseOP = 1 + Exp[I orthogPhase] c[CR, UP];
  test[
    orthogop[{phaseOP}, {1, c[CR, UP]}],
    {1/Sqrt[2] + Exp[I orthogPhase] c[CR, UP]/Sqrt[2]}
  ];
];

test[ orthogvc[{1/2 vc[1, 0, 0], 1/3vc[1, 0, 0] + 1/4 vc[0, 1, 0]},
  {vc[1, 0, 0], vc[0, 1, 0], vc[0, 0, 1]}],
{vc[1, 0, 0], vc[0, 1, 0]} ];

test[ orthogvc[{1/2 vc[1, 0, 0], 1/3vc[1, 0, 0] + 1/4 vc[0, 1, 0], 
  12 vc[0, 0, 1]},
  {vc[1, 0, 0], 1/Sqrt[2](vc[0, 1, 0] + vc[0, 0, 1]), 
    1/Sqrt[2](vc[0, 1, 0] - vc[0, 0, 1])}],
{vc[1, 0, 0], vc[0, 1, 0], vc[0, 0, 1]} ];


Print["****** GENERATION OF BASIS STATES ******"];

Print["** basis with no symmetries **"];
test[ bzQ[nonebasisvc[{c[]}]], True ];
test[ nonebasisvc[{c[]}], {{{}, {vc[0, 0], vc[0, 1], vc[1, 0], vc[1, 1]}}} ];

snegspinlessfermionoperators[spinlessop];
snegfermionoperators[spin1op];
snegfermionoperators[spin32op];
spinof[spin1op] ^= 1;
spinof[spin32op] ^= 3/2;

test[ basis[spinlessop[]], {1, spinlessop[CR, 0]} ];
test[ basis[spin1op[]], {1, spin1op[CR, 1], spin1op[CR, 0],
  spin1op[CR, -1], nc[spin1op[CR, 0], spin1op[CR, 1]],
  nc[spin1op[CR, -1], spin1op[CR, 1]],
  nc[spin1op[CR, -1], spin1op[CR, 0]],
  nc[spin1op[CR, -1], spin1op[CR, 0], spin1op[CR, 1]]} ];
test[ nonebasisvc[{spinlessop[]}], {{{}, {vc[0], vc[1]}}} ];
test[ Length[nonebasisvc[{spin32op[]}][[1, 2]]], 16 ];

Print["** basis with U(1) charge symmetry **"];
test[ qbasisvc[{c[]}],
{{{-1}, {vc[0, 0]}}, {{0}, {vc[0, 1], vc[1, 0]}},
 {{1}, {vc[1, 1]}}} ];
test[ qbasisvc[{spinlessop[]}],
{{{-1/2}, {vc[0]}}, {{1/2}, {vc[1]}}} ];
test[ Map[{First[#], Length[Last[#]]}&, qbasisvc[{spin32op[]}]],
{{{-2}, 1}, {{-1}, 4}, {{0}, 6}, {{1}, 4}, {{2}, 1}} ];

Print["** (Q,Sz) basis **"];
oplist = {c[], d[]};
makebasis[oplist];
test[ qszbasis[oplist],
{{{-2, 0}, {1}}, {{-1, -1/2}, {d[0, 0], c[0, 0]}}, 
 {{-1, 1/2}, {d[0, 1], c[0, 1]}}, 
 {{0, -1}, {nc[c[0, 0], d[0, 0]]}}, 
 {{0, 0}, {-nc[d[0, 0], d[0, 1]], nc[c[0, 0], d[0, 1]], 
   nc[c[0, 1], d[0, 0]], -nc[c[0, 0], c[0, 1]]}}, 
 {{0, 1}, {nc[c[0, 1], d[0, 1]]}}, 
 {{1, -1/2}, {-nc[c[0, 0], d[0, 0], d[0, 1]], 
   -nc[c[0, 0], c[0, 1], d[0, 0]]}}, 
 {{1, 1/2}, {-nc[c[0, 1], d[0, 0], d[0, 1]], 
   -nc[c[0, 0], c[0, 1], d[0, 1]]}}, 
 {{2, 0}, {nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1]]}}}
];

oplist = {c[], d[], e[]};
makebasis[oplist];
test[ qszbasis[oplist, 1/2],
{{{-3, 0}, {1}}, {{-2, -1/2}, {e[0, 0], d[0, 0], 
   c[0, 0]}}, {{-2, 1/2}, {e[0, 1], d[0, 1], c[0, 1]}}, 
 {{-1, -1}, {nc[d[0, 0], e[0, 0]], nc[c[0, 0], e[0, 0]], 
   nc[c[0, 0], d[0, 0]]}}, 
 {{-1, 0}, {nc[e[0, 1], e[0, 0]], nc[d[0, 0], e[0, 1]], 
   nc[c[0, 0], e[0, 1]], nc[d[0, 1], e[0, 0]], 
   -nc[d[0, 0], d[0, 1]], nc[c[0, 0], d[0, 1]], 
   nc[c[0, 1], e[0, 0]], nc[c[0, 1], d[0, 0]], 
   -nc[c[0, 0], c[0, 1]]}}, 
 {{-1, 1}, {nc[d[0, 1], e[0, 1]], nc[c[0, 1], e[0, 1]], 
   nc[c[0, 1], d[0, 1]]}}, {{0, -3/2}, 
  {nc[c[0, 0], d[0, 0], e[0, 0]]}}, 
 {{0, -1/2}, {nc[d[0, 0], e[0, 1], e[0, 0]], 
   nc[c[0, 0], e[0, 1], e[0, 0]], nc[c[0, 0], d[0, 0], 
    e[0, 1]], -nc[d[0, 0], d[0, 1], e[0, 0]], 
   nc[c[0, 0], d[0, 1], e[0, 0]], 
   -nc[c[0, 0], d[0, 0], d[0, 1]], nc[c[0, 1], d[0, 0], 
    e[0, 0]], -nc[c[0, 0], c[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], d[0, 0]]}}, 
 {{0, 1/2}, {nc[d[0, 1], e[0, 1], e[0, 0]], 
   -nc[d[0, 0], d[0, 1], e[0, 1]], nc[c[0, 0], d[0, 1], 
    e[0, 1]], nc[c[0, 1], e[0, 1], e[0, 0]], 
   nc[c[0, 1], d[0, 0], e[0, 1]], 
   -nc[c[0, 0], c[0, 1], e[0, 1]], nc[c[0, 1], d[0, 1], 
    e[0, 0]], -nc[c[0, 1], d[0, 0], d[0, 1]], 
   -nc[c[0, 0], c[0, 1], d[0, 1]]}}, 
 {{0, 3/2}, {nc[c[0, 1], d[0, 1], e[0, 1]]}}, 
 {{1, -1}, {nc[c[0, 0], d[0, 0], e[0, 1], e[0, 0]], 
   -nc[c[0, 0], d[0, 0], d[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], d[0, 0], e[0, 0]]}}, 
 {{1, 0}, {-nc[d[0, 0], d[0, 1], e[0, 1], e[0, 0]], 
   nc[c[0, 0], d[0, 1], e[0, 1], e[0, 0]], 
   -nc[c[0, 0], d[0, 0], d[0, 1], e[0, 1]], 
   nc[c[0, 1], d[0, 0], e[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], e[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], d[0, 0], e[0, 1]], 
   -nc[c[0, 1], d[0, 0], d[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], d[0, 1], e[0, 0]], 
   nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1]]}}, 
 {{1, 1}, {nc[c[0, 1], d[0, 1], e[0, 1], e[0, 0]], 
   -nc[c[0, 1], d[0, 0], d[0, 1], e[0, 1]], 
   -nc[c[0, 0], c[0, 1], d[0, 1], e[0, 1]]}}, 
 {{2, -1/2}, {-nc[c[0, 0], d[0, 0], d[0, 1], e[0, 1], 
     e[0, 0]], -nc[c[0, 0], c[0, 1], d[0, 0], e[0, 1], 
     e[0, 0]], nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1], 
    e[0, 0]]}}, {{2, 1/2}, 
  {-nc[c[0, 1], d[0, 0], d[0, 1], e[0, 1], e[0, 0]], 
   -nc[c[0, 0], c[0, 1], d[0, 1], e[0, 1], e[0, 0]], 
   nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1], e[0, 1]]}}, 
 {{3, 0}, {nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1], 
    e[0, 1], e[0, 0]]}}}
];

Print["* Miscelaneous functions *"];
test[ interleave[{a,b,c},{1,2,3}], {a, 1, b, 2, c, 3} ];
test[ spinflip[c[CR,UP]], invertspin[c[CR,UP]] ];
test[ keymerge[{{1, {a, b}}, {2, {c, d}}, {1, {a2, b2}},
    {2, {c2, d2}}}],
{{1, {a, b, a2, b2}}, {2, {c, d, c2, d2}}}];
test[ keymerge[{{1, {a, b}}, {2, {c, d}}, {1, {a2, b2}},
    {2, {c2, d2}}}],
mergebasis[{{1, {a, b}}, {2, {c, d}}, {1, {a2, b2}},
    {2, {c2, d2}}}]
];

Print["** applytocr/spindown zero normalization **"];
test[applytocr[c[AN, UP], c[CR, DO]], 0];
test[spindown[c[CR, DO]], 0];
test[spindownvc[vc[1, 0, ket[tensorX]]], vc[0, 1, ket[tensorX]]];
test[spindownvc[vc[ket[tensorX]]], 0];

Print["** spindown **"];
test[spindown[c[CR, UP]], c[0, 0]];
test[spindown[c[CR, 1, UP]], c[0, 1, 0]];
test[spindown[nc[c[CR, 1, UP], c[CR, 2, UP]]], (nc[c[0, 1, 0], c[0, 2, 1]] + 
nc[c[0, 1, 1], c[0, 2, 0]])/Sqrt[2]];
test[spindown[nc[c[CR, 1, UP], c[CR, 2, DO]]], nc[c[0, 1, 0], c[0, 2, 0]]];
test[spindown[nc[c[CR, 1, DO], c[CR, 2, UP]]], nc[c[0, 1, 0], c[0, 2, 0]]];
test[spindown[spindown[nc[c[CR, 1, UP], c[CR, 2, UP]]]], nc[c[0, 1, 0], 
c[0, 2, 0]]];
test[spindown[nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP]]], (nc[c[0, 1, 0], 
c[0, 2, 1], c[0, 3, 1]] + nc[c[0, 1, 1], c[0, 2, 0], c[0, 3, 1]] + 
nc[c[0, 1, 1], c[0, 2, 1], c[0, 3, 0]])/Sqrt[3]];
test[spindown[nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP], c[CR, 4, UP]]], 
(nc[c[0, 1, 0], c[0, 2, 1], c[0, 3, 1], c[0, 4, 1]] + nc[c[0, 1, 1], 
c[0, 2, 0], c[0, 3, 1], c[0, 4, 1]] + nc[c[0, 1, 1], c[0, 2, 1], c[0, 3, 0], 
c[0, 4, 1]] + nc[c[0, 1, 1], c[0, 2, 1], c[0, 3, 1], c[0, 4, 0]])/2];

makebasis[{c[1], c[2], c[3], c[4]}];
test[spindownvc[ap[1, VACUUM]], 0];
test[spindownvc[ap[c[CR, 1, UP], VACUUM]], vc[0, 1, 0, 0, 0, 0, 0, 0]];
test[spindownvc[ap[c[CR, 1, DO], VACUUM]], 0];
test[spindownvc[ap[nc[c[CR, 1, UP], c[CR, 1, DO]], VACUUM]], 0];

expr = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];

expr = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, UP], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, UP], c[CR, 3, DO], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 3, UP], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 3, UP], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 3, DO], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];

expr = nc[c[CR, 1, DO], c[CR, 2, UP], c[CR, 3, UP], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, UP], c[CR, 3, UP], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, UP], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, UP], c[CR, 3, DO], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, DO], c[CR, 3, UP], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, DO], c[CR, 3, UP], c[CR, 4, DO]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, DO], c[CR, 2, DO], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];

expr = nc[c[CR, 1, UP], c[CR, 3, UP], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 2, UP], c[CR, 2, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 2, UP], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];
expr = nc[c[CR, 2, DO], c[CR, 3, DO], c[CR, 4, UP]];
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];

expr = 1/Sqrt[2](nc[c[CR, 1, DO], c[CR, 2, UP], c[CR, 3, UP]] - 
        nc[c[CR, 1, UP], c[CR, 2, DO], c[CR, 3, UP]]);
test[ap[spindown[expr], VACUUM], spindownvc[ap[expr, VACUUM]]];

Print["** (Q,S) basis **"];
test[ qsbasisvc[{}], {{{0, 0}, {vc[]}}} ];
test[ qsbasis[{}], {{{0, 0}, {1}}} ];
test[ sbasisvc[{}], {{{0}, {vc[]}}} ];
test[ sbasis[{}], {{{0}, {1}}} ];
test[ Quiet[qsbasis[{spinlessop[]}], qsbasis::spin], $Failed ];
test[ Quiet[Check[qsbasis[{spinlessop[]}], qsbasisSpinMessage,
  qsbasis::spin], qsbasis::spin], qsbasisSpinMessage ];
test[ Quiet[qsbasisvc[{spinlessop[]}], qsbasisvc::spin], $Failed ];
test[ Quiet[Check[qsbasisvc[{spinlessop[]}], qsbasisvcSpinMessage,
  qsbasisvc::spin], qsbasisvc::spin], qsbasisvcSpinMessage ];

oplist = {c[], d[]};
makebasis[oplist];
test[ Simplify @ qsbasis[oplist],
Simplify @ {{{-2, 0}, {1}}, {{-1, 1/2}, {d[0, 1], c[0, 1]}}, 
 {{0, 0}, {nc[d[0, 0], d[0, 1]], 
   nc[c[0, 0], d[0, 1]]/Sqrt[2] - nc[c[0, 1], d[0, 0]]/
     Sqrt[2], nc[c[0, 0], c[0, 1]]}}, 
 {{0, 1}, {-nc[c[0, 1], d[0, 1]]}}, 
 {{1, 1/2}, {nc[c[0, 1], d[0, 0], d[0, 1]], 
   nc[c[0, 0], c[0, 1], d[0, 1]]}}, 
 {{2, 0}, {nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1]]}}}
];

oplist = {c[], d[], e[]};
makebasis[oplist];
test[ Simplify @ qsbasis[oplist],
Simplify @ {{{-3, 0}, {1}}, {{-2, 1/2}, {e[0, 1], d[0, 1], 
   c[0, 1]}}, {{-1, 0}, {nc[e[0, 0], e[0, 1]], 
   nc[d[0, 0], e[0, 1]]/Sqrt[2] - nc[d[0, 1], e[0, 0]]/
     Sqrt[2], nc[c[0, 0], e[0, 1]]/Sqrt[2] - 
    nc[c[0, 1], e[0, 0]]/Sqrt[2], nc[d[0, 0], d[0, 1]], 
   nc[c[0, 0], d[0, 1]]/Sqrt[2] - nc[c[0, 1], d[0, 0]]/
     Sqrt[2], nc[c[0, 0], c[0, 1]]}}, 
 {{-1, 1}, {-nc[d[0, 1], e[0, 1]], 
   -nc[c[0, 1], e[0, 1]], -nc[c[0, 1], d[0, 1]]}}, 
 {{0, 1/2}, {nc[d[0, 1], e[0, 0], e[0, 1]], 
   nc[c[0, 1], e[0, 0], e[0, 1]], nc[d[0, 0], d[0, 1], 
    e[0, 1]], nc[c[0, 0], d[0, 1], e[0, 1]]/Sqrt[2] - 
    nc[c[0, 1], d[0, 0], e[0, 1]]/Sqrt[2], 
   nc[c[0, 0], c[0, 1], e[0, 1]], 
   -((-nc[c[0, 0], d[0, 1], e[0, 1]] - 
       nc[c[0, 1], d[0, 0], e[0, 1]])/Sqrt[6]) - 
    Sqrt[2/3]*nc[c[0, 1], d[0, 1], e[0, 0]], 
   nc[c[0, 1], d[0, 0], d[0, 1]], nc[c[0, 0], c[0, 1], 
    d[0, 1]]}}, {{0, 3/2}, 
  {-nc[c[0, 1], d[0, 1], e[0, 1]]}}, 
 {{1, 0}, {nc[d[0, 0], d[0, 1], e[0, 0], e[0, 1]], 
   nc[c[0, 0], d[0, 1], e[0, 0], e[0, 1]]/Sqrt[2] - 
    nc[c[0, 1], d[0, 0], e[0, 0], e[0, 1]]/Sqrt[2], 
   nc[c[0, 0], c[0, 1], e[0, 0], e[0, 1]], 
   nc[c[0, 0], d[0, 0], d[0, 1], e[0, 1]]/Sqrt[2] - 
    nc[c[0, 1], d[0, 0], d[0, 1], e[0, 0]]/Sqrt[2], 
   nc[c[0, 0], c[0, 1], d[0, 0], e[0, 1]]/Sqrt[2] - 
    nc[c[0, 0], c[0, 1], d[0, 1], e[0, 0]]/Sqrt[2], 
   nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1]]}}, 
 {{1, 1}, {-nc[c[0, 1], d[0, 1], e[0, 0], e[0, 1]], 
   -nc[c[0, 1], d[0, 0], d[0, 1], e[0, 1]], 
   -nc[c[0, 0], c[0, 1], d[0, 1], e[0, 1]]}}, 
 {{2, 1/2}, {nc[c[0, 1], d[0, 0], d[0, 1], e[0, 0], 
    e[0, 1]], nc[c[0, 0], c[0, 1], d[0, 1], e[0, 0], 
    e[0, 1]], nc[c[0, 0], c[0, 1], d[0, 0], d[0, 1], 
    e[0, 1]]}}, {{3, 0}, {nc[c[0, 0], c[0, 1], d[0, 0], 
    d[0, 1], e[0, 0], e[0, 1]]}}}
];

Print["** (I,S) basis **"];
oplist = {c[]};
makebasis[oplist];
bz = qsbasis[oplist];
bvc = bzop2bzvc[bz, vacuum[]];
Tops = manyisospin [oplist, {1}];
Tminus = Simplify[Tops[[1]] - I Tops[[2]]];

test[ transformQStoIS[bvc],
  {{{1/2, 0}, {-vc[1, 1]}}, {{0, 1/2}, {vc[1, 0]}}}
];


oplist = {c[], d[]};
makebasis[oplist];
bz = qsbasis[oplist];
bvc = bzop2bzvc[bz, vacuum[]];
Tops = manyisospin [oplist, {1, 1}];
Tminus = Simplify[Tops[[1]] - I Tops[[2]]];

test[ transformQStoIS[bvc],
{{{1, 0}, {vc[1, 1, 1, 1]}}, {{1/2, 1/2}, 
  {-vc[1, 0, 1, 1], -vc[1, 1, 1, 0]}}, 
 {{0, 1}, {-vc[1, 0, 1, 0]}}, 
 {{0, 0}, {-(vc[0, 0, 1, 1]/Sqrt[2]) + 
    vc[1, 1, 0, 0]/Sqrt[2], vc[0, 1, 1, 0]/Sqrt[2] - 
    vc[1, 0, 0, 1]/Sqrt[2]}}}
];

oplist = {c[], d[], e[]};
makebasis[oplist];
bz = qsbasis[oplist];
bvc = bzop2bzvc[bz, vacuum[]];
Tops = manyisospin [oplist, {1, 1, 1}];
Tminus = Simplify[Tops[[1]] - I Tops[[2]]];

test[ transformQStoIS[bvc],
{{{3/2, 0}, {-vc[1, 1, 1, 1, 1, 1]}}, 
 {{1, 1/2}, {vc[1, 0, 1, 1, 1, 1], vc[1, 1, 1, 0, 1, 1], 
   vc[1, 1, 1, 1, 1, 0]}}, {{1/2, 1}, 
  {vc[1, 0, 1, 0, 1, 1], vc[1, 0, 1, 1, 1, 0], 
   vc[1, 1, 1, 0, 1, 0]}}, {{1/2, 0}, 
  {Sqrt[2/3]*vc[0, 0, 1, 1, 1, 1] - vc[1, 1, 0, 0, 1, 1]/
     Sqrt[6] - vc[1, 1, 1, 1, 0, 0]/Sqrt[6], 
   -(vc[0, 1, 1, 0, 1, 1]/Sqrt[2]) + 
    vc[1, 0, 0, 1, 1, 1]/Sqrt[2], 
   vc[1, 1, 0, 0, 1, 1]/Sqrt[2] - vc[1, 1, 1, 1, 0, 0]/
     Sqrt[2], -(vc[0, 1, 1, 1, 1, 0]/Sqrt[2]) + 
    vc[1, 0, 1, 1, 0, 1]/Sqrt[2], 
   -(vc[1, 1, 0, 1, 1, 0]/Sqrt[2]) + 
    vc[1, 1, 1, 0, 0, 1]/Sqrt[2]}}, 
 {{0, 3/2}, {-vc[1, 0, 1, 0, 1, 0]}}, 
 {{0, 1/2}, {-(vc[0, 0, 1, 0, 1, 1]/Sqrt[2]) + 
    vc[1, 1, 1, 0, 0, 0]/Sqrt[2], 
   -(vc[1, 0, 0, 0, 1, 1]/Sqrt[2]) + 
    vc[1, 0, 1, 1, 0, 0]/Sqrt[2], 
   -(vc[0, 0, 1, 1, 1, 0]/Sqrt[2]) + 
    vc[1, 1, 0, 0, 1, 0]/Sqrt[2], 
   vc[0, 1, 1, 0, 1, 0]/Sqrt[2] - vc[1, 0, 0, 1, 1, 0]/
     Sqrt[2], vc[0, 1, 1, 0, 1, 0]/Sqrt[6] + 
    vc[1, 0, 0, 1, 1, 0]/Sqrt[6] - 
    Sqrt[2/3]*vc[1, 0, 1, 0, 0, 1]}}}
];

Print["* LR Z_2 symmetry *"];

test[mbfunc[c[]], {c[0, 1], c[0, 0]}];
test[mbfunc[{c[]}], {{c[0, 1], c[0, 0]}}];
test[mbfunc[{{c[]}}], {{{c[0, 1], c[0, 0]}}}];

test[ lrmap[{c[]}],
{c[0, 1] -> c[0, 1], c[0, 0] -> c[0, 0]}
];

test[ lrmap[{c[], b[]}],
{c[0, 1] -> b[0, 1], c[0, 0] -> b[0, 0], b[0, 1] -> c[0, 1],
b[0, 0] -> c[0, 0]} 
];

test[ lrmap[{a[], b[], c[]}],
{a[0, 1] -> c[0, 1], a[0, 0] -> c[0, 0], b[0, 1] -> b[0, 1],
b[0, 0] -> b[0, 0], c[0, 1] -> a[0, 1], c[0, 0] -> a[0, 0]}
];

test[ lrmap[{a[], {b[], c[]}, d[]}],
{a[0, 1] -> d[0, 1], a[0, 0] -> d[0, 0], b[0, 1] -> b[0, 1],
 b[0, 0] -> b[0, 0], c[0, 1] -> c[0, 1], c[0, 0] -> c[0, 0],
 d[0, 1] -> a[0, 1], d[0, 0] -> a[0, 0]}
];
 
test[ lrmap[{{a[], b[]}, {c[], d[]}}],
{a[0, 1] -> c[0, 1], a[0, 0] -> c[0, 0], b[0, 1] -> d[0, 1],
 b[0, 0] -> d[0, 0], c[0, 1] -> a[0, 1], c[0, 0] -> a[0, 0],
 d[0, 1] -> b[0, 1], d[0, 0] -> b[0, 0]}
];

test[ lrmap[{e[], {a[], b[]}, {c[], d[]}, f[]}],
{e[0, 1] -> f[0, 1], e[0, 0] -> f[0, 0], a[0, 1] -> c[0, 1],
 a[0, 0] -> c[0, 0], b[0, 1] -> d[0, 1], b[0, 0] -> d[0, 0],
 c[0, 1] -> a[0, 1], c[0, 0] -> a[0, 0], d[0, 1] -> b[0, 1],
 d[0, 0] -> b[0, 0], f[0, 1] -> e[0, 1], f[0, 0] -> e[0, 0]}
];

Print["* Projection operators *"];
test[ projector[c[], PROJ0], 1 - nc[c[0, 0], c[1, 0]] - nc[c[0, 1], c[1, 1]] - 
  nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projector[c[], PROJUP],
nc[c[0, 1], c[1, 1]] + nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projector[c[], PROJDO],
nc[c[0, 0], c[1, 0]] + nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projector[c[], PROJ2],
-nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projector[c[], PROJ1],
nc[c[0, 0], c[1, 0]] + nc[c[0, 1], c[1, 1]] + 
  2 nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projector[c[], PROJ02],
1 - nc[c[0, 0], c[1, 0]] - nc[c[0, 1], c[1, 1]] - 
  2 nc[c[0, 0], c[0, 1], c[1, 0], c[1, 1]] ];
test[ projectorEphi[c, phi], projectorEphi[c[], phi] ];
test[ projectorOphi[c, phi], projectorOphi[c[], phi] ];


Print["*** Bra-ket notation ***"];

Print["* Helper functions *"];
test[ pairpattern[{Null}, {Null}], True ];
test[ pairpattern[{1}, {2}], True ];
test[ pairpattern[{Null, Null}, {Null, Null}], True ];
test[ pairpattern[{Null, 1}, {Null, 1}], True ];
test[ pairpattern[{1, Null}, {1, Null}], True ];
test[ pairpattern[{1, 2}, {3, 4}], True ];

test[ pairpattern[{1}, {Null}], False ];
test[ pairpattern[{Null}, {1}], False ];
test[ pairpattern[{Null, Null}, {Null, 1}], False ];
test[ pairpattern[{Null, Null}, {1, 1}], False ];
test[ pairpattern[{Null, 1}, {1, 1}], False ];
test[ pairpattern[{Null, 1}, {Null, Null}], False ];
test[ pairpattern[{1, 1}, {Null, Null}], False ];
test[ pairpattern[{Null, Null, x, Null}, {Null, Null, y, 1}], False ];

test[ nonnullpos[{Null, 1, 2, Null, 5}], {{2}, {3}, {5}} ];

test[ nonnullpattern[{a, b, Null, d, e, Null}], 
  {True, True, False, True, True, False} ];

test[ compatiblepattern[{Null}, {Null}], True ];
test[ compatiblepattern[{Null}, {a}], True ];
test[ compatiblepattern[{b}, {Null}], True ];

test[ compatiblepattern[{b}, {b}], False ];
test[ compatiblepattern[{a, b, Null}, {c, d, Null}], False];

test[ bkcombine[{Null}, {Null}], {Null} ];
test[ bkcombine[{Null}, {a}], {a} ];
test[ bkcombine[{a}, {Null}], {a} ];

Print["* Simple bra-kets *"];

snegrealconstants[y,w,z];

test[ nc[KroneckerDelta[x,y]], KroneckerDelta[x,y] ];
test[ nc[KroneckerDelta[x,y] KroneckerDelta[w,z]],
  KroneckerDelta[x,y] KroneckerDelta[w,z] ];
test[ nc[KroneckerDelta[x,y], KroneckerDelta[w,z]],
  KroneckerDelta[x,y] KroneckerDelta[w,z] ];

test[ nc[bra[1], ket[1]], 1 ];
test[ nc[bra[1], ket[2]], 0 ];
test[ nc[bra[x], ket[y]], KroneckerDelta[x, y] ];
test[ nc[bra[x], ket[y], bra[w], ket[z]], 
  KroneckerDelta[x,y] KroneckerDelta[w,z] ];

test[ nc[bra[Null], ket[Null]], 1 ];
test[ nc[bra[Null, Null], ket[Null, Null]], 1 ];
test[ nc[bra[Null, Null, x, Null], ket[Null, Null, y, Null]],
  KroneckerDelta[x,y] ];

test[ Apply[List, nc[bra[Null, 2], ket[1, Null]]],
  {bra[Null, 2], ket[1, Null]} ];

test[ nc[ket[1], bra[1], c[]], nc[c[], ket[1], bra[1]] ];
test[ nc[bra[], c[]], nc[c[], bra[]] ];
test[ nc[c[], bra[], d[]], nc[c[], d[], bra[]] ];
test[ nc[bra[1], c[], ket[1]], c[] ];

Print["* Concatenation of brakets *"];
test[ nc[ket[Null, y1], ket[x1, Null]], ket[x1, y1] ];
test[ nc[ket[x1, Null], ket[Null, y1]], ket[x1, y1] ];
test[ nc[bra[Null, y2], bra[x2, Null]], bra[x2, y2] ];
test[ nc[bra[x2, Null], bra[Null, y2]], bra[x2, y2] ];

test[
  nc[bra[1, Null], bra[Null, 2], ket[1, Null]],
  nc[bra[1, Null], nc[bra[Null, 2], ket[1, Null]]]
];

test[
  nc[ket[tensorK1, Null], bra[tensorB1, Null],
    ket[Null, tensorK2], bra[Null, tensorB2]],
  nc[ket[tensorK1, tensorK2], bra[tensorB1, tensorB2]]
];

test[ nc[ket[1], bra[2], ket[2], bra[3]],
  nc[ket[1], bra[3]] ];

test[ nc[ nc[bra[x2, Null], bra[Null, y2]], nc[ket[Null, y1], ket[x1, Null]] ],
KroneckerDelta[x1, x2] KroneckerDelta[y1, y2] ];

Print["* Macro bras and kets *"];
test[ spinket[1/2], {ket[1/2], ket[-1/2]} ];
test[ spinbra[1/2], {bra[1/2], bra[-1/2]}];

test[ spinbasis[1/2],
  {{{1/2}, {ket[1/2]}}, {{-1/2}, {ket[-1/2]}}} ];
test[ spinbasis[1/2, {0, 1}],
  {{{1/2}, {ket[Null, 1/2]}}, {{-1/2}, {ket[Null, -1/2]}}} ];
test[ spinbasis[{1/2, 1/2}],
  {{{-1}, {ket[-1/2, -1/2]}},
   {{0}, {ket[1/2, -1/2], ket[-1/2, 1/2]}},
   {{1}, {ket[1/2, 1/2]}}} ];
test[ spinbasis[{1/2, 1}],
  {{{-3/2}, {ket[-1/2, -1]}},
   {{-1/2}, {ket[1/2, -1], ket[-1/2, 0]}},
   {{1/2}, {ket[1/2, 0], ket[-1/2, 1]}},
   {{3/2}, {ket[1/2, 1]}}} ];
test[ spinbasis[{1/2, 1/2, 1/2}],
  {{{-3/2}, {ket[-1/2, -1/2, -1/2]}},
   {{-1/2}, {ket[-1/2, -1/2, 1/2], ket[1/2, -1/2, -1/2],
     ket[-1/2, 1/2, -1/2]}},
   {{1/2}, {ket[1/2, -1/2, 1/2], ket[-1/2, 1/2, 1/2],
     ket[1/2, 1/2, -1/2]}},
   {{3/2}, {ket[1/2, 1/2, 1/2]}}} ];

test[ spinketbraZ[1/2], 
  -nc[ket[-1/2], bra[-1/2]]/2 + nc[ket[1/2], bra[1/2]]/2 ];
test[ spinketbraP[1/2],
  nc[ket[1/2], bra[-1/2]] ];
test[ spinketbraM[1/2],
  nc[ket[-1/2], bra[1/2]] ];
test[ spinketbraX[1/2],
  nc[ket[-1/2], bra[1/2]]/2 + nc[ket[1/2], bra[-1/2]]/2 ];
test[ spinketbraY[1/2],
  (I/2)*nc[ket[-1/2], bra[1/2]] - 
 (I/2)*nc[ket[1/2], bra[-1/2]]
];


Print["* Phonons *"];
testNph=5;
test[ phononnumber[testNph], 
nc[ket[1], bra[1]] + 2 nc[ket[2], bra[2]] + 3 nc[ket[3], bra[3]] + 
4 nc[ket[4], bra[4]] + 5 nc[ket[5], bra[5]]
];
test[ nc[phononnumber[testNph], phononnumber[testNph]],
nc[ket[1], bra[1]] + 4 nc[ket[2], bra[2]] + 9 nc[ket[3], bra[3]] + 
16 nc[ket[4], bra[4]] + 25 nc[ket[5], bra[5]]
];
test[ phononplus[testNph], 
nc[ket[1], bra[0]] + Sqrt[2]*nc[ket[2], bra[1]] + 
Sqrt[3]*nc[ket[3], bra[2]] + 2*nc[ket[4], bra[3]] + 
Sqrt[5]*nc[ket[5], bra[4]]
];
test[ phononminus[testNph],
nc[ket[0], bra[1]] + Sqrt[2]*nc[ket[1], bra[2]] + 
Sqrt[3]*nc[ket[2], bra[3]] + 2*nc[ket[3], bra[4]] + 
Sqrt[5]*nc[ket[4], bra[5]]
];
test[ phononbasis[{1, 1, 1}],
{ket[0, 0, 0], ket[0, 0, 1], ket[0, 1, 0], ket[0, 1, 1],
 ket[1, 0, 0], ket[1, 0, 1], ket[1, 1, 0], ket[1, 1, 1]}
];
test[ ketbratensorproduct[
  nc[ket[ka], bra[ba]], nc[ket[kb], bra[bb]], nc[ket[kc], bra[bc]]],
nc[ket[ka, kb, kc], bra[ba, bb, bc]]
];
test[ ketbratensorproduct[ket[1], ket[2]], ket[1, 2] ];
test[ ketbratensorproduct[ket[ka], ket[kb], ket[kc]], ket[ka, kb, kc] ];
test[ phononnumber[2, {1, 1, 1}],
nc[ket[0, 1, 0], bra[0, 1, 0]] +
nc[ket[0, 1, 1], bra[0, 1, 1]] +
nc[ket[1, 1, 0], bra[1, 1, 0]] +
nc[ket[1, 1, 1], bra[1, 1, 1]]
];

test[
  Quiet[
    {phononnumber[3, {1, 1}], phononplus[3, {1, 1}],
      phononminus[3, {1, 1}], phononx[3, {1, 1}]},
    {phononnumber::mode, phononplus::mode,
      phononminus::mode, phononx::mode}],
  ConstantArray[$Failed, 4]
];
test[
  {Quiet[Check[phononnumber[3, {1, 1}], phononModeMessage,
      phononnumber::mode], phononnumber::mode],
    Quiet[Check[phononplus[3, {1, 1}], phononModeMessage,
      phononplus::mode], phononplus::mode],
    Quiet[Check[phononminus[3, {1, 1}], phononModeMessage,
      phononminus::mode], phononminus::mode],
    Quiet[Check[phononx[3, {1, 1}], phononModeMessage,
      phononx::mode], phononx::mode]},
  ConstantArray[phononModeMessage, 4]
];
test[
  Quiet[
    Map[phononnumber[#, {1, 1}] &, {0, -1, 3, 3/2, 1., phononMode}],
    phononnumber::mode],
  ConstantArray[$Failed, 6]
];
test[
  {phononnumber[1, {2}], phononplus[1, {2}],
    phononminus[1, {2}], phononx[1, {2}]},
  {phononnumber[2], phononplus[2], phononminus[2], phononx[2]}
];
test[
  {phononnumber[1, {1, 1}], phononnumber[2, {1, 1}]},
  {ketbratensorproduct[phononnumber[1], phononid[1]],
    ketbratensorproduct[phononid[1], phononnumber[1]]}
];

Print["** Grassman numbers **"];
sneggrassmanconstants[zz, z1, z2];
test[conj[conj[zz]], zz];
test[nc[zz, zz], 0];
test[nc[conj[zz], conj[zz]], 0];
test[nc[conj[zz], zz], -nc[zz, conj[zz]]];
test[conj[nc[conj[zz], zz]], -nc[zz, conj[zz]]];
test[conj[nc[z1,z2]], -nc[conj[z1], conj[z2]]];

test[antikomutator[z1,z2], 0];
test[komutator[z1,z2], 2 nc[z1, z2]];

test[nc[c[CR], zz], -nc[zz, c[CR]]];
test[nc[c[CR], z1, z2], nc[z1, z2, c[CR]]];
test[nc[z1, c[CR], z2], -nc[z1, z2, c[CR]]];
test[nc[m1, z1], -nc[z1, m1]];
test[nc[m1, z1] + nc[z1, m1], 0];
test[conj @ nc[z1,z2,c[CR]], -nc[conj[z1], conj[z2], c[AN]]];

test[isnumericQ[z1], False];
test[
  {vev[z1], vevwick[z1], vevwick2[z1], vevwicknew[z1]},
  ConstantArray[z1, 4]
];
test[
  {vev[nc[z1, z2]], vevwick[nc[z1, z2]],
    vevwick2[nc[z1, z2]], vevwicknew[nc[z1, z2]]},
  ConstantArray[nc[z1, z2], 4]
];
test[
  {vev[nc[z1, c[AN], c[CR]]],
    vev[nc[c[AN], z1, c[CR]]],
    vev[nc[c[AN], c[CR], z1]]},
  {z1, -z1, z1}
];
test[vev[nc[z1, c[AN], z2, c[CR]]], -nc[z1, z2]];
test[vev[nc[c[AN], z1, z2, c[CR]]], nc[z1, z2]];
test[vev[nc[m1, z1, m1]], -z1/2];
test[
  {normalorder[nc[z1, c[AN], c[CR]]],
    normalorderwick[nc[z1, c[AN], c[CR]]]},
  ConstantArray[-nc[z1, c[CR], c[AN]], 2]
];

snegfermionoperators[grassVevFermion];
ordering[grassVevFermion] = NONE;
test[
  {vevwick[nc[z1, grassVevFermion[AN], grassVevFermion[CR]]],
    vevwick2[nc[z1, grassVevFermion[AN], grassVevFermion[CR]]],
    vevwicknew[nc[z1, grassVevFermion[AN], grassVevFermion[CR]]]},
  ConstantArray[z1, 3]
];

snegbosonoperators[grassVevBoson];
ordering[grassVevBoson] = NONE;
Module[{expr = nc[grassVevBoson[AN], z1, grassVevBoson[CR]]},
  test[expr, nc[z1, grassVevBoson[AN], grassVevBoson[CR]]];
  test[
    {vev[expr], vevwick[expr], vevwick2[expr], vevwicknew[expr]},
    ConstantArray[z1, 4]
  ];
];

Print["* acmtcount *"];
test[acmtcount[{}], 0];
test[acmtcount[{2}], 0];
test[acmtcount[{Exp[zz]}], 0];
test[acmtcount[{zz}], 1];
test[acmtcount[{zz, conj[zz]}], 2];
test[acmtcount[{c[CR]}], 1];
test[acmtcount[{c[CR], c[AN]}], 2];
test[acmtcount[{m1}], 1];
test[acmtcount[{m1[1]}], 1];
test[acmtcount[{c[CR], c[AN], zz}], 3];
test[acmtcount[{c[CR], c[AN], zz, conj[zz]}], 4];

Print["* Berezin integral *"];
test[nc[int[z1]], 0];
test[nc[int[z1],z1], 1];
test[nc[int[z1,z2]], 0];
test[nc[int[z1],int[z1],z1], 0];

test[ nc[int[z1], z1, z2], z2 ];
test[ nc[int[z1], z2, z1], -z2 ];
test[ nc[int[z1], m1, z1], -m1 ];
test[ nc[int[z2], z1, z2], -z1 ];
test[ nc[int[z2], z2, z1], z1 ];

test[ nc[int[z1, z2], z1, z2], 1 ];
test[ nc[int[z1, z2], z2, z1], -1 ]; 
test[ nc[int[z2, z1], z1, z2], 1 ];
test[ nc[int[z2, z1], z2, z1], -1];

(* ??? *)

test[ nc[ int[z1], int[z2], z2, z1], -1 ];
test[ nc[int[z1], vev[nc[z1, c[AN], c[CR]]]], 1 ];
test[
  nc[int[z1, z2], vev[nc[z1, z2, c[AN], c[CR]]]],
  1
];

Print["* Creation/annihilation *"];

test[ordering[c] == EMPTY, True];

test[isannihilation[c[AN]], True];
test[isannihilation[c[CR]], False];
test[iscreation[c[AN]], False];
test[iscreation[c[CR]], True];

test[nc[c[AN], VACUUM], 0];
test[nc[conj[VACUUM], c[CR]], 0];

test[nc[c[CR], VACUUM], 0, 0, True];
test[nc[conj[VACUUM], c[AN]], 0, 0, True];

test[nc[conj[VACUUM], VACUUM], 1];

Print["* Coherent states *"];

test[ xx = coh[zz, c[]], 
VACUUM - nc[zz, c[0], VACUUM] + nc[zz, conj[zz], VACUUM]/2
];
test[ nc[int[zz], xx],
-nc[c[0], VACUUM] + nc[conj[zz], VACUUM]/2
];
test[ nc[int[zz, conj[zz]], xx], VACUUM/2 ];

test[ xx = conj[coh[zz, c[]]],
conj[VACUUM] + nc[zz, conj[zz], conj[VACUUM]]/2 +
  nc[conj[zz], conj[VACUUM], c[1]]
];

test[ nc[int[zz], xx],
  nc[conj[zz], conj[VACUUM]]/2
];
test[ nc[int[conj[zz]], xx],
  -nc[zz, conj[VACUUM]]/2 + nc[conj[VACUUM], c[1]]
];
test[ nc[int[zz, conj[zz]], xx],
  conj[VACUUM]/2
];
test[ nc[int[conj[zz], zz], xx],
  conj[VACUUM]/2
];
  
  

test[Expand[nc[conj[coh[z1, c[]]], coh[z2, c[]]]], 1 + nc[z1, 
conj[z1]]/2 - nc[z2, conj[z1]] + nc[z2, conj[z2]]/2 - nc[z1, z2, conj[z1], 
conj[z2]]/4];

xx = Expand[nc[conj[coh[z1, c[]]], coh[z2, c[]]]];

test[nc[int[z1], xx], conj[z1]/2 - nc[z2, conj[z1], conj[z2]]/4];
test[nc[int[z2], xx], -conj[z1] + conj[z2]/2 + nc[z1, conj[z1], conj[z2]]/4];
test[nc[int[conj[z1]], xx], -z1/2 + z2 - nc[z1, z2, conj[z2]]/4];
test[nc[int[conj[z2]], xx], -z2/2 + nc[z1, z2, conj[z1]]/4];
test[nc[int[z1, conj[z1]], xx], 1/2 + nc[z2, conj[z2]]/4];
test[nc[int[z1, z2], xx], -nc[conj[z1], conj[z2]]/4];
test[nc[int[z1, conj[z2]], xx], -nc[z2, conj[z1]]/4];
test[nc[int[z1, z2, conj[z1]], xx], -conj[z2]/4];
test[nc[int[z1, conj[z1], z2, conj[z2]], xx], 1/4];

Print["***** Wick's theorem *****"];

(* We redefine some operators *)

ordering[c] = SEA;

test[ ordering[c], SEA ];
test[ ordering[d], EMPTY ];

test[ iscreation[c[CR, 1]], True];
test[ iscreation[c[CR, -1]], False];
test[ isannihilation[c[CR, 1]], False];
test[ isannihilation[c[CR, -1]], True];

test[ iscreation[c[AN, 1]], False];
test[ iscreation[c[AN, -1]], True];
test[ isannihilation[c[AN, 1]], True];
test[ isannihilation[c[AN, -1]], False];



snegfermionoperators[a];
snegfreeindexes[k, k1, k2, k3, k4, kp];
snegfreeindexes[alpha, alpha1, alpha2, beta];
snegfreeindexes[sigma, sigma1, tau];

Conjugate[k] ^= k; (* Wave vectors are real *)

Print["* sumSimplifyKD[] *"];
test[ sumSimplifyKD[sum[scalar[(KroneckerDelta[k, k1] z)/U] a[CR, k], {k}]],
  scalar[z/U] a[CR, k1] ];
test[ sumSimplifyKD[sum[scalar[(KroneckerDelta[k1, k] z)/U] a[CR, k], {k}]],
  scalar[z/U] a[CR, k1] ];
test[ sumSimplifyKD[sum[KroneckerDelta[k, k+1] f[k], {k}]],
  sum[KroneckerDelta[k, k+1] f[k], {k}] ];

test[ SimplifyKD[KroneckerDelta[k1, k2]^2], KroneckerDelta[k1, k2] ];
test[ SimplifyKD[KroneckerDelta[k1, k2]^(-1)],
  KroneckerDelta[k1, k2]^(-1) ];
test[ SimplifyKD[UnitStep[k]^2], UnitStep[k] ];
test[ SimplifyKD[UnitStep[k]^(-1)], UnitStep[k]^(-1) ];

Print["* vev *"];

test[ vev[c[CR]], 0];
test[ vev[c[AN]], 0];
test[ vev[c[t]], 0];

test[ vev[c[CR, k]], 0];
test[ vev[c[AN, k]], 0];
test[ vev[c[tip, k]], 0];

test[ vev[c[CR, k, sigma]], 0];
test[ vev[c[AN, k, sigma]], 0];
test[ vev[c[tip, k, sigma]], 0];

test[ vev[nc[c[CR, k], c[CR, k]]], 0];
test[ vev[nc[c[AN, k], c[AN, k]]], 0];
test[ vev[nc[c[CR, k1], c[CR, k2]]], 0];
test[ vev[nc[c[AN, k1], c[AN, k2]]], 0];

test[ SimplifyKD[vev[nc[c[AN, k], c[CR, k]]]], UnitStep[k]];
test[ vev[nc[c[CR, k], c[AN, k]]], UnitStep[-k]];
test[ SimplifyKD[vev[nc[c[AN, k1], c[CR, k2]]]], KroneckerDelta[k1, 
  k2]*UnitStep[k2]];
test[ vev[nc[c[CR, k1], c[AN, k2]]], KroneckerDelta[k1, k2]*UnitStep[-k1]];
test[ Module[{old = ordering[d], result},
  ordering[d] = SEA;
  result = vev[nc[c[AN, -1], d[CR, -1]]];
  ordering[d] = old;
  result], 0];
  
test[ Simplify[vev[nc[c[tip, k1], c[AN, k2]]]], -((-1 + If[tip == 0, UnitStep[k2], 
0])*KroneckerDelta[k1, k2])];
test[ Simplify[vev[nc[c[tip, k1], c[tip, k2]]]], 0];

test[ vev[nc[c[CR, k, UP], c[CR, k, UP]]], 0];
test[ vev[nc[c[AN, k, UP], c[AN, k, UP]]], 0];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, UP]]], 0];
test[ vev[nc[c[AN, k1, UP], c[AN, k2, UP]]], 0];

test[ SimplifyKD[vev[nc[c[AN, k, UP], c[CR, k, UP]]]], UnitStep[k]];
test[ vev[nc[c[CR, k, UP], c[AN, k, UP]]], UnitStep[-k]];
test[ SimplifyKD[vev[nc[c[AN, k1, UP], c[CR, k2, UP]]]], KroneckerDelta[k1, 
k2]*UnitStep[k2]];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, UP]]], KroneckerDelta[k1, 
k2]*UnitStep[-k1]];

test[ vev[nc[c[CR, k, UP], c[CR, k, DO]]], 0];
test[ vev[nc[c[AN, k, UP], c[AN, k, DO]]], 0];
test[ vev[nc[c[AN, k, UP], c[CR, k, DO]]], 0];
test[ vev[nc[c[CR, k, UP], c[AN, k, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, DO]]], 0];
test[ vev[nc[c[AN, k1, UP], c[AN, k2, DO]]], 0];
test[ vev[nc[c[AN, k1, UP], c[CR, k2, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO]]], 0];

test[ vev[nc[c[CR, k, UP], c[CR, k, sigma]]], 0];
test[ vev[nc[c[AN, k, UP], c[AN, k, sigma]]], 0];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, sigma]]], 0];
test[ vev[nc[c[AN, k1, UP], c[AN, k2, sigma]]], 0];

test[ SimplifyKD[vev[nc[c[AN, k, UP], c[CR, k, sigma]]]], 
 KroneckerDelta[1, sigma]*UnitStep[k]];
test[ vev[nc[c[CR, k, UP], c[AN, k, sigma]]], 
 KroneckerDelta[1, sigma]*UnitStep[-k]];
test[ SimplifyKD[vev[nc[c[AN, k1, UP], c[CR, k2, sigma]]]], 
 KroneckerDelta[1, sigma]*KroneckerDelta[k1, k2]*UnitStep[k2]];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, sigma]]], 
 KroneckerDelta[1, sigma]*KroneckerDelta[k1, k2]*UnitStep[-k1] ];

test[ vev[nc[c[CR, k1, UP], c[AN, k2, UP]]], 
 KroneckerDelta[k1, k2]*UnitStep[-k1]];
test[ vev[nc[c[CR, k1, DO], c[AN, k2, DO]]], 
 KroneckerDelta[k1, k2]*UnitStep[-k1]];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, DO]]], 0];
test[ vev[nc[c[AN, k1, UP], c[AN, k2, DO]]], 0];

test[ vev[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP]]], 0];

test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO], c[CR, k3, UP], c[AN, k4, DO]]], 0];

test[ vev[nc[c[CR, k1, DO], c[AN, k2, DO], c[CR, k3, UP], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO], c[CR, k3, DO], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO], c[CR, k3, UP], c[AN, k4, UP]]], 0];

test[ vev[nc[c[AN, k1, UP], c[AN, k2, DO], c[CR, k3, UP], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, DO], c[CR, k3, UP], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO], c[AN, k3, UP], c[AN, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[AN, k2, DO], c[CR, k3, UP], c[CR, k4, DO]]], 0];
test[ vev[nc[c[CR, k1, UP], c[CR, k2, DO], c[CR, k3, UP], c[CR, k4, DO]]], 0];
test[ vev[nc[c[AN, k1, UP], c[AN, k2, DO], c[AN, k3, UP], c[AN, k4, DO]]], 0];


Print["* vev *"];

test[ vevwick[c[CR]], 0 ];
test[ vevwick[c[AN]], 0 ];
test[ vevwick[d[CR]], 0 ];
test[ vevwick[d[AN]], 0 ];

snegbosonoperators[wickboson];
test[ vevwick[wickboson[CR]], 0 ];
test[ vevwick[wickboson[AN]], 0 ];
test[ normalorderwick[wickboson[CR]], wickboson[CR] ];
test[ normalorderwick[wickboson[AN]], wickboson[AN] ];

test[ vevwick[nc[c[CR, 1], c[CR, 2], c[CR, 3]]], 0 ];
test[ vevwick[nc[d[CR, 1], d[CR, 2], d[CR, 3]]], 0 ];

expr=.;
test[ vevwick[nc[expr, d[AN]]], 0 ];
test[ vevwick[nc[d[CR], expr]], 0 ];


Print["* Test for numericity *"];
test[ isnumericQ[k], True ];
test[ isnumericQ[UnitStep[k] UnitStep[k1]], True ];
test[ isnumericQ[sum[1, {k}]], True ];
test[ isnumericQ[sum[UnitStep[k], {k}]], True ];
test[ isnumericQ[sum[c[CR, k], {k}]], False ];


Print["* Test acmt rules *"];

x1 = hop[c[k1], a[], sigma1];
x2 = number[c[k], sigma] + number[a[], sigma];

test[ (komutator[x1, x2]//Expand) /. {k1->k, sigma1->sigma}, 0];
test[ nc[x1 /. {k1->k, sigma1->sigma}, x2],
      nc[x1, x2] /. {k1->k, sigma1->sigma} ];


Print["* Test reordering *"];
test[ nc[c[AN, k, 1, UP], d[CR, k, 1, UP]], 
  -nc[d[0, k, 1, 1], c[1, k, 1, 1]] ];
test[ nc[a[AN, 1], a[CR, 1]], 1 - nc[a[0, 1], a[1, 1]] ];
test[ Hold[Evaluate[ nc[c[1, k, 1, 1], c[0, k, 1, 1]] ]],
  Hold[ nc[c[1, k, 1, 1], c[0, k, 1, 1]] ] ];
test[ nc[c[AN, k, 1, UP], c[CR, k, 1, DO]],
  -nc[c[0, k, 1, 0], c[1, k, 1, 1]] ];
test[ nc[c[AN, k, 1, UP], c[CR, k, 1, DO], c[AN, k1, 1, UP], c[CR, k1, 1, UP]],
 -nc[c[0, k, 1, 0], c[1, k, 1, 1], c[1, k1, 1, 1], c[0, k1, 1, 1]] ];
test[ nc[d[AN, k, 1, sp1], d[CR, k, 1, sp2]],
  KroneckerDelta[sp1, sp2] - nc[d[0, k, 1, sp2], d[1, k, 1, sp1]] ];

Print["* Tests of vev[] *"];

test[ vev[number[c[k, alpha]]], 2UnitStep[-k] ];
test[ vev[number[c[k, alpha], sigma]], UnitStep[-k] ];

test[ SimplifyKD[vev[nc[c[AN, k, alpha, sigma], c[CR, k, alpha, sigma]]]],
UnitStep[k] ];
test[ vev[nc[c[AN, k, alpha, sigma], c[CR, k, alpha, sigma]]],
1-UnitStep[-k] ];

test[ vev[nc[c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma]]],
  KroneckerDelta[k1, k2]*UnitStep[-k1] ];
test[ vev[nc[c[AN, k, 1, UP], c[CR, k, 1, DO], c[AN, k1, 1, UP], 
  c[CR, k1, 1, UP]]], 0 ];

test[ SimplifyKD[vevwick[nc[c[AN, k, 1, UP], c[CR, k, 1, UP], c[AN, k1, 1, UP],
  c[CR, k1, 1, UP]]]], UnitStep[k]*UnitStep[k1] ];

Print["* Tests of normal ordering *"];
test[ vev[normalorder[
    nc[c[AN, k, 1, UP], c[CR, k, 1, UP], c[AN, k1, 1, UP], c[CR, k1, 1, UP]]]],
  0 ];
test[ normalorder @ nc[c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma]],
  nc[c[0, k1, alpha, sigma], c[1, k2, alpha, sigma]] - 
  KroneckerDelta[k1, k2]*UnitStep[-k1] ];

Print["* Fermion fields in continuous space *"];

psi = sum[Exp[I k x] c[AN, k, sigma], {k}];

test[ isnumericQ[Exp[I k x]], True ];
test[ conj[Exp[I k x]], Exp[-I k x] ];

test[ conj[psi], sum[Exp[-I k x] c[CR, k, sigma], {k}] ];

Print["** sum[] **"];

Print["* Automatic renaming *"];

If[$VersionNumber >= 5.1, 
  test[ ssJWRnewname[ka, {ka}], ka1 ];
  test[ ssJWRnewname[ka, {ka, ka1}], ka2 ];
  test[ ssJWRnewname[ka, {ka, ka1, ka2}], ka3 ];
  test[ ssJWRnewname[kb, {kb}], kb1 ];
  test[ ssJWRnewname[kb, {kb, kb1}], kb2 ];
  test[ ssJWRnewname[kb, {kb, kb1, kb2}], kb3 ];

  test[ ssJWRnewname[ka, {ka}, 1], ka1 ];
  test[ ssJWRnewname[ka, {ka}, ka1], ka2 ];
  test[ ssJWRnewname[ka, {ka}, ka1 ka2], ka3 ];
  test[ ssJWRnewname[ka, {ka}, ka1 ka2 ka3], ka4 ];
  test[ ssJWRnewname[ka, {ka, ka2}, ka1], ka3 ];

  test[ snegsumJoinWithRenaming[expr1[kc], expr2[kd], {kc}, {kd}],
    sum[expr1[kc] ~ nc ~ expr2[kd], {kc, kd}] ];
  test[ snegsumJoinWithRenaming[expr1[kc], expr2[kc], {kc}, {kc}],
    sum[expr1[kc] ~ nc ~ expr2[kc1], {kc, kc1}] ];
  test[ snegsumJoinWithRenaming[expr1[kc], expr2[kc], {kc}, {kc}],
    sum[expr1[kc] ~ nc ~ expr2[kc1], {kc, kc1}] ];
  test[ snegsumJoinWithRenaming[expr1[kc], expr2[kc], {kc}, {kc}],
    sum[expr1[kc] ~ nc ~ expr2[kc1], {kc, kc1}] ];

  test[ nc[sum[expr1[kd], {kd}], sum[expr2[kd], {kd}]],
    sum[expr1[kd] ~ nc ~ expr2[kd1], {kd, kd1}] ];

  test[ nc[sum[expr1[ka], {ka}], expr2[kb], sum[expr3[kb], {kb}]],
    sum[nc[expr1[ka], expr2[kb], expr3[kb1]], {ka, kb1}] ];

  test[ nc[sum[expr1[ka], {ka}], expr2[kb, kb1], sum[expr3[kb], {kb}]],
    sum[nc[expr1[ka], expr2[kb, kb1], expr3[kb2]], {ka, kb2}] ];

  (* Guard nc[] sum pullout against dummy-index capture and nested sums. *)
  test[ nc[expr1[ka], sum[expr2[kd], {kd}], expr3[kb]],
    sum[nc[expr1[ka], expr2[kd], expr3[kb]], {kd}] ];

  test[ nc[expr1[kd], sum[expr2[kd], {kd}]],
    nc[expr1[kd], sum[expr2[kd], {kd}]] ];

  test[ nc[sum[expr1[kd], {kd}], expr2[kd]],
    nc[sum[expr1[kd], {kd}], expr2[kd]] ];

  test[ nc[expr1[sum[expr2[kb], {kb}]], sum[expr3[kd], {kd}]],
    nc[expr1[sum[expr2[kb], {kb}]], sum[expr3[kd], {kd}]] ];

  test[ nc[sum[expr1[ka] + sum[expr2[kb], {kb}], {ka}], sum[expr3[ka], {ka}]],
    sum[nc[expr1[ka], expr3[ka1]] + 
    nc[sum[expr2[kb], {kb}], expr3[ka1]], {ka, ka1}]];
  
  test[ nc[sum[expr1[ka] + sum[expr2[kb], {kb}], {ka}], sum[expr3[kb], {kb}]], 
    sum[nc[expr1[ka], expr3[kb1]] + 
    nc[sum[expr2[kb], {kb}], expr3[kb1]], {ka, kb1}]];
];

Print["* sumCollect[] *"];

test[ sumCollect[ka sum[expr1[ka], {ka}] + sum[expr2[ka], {ka}]],
  ka sum[expr1[ka], {ka}] + sum[expr2[ka], {ka}] ];

test[ sumCollect[
    kb sum[expr1[ka, kb], {ka, kb}] + sum[expr2[ka, kc], {ka, kc}]],
  kb sum[expr1[ka, kb], {ka, kb}] + sum[expr2[ka, kc], {ka, kc}] ];

test[ sumCollect[2 sum[expr1[ka], {ka}] + 3 sum[expr2[ka], {ka}]],
  sum[2 expr1[ka] + 3 expr2[ka], {ka}] ];
    
Print["* Utility functions *"];

test[ allpairs[{1,2}, {3,4}], {{1,3}, {1,4}, {2,3}, {2,4}} ];

expr = nc[c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma],
  c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]];

test[ indecesCR[expr], {1,2} ];
test[ indecesAN[expr], {3,4} ];
test[ maxcran[expr], 2 ];
test[ disjointQ[{{1,2}}], True ];
test[ disjointQ[{{1,2},{3,4}}], True ];
test[ disjointQ[{{1,2},{1,3}}], False ];
test[ disjointQ[{{1,2},{1,3},{1,4}}], False ];
test[ disjointQ[{{1,3},{1,4},{2,3}}], False ];
test[ dropzeros[{0, 1, 2, 3, 0, 2, 3, {1}, 0}], 
  {1, 2, 3, 2, 3, {1}} ];
test[ contractedpairs[1, {{1, 3}, {1, 4}, {2, 3}, {2, 4}}],
  {{{1, 3}}, {{1, 4}}, {{2, 3}}, {{2, 4}}} ];
test[ contractedpairs[2, {{1, 3}, {1, 4}, {2, 3}, {2, 4}}],
  {{{1, 3}, {2, 4}}, {{1, 4}, {2, 3}}} ];
test[ contractedpairs[3, {{1, 3}, {1, 4}, {2, 3}, {2, 4}}],
  {} ];

Print["* Contractions *"];
snegfermionoperators[noneWickFermion];
snegfreeindexes[noneWickI, noneWickJ, noneWickK, noneWickL];
ordering[noneWickFermion] = NONE;

test[ contraction[noneWickFermion[AN], noneWickFermion[CR]], 1 ];
test[
  contraction[noneWickFermion[AN, noneWickI],
    noneWickFermion[CR, noneWickJ]],
  KroneckerDelta[noneWickI, noneWickJ]
];
test[
  contraction[noneWickFermion[AN, noneWickI],
      noneWickFermion[CR, noneWickJ]] /. noneWickJ -> noneWickI,
  1
];
test[
  contraction[noneWickFermion[AN, noneWickI, noneWickK],
    noneWickFermion[CR, noneWickJ, noneWickL]],
  KroneckerDelta[noneWickI, noneWickJ]
    KroneckerDelta[noneWickK, noneWickL]
];
test[
  contraction[noneWickFermion[AN, noneWickI],
    noneWickFermion[CR, noneWickJ, noneWickK]],
  0
];
test[
  contraction[noneWickFermion[AN, 1], noneWickFermion[CR, 2]],
  0
];
test[
  {contraction[noneWickFermion[CR, noneWickI],
      noneWickFermion[AN, noneWickJ]],
    contraction[noneWickFermion[AN, noneWickI],
      noneWickFermion[AN, noneWickJ]],
    contraction[noneWickFermion[CR, noneWickI],
      noneWickFermion[CR, noneWickJ]]},
  {0, 0, 0}
];

Module[{pair, four, pairExpected, fourExpected},
  pair = nc[noneWickFermion[AN, noneWickI],
    noneWickFermion[CR, noneWickJ]];
  pairExpected = KroneckerDelta[noneWickI, noneWickJ];
  test[
    {vevwick[pair], vevwick2[pair], vevwicknew[pair]},
    ConstantArray[pairExpected, 3]
  ];

  four = nc[
    noneWickFermion[AN, noneWickI],
    noneWickFermion[AN, noneWickJ],
    noneWickFermion[CR, noneWickK],
    noneWickFermion[CR, noneWickL]
  ];
  fourExpected =
    KroneckerDelta[noneWickI, noneWickL]
      KroneckerDelta[noneWickJ, noneWickK] -
    KroneckerDelta[noneWickI, noneWickK]
      KroneckerDelta[noneWickJ, noneWickL];
  test[
    {vevwick[four], vevwick2[four], vevwicknew[four]},
    ConstantArray[fourExpected, 3]
  ];
];

snegfermionoperators[noneWickHookFermion];
ordering[noneWickHookFermion] = NONE;
noneWickHookFermion /:
  acmt[noneWickHookFermion[AN, i_], noneWickHookFermion[CR, j_]] :=
    noneWickHook[i, j];
test[
  contraction[noneWickHookFermion[AN, noneWickI],
    noneWickHookFermion[CR, noneWickJ]],
  noneWickHook[noneWickI, noneWickJ]
];

test[ contractone[{1, 1, {aa, bb, cc, dd}}, {1, 3}],
      {2, contraction[aa, cc], {1, bb, 1, dd}} ];

test[ contraction[c[CR, k, alpha, sigma], c[AN, k1, alpha1, sigma1]],
  KroneckerDelta[k, k1]*KroneckerDelta[{alpha, sigma}, {alpha1, sigma1}]*
  UnitStep[-k] ];

test[ contraction[c[AN, k, alpha, sigma], c[CR, k1, alpha1, sigma1]] 
  // Simplify, 
  KroneckerDelta[k, k1] 
  (KroneckerDelta[alpha, alpha1] KroneckerDelta[sigma, sigma1] - 
   KroneckerDelta[{alpha, sigma}, {alpha1, sigma1}] UnitStep[-k1])//Simplify
];

test[ contractone[{1, 1, {c[CR, k, alpha, sigma], c[AN, k, alpha, sigma]}}, 
  {1, 2}], {2, UnitStep[-k], {1, 1}} ];
test[ contractone[{1, 1,
    {noneBosonA[AN], aa, noneBosonA[CR]}}, {1, 3}],
  {2, 1, {wickslot[1, 0], aa, wickslot[1, 0]}} ];

test[ remainder[{2, 2, a[CR, sigma], 1, 1}], dd[a[0, sigma]] ];
test[ remainder[{2,1}], -1 ];
test[ remainder[{1,1}], 1 ];
test[ remainder[{2,2,1,1}], 1 ];
test[ remainder[{2,1,1}], 1 ];
test[ remainder[{4,3,2,1}], 1 ];
test[ remainder[{wickslot[2, 0], wickslot[1, 0],
    wickslot[2, 0], wickslot[1, 0]}], 1 ];
test[ remainder[{wickslot[2, 1], wickslot[1, 1],
    wickslot[2, 1], wickslot[1, 1]}], -1 ];

test[ contract[{a[AN, sigma], a[CR, sigma]}, {{1,2}}], 1 ];
test[ contract[{c[CR, k], c[AN, k]}, {{1,2}}], UnitStep[-k] ];

test[ contract[{c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma], 
    c[CR, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 2}}],
  dd[nc[c[0, k3, alpha, sigma], c[1, k4, alpha, sigma]]]*
  KroneckerDelta[k1, k2]*UnitStep[-k1] ];

test[ contract[{c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma], 
    c[CR, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 2}, {3, 4}} ],
  KroneckerDelta[k1, k2]*KroneckerDelta[k3, k4]*UnitStep[-k1]*UnitStep[-k3] ];

(* The reversed second pair contracts an annihilator with a creator. *)
test[ SimplifyKD[contract[{c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma],
    c[CR, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 4}, {3, 2}}]],
  KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*UnitStep[-k1]*UnitStep[k3] ];

test[ contract[{c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
    c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 3}, {2, 4}}],
-(KroneckerDelta[k1, k3]*KroneckerDelta[k2, k4]*UnitStep[-k1]*UnitStep[-k2]) ];

test[ contract[{c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
    c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 4}, {2, 3}}],
  KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*UnitStep[-k1]*UnitStep[-k2] ];

test[ contract[
    {noneBosonA[AN], noneBosonB[AN],
      noneBosonA[CR], noneBosonB[CR]},
    {{1, 3}, {2, 4}}], 1 ];
test[ contract[
    {noneWickFermion[AN, 1], noneWickFermion[AN, 2],
      noneWickFermion[CR, 1], noneWickFermion[CR, 2]},
    {{1, 3}, {2, 4}}], -1 ];
test[ contract[
    {noneBosonA[AN], noneWickFermion[AN],
      noneBosonA[CR], noneWickFermion[CR]},
    {{1, 3}, {2, 4}}], 1 ];
test[ contract[
    {noneWickFermion[AN], noneBosonA[CR], noneWickFermion[CR]},
    {{1, 3}}], dd[noneBosonA[CR]] ];
test[ contract[
    {noneBosonA[AN], noneWickFermion[CR], noneBosonA[CR]},
    {{1, 3}}], dd[noneWickFermion[CR]] ];


Print["* wickorder[] *"];
test[ wickorder[0, nc[c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
  c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]]],
dd[nc[c[0, k1, alpha, sigma], c[0, k2, alpha, sigma], c[1, k3, alpha, sigma], 
    c[1, k4, alpha, sigma]]] ];

test[ wickorder[1, nc[c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
  c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]]],
-(dd[nc[c[0, k2, alpha, sigma], c[1, k4, alpha, sigma]]]*
   KroneckerDelta[k1, k3]*UnitStep[-k1]) + 
 dd[nc[c[0, k2, alpha, sigma], c[1, k3, alpha, sigma]]]*
  KroneckerDelta[k1, k4]*UnitStep[-k1] + 
 dd[nc[c[0, k1, alpha, sigma], c[1, k4, alpha, sigma]]]*
  KroneckerDelta[k2, k3]*UnitStep[-k2] - 
 dd[nc[c[0, k1, alpha, sigma], c[1, k3, alpha, sigma]]]*
  KroneckerDelta[k2, k4]*UnitStep[-k2] ];

test[ wickorder[2, nc[c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
  c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]]],
KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*UnitStep[-k1]*
  UnitStep[-k2] - KroneckerDelta[k1, k3]*KroneckerDelta[k2, k4]*
  UnitStep[-k1]*UnitStep[-k2] ];

test[ wickorder[2, nc[c[CR, k1, alpha, sigma], c[CR, k2, alpha, sigma], 
  c[AN, k3, alpha, sigma], c[AN, k4, alpha, sigma]]] // Simplify,
(KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3] - 
  KroneckerDelta[k1, k3]*KroneckerDelta[k2, k4])*UnitStep[-k1]*
 UnitStep[-k2] ];

test[ SimplifyKD[
  wickorder[2, nc[c[0, k1, 1, 1], c[0, k2, 1, 1], c[1, k1, 1, 1], 
    c[1, k2, 1, 1]] ] -
  ((-1 + KroneckerDelta[k1, k2]^2)*UnitStep[-k1]*UnitStep[-k2])],
  0 ];

test[ wickorder[1, nc[
    noneBosonA[AN], noneBosonA[AN], noneBosonA[CR]]],
  2 dd[noneBosonA[AN]] ];
test[ wickorder[2, nc[
    noneBosonA[AN, 1], noneBosonA[AN, 2],
    noneBosonA[CR, 1], noneBosonA[CR, 2]]], 1 ];

test[ wickorder[2, nc[c[0, k, 1, 0], c[0, k1, 1, 1],
  c[1, k, 1, 1], c[1, k1, 1, 1]]], 0 ];
test[ vevwick[nc[c[0, k, 1, 0], c[1, k, 1, 1], c[1, k1, 1, 1],
  c[0, k1, 1, 1]]], 0 ];
  
Print["* wick[] *"];

test[ wick[], 1 ];
test[ wick[sum[
    nc[noneBosonA[AN, noneBosonI], noneBosonA[CR, noneBosonJ]],
    {noneBosonI}]],
  sum[dd[nc[
      noneBosonA[AN, noneBosonI], noneBosonA[CR, noneBosonJ]]] +
    KroneckerDelta[noneBosonI, noneBosonJ], {noneBosonI}] ];

test[ wick[nc[
    noneBosonA[AN], noneBosonA[AN], noneBosonA[CR]]],
  dd[nc[noneBosonA[AN], noneBosonA[AN], noneBosonA[CR]]] +
    2 dd[noneBosonA[AN]] ];

test[ wick[nc[c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma], 
  c[CR, k3, alpha, tau], c[AN, k4, alpha, tau]]] // Expand,
 -dd[nc[c[0, k1, alpha, sigma], c[0, k3, alpha, tau], 
    c[1, k2, alpha, sigma], c[1, k4, alpha, tau]]] + 
 dd[nc[c[0, k1, alpha, sigma], KroneckerDelta[sigma, tau], 
    c[1, k4, alpha, tau]]]*KroneckerDelta[k2, k3] + 
 dd[nc[c[0, k3, alpha, tau], c[1, k4, alpha, tau]]]*
  KroneckerDelta[k1, k2]*UnitStep[-k1] - 
 dd[nc[c[0, k3, alpha, tau], c[1, k2, alpha, sigma]]]*
  KroneckerDelta[k1, k4]*KroneckerDelta[{alpha, sigma}, 
   {alpha, tau}]*UnitStep[-k1] + dd[KroneckerDelta[sigma, tau]]*
  KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*
  KroneckerDelta[{alpha, sigma}, {alpha, tau}]*UnitStep[-k1] + 
 dd[nc[c[0, k1, alpha, sigma], c[1, k2, alpha, sigma]]]*
  KroneckerDelta[k3, k4]*UnitStep[-k3] - 
 dd[nc[c[0, k1, alpha, sigma], c[1, k4, alpha, tau]]]*
  KroneckerDelta[k2, k3]*KroneckerDelta[{alpha, sigma}, 
   {alpha, tau}]*UnitStep[-k3] + KroneckerDelta[k1, k2]*
  KroneckerDelta[k3, k4]*UnitStep[-k1]*UnitStep[-k3] - 
 KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*
  KroneckerDelta[{alpha, sigma}, {alpha, tau}]^2*UnitStep[-k1]*
  UnitStep[-k3] ];

Print["* vevwick[] *"];

test[ vevwick[], 1 ];
test[ vevwick[sum[
    nc[noneBosonA[AN, noneBosonI], noneBosonA[CR, noneBosonJ]],
    {noneBosonI}]],
  sum[KroneckerDelta[noneBosonI, noneBosonJ], {noneBosonI}] ];

Module[{boson4, boson6, indexedBoson4, mixed4, expected},
  boson4 = nc[
    noneBosonA[AN], noneBosonA[AN],
    noneBosonA[CR], noneBosonA[CR]];
  test[
    {vev[boson4], vevwick[boson4],
      vevwick2[boson4], vevwicknew[boson4]},
    {2, 2, 2, 2}
  ];
  test[ normalorderwick[boson4], -2 + boson4 ];

  boson6 = nc[
    noneBosonA[AN], noneBosonA[AN], noneBosonA[AN],
    noneBosonA[CR], noneBosonA[CR], noneBosonA[CR]];
  test[
    {vev[boson6], vevwick[boson6],
      vevwick2[boson6], vevwicknew[boson6]},
    {6, 6, 6, 6}
  ];

  test[
    {vevwick[nc[
        noneBosonA[AN, 1], noneBosonA[AN, 2],
        noneBosonA[CR, 1], noneBosonA[CR, 2]]],
      vevwick[nc[
        noneBosonA[AN, 1], noneBosonA[AN, 2],
        noneBosonA[CR, 2], noneBosonA[CR, 1]]]},
    {1, 1}
  ];

  indexedBoson4 = nc[
    noneBosonA[AN, noneBosonI],
    noneBosonA[AN, noneBosonJ],
    noneBosonA[CR, noneBosonK],
    noneBosonA[CR, noneBosonL]];
  expected =
    KroneckerDelta[noneBosonI, noneBosonK]
      KroneckerDelta[noneBosonJ, noneBosonL] +
    KroneckerDelta[noneBosonI, noneBosonL]
      KroneckerDelta[noneBosonJ, noneBosonK];
  test[
    {vevwick[indexedBoson4], vevwick2[indexedBoson4],
      vevwicknew[indexedBoson4]},
    ConstantArray[expected, 3]
  ];

  mixed4 = nc[
    noneBosonA[AN], noneWickFermion[AN],
    noneBosonA[CR], noneWickFermion[CR]];
  test[
    {vevwick[mixed4], vevwick2[mixed4], vevwicknew[mixed4]},
    {1, 1, 1}
  ];
];

Module[{ops, failures},
  ops = {
    noneBosonA[AN, 1], noneBosonA[AN, 2],
    noneBosonA[CR, 1], noneBosonA[CR, 2]
  };
  failures = Select[Permutations[ops], Function[p,
    With[{x = Apply[nc, p], expected = vev[Apply[nc, p]]},
      {vevwick[x], vevwick2[x], vevwicknew[x]} =!=
        ConstantArray[expected, 3]
    ]
  ]];
  test[failures, {}];
];

test[vevwick2[nc[c[CR, k1, sigma1], c[AN, k2, sigma]]],
  KroneckerDelta[k1, k2]*KroneckerDelta[sigma1, sigma]*UnitStep[-k1]];
test[vevwicknew[nc[c[CR, k1, sigma1], c[AN, k2, sigma]]],
  KroneckerDelta[k1, k2]*KroneckerDelta[sigma1, sigma]*UnitStep[-k1]];
test[vevwick2[nc[c[CR, k1], d[AN, k2]]], 0];
test[vevwicknew[nc[c[CR, k1], d[AN, k2]]], 0];

test[SimplifyKD[vevwick[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, UP]]]], 
UnitStep[-k1]*(KroneckerDelta[k1, k2]*KroneckerDelta[k3, k4]*UnitStep[-k3] + KroneckerDelta[k1, k4]*
KroneckerDelta[k2, k3]*UnitStep[k3])];

test[SimplifyKD[vevwick2[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, UP]]]],
UnitStep[-k1]*(KroneckerDelta[k1, k2]*KroneckerDelta[k3, k4]*UnitStep[-k3] + KroneckerDelta[k1, k4]*
KroneckerDelta[k2, k3]*UnitStep[k3])];

test[SimplifyKD[vevwicknew[nc[c[CR, k1, UP], c[AN, k2, UP], c[CR, k3, UP], c[AN, k4, UP]]]],
UnitStep[-k1]*(KroneckerDelta[k1, k2]*KroneckerDelta[k3, k4]*UnitStep[-k3] + KroneckerDelta[k1, k4]*
KroneckerDelta[k2, k3]*UnitStep[k3])];

test[SimplifyKD[vevwick[nc[c[CR, k1, DO], c[AN, k2, DO], c[CR, k3, DO], c[AN, k4, DO]]]], 
UnitStep[-k1]*(KroneckerDelta[k1, k2]*KroneckerDelta[k3, k4]*UnitStep[-k3] + KroneckerDelta[k1, k4]*
KroneckerDelta[k2, k3]*UnitStep[k3])];

test[SimplifyKD[vevwick[nc[c[CR, k, UP], c[AN, k, UP], c[CR, k1, UP], c[AN, k1, UP]]]], 
  UnitStep[-k]*(UnitStep[-k1] + KroneckerDelta[k, k1]*UnitStep[k1])];

test[SimplifyKD[vevwick[nc[c[CR, k, UP], c[AN, k1, UP], c[CR, k, UP], c[AN, k1, UP]]]], 
  KroneckerDelta[k, k1]*UnitStep[-k]];

test[SimplifyKD[
  vevwick[nc[c[AN, k, UP], c[CR, k, UP], c[AN, k1, UP], c[CR, k1, UP]]] -
  ((KroneckerDelta[k, k1]*UnitStep[-k] + UnitStep[k])*UnitStep[k1])], 
  0];

Module[{y1, y2, y3},
  y1 = normalorder[c[CR, k1, alpha, sigma] ~ nc ~ c[AN, k2, alpha, sigma]];
  y2 = normalorder[c[CR, k3, beta, tau] ~ nc ~ c[AN, k4, beta, tau]];

  y3 = y1 ~ nc ~ y2;

  test[ SimplifyKD @ vevwick[y3],
    SimplifyKD[
    KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*
    KroneckerDelta[{alpha, sigma}, {beta, tau}]*UnitStep[-k1]*
    (KroneckerDelta[alpha, beta]*KroneckerDelta[sigma, tau] - 
    KroneckerDelta[{alpha, sigma}, {beta, tau}]*UnitStep[-k3]) ]
  ];
];

Print["* misc *"];
test[ indexvalue2list[{{2, xa}, {4, xb}}, 6], {0, xa, 0, xb, 0, 0} ];

test[ disjointQ[{{1, 2}}], True ];
test[ disjointQ[{{1, 2}, {3, 4}}], True ];
test[ disjointQ[{{1, 2}, {1, 3}}], False ];
test[ disjointQ[{{1, 2}, {1, 3}, {1, 4}}], False ];
test[ disjointQ[{{1, 3}, {1, 4}, {2, 3}}], False ];

test[ allpairs[{1,2}, {3,4}], {{1,3},{1,4},{2,3},{2,4}} ];

test[ dropzeros[{0, 1, 2, 3, 0, 2, 3, {1}, 0}], {1, 2, 3, 2, 3, {1}} ];

test[remainder[{2, 2, a[CR, \[Sigma]], 1, 1}], dd[a[0, \[Sigma]]]];
test[remainder[{2, 1}], -1];
test[remainder[{1, 1}], 1];
test[remainder[{2, 2, 1, 1}], 1];
test[remainder[{2, 1, 1}], 1];
test[remainder[{4, 3, 2, 1}], 1];

(*** Independent finite-space checks ***
   The reference matrices below use the Jordan-Wigner construction on binary
   occupation strings. They do not use nc, ap, Wick contractions, or SNEG's
   matrix constructors. Exact phase checks complement invariant checks. *)

Begin["SnegRegression`"];

fockstates[n_] := vc @@@ Tuples[{0, 1}, n];
fockcreate[1, 1] := {{0, 0}, {1, 0}};
fockcreate[n_, k_] := KroneckerProduct @@ Table[
  Which[j < k, {{1, 0}, {0, -1}},
        j == k, {{0, 0}, {1, 0}},
        True, IdentityMatrix[2]], {j, n}];
coordinates[v_, states_List] := Coefficient[Expand[v], #] & /@ states;
zerotest[name_, value_] := testcase[name,
  And @@ (TrueQ[Simplify[#] === 0] & /@ Flatten[{value}]), True];

checkbasis[name_, bz_, states_, observables_, eigenvalues_] := Module[
  {sector, vectors, values, j, label},
  testcase[name <> " structure", bzQ[bz], True];
  Do[
    vectors = coordinates[#, states] & /@ sector[[2]];
    values = eigenvalues[sector[[1]]];
    label = name <> " " <> ToString[sector[[1]], InputForm];
    testcase[label <> " nonempty sector", Length[vectors] > 0, True];
    zerotest[label <> " finite-space support", sector[[2]] - vectors.states];
    zerotest[label <> " orthonormality",
      Conjugate[vectors].Transpose[vectors] - IdentityMatrix[Length[vectors]]];
    Do[
      zerotest[label <> " quantum number " <> ToString[j],
        (observables[[j]].# - values[[j]] #) & /@ vectors],
      {j, Length[observables]}],
    {sector, bz}];
];

(* Isolate the mutable package state used by the following groups. *)
savedRegressionOrdering = DownValues[ordering];
Block[{BASIS = BASIS, PrettyOutput = False,
    listfermionoperators = listfermionoperators,
    listbosonoperators = listbosonoperators,
    listspinoperators = listspinoperators,
    listmajoranaoperators = listmajoranaoperators,
    listrealconstants = listrealconstants,
    listcomplexconstants = listcomplexconstants,
    listintegerconstants = listintegerconstants,
    listgrassmanconstants = listgrassmanconstants,
    listfreeindexes = listfreeindexes, nnop, Tminus, ordering},
DownValues[ordering] = savedRegressionOrdering;

testgroup["Symmetry-adapted finite bases",
 Module[{f, ops, states, cr, an, qmat, szmat, spmat, smmat, ss,
     bz, bzop, n, j, v, ref, expected, single},
  snegfermionoperators[f];
  Do[
    ops = Table[f[j], {j, n}];
    states = fockstates[2 n];
    cr = Table[fockcreate[2 n, j], {j, 2 n}];
    an = Transpose /@ cr;
    qmat = Total[MapThread[Dot, {cr, an}]] - n IdentityMatrix[4^n];
    szmat = Sum[(cr[[2 j-1]].an[[2 j-1]] - cr[[2 j]].an[[2 j]])/2, {j, n}];
    spmat = Sum[cr[[2 j-1]].an[[2 j]], {j, n}];
    smmat = Transpose[spmat];
    ss = szmat.szmat + (spmat.smmat + smmat.spmat)/2;

    bz = qsbasisvc[ops];
    checkbasis["QS " <> ToString[n], bz, states, {qmat, szmat, ss},
      {#[[1]], #[[2]], #[[2]] (#[[2]] + 1)} &];
    testcase["QS multiplicity-weighted dimension",
      Total[(2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz], 4^n];
    bzop = qsbasis[ops];
    testcase["QS operator/occupation convention", Expand[bzop2bzvc[bzop]], Expand[bz]];
    testcase["QS round trip", Expand[bzop2bzvc[bzvc2bzop[bz]]], Expand[bz]];

    bz = qszbasisvc[ops];
    checkbasis["QSZ", bz, states, {qmat, szmat}, # &];
    testcase["QSZ dimension", Total[Length[#[[2]]] & /@ bz], 4^n];
    (* The two enumerators use different orders within a sector. *)
    testcase["QSZ specialized implementation",
      {First[#], Sort[Last[#]]} & /@ qszbasisvc[ops, 1/2],
      {First[#], Sort[Last[#]]} & /@ bz];
    bz = qbasisvc[ops];
    checkbasis["Q", bz, states, {qmat}, # &];
    testcase["Q binomial sector sizes", Length[#[[2]]] & /@ bz,
      Table[Binomial[2 n, j], {j, 0, 2 n}]];
    testcase["Q operator conversion", bzop2bzvc[qbasis[ops]], bz];
    bz = szbasisvc[ops];
    checkbasis["SZ", bz, states, {szmat}, # &];
    testcase["SZ dimension", Total[Length[#[[2]]] & /@ bz], 4^n];
    testcase["SZ operator conversion", bzop2bzvc[szbasis[ops]], bz];
    bz = sbasisvc[ops];
    checkbasis["S", bz, states, {szmat, ss}, {First[#], First[#] (First[#]+1)} &];
    testcase["S multiplicity-weighted dimension",
      Total[(2 First[#[[1]]] + 1) Length[#[[2]]] & /@ bz], 4^n];
    testcase["S operator conversion", Expand[bzop2bzvc[sbasis[ops]]], Expand[bz]];
    testcase["Unrestricted basis", nonebasisvc[ops], {{{}, states}}];
    testcase["Unrestricted operator conversion", bzop2bzvc[nonebasis[ops]], {{{}, states}}],
    {n, 1, 3}];

  testcase["One-site QS phases", qsbasisvc[{f[1]}],
    {{{-1, 0}, {vc[0, 0]}}, {{0, 1/2}, {vc[1, 0]}}, {{1, 0}, {-vc[1, 1]}}}];
  bz = qsbasisvc[{f[1], f[2]}];
  testcase["Two-site singlet phase", keyselect[bz, {0, 0}][[1, 2, 2]],
    (vc[0, 1, 1, 0] - vc[1, 0, 0, 1])/Sqrt[2]];
  testcase["Two-site triplet phase", keyselect[bz, {0, 1}][[1, 2]], {-vc[1, 0, 1, 0]}];

  states = fockstates[4];
  cr = Table[fockcreate[4, j], {j, 4}];
  an = Transpose /@ cr;
  smmat = cr[[2]].an[[1]] + cr[[4]].an[[3]];
  Do[
    ref = smmat.coordinates[v, states];
    expected = If[ref === ConstantArray[0, 16], ref, ref/Sqrt[Conjugate[ref].ref]];
    zerotest["Occupation spin lowering", coordinates[spindownvc[v], states] - expected];
    zerotest["Operator spin lowering", coordinates[ops2vc[spindown[vc2ops[v]]], states] - expected],
    {v, states}];
  single = (vc[1, 0, 0, 1] - vc[0, 1, 1, 0])/Sqrt[2];
  testcase["Singlet lowering", spindownvc[single], 0];
  testcase["Triplet lowering ladder",
    NestList[spindownvc, vc[1, 0, 1, 0], 3],
    {vc[1, 0, 1, 0], (vc[0, 1, 1, 0] + vc[1, 0, 0, 1])/Sqrt[2], vc[0, 1, 0, 1], 0}];
 ]];

testgroup["Independent operator and rectangular matrices",
 Module[{f, states, cr, an, ops, mats, op, mat, left, right, lmat, rmat,
     j, bz, bzop, expected, sector1, sector2, values, dispatch, h, href, parameter},
  snegfermionoperators[f];
  makebasis[{f[1], f[2]}];
  states = fockstates[4];
  cr = Table[fockcreate[4, j], {j, 4}];
  an = Transpose /@ cr;
  h = (1+I) nc[f[CR, 1, UP], f[AN, 2, DO]] +
      (1-I) nc[f[CR, 2, DO], f[AN, 1, UP]] + 3 number[f[1]] + 2 hubbard[f[2]];
  href = (1+I) cr[[1]].an[[4]] + (1-I) cr[[4]].an[[1]] +
    3 (cr[[1]].an[[1]] + cr[[2]].an[[2]]) + 2 cr[[3]].an[[3]].cr[[4]].an[[4]];
  ops = Join[BASIS, conj[BASIS], {h}];
  mats = Join[cr, an, {href}];
  Do[
    op = ops[[j]]; mat = mats[[j]];
    testcase["Dense occupation matrix", matrixrepresentationvc[op, states], mat];
    testcase["Fast occupation matrix", matrixrepresentationvcfast[op, states], mat];
    testcase["Sparse occupation matrix", Normal[matrixrepresentationvcsparse[op, states]], mat];
    testcase["Operator-form matrix", matrixrepresentationop[op, vc2ops[states]], mat];
    dispatch = opdispatch[op];
    testcase["Dispatch action", states /. dispatch, Transpose[mat].states];
    zerotest["Matrix adjoint", matrixrepresentationvc[conj[op], states] - ConjugateTranspose[mat]],
    {j, Length[ops]}];

  (* Complex monomials, dependent states, superpositions, and zero vectors
     exercise both optimized and fallback paths with unequal bra/ket sizes. *)
  Do[
    left = pair[[1]]; right = pair[[2]];
    lmat = coordinates[#, states] & /@ left;
    rmat = coordinates[#, states] & /@ right;
    expected = Conjugate[lmat].href.Transpose[rmat];
    testcase["Rectangular dense", matrixrepresentationvc[h, left, right], expected];
    testcase["Rectangular fast", matrixrepresentationvcfast[h, left, right], expected];
    testcase["Rectangular operator", matrixrepresentationop[h, vc2ops[left], vc2ops[right]], expected],
    {pair, {
      {{I states[[9]], -2 states[[2]]}, {states[[2]], (1+I) states[[9]], states[[1]]}},
      {{states[[9]], I states[[9]]}, {states[[9]], states[[2]], states[[1]]}},
      {{(states[[9]]+I states[[2]])/Sqrt[2], 0}, {states[[9]], states[[2]], states[[1]]}}
    }}];
  left = {states[[9]], I states[[9]], 0};
  lmat = coordinates[#, states] & /@ left;
  testcase["Sparse dependent-state fallback", Normal[matrixrepresentationvcsparse[h, left]],
    Conjugate[lmat].href.Transpose[lmat]];

  bz = qszbasisvc[{f[1], f[2]}];
  bzop = bzvc2bzop[bz];
  expected = Table[
    lmat = coordinates[#, states] & /@ sector1[[2]];
    {sector1[[1]], Conjugate[lmat].href.Transpose[lmat]}, {sector1, bz}];
  testcase["Diagonal occupation blocks", makematricesbzvc[h, bz], expected];
  testcase["Diagonal operator blocks", makematricesbzop[h, bzop], expected];
  expected = {};
  Do[
    lmat = coordinates[#, states] & /@ sector1[[2]];
    rmat = coordinates[#, states] & /@ sector2[[2]];
    values = Conjugate[lmat].cr[[1]].Transpose[rmat];
    If[Total[Abs[values], 2] =!= 0,
      AppendTo[expected, {{sector1[[1]], sector2[[1]]}, values}]],
    {sector1, bz}, {sector2, bz}];
  testcase["All occupation blocks", makeallmatricesbzvc[f[CR, 1, UP], bz], expected];
  testcase["All operator blocks", makeallmatricesbzop[f[CR, 1, UP], bzop], expected];
  testcase["Zero blocks omitted", makeallmatricesbzvc[0, bz], {}];
  testcase["Zero operator blocks omitted", makeallmatricesbzop[0, bzop], {}];
  testcase["Machine-zero blocks omitted", makeallmatricesbzvc[0. h, bz], {}];
  testcase["Undecidable symbolic blocks retained",
    mambpair[{{0}, {1}}, {{1}, {1}}, 1, Function[{o, l, r}, {{parameter}}]],
    {{{0}, {1}}, {{parameter}}}];
  testcase["Tiny nonzero blocks retained", Length[makeallmatricesbzvc[10.^-14 f[CR, 1, UP], bz]],
    Length[expected]];
 ]];

testgroup["Isospin, product bases, and projections",
 Module[{f, states, cr, an, n1, n2, sz, sp, ss, ip1, ip2, ip, iz, ii,
     phases, tm, qs, bz, projected, projectorMatrix},
  snegfermionoperators[f];
  states = fockstates[4];
  cr = Table[fockcreate[4, j], {j, 4}]; an = Transpose /@ cr;
  n1 = cr[[1]].an[[1]] + cr[[2]].an[[2]];
  n2 = cr[[3]].an[[3]] + cr[[4]].an[[4]];
  sz = (cr[[1]].an[[1]] - cr[[2]].an[[2]] + cr[[3]].an[[3]] - cr[[4]].an[[4]])/2;
  sp = cr[[1]].an[[2]] + cr[[3]].an[[4]];
  ss = sz.sz + (sp.Transpose[sp] + Transpose[sp].sp)/2;
  iz = (n1 + n2 - 2 IdentityMatrix[16])/2;
  Do[
    nnop[f[1]] = phases[[1]]; nnop[f[2]] = phases[[2]];
    ip1 = (-1)^phases[[1]] cr[[1]].cr[[2]];
    ip2 = (-1)^phases[[2]] cr[[3]].cr[[4]];
    ip = ip1 + ip2;
    ii = iz.iz + (ip.Transpose[ip] + Transpose[ip].ip)/2;
    tm = isospinminus[f[1], phases[[1]]] + isospinminus[f[2], phases[[2]]];
    qs = qsbasisvc[{f[1], f[2]}];
    bz = transformQStoIS[qs, tm];
    checkbasis["IS", bz, states, {iz, ii, sz, ss},
      {#[[1]], #[[1]] (#[[1]]+1), #[[2]], #[[2]] (#[[2]]+1)} &];
    testcase["IS dimension", Total[(2 #[[1, 1]]+1) (2 #[[1, 2]]+1) Length[#[[2]]] & /@ bz], 16];
    Tminus = tm;
    testcase["Implicit isospin lowering", transformQStoIS[qs], bz];
    testcase["Quick IS basis", Expand[bzop2bzvc[quickISObasis[{f[1], f[2]}]]], Expand[bz]];
    bz = bzop2bzvc[quickISOSZbasis[{f[1], f[2]}]];
    checkbasis["ISZ", bz, states, {iz, ii, sz}, {#[[1]], #[[1]] (#[[1]]+1), #[[2]]} &];
    testcase["ISZ dimension", Total[(2 #[[1, 1]]+1) Length[#[[2]]] & /@ bz], 16];
    bz = bzop2bzvc[quickSU2basis[{f[1], f[2]}]];
    checkbasis["Isospin only", bz, states, {iz, ii}, {First[#], First[#] (First[#]+1)} &];
    testcase["Isospin-only dimension", Total[(2 First[#[[1]]]+1) Length[#[[2]]] & /@ bz], 16],
    {phases, {{0, 0}, {0, 1}}}];
  Block[{Tminus},
    testcase["Missing lowering operator", Quiet[transformQStoIS[qs], transformQStoIS::Tminus], $Failed]];

  bz = bzop2bzvc[quickDBLSZ[{f[1]}, {f[2]}, qszbasis]];
  checkbasis["Two charges and SZ", bz, states,
    {n1 - IdentityMatrix[16], n2 - IdentityMatrix[16], sz}, # &];
  testcase["Two-charge dimension", Total[Length[#[[2]]] & /@ bz], 16];
  testcase["Product basis restores all modes", BASIS,
    {f[CR, 1, UP], f[CR, 1, DO], f[CR, 2, UP], f[CR, 2, DO]}];
  bz = bzop2bzvc[quickDBL[{f[1]}, {f[2]}, quickSU2basis]];
  ip1 = cr[[1]].cr[[2]]; ip2 = cr[[3]].cr[[4]];
  checkbasis["Two isospins", bz, states,
    {(n1-IdentityMatrix[16]).(n1-IdentityMatrix[16])/4 + (ip1.Transpose[ip1]+Transpose[ip1].ip1)/2,
     (n2-IdentityMatrix[16]).(n2-IdentityMatrix[16])/4 + (ip2.Transpose[ip2]+Transpose[ip2].ip2)/2},
    {#[[1]] (#[[1]]+1), #[[2]] (#[[2]]+1)} &];
  testcase["Two-isospin dimension", Total[(2 #[[1, 1]]+1) (2 #[[1, 2]]+1) Length[#[[2]]] & /@ bz], 16];
  bz = bzop2bzvc[quickDBLSZ[{f[1]}, {f[2]}, quickISOSZbasis]];
  checkbasis["Two isospins and SZ", bz, states, {sz}, {Last[#]} &];
  testcase["Two-isospin SZ dimension", Total[(2 #[[1, 1]]+1) (2 #[[1, 2]]+1) Length[#[[2]]] & /@ bz], 16];

  qs = qsbasisvc[{f[1], f[2]}];
  projected = transformbasis[qs, projector1[f[1]]];
  projectorMatrix = n1 - 2 cr[[1]].an[[1]].cr[[2]].an[[2]];
  checkbasis["Projected QS", projected, states, {projectorMatrix}, {1} &];
  testcase["Projected dimension", Total[(2 #[[1, 2]]+1) Length[#[[2]]] & /@ projected], 8];
  testcase["Projection removes all sectors", transformbasis[qs, 0], {}];
  testcase["Projection within one sector",
    transformfunc[{vc[1, 0, 0, 0], vc[0, 0, 1, 0]}, projector1[f[1]]], {vc[1, 0, 0, 0]}];
  testcase["Remove a complex state",
    Length[remove1state[{vc[1, 0, 0, 0], vc[0, 0, 1, 0]},
      (vc[1, 0, 0, 0] + I vc[0, 0, 1, 0])/Sqrt[2]]], 1];
  bz = bzop2bzvc[spinlessbasis[qbasis[{f[1], f[2]}]]];
  checkbasis["Spinless projection", bz, states,
    {cr[[2]].an[[2]] + cr[[4]].an[[4]]}, {0} &];
  testcase["Spinless dimension", Total[Length[#[[2]]] & /@ bz], 4];
 ]];

testgroup["Reflection parity",
 Module[{f, states, bits, reflect, bzop, evenodd, bz},
  snegfermionoperators[f];
  bits = Tuples[{0, 1}, 4]; states = vc @@@ bits;
  reflect = Table[If[r === v[[{3, 4, 1, 2}]],
    (-1)^((v[[1]]+v[[2]]) (v[[3]]+v[[4]])), 0], {r, bits}, {v, bits}];
  bzop = qsbasis[{f[1], f[2]}];
  evenodd = transformtoLR[bzop, {f[1], f[2]}];
  bz = bzop2bzvc[evenodd];
  checkbasis["Reflection", bz, states, {reflect}, {Last[#]} &];
  testcase["Reflection completeness", Total[(2 #[[1, 2]]+1) Length[#[[2]]] & /@ bz], 16];
  testcase["Occupation reflection transform",
    Expand[transformtoLRvc[bzop, {f[1], f[2]}, vacuum[]]], Expand[bz]];
  testcase["Both reflection parities", Union[bz[[All, 1, -1]]], {-1, 1}];
  testcase["Reflection is an involution", reflect.reflect, IdentityMatrix[16]];
 ]];

testgroup["SU2 coupling and spin kets",
 Module[{f, j, localz, localp, cr, an, sz, sp, totalz, totalp, totalss,
     bz, states, firstspin, secondspin, coupled, triplet, singlet, fresh, index},
  snegfermionoperators[f];
  cr = {fockcreate[2, 1], fockcreate[2, 2]}; an = Transpose /@ cr;
  sz = (cr[[1]].an[[1]] - cr[[2]].an[[2]])/2;
  sp = cr[[1]].an[[2]];
  Do[
    localz = DiagonalMatrix[Range[j, -j, -1]];
    localp = Table[If[m == n+1, Sqrt[(j-n) (j+n+1)], 0], {m, j, -j, -1}, {n, j, -j, -1}];
    totalz = KroneckerProduct[sz, IdentityMatrix[2 j+1]] + KroneckerProduct[IdentityMatrix[4], localz];
    totalp = KroneckerProduct[sp, IdentityMatrix[2 j+1]] + KroneckerProduct[IdentityMatrix[4], localp];
    totalss = totalz.totalz + (totalp.Transpose[totalp] + Transpose[totalp].totalp)/2;
    states = Flatten[Table[vc @@ Join[bits, {ket[m]}], {bits, Tuples[{0, 1}, 2]}, {m, j, -j, -1}], 1];
    bz = SU2basistensorproduct[qsbasis[{f[]}], {First[spinbasis[j]]},
      Last, First, qsspincombinefnc, fixspin, fixspinket];
    bz = bzop2bzvc[bz];
    checkbasis["Coupled spin " <> ToString[j], bz, states, {totalz, totalss},
      {Last[#], Last[#] (Last[#]+1)} &];
    testcase["Coupled spin dimension", Total[(2 Last[#[[1]]]+1) Length[#[[2]]] & /@ bz], 4 (2 j+1)];
    testcase["Spin-ket identity", matrixrepresentationop[spinketbraI[j], spinket[j]], IdentityMatrix[2 j+1]];
    testcase["Spin-ket raising", matrixrepresentationop[spinketbraP[j], spinket[j]], localp];
    testcase["Spin-ket lowering", matrixrepresentationop[spinketbraM[j], spinket[j]], Transpose[localp]],
    {j, {0, 1/2, 1, 3/2}}];

  (* Independent callbacks for two complementary tensor factors. *)
  firstspin[k_, s_, m_] := k /. ket[_, b_] :> ket[m, b];
  secondspin[k_, s_, m_] := k /. ket[a_, _] :> ket[a, m];
  coupled = SU2basistensorproduct[
    {{{1/2}, {ket[1/2, Null]}}}, {{{1/2}, {ket[Null, 1/2]}}},
    First, First, Function[{s, q1, q2}, {s}], firstspin, secondspin];
  singlet = (ket[1/2, -1/2] - ket[-1/2, 1/2])/Sqrt[2];
  triplet = ket[1/2, 1/2];
  testcase["Clebsch-Gordan singlet phase", Expand[coupled[[1]]], Expand[{{0}, {singlet}}]];
  testcase["Clebsch-Gordan triplet phase", coupled[[2]], {{1}, {triplet}}];
  zerotest["Two-spin singlet exchange", nc[spinketbraspinspin[{1/2, 1/2}], singlet] + 3 singlet/4];
  testcase["Two-spin triplet exchange", nc[spinketbraspinspin[{1/2, 1/2}], triplet], triplet/4];
  testcase["Spin-ket Cartesian components", spinketbraXYZ[1/2],
    {spinketbraX[1/2], spinketbraY[1/2], spinketbraZ[1/2]}];
  testcase["Spin-ket all components", spinketbraOPS[1/2],
    {spinketbraX[1/2], spinketbraY[1/2], spinketbraZ[1/2], spinketbraP[1/2], spinketbraM[1/2]}];
  Do[
    testcase["SU2 iterator hygiene",
      SU2combine[f[CR, index, UP], ket[1/2], {1/2, 1/2}, fixspin, fixspinket],
      SU2combine[f[CR, fresh, UP], ket[1/2], {1/2, 1/2}, fixspin, fixspinket] /. fresh -> index],
    {index, {Sneg`s, Sneg`sz1, Sneg`sz2}}];
 ]];

testgroup["Finite phonons and fermion-phonon products",
 Module[{f, n, cutoff, states, raising, lowering, numbers, bz, bvc, coords,
     fermionStates, allStates, cr, an, nmat, h, reference, reflection, bits, extra},
  Do[
    states = Table[ket[k], {k, 0, n}];
    raising = Table[If[r == c+1, Sqrt[c+1], 0], {r, 0, n}, {c, 0, n}];
    lowering = Transpose[raising]; numbers = DiagonalMatrix[Range[0, n]];
    testcase["Phonon basis", phononbasis[n], states];
    testcase["Phonon creation matrix", matrixrepresentationop[phononplus[n], states], raising];
    testcase["Phonon annihilation matrix", matrixrepresentationop[phononminus[n], states], lowering];
    testcase["Phonon number matrix", matrixrepresentationop[phononnumber[n], states], numbers];
    testcase["Phonon identity matrix", matrixrepresentationop[phononid[n], states], IdentityMatrix[n+1]];
    testcase["Truncated boson commutator",
      matrixrepresentationop[commutator[phononminus[n], phononplus[n]], states],
      DiagonalMatrix[Join[ConstantArray[1, n], {-n}]]];
    testcase["Phonon upper boundary", nc[phononplus[n], ket[n]], 0];
    testcase["Phonon lower boundary", nc[phononminus[n], ket[0]], 0],
    {n, 0, 3}];
  cutoff = {1, 2};
  states = ket @@@ Tuples[{Range[0, 1], Range[0, 2]}];
  testcase["Unequal phonon cutoffs", phononbasis[cutoff], states];
  Do[
    raising = Table[If[r == c+1, Sqrt[c+1], 0], {r, 0, cutoff[[n]]}, {c, 0, cutoff[[n]]}];
    reference = If[n == 1, KroneckerProduct[raising, IdentityMatrix[3]], KroneckerProduct[IdentityMatrix[2], raising]];
    testcase["Multimode raising", matrixrepresentationop[phononplus[n, cutoff], states], reference];
    testcase["Multimode lowering", matrixrepresentationop[phononminus[n, cutoff], states], Transpose[reference]];
    testcase["Multimode number", matrixrepresentationop[phononnumber[n, cutoff], states], reference.Transpose[reference]];
    testcase["Multimode displacement", matrixrepresentationop[phononx[n, cutoff], states], reference + Transpose[reference]],
    {n, 2}];
  testcase["Different phonon modes commute", commutator[phononminus[1, cutoff], phononplus[2, cutoff]], 0];

  snegfermionoperators[f];
  bz = transformtoPH[qszbasis[{f[]}], cutoff];
  bvc = bzop2bzvc[bz];
  fermionStates = Tuples[{0, 1}, 2];
  allStates = Flatten[Table[vc @@ Join[bits, {state}], {bits, fermionStates}, {state, states}], 1];
  cr = {fockcreate[2, 1], fockcreate[2, 2]}; an = Transpose /@ cr;
  nmat = cr[[1]].an[[1]] + cr[[2]].an[[2]];
  checkbasis["Fermion-phonon quantum numbers", bvc, allStates,
    {KroneckerProduct[nmat-IdentityMatrix[4], IdentityMatrix[6]],
     KroneckerProduct[(cr[[1]].an[[1]]-cr[[2]].an[[2]])/2, IdentityMatrix[6]]}, # &];
  testcase["Fermion-phonon dimension", Total[Length[#[[2]]] & /@ bvc], 24];
  h = 2 number[f[]] + 3 hubbard[f[]] + 5 phononnumber[1, cutoff] +
    7 phononnumber[2, cutoff] + 11 nc[number[f[]]-1, phononx[1, cutoff]];
  reference = KroneckerProduct[2 nmat + 3 cr[[1]].an[[1]].cr[[2]].an[[2]], IdentityMatrix[6]] +
    KroneckerProduct[IdentityMatrix[4], 5 KroneckerProduct[DiagonalMatrix[{0, 1}], IdentityMatrix[3]] +
      7 KroneckerProduct[IdentityMatrix[2], DiagonalMatrix[{0, 1, 2}]]] +
    11 KroneckerProduct[nmat-IdentityMatrix[4], KroneckerProduct[{{0, 1}, {1, 0}}, IdentityMatrix[3]]];
  coords = coordinates[#, allStates] & /@ Flatten[bvc[[All, 2]], 1];
  testcase["Coupled fermion-phonon Hamiltonian",
    matrixrepresentationvc[h, Flatten[bvc[[All, 2]], 1]], Conjugate[coords].reference.Transpose[coords]];

  bz = transformtoPH[qszbasis[{f[1], f[2]}], {1}];
  extra = {ket[k_] :> (-1)^k ket[k]};
  bvc = bzop2bzvc[transformtoLR[bz, {f[1], f[2]}, extra]];
  bits = Tuples[{0, 1}, 4];
  reflection = KroneckerProduct[
    Table[If[r === v[[{3, 4, 1, 2}]], (-1)^((v[[1]]+v[[2]]) (v[[3]]+v[[4]])), 0], {r, bits}, {v, bits}],
    DiagonalMatrix[{1, -1}]];
  allStates = Flatten[Table[vc @@ Join[v, {ket[k]}], {v, bits}, {k, 0, 1}], 1];
  checkbasis["Phonon reflection sign", bvc, allStates, {reflection}, {Last[#]} &];
  testcase["Phonon reflection dimension", Total[Length[#[[2]]] & /@ bvc], 32];
 ]];

testgroup["Wick contractions against finite Fock spaces",
 Module[{f, g, b, otherBoson, cr, matrices, ops, words, word, value, expected,
     actual, failures, mode, vacuumIndex, bp, mixedWords, hook, i, j},
  snegfermionoperators[f, g];
  cr = {fockcreate[2, 1], fockcreate[2, 2]};
  matrices = {Transpose[cr[[1]]], cr[[1]], Transpose[cr[[2]]], cr[[2]]};
  words = Join[Flatten[Table[Tuples[Range[4], n], {n, 0, 4}], 1],
    {{1, 3, 2, 1, 4, 2}, {1, 3, 4, 2, 1, 2}, {1, 2, 3, 4, 1, 2},
     {1, 3, 2, 4, 1, 3, 2, 4}, {1, 2, 1, 2, 3, 4, 3, 4}}];
  Do[
    ordering[f] = mode; ordering[g] = mode;
    ops = If[mode === SEA, {f[AN, -1], f[CR, -1], g[AN, 1], g[CR, 1]},
      {f[AN], f[CR], g[AN], g[CR]}];
    vacuumIndex = If[mode === SEA, 3, 1];
    failures = {};
    Do[
      expected = Fold[Dot, IdentityMatrix[4], matrices[[word]]][[vacuumIndex, vacuumIndex]];
      value = nc @@ ops[[word]];
      actual = {vevwick[value], vevwick2[value], vevwicknew[value]};
      If[actual =!= ConstantArray[expected, 3], AppendTo[failures, {word, actual, expected}]],
      {word, words}];
    testcase["Fermion Wick " <> ToString[mode],
      {Length[failures], Take[failures, Min[8, Length[failures]]]}, {0, {}}],
    {mode, {NONE, EMPTY, SEA}}];

  ordering[f] = NONE; ordering[g] = NONE;
  snegbosonoperators[b]; ordering[b] = NONE;
  bp = Table[If[r == c+1, Sqrt[c+1], 0], {r, 0, 3}, {c, 0, 3}];
  matrices = Join[KroneckerProduct[#, IdentityMatrix[4]] & /@ matrices,
    {KroneckerProduct[IdentityMatrix[4], Transpose[bp]], KroneckerProduct[IdentityMatrix[4], bp]}];
  ops = {f[AN], f[CR], g[AN], g[CR], b[AN], b[CR]};
  mixedWords = Join[Permutations[{1, 2, 5, 6}],
    {{5, 5, 6, 6}, {5, 5, 5, 6, 6, 6}, {1, 5, 3, 2, 6, 4},
     {5, 1, 5, 2, 6, 6}, {1, 5, 2, 6, 1, 2}, {5, 6, 1, 3, 2, 4}}];
  failures = {};
  Do[
    expected = Fold[Dot, IdentityMatrix[16], matrices[[word]]][[1, 1]];
    value = nc @@ ops[[word]];
    actual = Simplify[{vevwick[value], vevwick2[value], vevwicknew[value]}];
    If[actual =!= ConstantArray[expected, 3], AppendTo[failures, {word, actual, expected}]],
    {word, mixedWords}];
  testcase["Mixed-statistics Wick",
    {Length[failures], Take[failures, Min[8, Length[failures]]]}, {0, {}}];

  snegfermionoperators[hook]; ordering[hook] = NONE;
  hook /: acmt[hook[AN, i_], hook[CR, j_]] := 3 KroneckerDelta[i, j];
  testcase["Custom contraction normalization",
    {vevwick[nc[hook[AN, 1], hook[AN, 2], hook[CR, 2], hook[CR, 1]]],
     vevwicknew[nc[hook[AN, 1], hook[AN, 2], hook[CR, 2], hook[CR, 1]]]}, {9, 9}];
  f /: acmt[f[AN], g[CR]] = 7;
  testcase["Custom cross-family contraction", contraction[f[AN], g[CR]], 7];
  testcase["Custom cross-family Wick pair", vevwick[nc[f[AN], g[CR]]], 7];
  snegbosonoperators[otherBoson]; ordering[otherBoson] = NONE;
  testcase["Independent unordered boson contraction", contraction[b[AN], otherBoson[CR]], 0];
  value = nc[b[AN], otherBoson[AN], b[CR], otherBoson[CR]];
  testcase["Independent unordered boson Wick", {vevwick[value], vevwick2[value], vevwicknew[value]}, {1, 1, 1}];
  ordering[f] = EMPTY;
  ordering[Sneg`op] = NONE;
  testcase["Contraction uses its actual operator heads", contraction[f[AN], f[CR]], 1];
 ]];

testgroup["Hamiltonian constructors and observables",
 Module[{f, states, cr, an, id, nup, ndown, n1, pair, forward,
     phase, z, constructors, references, projectors, pmats, j, k, h, ref,
     sx1, sy1, sz1, sx2, sy2, sz2},
  snegfermionoperators[f]; snegrealconstants[phase]; snegcomplexconstants[z];
  makebasis[{f[1], f[2]}]; states = fockstates[4];
  cr = Table[fockcreate[4, j], {j, 4}]; an = Transpose /@ cr; id = IdentityMatrix[16];
  nup = cr[[1]].an[[1]]; ndown = cr[[2]].an[[2]];
  n1 = nup + ndown;
  pair = cr[[1]].cr[[2]];
  forward = cr[[1]].an[[3]] + cr[[2]].an[[4]];
  sx1 = (cr[[1]].an[[2]] + cr[[2]].an[[1]])/2;
  sy1 = (cr[[1]].an[[2]] - cr[[2]].an[[1]])/(2 I);
  sz1 = (nup - ndown)/2;
  sx2 = (cr[[3]].an[[4]] + cr[[4]].an[[3]])/2;
  sy2 = (cr[[3]].an[[4]] - cr[[4]].an[[3]])/(2 I);
  sz2 = (cr[[3]].an[[3]] - cr[[4]].an[[4]])/2;
  constructors = {
    isozsq[f[1]], isospinx[f[1]], isospiny[f[1]], isospinz[f[1]],
    isospinplus[f[1]], isospinminus[f[1]], bcs[f[1], Pi/2], bcs[z, f[1]],
    genhop[z, f[1], f[2]], hopphi[f[1], f[2], phase], currentphi[f[1], f[2], phase],
    current[f[1], f[2]], spinfliphopphi[f[1], f[2], phase],
    genanhop[z, f[1], f[2]], spinspinsymmetric[{f[1], f[2]}],
    spinspinx[f[1], f[2]], spinspiny[f[1], f[2]], spinss[f[1]]};
  h = Exp[I phase] forward + Exp[-I phase] Transpose[forward];
  ref = cr[[1]].cr[[4]] - cr[[2]].cr[[3]];
  references = {
    (n1-id).(n1-id), (pair+Transpose[pair])/2, (pair-Transpose[pair])/(2 I), (n1-id)/2,
    pair, Transpose[pair], I pair-I Transpose[pair], z pair+Conjugate[z] Transpose[pair],
    z forward+Conjugate[z] Transpose[forward], h, I (h.n1-n1.h),
    I ((forward+Transpose[forward]).n1-n1.(forward+Transpose[forward])),
    Exp[I phase] (cr[[1]].an[[4]]+cr[[2]].an[[3]]) + Exp[-I phase] (cr[[4]].an[[1]]+cr[[3]].an[[2]]),
    z ref+Conjugate[z] Transpose[ref], sx1.sx2+sy1.sy2+sz1.sz2,
    sx1.sx2, sy1.sy2, 3 (n1-2 nup.ndown)/4};
  Do[
    zerotest["Constructor matrix " <> ToString[j], matrixrepresentationvc[constructors[[j]], states] - references[[j]]],
    {j, Length[constructors]}];
  zerotest["Singlet anomalous hopping", commutator[spinplus[f[1]]+spinplus[f[2]], anhop[f[1], f[2]]]];
  zerotest["Spin-resolved current", current[f[1], f[2]] - current[f[1], f[2], UP] - current[f[1], f[2], DO]];
  testcase["Current reverses direction", current[f[1], f[2]], -current[f[2], f[1]]];

  projectors = {projector0[f[1]], projectorUP[f[1]], projectorDO[f[1]], projector2[f[1]]};
  pmats = {(id-nup).(id-ndown), nup.(id-ndown), ndown.(id-nup), nup.ndown};
  testcase["Local projector matrices", matrixrepresentationvc[#, states] & /@ projectors, pmats];
  testcase["Projector completeness", Total[projectors], 1];
  Do[testcase["Projector multiplication", nc[projectors[[j]], projectors[[k]]],
    If[j == k, projectors[[j]], 0]], {j, 4}, {k, 4}];
  testcase["Single-occupancy projector", matrixrepresentationvc[projector1[f[1]], states], pmats[[2]]+pmats[[3]]];
  testcase["Even-charge projector", matrixrepresentationvc[projector02[f[1]], states], pmats[[1]]+pmats[[4]]];
  Do[
    h = projectorEphi[f[1], phase]; ref = projectorOphi[f[1], phase];
    zerotest["Phase projector idempotence", nc[h, h]-h];
    zerotest["Phase projectors orthogonal", nc[h, ref]];
    zerotest["Phase projectors complete", h+ref-projector02[f[1]]],
    {phase, {0, Pi/2, Pi}}];
  testcase["Real coherent projectors", {projectorE[f[1]], projectorO[f[1]]},
    {projectorEphi[f[1], 0], projectorOphi[f[1], 0]}];
  testcase["Pair transfer matrices", matrixrepresentationvc[#, states] & /@ {outofdiag02[f[1]], outofdiag20[f[1]]},
    {Transpose[pair], pair}];
  testcase["Single-particle equation of motion",
    extracteps[f[AN, 1, UP], f[AN, 1, UP], 3 number[f[1]]], 3];
  testcase["Noncommutative cross product", mcross[{f[CR, 1, UP], 0, 0}, {0, f[CR, 2, UP], 0}],
    {0, 0, nc[f[CR, 1, UP], f[CR, 2, UP]]}];
 ]];

testgroup["Spin and Majorana algebras",
 Module[{s, m, matrices, j, k, a, b, i1, i2, dirac, gellmann},
  snegspinoperators[s];
  matrices = {
    {{0, 1, 0}, {1, 0, 1}, {0, 1, 0}}/Sqrt[2],
    {{0, -I, 0}, {I, 0, -I}, {0, I, 0}}/Sqrt[2],
    DiagonalMatrix[{1, 0, -1}],
    Sqrt[2] {{0, 1, 0}, {0, 0, 1}, {0, 0, 0}},
    Sqrt[2] {{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}};
  Do[
    zerotest["Spin Lie algebra", (cmt[s[j], s[k]] /. s[t_] :> matrices[[t]]) -
      (matrices[[j]].matrices[[k]]-matrices[[k]].matrices[[j]])],
    {j, 5}, {k, 5}];
  testcase["Disjoint spins commute", commutator[s[1, SPININDEXx], s[2, SPININDEXy]], 0];
  testcase["Spin negative-component convention", s[-SPININDEXx], -s[SPININDEXx]];
  testcase["Spin zero component", s[0], 0];

  snegmajoranaoperators[m, a, b]; snegfreeindexes[i1, i2];
  testcase["Majorana normalization", nc[a, a], 1/2];
  testcase["Indexed Majorana CAR", anticommutator[m[i1], m[i2]], KroneckerDelta[i1, i2]];
  testcase["Indexed Majorana adjoint", conj[m[i1]], m[i1]];
  dirac = (a+I b)/Sqrt[2];
  testcase["Majorana to Dirac nilpotence", Expand[nc[dirac, dirac]], 0];
  testcase["Majorana to Dirac CAR", Expand[anticommutator[dirac, conj[dirac]]], 1];
  testcase["Majorana commutator sign", commutator[a, b], 2 nc[a, b]];
  gellmann = Table[su3g[j], {j, 8}];
  testcase["SU3 generators traceless", Tr /@ gellmann, ConstantArray[0, 8]];
  testcase["SU3 generator normalization", Table[Tr[su3g[j].su3g[k]], {j, 8}, {k, 8}], 2 IdentityMatrix[8]];
  testcase["SU3 generator adjoints", ConjugateTranspose /@ gellmann, gellmann];
  testcase["SU3 quadratic Casimir", Total[Table[su3t[j].su3t[j], {j, 8}]], 4 IdentityMatrix[3]/3];
 ]];

testgroup["Operator exponentials and power series",
 Module[{f, b, t, u, n, x, k},
  snegfermionoperators[f]; snegbosonoperators[b]; snegrealconstants[t, u];
  n = number[f[], UP]; x = f[CR, UP]+f[AN, UP];
  Do[
    testcase["Nested commutator", supercommutator[n, f[AN, UP], k], (-1)^k f[AN, UP]];
    testcase["Nested commutator alias", superkomutator[n, f[AN, UP], k], (-1)^k f[AN, UP]];
    testcase["Nested anticommutator", superanticommutator[n, f[AN, UP], k], f[AN, UP]];
    testcase["Nested anticommutator alias", superantikomutator[n, f[AN, UP], k], f[AN, UP]],
    {k, 0, 4}];
  testcase["Commuting exponentials", nc[Exp[t n], Exp[u number[f[], DO]]], Exp[t n+u number[f[], DO]]];
  testcase["Fermion similarity transform", nc[Exp[t n], f[AN, UP], Exp[-t n]], Exp[-t] f[AN, UP]];
  testcase["Boson displacement", Expand[nc[Exp[t (b[CR]-b[AN])], b[AN], Exp[-t (b[CR]-b[AN])]]], b[AN]-t];
  zerotest["Central-commutator BCH", nc[Exp[t b[CR]], Exp[u b[AN]]] - Exp[-t u/2] Exp[t b[CR]+u b[AN]]];
  testcase["Nilpotent exponential", nc[Exp[nc[f[CR, UP], f[CR, DO]]], VACUUM],
    VACUUM+nc[f[CR, UP], f[CR, DO], VACUUM]];
  testcase["Involution exponential", nc[Exp[x], VACUUM], Cosh[1] VACUUM+Sinh[1] nc[f[CR, UP], VACUUM]];
  testcase["Finite exponential series", Expand[snegSeries[Exp, x, 4]], Expand[37/24+7 x/6]];
  testcase["Finite sine series", Expand[snegSeries[Sin[x], 5]], Expand[101 x/120]];
  Do[testcase["Fast operator powers", fastpow[x, k], If[EvenQ[k], 1, x]], {k, 0, 8}];
  testcase["Negative power message", Quiet[pow[x, -1], pow::negativepower], Null];
  With[{savedPower = DownValues[Power]},
    definencPower[];
    testcase["Optional nc power syntax", n^3, n];
    testcase["Numeric powers retain their meaning", 3^4, 81];
    Unprotect[Power]; DownValues[Power] = savedPower; Protect[Power]];
 ]];

testgroup["Scalar declarations and masking",
 Module[{f, real, complex, integer, positive, rf, cf, argument, rules, inverse},
  snegfermionoperators[f];
  snegrealconstants[real]; snegcomplexconstants[complex]; snegintegerconstants[integer];
  snegrealfunctions[rf]; snegcomplexfunctions[cf];
  testcase["Real and integer adjoints", conj[{real, integer}], {real, integer}];
  testcase["Complex adjoint", conj[complex], Conjugate[complex]];
  testcase["Declared scalar functions", isnumericQ /@ {rf[argument], cf[argument], Conjugate[cf[argument]]}, {True, True, True}];
  testcase["Real function adjoint", conj[rf[argument]], rf[argument]];
  testcase["Complex function adjoint", conj[cf[argument]], Conjugate[cf[argument]]];
  testcase["Scalar function extraction", nc[rf[argument], f[CR, UP], cf[argument]], rf[argument] cf[argument] f[CR, UP]];
  testcase["Undeclared scalar is not inferred", isnumericQ[argument], False];
  testcase["Explicit scalar wrapper", vev[nc[scalar[argument], f[AN, UP], f[CR, UP]]], argument];
  testcase["Constants are registered", And @@ (MemberQ[getallconstants[], #] & /@ {real, complex, integer}), True];
  testcase["Masking suppresses real upvalue", maskOp[SameQ[Conjugate[real], real]], False];
  testcase["Masking function interface", maskOp[Function[v, SameQ[Conjugate[v], v]], real], False];
  testcase["Masking restores parameters", maskOp[Function[v, v^2+2 v], real], real^2+2 real];
  testcase["Masking preserves declarations", Conjugate[real], real];
  {rules, inverse} = maskconstants[];
  testcase["Mask rules invert", {real, complex, integer} /. rules /. inverse, {real, complex, integer}];
  With[{savedPower = DownValues[Power]},
    snegpositiveconstants[positive];
    testcase["Positive square root", Sqrt[positive^2], positive];
    testcase["Positive fourth power root", Sqrt[positive^4], positive^2];
    testcase["Negative square root", Sqrt[-positive^2], I positive];
    Unprotect[Power]; DownValues[Power] = savedPower; Protect[Power]];
 ]];

testgroup["Symbolic sums and simplification",
 Module[{i, j, k, f, g, h, op, marker, expr, result, warned},
  snegfreeindexes[i, j, k]; snegfermionoperators[op];
  testcase["Thread symbolic sums", sumThread[sum[{f[i], g[i]}, {i}]],
    {sum[f[i], {i}], sum[g[i], {i}]}];
  testcase["Expand under symbolic sums", sumExpand[sum[(f[i]+g[i]) h[i], {i}]],
    sum[f[i] h[i], {i}]+sum[g[i] h[i], {i}]];
  expr = sum[KroneckerDelta[i, j] f[i], {i}];
  testcase["Sum simplifier applies delta rules", sumSimplify[expr], f[j]];
  testcase["Full sum simplifier applies delta rules", sumFullSimplify[expr], f[j]];
  testcase["Collect then contract deltas",
    sumCollectSimplifyKD[sum[(f[i]+g[i]) h[i] KroneckerDelta[i, j], {i}]],
    (f[j]+g[j]) h[j]];
  testcase["Positive delta powers", FullSimplifyKD[KroneckerDelta[i, j]^4], KroneckerDelta[i, j]];
  testcase["Complementary sea occupations", FullSimplifyKD[UnitStep[k]+UnitStep[-k]], 1];
  testcase["Delta-linked opposite occupations", SimplifyKD[KroneckerDelta[i, j] UnitStep[i] UnitStep[-j]], 0];

  testcase["Separable sum with unused index",
    FactorSeparableSumCustom[sum[f[i] g[j], {i, j, k}]],
    sum[f[i], {i}] sum[g[j], {j}] sum[1, {k}]];
  expr = sum[f[i] g[j] h[i, j], {i, j}];
  testcase["Connected factors stay together", FactorSeparableSumCustom[expr], expr];
  testcase["Factorization rule retains constants",
    sum[3 f[i] g[j], {i, j}] /. rulesumFactor, 3 sum[f[i], {i}] sum[g[j], {j}]];
  testcase["Empty factors retain cardinalities", FactorSeparableSumCustom[sum[1, {i, j}]],
    sum[1, {i}] sum[1, {j}]];

  expr = sum[f[i, j], {i, j}];
  result = sumAbstractIndex[expr, marker];
  testcase["Abstract index positions", result, sum[f[marker[1], marker[2]], {marker[1], marker[2]}]];
  testcase["Abstract index round trip", sumNameIndex[result, marker, {i, j}], expr];
  testcase["Explicit index naming",
    sumNameIndex[sum[f[marker[1], marker[2]], {marker[1], marker[2]}], marker, {i, j}], expr];
  Block[{sumAutoRename = False},
    testcase["Nested sums without automatic renaming",
      sum[sum[f[i, j], {i}] + g[j], {j}], sum[f[i, j], {i, j}] + sum[g[j], {j}]];
    testcase["Overlapping indexes are reported",
      Quiet[Check[nc[sum[op[CR, i], {i}], sum[op[AN, i], {i}]], warned, snegsumJoin::overlap],
        snegsumJoin::overlap], warned]];

  expr = Expand[number[op[]] + 2 hubbard[op[]] + 3 spinz[op[]]];
  result = SnegFullSimplify[expr];
  zerotest["High-level simplification preserves algebra", (result /. HoldForm[a_] :> a) - expr];
  testcase["High-level simplification actually rewrites", !FreeQ[result, _HoldForm], True];
  testcase["Hubbard recognition",
    MatchQ[SnegSimplifyHubbard[-hubbard[op[]]], HoldPattern[-HoldForm[hubbard[op[]]]]], True];
  testcase["Vacuum projection under symbolic sums",
    Zeroonvac[sum[op[AN, i], {i}] + op[CR, 2]], op[CR, 2]];
 ]];

testgroup["Scoped assumptions and orbital transformations",
 Module[{p, q, savedUp, savedDown, value, f, g, old, new, rotation, rules},
  p[0] = 7; savedUp = UpValues[p]; savedDown = DownValues[p];
  testcase["Temporary sign assumption", snegAssuming[p /: p > 0 = True, {p > 0, p[0]}], {True, 7}];
  testcase["Assumption restores upvalues", UpValues[p], savedUp];
  testcase["Assumption restores downvalues", DownValues[p], savedDown];
  testcase["Multiple assumptions",
    snegAssuming[{p /: p > 0 = True, q /: q < 0 = True}, {p > 0, q < 0}], {True, True}];
  testcase["Nested assumption restoration",
    snegAssuming[p /: p > 0 = True,
      {snegAssuming[p /: p > 0 = False, p > 0], p > 0}], {False, True}];
  testcase["Held assumption result",
    MatchQ[snegAssumingHoldForm[p /: p > 0 = True, p > 0], HoldPattern[HoldForm[True]]], True];
  value = snegAssuming[p /: p > 0 = True, (p[1] = 9; p > 0)];
  testcase["Assumption body ran", value, True];
  testcase["Body definitions restored", DownValues[p], savedDown];
  testcase["Nested upvalues restored", UpValues[p], savedUp];

  snegfermionoperators[f, g];
  old = {f[CR, 1], f[CR, 2]}; new = {g[CR, 1], g[CR, 2]};
  rotation = {{1, I}, {I, 1}}/Sqrt[2];
  rules = snegold2newrules[old, new, rotation.old];
  zerotest["Inverse complex orbital transformation", (old /. rules) - ConjugateTranspose[rotation].new];
  testcase["Transformed orbital CAR", Expand[Outer[anticommutator, conj[old /. rules], old /. rules]], IdentityMatrix[2]];
  testcase["Nonunitary orbital transformation rejected",
    Block[{Print = Function[Null]}, snegold2newrules[old, new, 2 rotation.old]], Null];
  testcase["Orbital dimension mismatch rejected",
    Block[{Print = Function[Null]}, snegold2newrules[old, {First[new]}, rotation.old]], Null];
 ]];

testgroup["ASCII operator and bra-ket conversion",
 Module[{f, b, expressions, expr, text},
  snegfermionoperators[f]; snegbosonoperators[b];
  expressions = {f[CR], f[AN], f[CR, 3, UP], f[AN, -2, DO], b[CR, 2],
    ket[], bra[], ket[1, Null, 3], bra[2, Null],
    nc[f[CR, 1, UP], f[AN, 2, DO]], nc[b[CR, 2], f[CR, 1, UP], ket[1, 2]],
    2 f[CR, UP] + I f[AN, DO]};
  Do[
    text = snegtoascii[expr];
    testcase["ASCII round trip", asciitosneg[text], expr],
    {expr, expressions}];
  testcase["ASCII creator syntax", snegtoascii[f[CR, 3, UP]], ToString[f] <> "+(3,1)"];
  testcase["ASCII annihilator syntax", snegtoascii[f[AN]], ToString[f] <> "()"];
  testcase["ASCII scalar product", asciitosneg["<1|" <> "|1>"], 1];
  testcase["ASCII orthogonal states", asciitosneg["<1|" <> "|2>"], 0];
  testcase["ASCII reordering sign",
    asciitosneg[ToString[f] <> "(1)" <> ToString[f] <> "+(2)"], -nc[f[CR, 2], f[AN, 1]]];
  testcase["Center-dot input", CenterDot[f[AN, UP], f[CR, UP]], 1-number[f[], UP]];
 ]];

testgroup["Higher-spin and spinless occupation bases",
 Module[{f, spin, modes, states, cr, an, nmat, sz, jp, localp, j, k, bz},
  Do[
    snegfermionoperators[{f, spin}];
    modes = 2 spin+1; states = fockstates[modes];
    cr = Table[fockcreate[modes, j], {j, modes}]; an = Transpose /@ cr;
    nmat = Total[MapThread[Dot, {cr, an}]];
    sz = Total[MapThread[Times, {Range[spin, -spin, -1], MapThread[Dot, {cr, an}]}]];
    localp = Table[If[m == n+1, Sqrt[(spin-n) (spin+n+1)], 0], {m, spin, -spin, -1}, {n, spin, -spin, -1}];
    jp = Sum[localp[[j, k]] cr[[j]].an[[k]], {j, modes}, {k, modes}];
    bz = qszbasisvc[{f[]}];
    checkbasis["Generic QSZ", bz, states, {nmat - modes IdentityMatrix[2^modes]/2, sz}, # &];
    testcase["Generic basis dimension", Total[Length[#[[2]]] & /@ bz], 2^modes];
    testcase["Generic number operator", matrixrepresentationvc[number[f[]], states], nmat];
    testcase["Generic Hubbard pairs", matrixrepresentationvc[hubbard[f[]], states], nmat.(nmat-IdentityMatrix[2^modes])/2];
    testcase["Generic spin raising", matrixrepresentationvc[spinplus[f[]], states], jp];
    testcase["Generic spin Z", matrixrepresentationvc[spinz[f[]], states], sz];
    testcase["Single-site operator basis dimension", Length[basis[f[]]], 2^modes],
    {spin, {0, 1, 3/2}}];
  testcase["Subset generator boundary", {KSubsets[{1, 2, 3}, 0], KSubsets[{1, 2, 3}, 4]}, {{{}}, {}}];
  testcase["General subset generator", KSubsets[{1, 2, 3, 4, 5}, 3],
    {{1, 2, 3}, {1, 2, 4}, {1, 2, 5}, {1, 3, 4}, {1, 3, 5},
     {1, 4, 5}, {2, 3, 4}, {2, 3, 5}, {2, 4, 5}, {3, 4, 5}}];
 ]];

testgroup["State and basis utilities",
 Module[{f, b, m, z, states, v, w, bz, projected, coords, target,
     savedBrakets, aux, gram, cr, numberMatrix, sz, allStates},
  snegfermionoperators[f]; snegbosonoperators[b];
  snegmajoranaoperators[m]; sneggrassmanconstants[z];
  testcase["Boson predicates", {bosonQ[b], bosonQ[b[CR, 2]]}, {True, True}];
  testcase["Majorana predicates", {majoranaQ[m], majoranaQ[m[2]]}, {True, True}];
  testcase["Grassmann predicates", {grassmanQ[z], grassmanQ[conj[z]]}, {True, True}];
  testcase["Operator families stay distinct", TrueQ /@ {fermionQ[b], bosonQ[f], majoranaQ[f]}, {False, False, False}];
  makebasis[{f[]}]; states = {vc[1, 0], vc[0, 1]};
  v = (1+I) states[[1]] - 2 I states[[2]];
  w = (1-I) states[[1]] + 2 states[[2]];
  testcase["Occupation decomposition", decomposevc[v, states], {1+I, -2 I}];
  testcase["Operator decomposition", decomposeop[(1+I) f[CR, UP]-2 I f[CR, DO], {f[CR, UP], f[CR, DO]}], {1+I, -2 I}];
  testcase["Complex scalar product", scalarproductvc[v, w], 2 I];
  testcase["Complex expectation value", braketvc[v, spinz[f[]], v], -1];
  testcase["Unnormalized expectation value", expvvc[number[f[]], v], 6];
  testcase["Complex state norm", normvc[v], Sqrt[6]];
  bz = {{{0, 1/2}, {states[[1]]}}, {{0, -1/2}, {states[[2]]}}};
  testcase["Map over basis states", applybasis[bz, I # &],
    {{{0, 1/2}, {I vc[1, 0]}}, {{0, -1/2}, {I vc[0, 1]}}}];
  testcase["Drop empty sectors", dropemptysubspaces[{{{1}, {}}, {{2}, {v}}, {{3}, {}}}], {{{2}, {v}}}];
  target = (states[[1]]+I states[[2]])/Sqrt[2];
  projected = remove1state[states, target];
  coords = coordinates[#, states] & /@ projected;
  testcase["Complex orthogonal complement dimension", Length[projected], 1];
  zerotest["Complex orthogonal complement norm", Conjugate[coords].Transpose[coords]-IdentityMatrix[1]];
  zerotest["Complex orthogonal complement overlap", Conjugate[coordinates[target, states]].Transpose[coords]];
  testcase["Remove a spanning set", removestates[states, {target, (states[[1]]-I states[[2]])/Sqrt[2]}], {}];

  savedBrakets = DownValues[braketrule];
  Block[{braketrule},
    DownValues[braketrule] = savedBrakets;
    braketrule[bra[1], ket[2]] = I/2;
    braketrule[bra[2], ket[1]] = -I/2;
    aux = {vc[ket[1]], vc[ket[2]]}; gram = {{1, I/2}, {-I/2, 1}};
    testcase["Custom ket overlap", scalarproductvc[aux[[1]], aux[[2]]], I/2];
    testcase["Fast matrix honors ket overlaps", matrixrepresentationvcfast[1, aux], gram];
    testcase["Sparse matrix honors ket overlaps", Normal[matrixrepresentationvcsparse[1, aux]], gram]];
  testcase["Ket-overlap definitions restored", DownValues[braketrule], savedBrakets];
  testcase["Default ket overlap", braketrule[bra[1], ket[2]], 0];
  testcase["Spin-threaded number operator", snegSpinThread[nc[f[CR], f[AN]], {UP, DO}], number[f[]]];

  cr = {fockcreate[2, 1], fockcreate[2, 2]};
  numberMatrix = cr[[1]].Transpose[cr[[1]]] + cr[[2]].Transpose[cr[[2]]];
  sz = (cr[[1]].Transpose[cr[[1]]] - cr[[2]].Transpose[cr[[2]]])/2;
  bz = QSZaddspinket[qszbasisvc[{f[]}], 1];
  allStates = Flatten[Table[vc @@ Join[bits, {ket[k]}], {bits, Tuples[{0, 1}, 2]}, {k, 1, -1, -1}], 1];
  checkbasis["Abelian spin-ket product", bz, allStates,
    {KroneckerProduct[numberMatrix-IdentityMatrix[4], IdentityMatrix[3]],
     KroneckerProduct[sz, IdentityMatrix[3]] + KroneckerProduct[IdentityMatrix[4], DiagonalMatrix[{1, 0, -1}]]}, # &];
  testcase["Abelian spin-ket dimension", Total[Length[#[[2]]] & /@ bz], 12];
 ]];

(* Momentum space and lattices. Global symbols are used for the indexes so
   that automatically renamed dummy indexes (momk1, ...) have a root name. *)
sneglatticeindexes[{momi, momj}, momN];
snegmomentumindexes[{momk, momp, momq}, momN];
sneglatticevectors[moma1, moma2];
snegfermionoperators[momc, momck, momd];
snegspinlessfermionoperators[momf, momfk];
snegbosonoperators[momb, mombk];
snegrealconstants[momt, momU, momv, momw, momalpha, momV, momtheta, momz];
snegcomplexfunctions[momg];
snegfreeindexes[momgi, momgj];

testgroup["Momentum-space deltas and lattice sums",
 Module[{expr, renamed, prod},
  testcase["Index declarations",
    {latticeindexQ[momi], momentumindexQ[momk], latticeindexQ[momk],
     latticesize[momk], Conjugate[momk], TrueQ[latticevectorQ[moma1]]},
    {True, True, False, momN, momk, True}];
  testcase["Shifted momentum delta",
    sumSimplifyKD[sum[KroneckerDelta[momp, momk + momq] momg[momk], {momk}]],
    momg[momp - momq]];
  testcase["Reflected momentum delta",
    sumSimplifyKD[sum[KroneckerDelta[momp, momq - momk] momg[momk], {momk}]],
    momg[momq - momp]];
  expr = sum[KroneckerDelta[2 momk, momq] momg[momk], {momk}];
  testcase["Non-unit coefficients are not solved", sumSimplifyKD[expr], expr];
  expr = sum[KroneckerDelta[momgj, momgi + 1] momg[momgi], {momgi}];
  testcase["Generic indexes keep bare-delta behavior", sumSimplifyKD[expr], expr];
  expr = sum[KroneckerDelta[momk + momq, momi + 1] momg[momk], {momk}];
  testcase["Mixed lattice and momentum deltas are not solved", sumSimplifyKD[expr], expr];
  testcase["Delta of a pure momentum transfer",
    sumSimplifyKD[KroneckerDelta[momk, momk + momq]], KroneckerDelta[momq, 0]];
  testcase["Delta inside scalar blocks",
    sumSimplifyKD[sum[scalar[KroneckerDelta[momp, momk + momq] momz + momt] momc[CR, momk, UP], {momk}]],
    scalar[momz] momc[CR, momp - momq, UP] + sum[scalar[momt] momc[CR, momk, UP], {momk}]];
  testcase["Two summed momenta: the last is eliminated",
    sumSimplifyKD[sum[KroneckerDelta[momq, momk - momp] momg[momk, momp], {momk, momp}]],
    sum[momg[momk, momk - momq], {momk}]];
  testcase["Number of unit cells", sum[momg[momq], {momk}], momN momg[momq]];
  testcase["Number of unit cells for two indexes", sum[1, {momk, momi}], momN^2];
  testcase["Density commutator",
    snegsumreduce[komutator[momc[AN, momp, UP],
      sum[nc[momc[CR, momk + momq, UP], momc[AN, momk, UP]], {momk}]]],
    momc[AN, momp - momq, UP]];
  prod = nc[sum[momc[CR, momk, UP], {momk}], sum[momc[AN, momk, UP], {momk}]];
  renamed = First[Complement[Cases[prod, sum[_, l_] :> l, {0, Infinity}][[1]], {momk}]];
  testcase["Renamed indexes inherit index properties",
    {momentumindexQ[renamed], latticesize[renamed], Conjugate[renamed]},
    {True, momN, renamed}];

  testcase["Bloch phase splitting", blochphase[momk, momi + 2],
    blochphase[momk, momi] blochphase[2 momk, 1]];
  testcase["Bloch phase products", blochphase[momk, momi] blochphase[momq, momi],
    blochphase[momk + momq, momi]];
  testcase["Bloch phase conjugation", Conjugate[blochphase[momk, momi]],
    blochphase[-momk, momi]];
  testcase["Bloch phase conjugation by conj", conj[momt blochphase[momk, moma1]],
    momt blochphase[-momk, moma1]];
  testcase["Bloch phase trivial arguments", {blochphase[0, momi], blochphase[momk, 0]}, {1, 1}];
  testcase["Explicit Bloch phases", blochphase[{1, 2}, {3, 4}], Exp[11 I]];
  testcase["Bloch phase evaluation", blochphaseexp[blochphase[momk, moma1] + blochphase[momk, moma2],
      {momk -> {1, 2}, moma1 -> {1/2, 1}, moma2 -> {0, 1}}], Exp[2 I] + Exp[5 I/2]];
  testcase["One-dimensional Bloch phase evaluation", blochphaseexp[blochphase[momk, 1]],
    Exp[I momk]];
  testcase["Lattice sum over sites",
    sumSimplifyKD[sum[blochphase[momk - momp, momi], {momi}]], momN KroneckerDelta[momk, momp]];
  testcase["Lattice sum over momenta",
    sumSimplifyKD[sum[blochphase[momk, momi - momj], {momk}]], momN KroneckerDelta[momi, momj]];
  testcase["Lattice sum with displacement",
    sumSimplifyKD[sum[blochphase[momk, momi + moma1] blochphase[-momp, momi] momg[momk], {momi}]],
    momN KroneckerDelta[momk, momp] blochphase[momk, moma1] momg[momk]];
  expr = sum[Sin[blochphase[momk, momi]], {momi}];
  testcase["Non-multiplicative phases are not summed", sumSimplifyKD[expr], expr];
 ]];

testgroup["Fourier transformation and Bloch Hamiltonians",
 Module[{H, Hk, h, hR, cr, an, hssh, hssh2, hg, expected},
  H = -momt sum[hop[momc[momi], momc[momi + 1]], {momi}];
  Hk = fourier[H, momc -> momck, momi -> momk];
  testcase["Chain dispersion",
    Hk, sum[Expand[-momt (blochphase[-momk, 1] + blochphase[momk, 1]) number[momck[momk]]], {momk}]];
  expected = Expand[-momt (blochphase[-momk, 1] + blochphase[momk, 1]) IdentityMatrix[2]];
  testcase["Chain Bloch Hamiltonian", blochhamiltonian[Hk, {momck}, momk], expected];
  testcase["Bloch Hamiltonian with a different summation index",
    blochhamiltonian[fourier[H, momc -> momck, momi -> momq], {momck}, momk], expected];
  (* Independent route: h(k) = Sum_R e^(-i k R) {c_0, [H, c_R^dag]}. *)
  hR = snegsumreduce[sum[blochphase[-momk, momj] antikomutator[momc[AN, 0, UP],
     komutator[H, momc[CR, momj, UP]]], {momj}]];
  testcase["Real-space matrix-element route", hR, expected[[1, 1]]];
  testcase["Parseval identity",
    fourier[sum[number[momc[momi]], {momi}], momc -> momck, momi -> momk],
    sum[number[momck[momk]], {momk}]];
  testcase["Inverse transformation",
    fourier[sum[number[momck[momk]], {momk}], momc -> momck, momi -> momk, "Inverse" -> True],
    sum[number[momc[momi]], {momi}]];
  testcase["Transformed operators keep canonical anticommutators",
    snegsumreduce[antikomutator[
      fourier[momc[AN, momi, UP], momc -> momck, momi -> momk],
      fourier[momc[CR, momj, UP], momc -> momck, momi -> momp]]],
    KroneckerDelta[momi, momj]];
  Hk = fourier[momU sum[hubbard[momc[momi]], {momi}], momc -> momck, momi -> momk];
  cr = Cases[Hk, momck[CR, q_, _] :> q, Infinity];
  an = Cases[Hk, momck[AN, q_, _] :> q, Infinity];
  testcase["Hubbard interaction conserves momentum",
    {Length[cr], Simplify[Total[cr] - Total[an]], FreeQ[Hk, blochphase], !FreeQ[Hk, momU/momN]},
    {2, 0, True, True}];
  testcase["Impurity hybridization",
    fourier[momV hop[momd[], momc[0]], momc -> momck, momi -> momk],
    sum[Expand[momV/Sqrt[momN] hop[momd[], momck[momk]]], {momk}]];
  testcase["Bosonic chain",
    blochhamiltonian[fourier[-momt sum[nc[momb[CR, momi], momb[AN, momi + 1]] +
      nc[momb[CR, momi + 1], momb[AN, momi]], {momi}], momb -> mombk, momi -> momk], {mombk}, momk],
    {{expected[[1, 1]]}}];
  testcase["Fourier capture check",
    Quiet[fourier[number[momck[momk]], momc -> momck, momi -> momk], fourier::capture], $Failed];
  testcase["Fourier rejects undeclared indexes",
    Quiet[fourier[number[momc[1]], momc -> momck, momgi -> momk], fourier::index], $Failed];

  H = sum[momv hop[momf[momi, "A"], momf[momi, "B"]] + momw hop[momf[momi + 1, "A"], momf[momi, "B"]], {momi}];
  Hk = fourier[H, momf -> momfk, momi -> momk];
  hssh = blochhamiltonian[Hk, {momfk["A"], momfk["B"]}, momk];
  testcase["SSH Bloch Hamiltonian", hssh,
    {{0, momv + momw blochphase[-momk, 1]}, {momv + momw blochphase[momk, 1], 0}}];
  hssh2 = blochhamiltonian[Hk, {momfk["A"], momfk["B"]}, momk, "OrbitalPositions" -> {0, 1/2}];
  testcase["Orbital-position convention", hssh2,
    {{0, (momv + momw blochphase[-momk, 1]) blochphase[momk, 1/2]},
     {(momv + momw blochphase[momk, 1]) blochphase[-momk, 1/2], 0}}];
  zerotest["Both conventions have the same spectrum",
    blochphaseexp[Det[hssh] - Det[hssh2]]];

  H = -momt sum[hop[momf[momi, "A"], momf[momi, "B"]] + hop[momf[momi, "A"], momf[momi - moma1, "B"]] +
      hop[momf[momi, "A"], momf[momi - moma2, "B"]], {momi}];
  hg = blochhamiltonian[fourier[H, momf -> momfk, momi -> momk], {momfk["A"], momfk["B"]}, momk];
  testcase["Graphene Bloch Hamiltonian", hg,
    Expand[{{0, -momt (1 + blochphase[-momk, moma1] + blochphase[-momk, moma2])},
     {-momt (1 + blochphase[momk, moma1] + blochphase[momk, moma2]), 0}}]];
  zerotest["Graphene Dirac point",
    blochphaseexp[hg, {momk -> {4 Pi/3, 0}, moma1 -> {1, 0}, moma2 -> {1/2, Sqrt[3]/2}}]];

  h = blochhamiltonian[fourier[sum[matrixhop[-momt IdentityMatrix[2] + I momalpha PauliY,
      momc[momi], momc[momi + 1]], {momi}], momc -> momck, momi -> momk], {momck}, momk];
  zerotest["Rashba chain",
    ExpToTrig[blochphaseexp[h]] - (-2 momt Cos[momk] IdentityMatrix[2] - 2 momalpha Sin[momk] PauliY)];
  testcase["Translation-invariance check",
    Quiet[blochhamiltonian[fourier[sum[blochphase[Pi, momi] number[momf[momi]], {momi}],
      momf -> momfk, momi -> momk], {momfk}, momk], blochhamiltonian::transl], $Failed];
  testcase["Hermiticity check",
    Quiet[Check[blochhamiltonian[sum[I nc[momfk[CR, momk, "A", 0], momfk[AN, momk, "B", 0]], {momk}],
      {momfk["A"], momfk["B"]}, momk], "msg", blochhamiltonian::herm], blochhamiltonian::herm], "msg"];
 ]];

testgroup["Momentum shifts",
 Module[{rho, cm, x, savedOrdering},
  rho[qq_] := sum[nc[momck[CR, momp + qq, UP], momck[AN, momp, UP]], {momp}];
  cm = snegsumreduce[komutator[rho[momq], rho[-momq]]];
  testcase["Lattice density operators commute",
    sumCollect[sumMomentumCanonical[sumExpand[cm]]], 0];
  x = sum[number[momck[momk + momq]], {momk}] - sum[number[momck[momk]], {momk}];
  testcase["Shift invariance of lattice sums", sumFullSimplify[x], 0];
  testcase["Explicit shift",
    sumShift[sum[number[momfk[momk]], {momk}], momk -> momk + momq],
    sum[number[momfk[momk + momq]], {momk}]];
  testcase["Invalid shifts are rejected",
    Quiet[sumShift[sum[number[momfk[momk]], {momk}], momk -> 2 momk], sumShift::shift],
    sum[number[momfk[momk]], {momk}]];
  testcase["Shifts require lattice or momentum indexes",
    Quiet[sumShift[sum[momg[momgi], {momgi}], momgi -> momgi + 1], sumShift::index],
    sum[momg[momgi], {momgi}]];
  snegfermionoperators[momsea];
  ordering[momsea] = SEA;
  x = sum[number[momsea[momk + momq]], {momk}];
  testcase["SEA-ordered sums are not canonicalized", sumMomentumCanonical[x], x];
  testcase["SEA-ordered sums are not shifted",
    Quiet[sumShift[x, momk -> momk - momq], sumShift::sea], x];
 ]];

testgroup["Finite periodic clusters",
 Module[{L = 4, HR, Hsym, HK, HF, ops, opsR, bz, bzR, bzK, sector, eigK, eigR, xx,
   H1, b1, H2, H2k, offdiag},
  HR = clusterhop[-momt, momc, momc, 1, {L}] + momU Sum[hubbard[momc[n]], {n, 0, L - 1}];
  Hsym = fourier[-momt sum[hop[momc[momi], momc[momi + 1]], {momi}] +
    momU sum[hubbard[momc[momi]], {momi}], momc -> momck, momi -> momk];
  HK = sumCluster[Hsym, {L}];
  HF = Expand[HR /. fourierrules[momc -> momck, {L}]];
  zerotest["Explicit Fourier rules agree with the specialized symbolic result", Expand[HF - HK]];
  testcase["Real-space cluster from a symbolic lattice sum",
    sumCluster[-momt sum[hop[momc[momi], momc[momi + 1]], {momi}], {L}],
    Expand[clusterhop[-momt, momc, momc, 1, {L}]]];
  ops = Table[momck[m], {m, 0, L - 1}];
  makebasis[ops];
  bzK = momentumbasis[qszbasis[ops], {L}];
  sector = Select[bzK, Take[#[[1]], 2] == {-2, 0} &];
  testcase["Total-momentum sectors", sector[[All, 1]],
    {{-2, 0, 0}, {-2, 0, 1}, {-2, 0, 2}, {-2, 0, 3}}];
  testcase["Sector dimensions", Total[Length /@ sector[[All, 2]]], 16];
  offdiag = Flatten[Outer[braketop[#1, HK, #2] &, sector[[1, 2]], sector[[2, 2]]]];
  testcase["Hamiltonian is block diagonal in K", Union[Expand[offdiag]], {0}];
  eigK = Flatten[Map[Eigenvalues[matrixrepresentationop[HK /. {momt -> 1, momU -> 3}, #]] &,
    sector[[All, 2]]]];
  opsR = Table[momc[n], {n, 0, L - 1}];
  makebasis[opsR];
  bzR = Select[qszbasis[opsR], #[[1]] == {-2, 0} &];
  eigR = Eigenvalues[matrixrepresentationop[HR /. {momt -> 1, momU -> 3}, bzR[[1, 2]]]];
  zerotest["Hubbard ring spectra in real and momentum space",
    Expand[Times @@ (xx - eigK) - Times @@ (xx - eigR)]];
  testcase["Two-site ring doubles the hopping", clusterhop[1, momc, momc, 1, {2}],
    Expand[2 hop[momc[0], momc[1]]]];
  H1 = clusterhop[-1, momc, momc, 1, {3}, "Twist" -> momtheta];
  makebasis[Table[momc[n], {n, 0, 2}]];
  b1 = Select[qszbasis[Table[momc[n], {n, 0, 2}]], #[[1]] == {-2, 1/2} &][[1, 2]];
  testcase["Twisted single-particle levels",
    Chop[Sort[N[Eigenvalues[matrixrepresentationop[H1, b1] /. momtheta -> 7/10]]] -
      Sort[N[Table[-2 Cos[(2 Pi m + 7/10)/3], {m, 0, 2}]]]], {0, 0, 0}];
  zerotest["Twisted Fourier rules diagonalize the ring",
    Expand[H1 /. fourierrules[momc -> momck, {3}, "Twist" -> momtheta]] -
      Sum[-2 Cos[(2 Pi m + momtheta)/3] number[momck[m]], {m, 0, 2}] // ExpToTrig];
  H2 = -momt sum[hop[momc[momi], momc[momi + moma1]] + hop[momc[momi], momc[momi + moma2]], {momi}];
  H2k = sumCluster[fourier[H2, momc -> momck, momi -> momk], {2, 3},
    {moma1 -> {1, 0}, moma2 -> {0, 1}}];
  zerotest["Two-dimensional cluster",
    Expand[H2k - Expand[(clusterhop[-momt, momc, momc, {1, 0}, {2, 3}] +
      clusterhop[-momt, momc, momc, {0, 1}, {2, 3}]) /. fourierrules[momc -> momck, {2, 3}]]]];
  testcase["Two-dimensional momentum labels",
    momentumbasis[{{{-1, 1/2}, {momck[CR, {1, 2}, UP], momck[CR, {0, 1}, UP]}}}, {2, 3}],
    {{{-1, 1/2, {0, 1}}, {momck[CR, {0, 1}, UP]}}, {{-1, 1/2, {1, 2}}, {momck[CR, {1, 2}, UP]}}}];
  testcase["Mixed-momentum states are rejected",
    Quiet[momentumbasis[{{{0}, {momck[CR, 1, UP] + momck[CR, 2, UP]}}}, {4}], momentumbasis::mixed],
    $Failed];
 ]];

testgroup["Momentum-space options and diagnostics",
 Module[{pr, renamed, Hk, H, ops, bz, bvc, x},
  sneglatticeindexes[{momR4}, 4];
  testcase["Integer lattice size", {latticesize[momR4], sum[1, {momR4}]}, {4, 4}];
  sneglatticeindexes[{momRL}, momL1 momL2];
  testcase["Product lattice size", {sum[1, {momRL}], Conjugate[momL1]}, {momL1 momL2, momL1}];
  testcase["Invalid lattice size",
    Quiet[Check[sneglatticeindexes[{momRbad}, -3], "msg", sneglatticeindexes::size],
      sneglatticeindexes::size], "msg"];
  testcase["Invalid lattice size declares nothing", latticeindexQ[momRbad], False];
  snegfreeindexes[momgr]; Conjugate[momgr] ^= momgr;
  pr = nc[sum[momc[CR, momgr, UP], {momgr}], sum[momc[AN, momgr, UP], {momgr}]];
  renamed = First[Complement[Cases[pr, sum[_, l_] :> l, {0, Infinity}][[1]], {momgr}]];
  testcase["Renamed generic indexes inherit realness",
    {Conjugate[renamed], latticetypedQ[renamed]}, {renamed, False}];

  testcase["Bare lattice delta", sumSimplifyKD[sum[KroneckerDelta[momi, momj + moma1] momg[momi], {momi}]],
    momg[momj + moma1]];
  testcase["Shifted lattice delta", sumSimplifyKD[sum[KroneckerDelta[momi + moma1, momj] momg[momi], {momi}]],
    momg[momj - moma1]];

  testcase["Integer factors move to the momentum", blochphase[momk, 2 momi], blochphase[2 momk, momi]];
  testcase["Powers of Bloch phases", blochphase[momk, momi]^3, blochphase[3 momk, momi]];
  testcase["Lattice sum inside scalar blocks",
    sumSimplifyKD[sum[scalar[momt blochphase[momk - momp, momi]] momc[CR, momk, UP], {momi}]],
    momc[CR, momk, UP] scalar[momN momt KroneckerDelta[momk, momp]]];
  testcase["Formatted Bloch phase",
    MatchQ[ToBoxes[blochphase[momk, momi], StandardForm], TagBox[SuperscriptBox["e", _RowBox], DisplayForm]],
    True];

  testcase["Reflection shift", sumShift[sum[number[momfk[momk]], {momk}], momk -> -momk + momq],
    sum[number[momfk[-momk + momq]], {momk}]];
  testcase["Mixed-type shifts are rejected",
    Quiet[Check[sumShift[sum[number[momfk[momk]], {momk}], momk -> momk + momi], "msg", sumShift::shift],
      sumShift::shift], "msg"];

  testcase["Fourier index position",
    fourier[sum[hop[momf["x", momi], momf["x", momi + 1]], {momi}], momf -> momfk, momi -> momk,
      "IndexPosition" -> 2],
    sum[Expand[(blochphase[-momk, 1] + blochphase[momk, 1]) number[momfk["x", momk]]], {momk}]];
  testcase["Fourier without simplification",
    fourier[momc[AN, momi, UP], momc -> momck, momi -> momk, "Simplify" -> False],
    sum[blochphase[momk, momi] momck[AN, momk, UP], {momk}]/Sqrt[momN]];
  snegmajoranaoperators[mommaj];
  testcase["Fourier rejects Majorana operators",
    Quiet[fourier[mommaj[momi], mommaj -> momck, momi -> momk], fourier::op], $Failed];
  testcase["Inverse Fourier rejects undeclared indexes",
    Quiet[fourier[momck[AN, momk, UP], momc -> momck, momgi -> momk, "Inverse" -> True], fourier::index],
    $Failed];

  Hk = fourier[-momt sum[hop[momc[momi], momc[momi + 1]], {momi}], momc -> momck, momi -> momk];
  testcase["Bloch Hamiltonian without checks", blochhamiltonian[Hk, {momck}, momk, "Check" -> False],
    Expand[-momt (blochphase[-momk, 1] + blochphase[momk, 1]) IdentityMatrix[2]]];
  testcase["Bloch Hamiltonian index position",
    blochhamiltonian[fourier[-momt sum[hop[momf["x", momi], momf["x", momi + 1]], {momi}],
      momf -> momfk, momi -> momk, "IndexPosition" -> 2], {momfk["x"]}, momk, "IndexPosition" -> 2],
    {{Expand[-momt (blochphase[-momk, 1] + blochphase[momk, 1])]}}];
  testcase["Bloch Hamiltonian needs a momentum index",
    Quiet[blochhamiltonian[Hk, {momck}, momi], blochhamiltonian::index], $Failed];
  testcase["Bloch Hamiltonian rejects invalid orbitals",
    Quiet[blochhamiltonian[Hk, {mommaj}, momk], blochhamiltonian::orb], $Failed];
  testcase["Bloch Hamiltonian rejects interacting terms",
    Quiet[blochhamiltonian[Hk + sum[hubbard[momck[momk]], {momk}], {momck}, momk], blochhamiltonian::rest],
    $Failed];
  snegfermionoperators[moms1, moms1k]; spinof[moms1] ^= 1; spinof[moms1k] ^= 1;
  testcase["Spin-1 fermions",
    blochhamiltonian[fourier[-momt sum[hop[moms1[momi], moms1[momi + 1]], {momi}], moms1 -> moms1k,
      momi -> momk], {moms1k}, momk],
    Expand[-momt (blochphase[-momk, 1] + blochphase[momk, 1]) IdentityMatrix[3]]];

  H = momt IdentityMatrix[2] + I momalpha PauliY;
  zerotest["Spin-matrix cluster hopping",
    Expand[clusterhop[H, momc, momc, 1, {3}] - Sum[matrixhop[H, momc[n], momc[Mod[n + 1, 3]]], {n, 0, 2}]]];
  testcase["Orbital cluster hopping", clusterhop[momt, momf["A"], momf["B"], 0, {2}],
    Expand[momt (hop[momf[0, "A"], momf[0, "B"]] + hop[momf[1, "A"], momf[1, "B"]])]];
  testcase["Bosonic cluster hopping", clusterhop[momt, momb, momb, 1, {3}],
    Expand[momt Sum[nc[momb[CR, n], momb[AN, Mod[n + 1, 3]]] + nc[momb[CR, Mod[n + 1, 3]], momb[AN, n]],
      {n, 0, 2}]]];
  testcase["Cluster hopping checks dimensions",
    Quiet[clusterhop[momt, momc, momc, {1, 0}, {3}], clusterhop::dims], $Failed];

  x = sum[number[momfk[momk + 1]], {momk}];
  testcase["Cluster labels are folded", sumCluster[x, {2}], Expand[number[momfk[1]] + number[momfk[0]]]];
  testcase["Folding can be disabled", sumCluster[x, {2}, {}, "Operators" -> {}],
    Expand[number[momfk[1]] + number[momfk[2]]]];
  testcase["Cluster index position",
    sumCluster[sum[nc[momfk[CR, "x", momk], momfk[AN, "x", momk + 3]], {momk}], {2}, {}, "IndexPosition" -> 2],
    nc[momfk[CR, "x", 0], momfk[AN, "x", 1]] + nc[momfk[CR, "x", 1], momfk[AN, "x", 0]]];
  testcase["Cluster size mismatch",
    Quiet[Check[sumCluster[sum[number[momc[momR4]], {momR4}], {3}], "msg", sumCluster::size],
      sumCluster::size], "msg"];
  testcase["Free momenta stay symbolic",
    sumCluster[sum[nc[momfk[CR, momk + momq], momfk[AN, momk]], {momk}], {2}],
    nc[momfk[CR, momq], momfk[AN, 0]] + nc[momfk[CR, 1 + momq], momfk[AN, 1]]];
  testcase["Free momenta substituted and folded",
    sumCluster[sum[nc[momfk[CR, momk + momq], momfk[AN, momk]], {momk}], {2}, {momq -> 3}],
    nc[momfk[CR, 1], momfk[AN, 0]] + nc[momfk[CR, 0], momfk[AN, 1]]];

  ops = {momck[0], momck[1]};
  makebasis[ops];
  bz = qszbasis[ops];
  bvc = bzop2bzvc[bz, vacuum[]];
  testcase["Momentum sectors of occupation-number bases",
    momentumbasis[bvc, {2}][[All, 1]], momentumbasis[bz, {2}][[All, 1]]];
  testcase["Momentum sectors are complete",
    Total[Length /@ momentumbasis[bvc, {2}][[All, 2]]], 16];
  testcase["Momentum basis index position",
    momentumbasis[{{{0}, {nc[momfk[CR, "x", 1, 0], momfk[CR, "x", 2, 0]]}}}, {4}, "IndexPosition" -> 2],
    {{{0, 3}, {nc[momfk[CR, "x", 1, 0], momfk[CR, "x", 2, 0]]}}}];
  testcase["Non-unitary transformations are reported",
    Quiet[Check[snegcheckunitary[{{1, 0}, {0, 2}}], "msg", fourierrules::unitary],
      fourierrules::unitary], "msg"];
  testcase["Unitary transformations pass silently",
    Check[snegcheckunitary[{{1, 1}, {1, -1}}/Sqrt[2]], "msg"], True];
  testcase["Fourier rules are unitary for numeric twists",
    Check[Length[fourierrules[momc -> momck, {3}, "Twist" -> 7/10]], "msg"], 1];
  testcase["Symbolic twists skip the numerical check",
    Check[Length[fourierrules[momc -> momck, {3}, "Twist" -> momtheta]], "msg"], 1];
  testcase["Fourier rules index position",
    Expand[nc[momf[CR, "x", 0, 0], momf[AN, "x", 1, 0]] /. fourierrules[momf -> momfk, {2}, "IndexPosition" -> 2]],
    Expand[(nc[momfk[CR, "x", 0, 0], momfk[AN, "x", 0, 0]] - nc[momfk[CR, "x", 0, 0], momfk[AN, "x", 1, 0]] +
      nc[momfk[CR, "x", 1, 0], momfk[AN, "x", 0, 0]] - nc[momfk[CR, "x", 1, 0], momfk[AN, "x", 1, 0]])/2]];
 ]];

(* Spin algebras and Hubbard X-operators. Global symbols are used for the
   component indexes so that renamed dummy indexes have a root name. *)
snegspinoperators[spG, spH, {spS, 1/2}, {spT, 1}, {spQ, sps}];
snegspinoperators[{spR1, 1/2}, {spR2, 1}, {spR3, 3/2}, {spR4, 2}, {spR5, 5/2}];
snegspincomponentindexes[spa, spb];
snegfermionoperators[spc, spd, {spc1, 1}];
snegspinlessfermionoperators[spf];
snegbosonoperators[spbo, spbk, spsa, spsb];
snegmajoranaoperators[spm];
sneggrassmanconstants[spz];
snegrealconstants[spJ, spJz, spt, spphi];
snegfreeindexes[spi, spj];
sneglatticeindexes[{spRR}, spN];
snegmomentumindexes[{spk}, spN];
snegxoperators[spX, "HubbardAtom"];
snegxoperators[spY, {1, 2, 3}, {0, 0, 0}];

spmat[s_, t_] := {spinmatrixX, spinmatrixY, spinmatrixZ, spinmatrixP, spinmatrixM}[[t]][s];
spkondo[op_] := spJ (nc[op[3], spinz[spc[]]] + (nc[op[4], spinminus[spc[]]] + nc[op[5], spinplus[spc[]]])/2);
spcommutes[ops_, rep_] := Expand[komutator[rep[#[[1]]], rep[#[[2]]]] - rep[#[[3]]]] & /@ ops;

testgroup["Spin conjugation, ordering, and declarations",
 Module[{h},
  testcase["Conjugate spin components",
    conj /@ {spG[1, 1], spG[1, 2], spG[1, 3], spG[1, 4], spG[1, 5], spG[1, spa]},
    {spG[1, 1], spG[1, 2], spG[1, 3], spG[1, 5], spG[1, 4], spG[1, spa]}];
  testcase["Exchange coupling is Hermitian",
    Expand[conj[spinspin[spG[1], spG[2]]] - spinspin[spG[1], spG[2]]], 0];
  testcase["Spins move left of fermions", nc[spc[CR, 1, UP], spG[1, 3]], nc[spG[1, 3], spc[CR, 1, UP]]];
  testcase["Spins move left of bosons", nc[spbo[AN], spG[1, 4]], nc[spG[1, 4], spbo[AN]]];
  testcase["Spins move left of Majoranas", nc[spm[1], spG[1, 4]], nc[spG[1, 4], spm[1]]];
  testcase["Grassmann numbers move left of spins", nc[spG[1, 4], spz], nc[spz, spG[1, 4]]];
  testcase["Different spin symbols are sorted", nc[spH[1, 3], spG[2, 5]], nc[spG[2, 5], spH[1, 3]]];
  testcase["Sizes", {spinof[spS], spinof[spT], spinof[spQ], snegspinsizeQ[spG], Conjugate[sps]},
    {1/2, 1, sps, False, sps}];
  testcase["Invalid spin size",
    Quiet[Check[snegspinoperators[{spBad, -1}], "msg", snegspinoperators::size], snegspinoperators::size], "msg"];
  testcase["Invalid spin size declares nothing", TrueQ[spinQ[spBad]], False];
  h = spkondo[spS];
  testcase["Kondo model conserves charge", Expand[komutator[h, number[spc[]]]], 0];
  testcase["Kondo model conserves S^z", Expand[komutator[h, spS[3] + spinz[spc[]]]], 0];
  testcase["Kondo model conserves S^+", Expand[komutator[h, spS[4] + spinplus[spc[]]]], 0];
  h = spkondo[spG];
  testcase["Kondo model with spin of unspecified size",
    spincomponents[Expand[komutator[h, #]]] & /@ {number[spc[]], spG[3] + spinz[spc[]], spG[4] + spinplus[spc[]]},
    {0, 0, 0}];
  testcase["Zero recognized after rewriting the components",
    spincomponents[Expand[komutator[spinspin[spG[1], spG[2]], spG[1, 4] + spG[2, 4]]]], 0];
  testcase["Cartesian rewriting", spincomponents[spG[1, 4] - spG[1, 5], "XYZ"], 2 I spG[1, 2]];
  testcase["Invalid component set",
    Quiet[spincomponents[spG[1, 1], "ABC"], spincomponents::basis], $Failed];
 ]];

testgroup["Spin size reductions against spin matrices",
 Module[{heads = {spR1, spR2, spR3, spR4, spR5}, words, s, h, e, monos, hh},
  SeedRandom[20261125];
  words = Table[RandomInteger[{1, 5}, RandomInteger[{2, 5}]], {30}];
  Do[
    s = spinof[h];
    e = Map[nc @@ Map[Function[t, h[1, t]], #] &, words];
    zerotest["Products for spin " <> ToString[s, InputForm],
      MapThread[spinclustermatrices[#1, {h[1]}, "Blocks" -> False] - Dot @@ Map[spmat[s, #] &, #2] &,
        {e, words}]];
    testcase["Normal form for spin " <> ToString[s, InputForm],
      And @@ Map[snegspinnormalQ[s, List @@ #[[All, -1]]] &,
        Cases[e, HoldPattern[nc[a__]] :> {a}, Infinity]], True];
    monos = Flatten[Table[Join[ConstantArray[3, b], ConstantArray[t, a]],
      {t, {4, 5}}, {a, If[t == 4, 0, 1], 2 s}, {b, 0, 2 s - a}], 2];
    testcase["Number of normal monomials for spin " <> ToString[s, InputForm], Length[monos], (2 s + 1)^2];
    testcase["Normal monomials are independent for spin " <> ToString[s, InputForm],
      MatrixRank[Map[Flatten[Dot @@ Append[Map[spmat[s, #] &, #], IdentityMatrix[2 s + 1]]] &, monos]],
      (2 s + 1)^2];
    testcase["Normal monomials are stable for spin " <> ToString[s, InputForm],
      Map[nc @@ Map[Function[t, h[1, t]], #] &, monos] /. h[1, t_] :> hh[t],
      Map[nc @@ Map[hh, #] &, monos]];
    testcase["Casimir for spin " <> ToString[s, InputForm], Expand[spinspin[h[1], h[1]]], s (s + 1)],
    {h, heads}];
  testcase["Pauli products stay Cartesian",
    {nc[spS[1, 1], spS[1, 2]], nc[spS[1, 2], spS[1, 3], spS[1, 1]], nc[spS[1, 1], spS[1, 1]]},
    {I spS[1, 3]/2, I/8, 1/4}];
  testcase["Mixed spin-1/2 products use S^+-",
    {nc[spS[1, 4], spS[1, 5]], nc[spS[1, 1], spS[1, 4]], nc[spS[1, 4], spS[1, 4]]},
    {1/2 + spS[1, 3], 1/4 - spS[1, 3]/2, 0}];
  testcase["Different sites are not reduced", nc[spS[1, 3], spS[2, 3]], nc[spS[1, 3], spS[2, 3]]];
  testcase["Casimir for symbolic size", Expand[spinspin[spQ[1], spQ[1]]], Expand[sps (sps + 1)]];
  testcase["Symbolic size eliminates S^+ S^-", nc[spQ[1, 5], spQ[1, 4]],
    sps (sps + 1) - nc[spQ[1, 3], spQ[1, 3]] - spQ[1, 3]];
  testcase["Symbolic size keeps powers", nc[spQ[1, 4], spQ[1, 4], spQ[1, 4]],
    nc[spQ[1, 4], spQ[1, 4], spQ[1, 4]]];
 ]];

testgroup["Spin component indexes",
 Module[{c, x},
  x = komutator[spG[1, spa], spG[1, spb]];
  c = First[Cases[x, HoldPattern[sum[_, {i_}]] :> i, Infinity]];
  testcase["Symbolic component commutator", x, I sum[levicivita[spa, spb, c] spG[1, c], {c}]];
  testcase["Renamed component index keeps its type", spincomponentindexQ[c], True];
  testcase["Symbolic commutator for explicit components",
    Table[spincomponents[x /. {spa -> j, spb -> k}] - spincomponents[cmt[spG[1, j], spG[1, k]]], {j, 3}, {k, 3}],
    ConstantArray[0, {3, 3}]];
  testcase["Symbolic commutator with S^+",
    spincomponents[komutator[spG[1, spa], spG[1, 4]] /. spa -> 3], spG[1, 4]];
  testcase["Different sites give a delta", komutator[spG[spi, spa], spG[spj, spb]],
    KroneckerDelta[spi, spj] komutator[spG[spi, spa], spG[spi, spb]]];
  testcase["Component sums", spincomponents[sum[nc[spG[1, spa], spG[2, spa]], {spa}]],
    spincomponents[spinspin[spG[1], spG[2]]]];
  testcase["Unused component index", sum[spJ, {spa}], 3 spJ];
  testcase["Levi-Civita symbol",
    {levicivita[1, 2, 3], levicivita[2, 1, 3], levicivita[1, 1, 2], levicivita[spa, spb, spa],
     levicivita[spb, spa, 3], levicivita[1, 2, 4], Conjugate[levicivita[spa, spb, 3]]},
    {1, -1, 0, 0, -levicivita[3, spa, spb], 0, levicivita[3, spa, spb]}];
 ]];

testgroup["Spin exact diagonalization",
 Module[{h, mats, r, bz, kb, ev1, ev2},
  h = spJ Sum[spinspin[spS[i], spS[Mod[i, 4] + 1]], {i, 4}];
  mats = spinclustermatrices[h, {spS -> Range[4]}];
  testcase["Heisenberg ring sectors", {First[#], Length[Last[#]]} & /@ mats,
    {{{-2}, 1}, {{-1}, 4}, {{0}, 6}, {{1}, 4}, {{2}, 1}}];
  testcase["Heisenberg ring ground state", Min[Flatten[Eigenvalues /@ (mats[[All, 2]] /. spJ -> 1)]], -2];
  testcase["Heisenberg ring full matrix",
    Sort[Eigenvalues[spinclustermatrices[h, {spS -> Range[4]}, "Blocks" -> False] /. spJ -> 1]],
    Sort[Flatten[Eigenvalues /@ (mats[[All, 2]] /. spJ -> 1)]]];
  testcase["Sparse output", Head[spinclustermatrices[spS[1, 3], {spS[1]}, "Sparse" -> True][[1, 2]]], SparseArray];
  testcase["Sites from zero", spinclustermatrices[spS[0, 3], {spS -> {0}}], {{{-1/2}, {{-1/2}}}, {{1/2}, {{1/2}}}}];
  h = spJ spinspin[spS[1], spT[1]] + spt spT[1, 3];
  r = spinketbrarules[{spS[1], spT[1]}];
  kb = Expand[h /. r];
  bz = spinbasis[{1/2, 1}];
  ev1 = Sort /@ Map[Eigenvalues[matrixrepresentationop[kb, #[[2]]]] &, bz];
  ev2 = Sort /@ Map[Eigenvalues[#[[2]]] &, spinclustermatrices[h, {spS[1], spT[1]}]];
  zerotest["Bra-ket and direct matrices agree", (ev1 - ev2) /. {spJ -> 1, spt -> 3/10}];
  bz = QSZaddspinket[qszbasisvc[{spc[]}], {1/2}];
  mats = makematricesbzvc[Expand[spkondo[spS] /. spinketbrarules[{spS[]}]], bz];
  testcase["Kondo singlet and triplet", Sort[Flatten[Eigenvalues /@ (mats[[All, 2]] /. spJ -> 1)]],
    Sort[{-3/4, 1/4, 1/4, 1/4, 0, 0, 0, 0}]];
  testcase["Several spin kets", Total[Length[#[[2]]] & /@ QSZaddspinket[qszbasisvc[{spc[]}], {1/2, 1}]], 24];
  bz = QSZaddspinket[qszbasisvc[{spc[]}], {1/2, 1/2}];
  mats = makematricesbzvc[Expand[(spinspin[spS[1], spS[2]] + spt spS[1, 3]) /. spinketbrarules[{spS[1], spS[2]}]], bz];
  testcase["Two impurity spins with a fermion level",
    Sort[Flatten[Eigenvalues /@ (mats[[All, 2]] /. spt -> 0)]],
    Sort[Flatten[ConstantArray[{-3/4, 1/4, 1/4, 1/4}, 4]]]];
  testcase["Single-spin terms with two spin kets",
    Sort[Flatten[Eigenvalues /@ (makematricesbzvc[spS[2, 3] /. spinketbrarules[{spS[1], spS[2]}], bz][[All, 2]])]],
    Sort[Flatten[ConstantArray[{-1/2, -1/2, 1/2, 1/2}, 4]]]];
  testcase["Undeclared spin size",
    Quiet[spinclustermatrices[spG[1, 3], {spG[1]}], snegspinsites::size], $Failed];
  testcase["S^z must be conserved",
    Quiet[spinclustermatrices[spS[1, 1], {spS[1]}], spinclustermatrices::sz], $Failed];
  testcase["Operators of other sites",
    Quiet[spinclustermatrices[spS[2, 3], {spS[1]}], spinclustermatrices::op], $Failed];
  testcase["Invalid site specification",
    Quiet[spinketbrarules[{spc[1]}], snegspinsites::spec], $Failed];
 ]];

testgroup["Spin representations",
 Module[{h, hf, ev1, ev2, bz, hk, hb, x, ops, aterm, bterm, w},
  h = Sum[spJ/2 (nc[spS[i, 4], spS[i + 1, 5]] + nc[spS[i, 5], spS[i + 1, 4]]) + spJz nc[spS[i, 3], spS[i + 1, 3]], {i, 3}];
  hf = jordanwigner[h, spS -> spf, Range[4]];
  bz = qszbasis[Table[spf[i], {i, 4}]];
  ev1 = Sort[Flatten[Eigenvalues /@ (makematricesbzop[hf /. {spJ -> 1, spJz -> 7/10}, bz][[All, 2]])]];
  ev2 = Sort[Flatten[Eigenvalues /@ (spinclustermatrices[h /. {spJ -> 1, spJz -> 7/10}, {spS -> Range[4]}][[All, 2]])]];
  testcase["Jordan-Wigner XXZ chain", Max[Abs[N[ev1 - ev2]]] < 10^-10, True];
  testcase["Jordan-Wigner XX chain is free",
    FreeQ[jordanwigner[h /. spJz -> 0, spS -> spf, Range[4]], HoldPattern[nc[_, _, _, _]]], True];
  ops = {{spS[1, 4], spS[2, 5], 0}, {spS[2, 4], spS[2, 5], 2 spS[2, 3]},
         {spS[3, 4], spS[3, 3], -spS[3, 4]}, {spS[1, 1], spS[1, 2], I spS[1, 3]}};
  testcase["Jordan-Wigner preserves the algebra",
    spcommutes[ops, jordanwigner[#, spS -> spf, Range[3]] &], {0, 0, 0, 0}];
  testcase["Jordan-Wigner needs spin 1/2", Quiet[jordanwigner[spT[1, 3], spT -> spf, {1}], jordanwigner::spin], $Failed];
  testcase["Jordan-Wigner needs spinless fermions", Quiet[jordanwigner[spS[1, 3], spS -> spc, {1}], jordanwigner::op], $Failed];

  testcase["Holstein-Primakoff S^z", holsteinprimakoff[spQ[1, 3], spQ -> spbo, 4], sps - nc[spbo[CR, 1], spbo[AN, 1]]];
  testcase["Holstein-Primakoff S^+",
    holsteinprimakoff[spQ[1, 4], spQ -> spbo, 3],
    Expand[Sqrt[2 sps] spbo[AN, 1] + (Sqrt[2 sps - 1] - Sqrt[2 sps]) nc[spbo[CR, 1], spbo[AN, 1], spbo[AN, 1]]]];
  testcase["Holstein-Primakoff exchange",
    holsteinprimakoff[spinspin[spQ[1], spQ[2]], spQ -> spbo, 2],
    Expand[sps^2 + sps (-number[spbo[1]] - number[spbo[2]] + nc[spbo[CR, 1], spbo[AN, 2]] + nc[spbo[CR, 2], spbo[AN, 1]])]];
  testcase["Holstein-Primakoff products are exact",
    Expand[holsteinprimakoff[nc[spT[1, 4], spT[1, 5]] - (2 + spT[1, 3] - nc[spT[1, 3], spT[1, 3]]), spT -> spbo, 8]], 0];
  (* The commutator of images truncated at n boson operators is exact up to
     n - 2 boson operators. *)
  testcase["Holstein-Primakoff preserves the algebra",
    Map[Total[Select[sum2list[#], Count[{#}, spbo[__], Infinity] <= 4 &]] &,
      spcommutes[{{spT[1, 4], spT[1, 5], 2 spT[1, 3]}, {spT[1, 3], spT[1, 4], spT[1, 4]}},
        holsteinprimakoff[#, spT -> spbo, 6] &]], {0, 0}];
  hk = fourier[holsteinprimakoff[-spJ sum[spinspin[spQ[spRR], spQ[spRR + 1]], {spRR}], spQ -> spbo, 2],
    spbo -> spbk, spRR -> spk];
  testcase["Ferromagnetic chain ground-state energy", hk /. sum[__] -> 0, -spJ spN sps^2];
  zerotest["Ferromagnetic magnon dispersion",
    ExpToTrig[blochphaseexp[blochhamiltonian[hk, {spbk}, spk], {spk -> x}]] - {{2 spJ sps (1 - Cos[x])}}];
  h = spJ sum[spinspin[spQ[spRR, "A"], spQ[spRR, "B"]] + spinspin[spQ[spRR, "B"], spQ[spRR + 1, "A"]], {spRR}];
  h = rotatespin[h, spQ, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
  hk = fourier[holsteinprimakoff[h, spQ -> spbo, 2], spbo -> spbk, spRR -> spk];
  hb = First[Cases[hk, HoldPattern[sum[z_, {spk}]] :> z, Infinity]];
  aterm = Coefficient[hb, nc[spbk[CR, spk, "A"], spbk[AN, spk, "A"]]];
  bterm = Coefficient[hb, nc[spbk[CR, -spk, "A"], spbk[CR, spk, "B"]]] +
    (Coefficient[hb, nc[spbk[CR, -spk, "B"], spbk[CR, spk, "A"]]] /. spk -> -spk);
  w = Simplify[ExpToTrig[blochphaseexp[aterm^2 - bterm conj[bterm], {spk -> x}]]];
  testcase["Antiferromagnetic chain magnon dispersion", Simplify[w - (2 spJ sps Sin[x/2])^2], 0];
  testcase["Holstein-Primakoff needs a size", Quiet[holsteinprimakoff[spG[1, 3], spG -> spbo, 2], holsteinprimakoff::size], $Failed];

  ops = {{spQ[1, 4], spQ[1, 5], 2 spQ[1, 3]}, {spQ[1, 3], spQ[1, 4], spQ[1, 4]}, {spQ[1, 1], spQ[1, 2], I spQ[1, 3]}};
  testcase["Schwinger bosons preserve the algebra", spcommutes[ops, schwingerbosons[#, spQ -> {spsa, spsb}] &], {0, 0, 0}];
  testcase["Schwinger-boson S^z", schwingerbosons[spQ[1, 3], spQ -> {spsa, spsb}],
    Expand[(number[spsa[1]] - number[spsb[1]])/2]];
  ops = {{spT[1, 4], spT[1, 5], 2 spT[1, 3]}, {spT[1, 1], spT[1, 2], I spT[1, 3]}};
  testcase["Abrikosov fermions preserve the algebra", spcommutes[ops, abrikosov[#, spT -> spc1] &], {0, 0}];
  testcase["Abrikosov spin 1/2", abrikosov[spS[1, 3], spS -> spc], Expand[spinz[spc[1]]]];
  testcase["Abrikosov needs equal spins", Quiet[abrikosov[spS[1, 3], spS -> spc1], abrikosov::spin], $Failed];
 ]];

testgroup["Spin tensors, Stevens operators, and rotations",
 Module[{x, z, p, m, o, mats, r, rot},
  testcase["Rank-one tensor", {spintensor[1, 1, spG[1]], spintensor[1, 0, spG[1]], spintensor[1, -1, spG[1]]},
    {-spG[1, 4]/Sqrt[2], spG[1, 3], spG[1, 5]/Sqrt[2]}];
  testcase["Rank-two tensor", spintensor[2, 0, spQ[1]],
    Expand[(3 nc[spQ[1, 3], spQ[1, 3]] - sps (sps + 1))/Sqrt[6]]];
  testcase["Tensor operator commutators",
    Expand[Flatten[Table[{komutator[spQ[1, 3], spintensor[3, q, spQ[1]]] - q spintensor[3, q, spQ[1]],
      If[q < 3, komutator[spQ[1, 4], spintensor[3, q, spQ[1]]] -
        Sqrt[12 - q (q + 1)] spintensor[3, q + 1, spQ[1]], 0]}, {q, -3, 3}]]],
    ConstantArray[0, 14]];
  testcase["Site given as a rule", spintensor[1, 0, spG -> 0], spG[0, 3]];
  testcase["Tensor rank check", Quiet[spintensor[1, 2, spG[1]], spintensor::range], $Failed];
  x = sps (sps + 1); z = spQ[1, 3]; p = spQ[1, 4]; m = spQ[1, 5];
  o[k_, q_] := stevens[k, q, spQ[1]];
  testcase["Stevens O20", o[2, 0], Expand[3 pow[z, 2] - x]];
  testcase["Stevens O2+-1", {o[2, 1], o[2, -1]},
    Expand[{nc[z, p + m] + nc[p + m, z], (nc[z, p - m] + nc[p - m, z])/I}/4]];
  testcase["Stevens O2+-2", {o[2, 2], o[2, -2]},
    Expand[{(pow[p, 2] + pow[m, 2])/2, (pow[p, 2] - pow[m, 2])/(2 I)}]];
  testcase["Stevens O22 is Sx^2 - Sy^2", Expand[o[2, 2] - spincomponents[pow[spQ[1, 1], 2] - pow[spQ[1, 2], 2]]], 0];
  testcase["Stevens O40", o[4, 0], Expand[35 pow[z, 4] - 30 x pow[z, 2] + 25 pow[z, 2] - 6 x + 3 x^2]];
  testcase["Stevens O42", o[4, 2],
    Expand[(nc[7 pow[z, 2] - x - 5, pow[p, 2] + pow[m, 2]] + nc[pow[p, 2] + pow[m, 2], 7 pow[z, 2] - x - 5])/4]];
  testcase["Stevens O44", o[4, 4], Expand[(pow[p, 4] + pow[m, 4])/2]];
  testcase["Stevens O60", o[6, 0],
    Expand[231 pow[z, 6] - (315 x - 735) pow[z, 4] + (105 x^2 - 525 x + 294) pow[z, 2] - 5 x^3 + 40 x^2 - 60 x]];
  testcase["Stevens O63", o[6, 3],
    Expand[(nc[11 pow[z, 3] - (3 x + 59) z, pow[p, 3] + pow[m, 3]] + nc[pow[p, 3] + pow[m, 3], 11 pow[z, 3] - (3 x + 59) z])/4]];
  testcase["Stevens operators are Hermitian", Expand[conj[o[4, #]] - o[4, #]] & /@ Range[-4, 4], ConstantArray[0, 9]];
  testcase["Stevens operators vanish for k > 2s", {stevens[4, 0, spS[1]], stevens[6, 3, spR5[1]], stevens[2, 1, spS[1]]}, {0, 0, 0}];
  mats = Table[spinclustermatrices[stevens[4, q, spR5[1]], {spR5[1]}, "Blocks" -> False], {q, -4, 4}];
  testcase["Stevens operators are traceless", Tr /@ mats, ConstantArray[0, 9]];
  testcase["Stevens operators are orthogonal",
    Table[If[q1 == q2, 0, Simplify[Tr[ConjugateTranspose[mats[[q1]]] . mats[[q2]]]]], {q1, 9}, {q2, 9}],
    ConstantArray[0, {9, 9}]];
  testcase["Stevens operators need a size", Quiet[stevens[2, 0, spG[1]], stevens::size], $Failed];

  testcase["Rotation by Pi about x", rotatespin[{spG[1, 3], spG[1, 4]}, spG, RotationMatrix[Pi, {1, 0, 0}]],
    {-spG[1, 3], spG[1, 1] - I spG[1, 2]}];
  testcase["Rotation about z", Simplify[spincomponents[rotatespin[spG[1, 4], spG, RotationMatrix[spphi, {0, 0, 1}]]]],
    ExpToTrig[Exp[I spphi]] spG[1, 4]];
  rot = RotationMatrix[2 Pi/7, {1, 2, 2}/3];
  testcase["Rotations preserve the exchange coupling",
    Simplify[spincomponents[rotatespin[spinspin[spG[1], spG[2]], spG, rot] - spinspin[spG[1], spG[2]]]], 0];
  r = rotatespin[{spG[1, 1], spG[1, 2], spG[1, 3]}, spG, rot];
  testcase["Rotations preserve the commutators",
    Simplify[spincomponents[komutator[r[[1]], r[[2]]] - I r[[3]]]], 0];
  testcase["Site-dependent rotations",
    rotatespin[spG[1, 3] + spG[2, 3], spG, If[# == 2, RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &],
    spG[1, 3] - spG[2, 3]];
  testcase["Rotations must be 3 by 3", Quiet[rotatespin[spG[1, 3], spG, IdentityMatrix[2]], rotatespin::matrix], $Failed];
 ]];

testgroup["Hubbard X-operators",
 Module[{st = {"0", "UP", "DO", "2"}, hop, sdot, nn, h, bz},
  testcase["Same-site products",
    Union[Flatten[Table[Expand[xtofermions[nc[spX[1, a, b], spX[1, c, d]], spX -> spc] -
      nc[xoperator[spc[1], a, b], xoperator[spc[1], c, d]]], {a, st}, {b, st}, {c, st}, {d, st}]]], {0}];
  testcase["Products on different sites",
    Union[Flatten[Table[Expand[xtofermions[nc[spX[2, a, b], spX[1, c, d]], spX -> spc] -
      nc[xoperator[spc[2], a, b], xoperator[spc[1], c, d]]], {a, st}, {b, st}, {c, st}, {d, st}]]], {0}];
  testcase["Products with fermions",
    Union[Flatten[Table[Expand[xtofermions[nc[spd[CR, UP], spX[1, a, b], spd[AN, DO]], spX -> spc] -
      nc[spd[CR, UP], xoperator[spc[1], a, b], spd[AN, DO]]], {a, st}, {b, st}]]], {0}];
  testcase["Completeness", Expand[Total[xoperator[spc[1], #, #] & /@ st]], 1];
  testcase["Hubbard operators in fermions", xoperator[spc[1], "0", "UP"],
    Expand[nc[spc[AN, 1, UP], 1 - number[spc[1], DO]]]];
  testcase["Conjugation", conj[spX[1, "0", "UP"]], spX[1, "UP", "0"]];
  testcase["Spin from X-operators", komutator[spX[1, "UP", "DO"], spX[1, "DO", "UP"]],
    spX[1, "UP", "UP"] - spX[1, "DO", "DO"]];
  testcase["Graded anticommutator for symbolic sites",
    Expand[antikomutator[spX[spi, "0", "UP"], spX[spj, "UP", "0"]]],
    Expand[KroneckerDelta[spi, spj] (spX[spj, "0", "0"] + spX[spj, "UP", "UP"])]];
  testcase["Bosonic X-operators commute", nc[spY[2, 1, 2], spY[1, 3, 3]], nc[spY[1, 3, 3], spY[2, 1, 2]]];
  testcase["SU(3) algebra", komutator[spY[1, 1, 2], spY[1, 2, 3]], spY[1, 1, 3]];
  testcase["Ordering with other operators",
    {nc[spbo[CR], spX[1, "0", "UP"]], nc[spc[CR, 1, UP], spX[1, "0", "UP"]], nc[spc[CR, 1, UP], spX[1, "0", "2"]],
     nc[spX[1, "0", "UP"], spG[3]], nc[spX[1, "0", "UP"], spz]},
    {nc[spX[1, "0", "UP"], spbo[CR]], -nc[spX[1, "0", "UP"], spc[CR, 1, UP]], nc[spX[1, "0", "2"], spc[CR, 1, UP]],
     nc[spG[3], spX[1, "0", "UP"]], -nc[spz, spX[1, "0", "UP"]]}];
  hop = -spt Sum[nc[spX[1, s, "0"], spX[2, "0", s]] + nc[spX[2, s, "0"], spX[1, "0", s]], {s, {"UP", "DO"}}];
  sdot[k_, l_] := (nc[spX[k, "UP", "DO"], spX[l, "DO", "UP"]] + nc[spX[k, "DO", "UP"], spX[l, "UP", "DO"]])/2 +
    nc[(spX[k, "UP", "UP"] - spX[k, "DO", "DO"])/2, (spX[l, "UP", "UP"] - spX[l, "DO", "DO"])/2];
  nn[k_] := spX[k, "UP", "UP"] + spX[k, "DO", "DO"];
  h = xtofermions[hop + spJ (sdot[1, 2] - nc[nn[1], nn[2]]/4), spX -> spc];
  bz = qszbasis[{spc[1], spc[2]}];
  testcase["t-J dimer", Map[{#[[1]], Sort[Eigenvalues[#[[2]] /. {spt -> 1, spJ -> 1/10}]]} &, makematricesbzop[h, bz]],
    {{{-2, 0}, {0}}, {{-1, -1/2}, {-1, 1}}, {{-1, 1/2}, {-1, 1}}, {{0, -1}, {0}}, {{0, 0}, {-1/10, 0, 0, 0}},
     {{0, 1}, {0}}, {{1, -1/2}, {0, 0}}, {{1, 1/2}, {0, 0}}, {{2, 0}, {0}}}];
  testcase["Invalid X-operator declaration",
    Quiet[snegxoperators[spZ, {1, 1}, {0, 1}], snegxoperators::spec], $Failed];
  testcase["Invalid Hubbard-atom state", Quiet[xoperator[spc[1], "UP", "x"], xoperator::state], $Failed];
  testcase["Hubbard-atom states required", Quiet[xtofermions[spY[1, 1, 2], spY -> spc], xtofermions::states], $Failed];
 ]];

snegspinoperators[spHA, spHB];
snegxoperators[spX2, "HubbardAtom"];
spHB /: cmt[spHB[sphook], spHA[sphook]] = spcustom;

(* Matrix of a normal-ordered expression in the boson operator spbo[1] on
   the lowest d Fock states, using ladder matrices with a cutoff of 12 bosons;
   normal ordering makes the low-lying matrix elements exact. *)
spbm = Normal[SparseArray[Table[{n, n + 1} -> Sqrt[n], {n, 12}], {13, 13}]];
spbterm[t_] := Module[{ops = Cases[{t}, spbo[_, 1], Infinity]},
  (t /. {HoldPattern[nc[__]] -> 1, spbo[_, 1] -> 1}) If[ops === {}, IdentityMatrix[13],
    Dot @@ Replace[ops, {spbo[CR, 1] -> Transpose[spbm], spbo[AN, 1] -> spbm}, {1}]]];
spbmat[e_, d_] := (Total[spbterm /@ sum2list[Expand[e]]])[[1 ;; d, 1 ;; d]];

testgroup["Spin and X-operator branches and diagnostics",
 Module[{words, s, x, st = {"0", "UP", "DO", "2"}},
  words = {{3, 5}, {3, 3, 5}, {3, 5, 5}, {5}, {5, 5}, {3, 4}, {3, 3, 4, 4}, {3}, {4, 4, 4}};
  Do[
    s = spinof[h];
    zerotest["Holstein-Primakoff reproduces the spin matrices for spin " <> ToString[s, InputForm],
      Map[spbmat[holsteinprimakoff[nc @@ Map[Function[t, h[1, t]], #], h -> spbo, 12], 2 s + 1] -
        Dot @@ Map[spmat[s, #] &, #] &, words]],
    {h, {spR2, spR3}}];
  testcase["Holstein-Primakoff S^z S^-",
    holsteinprimakoff[nc[spQ[1, 3], spQ[1, 5]], spQ -> spbo, 3],
    Expand[(sps - 1) Sqrt[2 sps] spbo[CR, 1] +
      ((sps - 2) Sqrt[2 sps - 1] - (sps - 1) Sqrt[2 sps]) nc[spbo[CR, 1], spbo[CR, 1], spbo[AN, 1]]]];

  testcase["Different fermionic X-operator symbols anticommute",
    nc[spX2[1, "0", "UP"], spX[1, "0", "UP"]], -nc[spX[1, "0", "UP"], spX2[1, "0", "UP"]]];
  testcase["Different bosonic and fermionic X-operator symbols commute",
    nc[spX2[1, "0", "2"], spX[1, "0", "UP"]], nc[spX[1, "0", "UP"], spX2[1, "0", "2"]]];
  testcase["Different X-operator symbols against fermions",
    Union[Flatten[Table[Expand[xtofermions[xtofermions[nc[spX2[1, a, b], spX[1, c, d]], spX -> spc], spX2 -> spd] -
      nc[xoperator[spd[1], a, b], xoperator[spc[1], c, d]]], {a, st}, {b, st}, {c, st}, {d, st}]]], {0}];
  testcase["Unknown X-operator labels are not reordered",
    nc[spX[2, "0", "q"], spX[1, "0", "UP"]], nc[spX[2, "0", "q"], spX[1, "0", "UP"]]];
  testcase["Unknown X-operator labels have no parity", snegxparity[spX[1, "0", "q"]], None];
  testcase["Site lists of different length", nc[spX[2, 1, "0", "UP"], spX[1, "UP", "0"]],
    -nc[spX[1, "UP", "0"], spX[2, 1, "0", "UP"]]];
  testcase["X-operators and Majoranas",
    {nc[spm[1], spX[1, "0", "UP"]], nc[spm[1], spX[1, "0", "2"]]},
    {-nc[spX[1, "0", "UP"], spm[1]], nc[spX[1, "0", "2"], spm[1]]}];
  testcase["Hubbard atom without site indexes", Expand[xoperator[spc, "0", "UP"] - xoperator[spc[], "0", "UP"]], 0];
  Block[{PrettyOutput = True}, snegxoperators[spXP, "HubbardAtom"]];
  testcase["X-operator formatting",
    MatchQ[ToBoxes[#, StandardForm], TagBox[StyleBox[_, ___], DisplayForm]] & /@ {spXP[1, "0", "UP"], spXP["0", "UP"]},
    {True, True}];

  testcase["Sums over two component indexes",
    spincomponents[sum[levicivita[spa, spb, 3] nc[spG[1, spa], spG[2, spb]], {spa, spb}]],
    spincomponents[nc[spG[1, 1], spG[2, 2]] - nc[spG[1, 2], spG[2, 1]]]];
  testcase["Sums over component and site indexes",
    spincomponents[sum[nc[spG[spi, spa], spG[spi + 1, spa]], {spa, spi}]],
    spincomponents[sum[spinspin[spG[spi], spG[spi + 1]], {spi}]]];
  testcase["Commutator for explicit components", snegspincmt[spG, {1}, 1, 2], I spG[1, 3]];

  testcase["Multi-index sites",
    Sort[Eigenvalues[spinclustermatrices[spinspin[spS[1, "a"], spS[1, "b"]], {spS -> {{1, "a"}, {1, "b"}}}, "Blocks" -> False]]],
    {-3/4, 1/4, 1/4, 1/4}];
  x = spinketbrarules[{spS -> {{1, "a"}, {1, "b"}}}];
  testcase["Multi-index bra-ket rules", {Length[x], First /@ x[[{1, 6}]]}, {10, {spS[1, "a", 1], spS[1, "b", 1]}}];
  testcase["Multi-index Jordan-Wigner sites",
    jordanwigner[nc[spS[1, "a", 4], spS[1, "b", 5]], spS -> spf, {{1, "a"}, {1, "b"}}],
    nc[spf[CR, 1, "a", 0], spf[AN, 1, "b", 0]]];
  testcase["Delayed site rules", Length[spinketbrarules[{spS :> {1, 2}}]], 10];

  testcase["Spin symbols swap with their commutator", nc[spHB[sphook], spHA[sphook]],
    nc[spHA[sphook], spHB[sphook]] + spcustom];

  testcase["Cluster matrices reject other operators",
    Quiet[spinclustermatrices[spc[CR, 1, UP], {spS[1]}], spinclustermatrices::op], $Failed];
  testcase["Cluster matrices check the sites",
    Quiet[spinclustermatrices[spS[1, 3], {spc[1]}], snegspinsites::spec], $Failed];
  testcase["Holstein-Primakoff needs bosons",
    Quiet[holsteinprimakoff[spQ[1, 3], spQ -> spc, 2], holsteinprimakoff::op], $Failed];
  testcase["Schwinger bosons need bosons",
    Quiet[schwingerbosons[spQ[1, 3], spQ -> {spc, spbo}], schwingerbosons::op], $Failed];
  testcase["Bra-ket rules need a size",
    Quiet[spinketbrarules[{spG[1]}], snegspinsites::size], $Failed];
  testcase["Rank-zero Stevens operator", stevens[0, 0, spS[1]], 1];
  testcase["Stevens rank check", Quiet[stevens[1, 2, spS[1]], spintensor::range], $Failed];
  testcase["Spherical tensors check the site", Quiet[spintensor[1, 0, spc[1]], snegspinsites::spec], $Failed];
  testcase["Non-half-integer size",
    Quiet[Check[snegspinoperators[{spBad1, 1/3}], "msg", snegspinoperators::size], snegspinoperators::size], "msg"];
  testcase["Size without symbols",
    Quiet[Check[snegspinoperators[{spBad2, "x"}], "msg", snegspinoperators::size], snegspinoperators::size], "msg"];
  testcase["Invalid sizes declare nothing", TrueQ[spinQ[#]] & /@ {spBad1, spBad2}, {False, False}];
 ]];

(* The examples of README_spin.md *)
snegspinoperators[{spF, 5/2}];
testgroup["Spin documentation examples",
 Module[{h, hk, bz},
  testcase["README Pauli products", {nc[spS[1, 1], spS[1, 2]], nc[spS[1, 4], spS[1, 5]], spinspin[spS[1], spS[1]]},
    {I/2 spS[1, 3], 1/2 + spS[1, 3], 3/4}];
  testcase["README spin 1", {nc[spT[1, 4], spT[1, 4], spT[1, 4]], nc[spT[1, 3], spT[1, 3], spT[1, 3]]}, {0, spT[1, 3]}];
  testcase["README symbolic size", nc[spQ[1, 5], spQ[1, 4]], sps (1 + sps) - spQ[1, 3] - nc[spQ[1, 3], spQ[1, 3]]];
  h = spJ Sum[spinspin[spS[i], spS[Mod[i, 4] + 1]], {i, 4}];
  testcase["README Heisenberg ring", {First[#], Min[Eigenvalues[Last[#] /. spJ -> 1]]} & /@ spinclustermatrices[h, {spS -> Range[4]}],
    {{{-2}, 1}, {{-1}, -1}, {{0}, -2}, {{1}, -1}, {{2}, 1}}];
  bz = QSZaddspinket[qszbasisvc[{spc[]}], {1/2}];
  testcase["README Kondo spectrum",
    {First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[Expand[spkondo[spS] /. spinketbrarules[{spS[]}]], bz],
    {{{-1, -1/2}, {0}}, {{-1, 1/2}, {0}}, {{0, -1}, {spJ/4}}, {{0, 0}, {-3 spJ/4, spJ/4}}, {{0, 1}, {spJ/4}},
     {{1, -1/2}, {0}}, {{1, 1/2}, {0}}}];
  hk = fourier[holsteinprimakoff[-spJ sum[spinspin[spQ[spRR], spQ[spRR + 1]], {spRR}], spQ -> spbo, 2], spbo -> spbk, spRR -> spk];
  testcase["README magnons",
    hk, -spJ spN sps^2 + sum[Expand[(2 spJ sps - spJ sps blochphase[-spk, 1] - spJ sps blochphase[spk, 1]) number[spbk[spk]]], {spk}]];
  testcase["README Stevens operators", {stevens[2, 0, spT[1]], stevens[4, 4, spQ[1]]},
    {-2 + 3 nc[spT[1, 3], spT[1, 3]], Expand[(pow[spQ[1, 4], 4] + pow[spQ[1, 5], 4])/2]}];
  testcase["README axial anisotropy",
    Sort[Eigenvalues[spinclustermatrices[stevens[2, 0, spF[1]], {spF[1]}, "Blocks" -> False]]], {-8, -8, -2, -2, 10, 10}];
  testcase["README X-operators", {nc[spX[1, "0", "UP"], spX[1, "UP", "2"]], komutator[spX[1, "UP", "DO"], spX[1, "DO", "UP"]]},
    {spX[1, "0", "2"], spX[1, "UP", "UP"] - spX[1, "DO", "DO"]}];
 ]];

]; (* isolated package state *)
testcase["Regression groups restore ordering", DownValues[ordering], savedRegressionOrdering];

End[];

Print["*** DONE. ***"];
Print["Passed: ", testpassed, " Failed: ", testfailed];
If[testfailed == 0,
  Print["### PASSED! ###"];
];


Exit[If[testfailed == 0, 0, 1]];

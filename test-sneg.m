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

Check[reslt=Get["sneg.m"], 
  Print["Messages detected. Aborting."];
  Exit[];
];
If[reslt == $Failed,
  Print["Loading failed. Aborting."];
  Exit[];
];

Print["test-sneg.m $Id: test-sneg.m,v 1.9 2007/12/03 15:46:57 rok Exp rok $"];

SetAttributes[test, HoldAll];

(* 'a' and 'b' are two expressions that will be tested for equality after
being explicitly evaluated. 'workingversion' is the last version where the
test succeeded. *)

testpassed = 0;
testfailed = 0;

(* If negate == True, we test for inequality! *)

test[a_, b_, workingversion_:0, negate_:False] := Module[{aa, bb, failed},
  aa = ReleaseHold[a];
  bb = ReleaseHold[b];
  If[negate == False,
    failed = (aa =!= bb),
    (* else *)
    failed = (aa === bb)
  ];
  If[failed,
    Print["test[] failed."];
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

(* Trap missing right sides in the tests *)
test[a_] := Print["sneg-test error. Missing right side. Left=", a];

(* Self-test: test if test[] really tests! *)
If[Block[{Print = Function[Null]}, test[ "self", "test" ]] === False,
  Print["*** Self-test OK. ***"];
  testfailed--,
  (* else *)
  Print["*** Self-test FAILED. ***"];
  Exit[];
];

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
test[ cmt[g[1, 2], g[1, 3]], cmt[g[1, 2], g[1, 3]] ];

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

(* TEMPORARILY COMMENTED OUT
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
*)

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

(* 
test[ vev[nc[c[AN, k, alpha, sigma], c[CR, k, alpha, sigma]]],
UnitStep[k] ];
*)
test[ vev[nc[c[AN, k, alpha, sigma], c[CR, k, alpha, sigma]]],
1-UnitStep[-k] ];

test[ vev[nc[c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma]]],
  KroneckerDelta[k1, k2]*UnitStep[-k1] ];
test[ vev[nc[c[AN, k, 1, UP], c[CR, k, 1, DO], c[AN, k1, 1, UP], 
  c[CR, k1, 1, UP]]], 0 ];

(*
  test[ vev[nc[c[AN, k, 1, UP], c[CR, k, 1, UP], c[AN, k1, 1, UP],
  c[CR, k1, 1, UP]]], UnitStep[k]*UnitStep[k1] ];
*)

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
      contractone[{1, 1, {aa, bb, cc, dd}}, {1, 3}] ]; (* test test[] *)

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

test[ contractone[{1, 1, {aa, bb, cc, dd}}, {1, 3}],
      contractone[{1, 1, {aa, bb, cc, dd}}, {1, 3}] ];

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

(* XXX test[ contract[{c[CR, k1, alpha, sigma], c[AN, k2, alpha, sigma], 
    c[CR, k3, alpha, sigma], c[AN, k4, alpha, sigma]}, {{1, 4}, {3, 2}}],
  KroneckerDelta[k1, k4]*KroneckerDelta[k2, k3]*UnitStep[-k1]*UnitStep[-k3] ]; *)

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

(*
test[ wickorder[2, nc[c[0, k, 1, 0], c[1, k, 1, 1], c[1, k1, 1, 1], 
  c[0, k1, 1, 1]] ],  0 ];
*)
  
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

Print["*** DONE. ***"];
Print["Passed: ", testpassed, " Failed: ", testfailed];
If[testfailed == 0,
  Print["### PASSED! ###"];
];


Exit[];

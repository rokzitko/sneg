(*
   Diagonalize - routines for exact diagonalization in a given basis

   Part of package SNEG, examples.
   
   Copyright (C) 2007 Rok Zitko

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

   Contact information:
   Rok Zitko
   F5 - Condensed matter physics
   "Jozef Stefan" Institute
   Jamova 39
   SI-1000 Ljubljana
   Slovenia

   rok.zitko@ijs.si

   $Id: diagonalize.m,v 1.1 2007/02/23 00:15:53 rok Exp rok $
*)
     
(* Setup a basis with well defined (Q,Sz) quantum numbers. The operators
   need to be defined in global variable "basisops". *)

diagSetupBasisQSZ[] := Module[{},
  makebasis[basisops];

  (* Basis states with (Q, Sz) quantum numbers *)
  bz = qszbasis[basisops];
  Scan[(baza[ #[[1]] ] = #[[2]]) &, bz];
  Print[bz // MatrixForm];

  (* Basis states in the occupation number representation *)
  vak = vacuum[];
  bvc = Map[{#[[1]], ap[#[[2]], vak]} &, bz];
  Scan[(bazavc[ #[[1]] ] = #[[2]]) &, bvc];
  Print[bvc // MatrixForm];
];

diagSetupBasisQS[] := Module[{},
  makebasis[basisops];

  (* Basis states with (Q, S) quantum numbers *)
  bz = qsbasis[basisops];
  Scan[(baza[ #[[1]] ] = #[[2]]) &, bz];
  Print[bz // MatrixForm];

  (* Basis states in the occupation number representation *)
  vak = vacuum[];
  bvc = Map[{#[[1]], ap[#[[2]], vak]} &, bz];
  Scan[(bazavc[ #[[1]] ] = #[[2]]) &, bvc];
  Print[bvc // MatrixForm];
];

(* Generate Hamiltonian matrix in each subspace. The Hamiltonian *)

diagSetupHam[] := Module[{},
  Hmat = makematricesbzvc[H, bvc];
  Print[MatrixForm @ Map[{#[[1]], MatrixForm @ #[[2]]} &, Hmat]];
];

(* Perform the diagonalization. Params is a list of replacement
rules that define all the model parameters. *)

diagonalize[params_] := Module[{Hmat1},
   (* Set up numerical Hamiltonian matrices *)     
   Hmat1 = Hmat /. params;
      
   (* Diagonalize them *)
   eig = {#[[1]], Eigensystem @ #[[2]]} & /@ Hmat1;
      
   (* Normalize eigenvectors *)
   eig[[All, 2, 2]] = Chop[ 
    eig[[All, 2, 2]] / Map[Norm, eig[[All, 2, 2]], {2}] 
   ];
      
   (* Occupation number representation *)
   eig2 = eig /. {{q_, sz_}, {val_, vec_}} :> 
      {{q, sz}, {val, vec . bazavc[{q, sz}] // Simplify}};
];

(* Sum over all invariant subspaces I defined in list eig.  List eig
   must be structured as a list of {{I (quantum numbers)},
   {{eigenvalues}, {eigenstates}}}.  The degeneracy of each subspace
   is taken into account by calling deg[I]. Pure function op operates
   on {eigval, eigvec} lists of each subspace. *)

sum1[eig_List, op_] := Module[{},
   Total @ Map[deg[#[[1]]] op[#[[2]]] & , eig]
];

(* Sum over all states : sum_n <n| expr |n>. expr is an operator
   expression.  sum1[] is called to iterate over all invariant
   subspaces and to take into account the degeneracies of states in
   each subspace. *)

sum1expr[eig_List, expr_] := Module[{},
  sum1[eig, 
  Function[valvec, 
    Total @ Map[expvvc[expr, #]&, valvec[[2]]] ]
  ]
];

(* Thermodynamic trace Tr[e^(-beta H) expr] = 
   sum_n exp(-beta E_n) < n | expr | n > *)

tdtr1expr[eig_List, expr_] := Module[{},
  sum1[eig,
    Function[valvec, 
      Exp[-beta valvec[[1]]] . 
        Map[expvvc[expr, #]&, valvec[[2]]]
    ]
  ]
];

(* Sum over all pairs of subspaces I1, I2. Degeneracies are taken into
  account, but this calculation is meaningful only for singlet
  operators (and also for the special case when deg[] always returns
  1).  op operates on {{val1, vec1}, {val2, vec2}} list of each pair
  of subspaces. *)

sum2[eig_List, op_] := Module[{xx12, mat},
  xx12[{qs1_, x1_}, {qs2_, x2_}] := deg[qs1] deg[qs2] op[x1, x2];
  mat = Outer[xx12, eig, eig, 1]; (* matrix of all <m| op |n> *)
  Total[mat, 2] (* Sum all elements down to level 2. *)
];

(* As above, but for different lists. *)
sum2[eig1_List, eig2_List, op_] := Module[{xx12, mat},
  xx12[{qs1_, x1_}, {qs2_, x2_}] := deg[qs1] deg[qs2] op[x1, x2];
  mat = Outer[xx12, eig1, eig2, 1]; (* matrix of all <m| op |n> *)
  Total[mat, 2] (* Sum all elements down to level 2. *)
];

(* As above, but also perform a check if a contribution is possible. *)
sum2[eig1_List, eig2_List, op_, check_] := Module[{xx12, mat},
  xx12[{qs1_, x1_}, {qs2_, x2_}] := deg[qs1] deg[qs2] op[x1, x2];
  mat = Outer[
    If[check[#1[[1]], #2[[1]]], xx12[#1,#2], 0]&, 
    eig1, eig2, 1]; (* matrix of all <m| op |n> *)
  Total[mat, 2] (* Sum all elements down to level 2. *)
];

(* Sum over all pairs of states : sum_mn <m| expr |n>. expr is an
   operator expression. *)

sum2expr[eig_List, expr_] := Module[{},
  sum2[eig, 
    Total[Outer[braket[#1, expr, #2]&, #1[[2]], #2[[2]]], 2] &
  ]
];

(* Thermodynamic trace : Tr[e^(-beta H) expr] = sum_mn exp(-beta E_m)
    <m| expr |n>. NOTE: this is redundant; use tdtr1expr[] instead. *)

tdtr2expr[eig_List, expr_] := Module[{},
  sum2[eig, 
    Total[Exp[-beta #1[[1]]] *
      Outer[braket[#1, expr, #2]&, #1[[2]], #2[[2]]], 2] &
  ]
];

(* Thermodynamic trace : Tr[e^(-beta H) expr] = sum_mn exp(-beta E_m)
    <m| expr |n>. NOTE: also redundant; use tdtr1expr[] instead. *)

tdtr2exprALT[eig_List, expr_] := Module[{},
  sum2[eig, 
    Total[ (* Element by element product!! *)
      Outer[Exp[-beta #1] &, #1[[1]], #2[[1]]] *
      Outer[braket[#1, expr, #2]&, #1[[2]], #2[[2]]],
   2] &]
];

(* Spectral function calculation: Tr[e^(-beta H) expr] = 
    sum_mn exp[-beta (E_m - E_n)] |<m| expr |n>|^2 delta(E_m - E_n). *)

spectral2expr[eig_List, expr_] := Module[{expreig},
  s2eapply[l_List] := Map[Simplify @ ap[expr, #]&, l];

  expreig = Map[{#[[1]], {#[[2,1]], s2eapply @ #[[2,2]]}}&, eig];

  checkspectral[{q1_, sz1_}, {q2_, sz2_}] := 
    ( (q2+1) == q1 ) && ( (sz2+1/2) == sz1 );

  sum2[eig, expreig, (* We use the 3rd version of sum2[] function. *)
    Total[ (* Element by element product! *)
      Outer[(Exp[-beta #1] + Exp[-beta #2]) *
        Delta[omega + (#1 - #2)]&, #1[[1]], #2[[1]]] *
      Outer[scalarproductvc[#1, #2]^2 &, #1[[2]], #2[[2]]],
   2] &, checkspectral] // Chop
];


(* Calculate expectation values and spectral functions. 
  eig2 must contain the solution of the diagonalization
  problem, as provided by a call to diagonalize[] function. *)

calculate[expvlist_, speclist_] := Module[{},
  Z = tdtr1expr[eig2, 1]; (* partition function *)
      
  expvs = Map[tdtr1expr[eig2, #] &, expvlist] / Z;
  specs = Map[spectral2expr[eig2, #] &, speclist] / Z;
];

(* Thermodynamic trace of singlet operators.  mat is a list of
   operator matrices as obtained, for example, using
   makematricesbzvc[] function. *)

tdtr1FAST::unordered = "eig and mat are not in the same order: `1` and `2`.";

tdtr1FAST[eig_List, mat_] := Module[{ord1, ord2, do1},
  (* Make sure both lists are in Zthe same order *)
  ord1 = Ordering @ eig[[All,1]];
  ord2 = Ordering @ mat[[All,1]];
  If[ord1 != ord2,
    Message[tdtr1FAST::unordered, ord1, ord2]; Return[Infinity]
  ];

  do1[{vals_, vecs_}, matrix_] := 
        Tr[vecs.matrix.Transpose[vecs], List] . Exp[-beta vals];
      
  Total @ MapThread[do1, {eig[[All, 2]], mat[[All, 2]]}]
];

(* Prepare matrices of operators for fast calculations. *)

calculateSetup[expvlist_, speclist_] := Module[{},
  Zmatrix = makematricesbzvc[1, bvc];
  CSexpvlist = Map[makematricesbzvc[#, bvc]&, expvlist];
  CSspeclist = Map[makeallmatricesbzvc[#, bvc]&, speclist];
];

(* Spectral function calculation. eig must contain eigenvector
   matrices.  lmat must be contructed using suitable
   makeallmatricesbzvc[] function. *)

spectral2FAST[eig_List, lmat_] := Module[{},
  qnlist = eig[[All, 1]];
  do1[l_] := 
    Module[{qn1, qn2, mat, pos1, pos2, vals1, vecs1, vals2, vecs2, 
            factors, skpdts},
      {{qn1, qn2}, mat} = l;
      pos1 = Position[qnlist, qn1][[1, 1]];
      pos2 = Position[qnlist, qn2][[1, 1]];
      {vals1, vecs1} = eig[[pos1, 2]];
      {vals2, vecs2} = eig[[pos2, 2]];
         
      factors = 
        Outer[(Exp[-beta #1] + Exp[-beta #2])Delta[omega + (#1 - #2)] &, 
              vals1, vals2];
      skpdts = Abs[vecs1.mat.Transpose[vecs2]]^2;
      Chop @ Total[factors skpdts, 2]
    ];
  Expand @ Total @ Map[do1, lmat]
];

(* calculateFAST[] does everything in the same way as calculate[],
   only much faster. calculateSetup[] must be called in advance. *)

calculateFAST[expvlist_, speclist_] := Module[{},
  Z = tdtr1FAST[eig, Zmatrix]; (* partition function *)
      
  expvs = Map[tdtr1FAST[eig, #] &, CSexpvlist] / Z;
  specs = Map[spectral2FAST[eig, #] &, CSspeclist] / Z;
];

(* Fourth-order Schrieffer-Wolff transformation of the half-filled Hubbard
   model on a four-site ring (2x2 plaquette): the spectrum of the effective
   spin Hamiltonian is compared with exact diagonalization. The errors of the
   second- and fourth-order effective Hamiltonians scale as t^4/U^3 and
   t^6/U^5. Run with: wolframscript -file schriefferwolff-plaquette.m
   (about 5 minutes). *)

Get[FileNameJoin[{DirectoryName[$InputFileName], "..", "sneg.m"}]];

snegfermionoperators[c];
snegrealconstants[t, U];

L = 4;
H0 = U Sum[nc[number[c[i], UP], number[c[i], DO]], {i, 0, L - 1}];
V = -t Sum[hop[c[i], c[Mod[i + 1, L]]], {i, 0, L - 1}];
P = nc @@ Table[projector1[c[i]], {i, 0, L - 1}];

{time, heff} = AbsoluteTiming[schriefferwolff[H0, V, P, 4, "Simplify" -> Together]];
Print["Schrieffer-Wolff transformation to fourth order: ", time, " s"];

(* Matrices in the Fock space of the 8 modes (Jordan-Wigner) *)
modes = Flatten[Table[{i, s}, {i, 0, L - 1}, {s, {UP, DO}}], 1];
nm = Length[modes];
fc[k_] := fc[k] = SparseArray[KroneckerProduct @@ Table[
  Which[j < k, {{1, 0}, {0, -1}}, j == k, {{0, 0}, {1, 0}}, True, IdentityMatrix[2]], {j, nm}]];
om[o_] := With[{m = fc[Position[modes, {o[[2]], o[[3]]}][[1, 1]]]}, If[o[[1]] === CR, m, Transpose[m]]];
mat[e_] := Normal[Total[Map[Function[term, Module[{l = Sneg`Private`snegtermops[term]},
  l[[1]] If[l[[2]] === {}, IdentityMatrix[2^nm, SparseArray], Dot @@ Map[om, l[[2]]]]]],
  sum2list[Expand[e]]]]];

pind = Flatten[Position[Diagonal[mat[P]], 1]];
nsec = Flatten[Position[Diagonal[mat[Sum[number[c[i]], {i, 0, L - 1}]]], L]];
hsym = Map[mat[#][[pind, pind]] &, heff];
hfull = mat[H0 + V][[nsec, nsec]];

Print["Odd orders vanish: ", hsym[[2]] === hsym[[4]] === ConstantArray[0, {16, 16}]];
Do[Module[{vals = {t -> 1, U -> u}, ex, e2, e4},
  ex = Sort[Eigenvalues[N[hfull /. vals]]][[1 ;; Length[pind]]];
  e2 = Sort[Eigenvalues[N[Total[hsym[[1 ;; 3]]] /. vals]]];
  e4 = Sort[Eigenvalues[N[Total[hsym] /. vals]]];
  Print["U/t = ", u, ": largest error of the 2nd-order spectrum ", Max[Abs[e2 - ex]],
    ", 4th-order spectrum ", Max[Abs[e4 - ex]]]],
  {u, {20, 40, 80}}];

---
keywords: matrix elements, transition matrix elements, invariant subspaces, Lehmann representation, spectral function, Green's function, creation operator, selection rules, occupation number representation
see also: makeallmatricesbzop, makematricesbzvc, matrixrepresentationvc, qszbasisvc, qsbasisvc, lehmann, keyselect
---
# makeallmatricesbzvc

## Usage
- `makeallmatricesbzvc[op, bz]` gives the list `{{{qn1, qn2}, mat}, ...}` of the nonzero blocks `mat` of the operator *op* between the invariant subspaces `qn1` and `qn2` of the basis *bz*, with the basis states in the occupation number representation.

## Details
- *bz* is a basis `{{qn, states}, ...}` as described in `makematricesbzvc`.
- For every ordered pair of subspaces, the block is $M_{ij} = \langle i | \hat O | j \rangle$ with $|i\rangle$ in the subspace `qn1` (bra, final state) and $|j\rangle$ in the subspace `qn2` (ket, initial state). The block has the dimensions `Length[states1]` × `Length[states2]`.
- Blocks with all elements zero are omitted; the remaining pairs are the transitions allowed by the selection rules of *op*. Blocks whose elements cannot be decided to vanish, e.g. with symbolic coefficients, are kept. The pairs are ordered as in `Outer[List, bz, bz, 1]`.
- Unlike `makematricesbzvc`, the blocks are not simplified.
- `makeallmatricesbzvc` is needed for operators that change the quantum numbers, such as creation and annihilation operators in the Lehmann representation of Green's functions, $A(\omega) = \sum_{n} |\langle n | c^\dagger | 0 \rangle|^2 \delta(\omega - E_n + E_0) + \sum_n |\langle n | c | 0 \rangle|^2 \delta(\omega + E_n - E_0)$.
- In a $(Q, S)$ basis, only the states with $S_z = S$ are present, and the blocks contain only the matrix elements between these states. For non-scalar operators, the full matrix elements follow from the Wigner-Eckart theorem; otherwise, use a $(Q, S_z)$ basis.
- The number of pairs grows quadratically with the number of subspaces; every pair is computed.

## Developer Notes
- For each pair, `matrixrepresentationvc[op, states1, states2]` is evaluated; the test for a vanishing block is `Total[Abs[mat], 2] == 0`.
- The internal helper `mambpair` processes one pair; `makeallmatricesbzop` uses the same helper with `matrixrepresentationop`.

## Examples
### Basic Examples
Blocks of the creation operator $c^\dagger_\uparrow$ of a single level in the $(Q, S_z)$ basis; $c^\dagger_\uparrow$ raises $Q$ by 1 and $S_z$ by 1/2:
```wl
snegfermionoperators[d];
bz = qszbasisvc[{d[]}];
makeallmatricesbzvc[d[CR, UP], bz]
```

### Scope
For an operator that conserves the quantum numbers, only diagonal pairs remain, and the blocks agree with `makematricesbzvc`:
```wl
snegfermionoperators[c];
snegrealconstants[t];
bz = qszbasisvc[{c[1], c[2]}];
Map[{First[First[#]], Last[#]} &, makeallmatricesbzvc[-t hop[c[1], c[2]], bz]] == DeleteCases[makematricesbzvc[-t hop[c[1], c[2]], bz], {_, {{0}}}]
```
---
The spin-raising operator $S^+_1$ connects subspaces with $\Delta S_z = 1$ at fixed $Q$:
```wl
First /@ makeallmatricesbzvc[spinplus[c[1]], bz]
```

### Applications
Lehmann representation of the local spectral function $A_{11}(\omega)$ of the half-filled Hubbard dimer at $T = 0$, for $t = 1$ and $U = 4$. The interaction is written in the particle-hole symmetric form $\frac{U}{2} \sum_i (n_i - 1)^2$, which includes the chemical potential $\mu = U/2$. Eigenvalues and eigenvectors by subspace:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
H = -hop[c[1], c[2]] + 2 Total[isozsq[{c[1], c[2]}]];
eig = Association[Map[First[#] -> Eigensystem[N[Last[#]]] &, makematricesbzvc[H, bz]]];
{vals, vecs} = eig[{0, 0}];
E0 = Min[vals]; gs = vecs[[First[Ordering[vals]]]];
E0
```
Matrix elements of $c^\dagger_{1\uparrow}$ and $c_{1\uparrow}$ between the subspaces:
```wl
cdag = Association[Rule @@@ makeallmatricesbzvc[c[CR, 1, UP], bz]];
cann = Association[Rule @@@ makeallmatricesbzvc[c[AN, 1, UP], bz]];
Keys[cdag]
```
Poles and weights of the electron-addition part, $\omega = E_n - E_0$ with weights $|\langle n | c^\dagger_{1\uparrow} | 0 \rangle|^2$ for the states $|n\rangle$ in the subspace $(Q, S_z) = (1, 1/2)$:
```wl
add = Transpose[{eig[{1, 1/2}][[1]] - E0, Abs[eig[{1, 1/2}][[2]] . cdag[{{1, 1/2}, {0, 0}}] . gs]^2}]
```
Electron-removal part, $\omega = E_0 - E_n$ with the states in $(-1, -1/2)$:
```wl
rem = Transpose[{E0 - eig[{-1, -1/2}][[1]], Abs[eig[{-1, -1/2}][[2]] . cann[{{-1, -1/2}, {0, 0}}] . gs]^2}]
```
The weights add up to $\langle \{c_{1\uparrow}, c^\dagger_{1\uparrow}\} \rangle = 1$, and the spectrum is particle-hole symmetric, with poles at $|\omega| = \sqrt{U^2/4 + 4t^2} \mp t$:
```wl
{Total[Join[add, rem][[All, 2]]], Sort[add[[All, 1]]] == Sort[-rem[[All, 1]]], N[Sqrt[4 + 4] + {-1, 1}]}
```

### Properties & Relations
The blocks of the Hermitian conjugate operator are the conjugate transposes of the blocks with the subspaces exchanged:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
a = makeallmatricesbzvc[c[CR, 1, DO], bz];
b = makeallmatricesbzvc[c[AN, 1, DO], bz];
Sort[{Reverse[First[#]], ConjugateTranspose[Last[#]]} & /@ a] == Sort[b]
```

### Possible Issues
In a $(Q, S)$ basis, only the states with $S_z = S$ are included. The operator $c^\dagger_{1\downarrow}$ lowers $S_z$ by 1/2. Acting on a doublet with $S_z = 1/2$, it gives states with $S_z = 0$, which are only represented for $S = 0$; the transitions to the triplet and from the vacuum to the doublet are missing:
```wl
snegfermionoperators[c];
First /@ makeallmatricesbzvc[c[CR, 1, DO], qsbasisvc[{c[1], c[2]}]]
```
In the $(Q, S_z)$ basis, all transitions are present:
```wl
First /@ makeallmatricesbzvc[c[CR, 1, DO], qszbasisvc[{c[1], c[2]}]]
```

---
keywords: matrix representation, matrix elements, occupation number representation, Fock space, exact diagonalization, Jordan-Wigner signs, bra-ket
see also: matrixrepresentationvcfast, matrixrepresentationvcsparse, matrixrepresentationop, makematricesbzvc, makeallmatricesbzvc, braketvc, scalarproductvc, ap, vc, makebasis, nonebasisvc
---
# matrixrepresentationvc

## Usage
- `matrixrepresentationvc[op, {v1, v2, ...}]` gives the matrix $M_{ij} = \langle v_i | \hat O | v_j \rangle$ of the operator *op* between the states $v_i$ in the occupation number representation.
- `matrixrepresentationvc[op, {u1, u2, ...}, {v1, v2, ...}]` gives the rectangular matrix $M_{ij} = \langle u_i | \hat O | v_j \rangle$.

## Details
- The states are linear combinations of occupation number vectors `vc[n1, n2, ...]`, where $n_k$ is the occupation of the k-th single-particle state of the global list `BASIS` (set by `makebasis` or by the basis generators such as `qszbasisvc`). The vector `vc[n1, n2, ...]` is $(c^\dagger_1)^{n_1} (c^\dagger_2)^{n_2} \cdots |0\rangle$, with the creation operators in the order of `BASIS`; the fermionic signs follow from this order.
- The operator is applied with `ap`, and the matrix elements are the scalar products `scalarproductvc[ui, ap[op, vj]]`, which are antilinear in the first argument. The result is `Outer[scalarproductvc, bras, ap[op, kets]]`.
- The states do not need to be normalized or orthogonal. The result is then the matrix of matrix elements, not the matrix of the operator in a non-orthonormal basis; for the identity operator, it is the Gram matrix $\langle v_i | v_j \rangle$.
- The rows correspond to the bras and the columns to the kets. With `matrixrepresentationvc[op, states]`, the matrix applied to coefficient vectors gives the action of $\hat O$ in the basis: if $|\psi\rangle = \sum_j a_j |v_j\rangle$ and the basis is orthonormal, $\hat O |\psi\rangle = \sum_i (M a)_i |v_i\rangle$.
- The states may end with kets of auxiliary degrees of freedom (`ket[...]`, e.g. phonons or spins); their overlaps are given by `braketrule`.
- The result is not simplified. Operators of levels that are not in `BASIS` are not applied, and the result stays unevaluated.
- `matrixrepresentationvcfast` and `matrixrepresentationvcsparse` give the same matrices faster for bases of single occupation number vectors.

## Developer Notes
- `matrixrepresentationvc` evaluates $N_\mathrm{bra} \times N_\mathrm{ket}$ scalar products. `matrixrepresentationvcfast` instead collects the coefficients of `ap[op, v]` directly, which avoids the quadratic number of scalar products; NRG Ljubljana uses it for the initial operator matrices.

## Examples
### Basic Examples
Matrix of the hopping operator $c^\dagger_{1\uparrow} c_{2\uparrow} + \mathrm{h.c.}$ between the one-electron states with spin up:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
BASIS
```
```wl
states = {vc[1, 0, 0, 0], vc[0, 0, 1, 0]};
matrixrepresentationvc[nc[c[CR, 1, UP], c[AN, 2, UP]] + nc[c[CR, 2, UP], c[AN, 1, UP]], states]
```
Rectangular matrix of $c^\dagger_{1\downarrow}$ between the two-electron states with $S_z = 0$ (bras) and the one-electron states with spin up (kets):
```wl
twoel = {vc[1, 1, 0, 0], vc[0, 1, 1, 0], vc[1, 0, 0, 1], vc[0, 0, 1, 1]};
matrixrepresentationvc[c[CR, 1, DO], twoel, states]
```

### Scope
Spin operators of a single level in the basis $|{\uparrow}\rangle, |{\downarrow}\rangle$ are the Pauli matrices divided by 2:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
matrixrepresentationvc[#, {vc[1, 0], vc[0, 1]}] & /@ spinxyz[d[]]
```
---
Superpositions as basis states; the bonding and antibonding orbitals of the dimer diagonalize the hopping:
```wl
snegfermionoperators[c];
snegrealconstants[t];
makebasis[{c[1], c[2]}];
matrixrepresentationvc[-t hop[c[1], c[2]], {(vc[1, 0, 0, 0] + vc[0, 0, 1, 0])/Sqrt[2], (vc[1, 0, 0, 0] - vc[0, 0, 1, 0])/Sqrt[2]}]
```
---
States with kets for a phonon mode; the displacement $b + b^\dagger$ with at most two phonons:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
matrixrepresentationvc[phononx[2], {vc[1, 0, ket[0]], vc[1, 0, ket[1]], vc[1, 0, ket[2]]}]
```

### Applications
Fock-space matrices of the creation operators of two spin-1/2 levels (16 × 16, Jordan-Wigner representation). They obey the canonical anticommutation relations:
```wl
snegfermionoperators[c];
fock = nonebasisvc[{c[1], c[2]}][[1, 2]];
cr = matrixrepresentationvc[#, fock] & /@ BASIS;
an = ConjugateTranspose /@ cr;
{Union[Flatten[Table[cr[[i]] . cr[[j]] + cr[[j]] . cr[[i]], {i, 4}, {j, 4}]]], Union[Flatten[Table[an[[i]] . cr[[j]] + cr[[j]] . an[[i]] - KroneckerDelta[i, j] IdentityMatrix[16], {i, 4}, {j, 4}]]]}
```
The Hubbard dimer in the full Fock space; the spectrum agrees with the subspace calculation in `makematricesbzvc`:
```wl
snegrealconstants[t, U];
Sort[N[Eigenvalues[matrixrepresentationvc[-t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]], fock] /. {t -> 1, U -> 4}]]]
```
---
Matrix elements between eigenstates: the total spin $\mathbf S^2$ of the eigenstates of the half-filled Hubbard dimer with $S_z = 0$, for $t = 1$ and $U = 4$:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
sec = keyselect[bz, {0, 0}][[1, 2]];
h = matrixrepresentationvc[-hop[c[1], c[2]] + 4 Total[hubbard[{c[1], c[2]}]], sec];
{vals, vecs} = Eigensystem[h];
eigenstates = vecs . sec;
s = spinxyz[c[1]] + spinxyz[c[2]];
Transpose[{vals, Diagonal[Simplify[matrixrepresentationvc[Expand[inner[s, s]], Simplify[eigenstates/(Norm /@ vecs)]]]]}]
```

### Properties & Relations
The matrix of the Hermitian conjugate operator is the conjugate transpose:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
fock = nonebasisvc[{c[1], c[2]}][[1, 2]];
op = I nc[c[CR, 1, UP], c[AN, 2, DO]];
matrixrepresentationvc[conj[op], fock] == ConjugateTranspose[matrixrepresentationvc[op, fock]]
```
---
Each matrix element is a `braketvc`:
```wl
matrixrepresentationvc[op, {vc[1, 0, 0, 0]}, {vc[0, 0, 0, 1]}] == {{braketvc[vc[1, 0, 0, 0], op, vc[0, 0, 0, 1]]}}
```
---
`matrixrepresentationop` gives the same matrix for the states in the creation operator representation:
```wl
matrixrepresentationvc[op, fock] == matrixrepresentationop[op, bzvc2bzop[{{{}, fock}}][[1, 2]]]
```

### Possible Issues
For states that are not orthonormal, the result is the matrix of matrix elements. For the identity operator, it is the Gram matrix:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
matrixrepresentationvc[1, {vc[1, 0, 0, 0], vc[1, 0, 0, 0] + vc[0, 0, 1, 0]}]
```
---
The vectors `vc[...]` are interpreted with the current `BASIS`. After `makebasis` for other levels, the operators are not applied, and the matrix elements remain unevaluated:
```wl
snegfermionoperators[c, d];
makebasis[{c[1]}];
states = {vc[1, 0], vc[0, 1]};
makebasis[{d[]}];
matrixrepresentationvc[c[CR, 1, UP], states]
```

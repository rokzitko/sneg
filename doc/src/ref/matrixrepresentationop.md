---
keywords: matrix representation, matrix elements, creation operator representation, vacuum expectation value, bra-ket, spin kets, phonon kets
see also: matrixrepresentationvc, makematricesbzop, makeallmatricesbzop, scalarproductop, braketop, vev, spinket, phononplus
---
# matrixrepresentationop

## Usage
- `matrixrepresentationop[op, {f1, f2, ...}]` gives the matrix $M_{ij} = \langle 0 | f_i^\dagger \hat O f_j | 0 \rangle$ of the operator *op* between the states $f_i |0\rangle$ given in the creation operator representation.
- `matrixrepresentationop[op, {g1, g2, ...}, {f1, f2, ...}]` gives the rectangular matrix $M_{ij} = \langle 0 | g_i^\dagger \hat O f_j | 0 \rangle$.

## Details
- In the creation operator representation, a state is an operator expression $f$ that creates it from the vacuum: `nc[c[CR, 1, UP], c[CR, 2, DO]]` is $c^\dagger_{1\uparrow} c^\dagger_{2\downarrow} |0\rangle$, and `1` is the vacuum. The vacuum is implied. States written with a trailing `VACUUM` also work, but the two forms must not be mixed.
- The matrix elements are computed with `scalarproductop[fi, nc[op, fj]]`, i.e. as vacuum expectation values with `vevwick`. Scalar coefficients of the bras are complex conjugated.
- The states may also be (or end with) kets, `ket[...]`, for spins, phonons, or other degrees of freedom; operators on them are written with `ket` and `bra`, e.g. `spinketbraP`, `phononplus`. Their overlaps are given by `braketrule`.
- Unlike `matrixrepresentationvc`, no global `BASIS` is needed. The calculation is much slower than in the occupation number representation; convert large bases with `bzop2bzvc`.
- The states do not need to be orthonormal; the result is then the matrix of matrix elements.

## Examples
### Basic Examples
Hopping between two sites in the basis of one-electron states with spin up:
```wl
snegfermionoperators[c];
snegrealconstants[t];
matrixrepresentationop[-t hop[c[1], c[2]], {c[CR, 1, UP], c[CR, 2, UP]}]
```

### Scope
Rectangular matrix of the pair creation operator $c^\dagger_{1\uparrow} c^\dagger_{1\downarrow}$ from the vacuum to the two-electron states:
```wl
snegfermionoperators[c];
matrixrepresentationop[nc[c[CR, 1, UP], c[CR, 1, DO]], {nc[c[CR, 1, UP], c[CR, 1, DO]], nc[c[CR, 2, UP], c[CR, 2, DO]]}, {1}]
```
---
Spin operators in the basis of spin kets $|S, m\rangle$, $m = S, \ldots, -S$; the matrix of $S^+$ for $S = 1$:
```wl
matrixrepresentationop[spinketbraP[1], spinket[1]]
```
---
Truncated boson (phonon) operators with at most three quanta in the basis $|0\rangle, \ldots, |3\rangle$:
```wl
matrixrepresentationop[#, {ket[0], ket[1], ket[2], ket[3]}] & /@ {phononplus[3], phononnumber[3]}
```

### Applications
Truncated phonon operators do not obey $[b, b^\dagger] = 1$ in the highest state:
```wl
matrixrepresentationop[komutator[phononminus[3], phononplus[3]], {ket[0], ket[1], ket[2], ket[3]}]
```

### Properties & Relations
The result agrees with `matrixrepresentationvc` for the corresponding occupation number vectors:
```wl
snegfermionoperators[c];
bz = qszbasisvc[{c[1], c[2]}];
states = Flatten[bz[[All, 2]]];
op = number[c[1]] + nc[c[CR, 1, UP], c[AN, 2, DO]];
matrixrepresentationop[op, vc2ops[states]] == matrixrepresentationvc[op, states]
```

### Possible Issues
States with and without an explicit `VACUUM` must not be mixed; their overlaps are not evaluated:
```wl
snegfermionoperators[c];
matrixrepresentationop[1, {nc[c[CR, 1, UP], VACUUM], c[CR, 1, UP]}]
```

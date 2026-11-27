---
keywords: exact diagonalization, Hamiltonian matrix, invariant subspace, symmetry sector, creation operator representation
see also: makematricesbzvc, makeallmatricesbzop, matrixrepresentationop, qszbasis, qsbasis, bzop2bzvc, transformtoLR
---
# makematricesbzop

## Usage
- `makematricesbzop[op, bz]` gives the list `{{qn, mat}, ...}` of the matrices of the operator *op* in the invariant subspaces of the basis *bz*, with the basis states in the creation operator representation.

## Details
- `makematricesbzop` is the variant of `makematricesbzvc` for bases whose states are operator expressions: a state $f(c^\dagger)|0\rangle$ is written as the expression $f(c^\dagger)$, e.g. `nc[c[CR, 1, UP], c[CR, 2, DO]]`, and the vacuum as `1`. Such bases are produced by `qszbasis`, `qsbasis`, `transformtoLR`, and `bzvc2bzop`. See `makematricesbzvc` for the structure of a basis.
- For each subspace with states $|i\rangle$, the matrix is $M_{ij} = \langle i | \hat O | j \rangle$, computed with `matrixrepresentationop` as vacuum expectation values $\langle 0 | f_i^\dagger \hat O f_j | 0 \rangle$ (Wick's theorem). The matrices are simplified with `Simplify`.
- The creation operator representation does not depend on the global `BASIS`, but it is much slower than the occupation number representation. For more than a few levels, convert the basis with `bzop2bzvc` and use `makematricesbzvc`.
- The states may contain kets for additional degrees of freedom, e.g. phonons (`transformtoPH`) or spins (`spinket`); their scalar products are given by `braketrule`.
- Matrix elements between different subspaces are not computed; see `makeallmatricesbzop`.
- `makematricesbzop` returns unevaluated if *bz* is not of the form tested by `bzQ`.

## Examples
### Basic Examples
Hubbard dimer in the $(Q, S)$ basis in the creation operator representation:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
bz = qsbasis[{c[1], c[2]}]
```
```wl
makematricesbzop[-t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]], bz]
```

### Scope
Holstein impurity: a level coupled to a local phonon mode, $H = \frac{U}{2}(n-1)^2 + \omega\, b^\dagger b + g (n - 1)(b + b^\dagger)$, with at most two phonons. The phonon states are kets added with `transformtoPH`:
```wl
snegfermionoperators[d];
snegrealconstants[U, omega, g];
bz = transformtoPH[qsbasis[{d[]}], {2}];
makematricesbzop[U/2 isozsq[d[]] + omega phononnumber[2] + g nc[number[d[]] - 1, phononx[2]], bz]
```

### Properties & Relations
The results agree with `makematricesbzvc` applied to the basis in the occupation number representation:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];
bz = qsbasis[{c[1], c[2]}];
makematricesbzop[H, bz] === makematricesbzvc[H, bzop2bzvc[bz]]
```

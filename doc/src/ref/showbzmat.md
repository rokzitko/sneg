---
keywords: display, matrix form, Hamiltonian matrices, invariant subspaces
see also: makematricesbzvc, makematricesbzop, bzQ, MatrixForm
---
# showbzmat

## Usage
- `showbzmat[mats]` displays a list of matrices `{{qn, mat}, ...}` in invariant subspaces as a table of the quantum numbers and the matrices in `MatrixForm`.

## Details
- *mats* is typically the result of `makematricesbzvc` or `makematricesbzop`. The argument must satisfy `bzQ`; otherwise `showbzmat` stays unevaluated.
- The result is wrapped in `MatrixForm` and is meant for display only.

## Examples
### Basic Examples
Hamiltonian of the Hubbard atom with a magnetic field in the $(Q, S_z)$ subspaces:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U, B];
showbzmat[makematricesbzvc[eps number[d[]] + U hubbard[d[]] + B spinz[d[]], qszbasisvc[{d[]}]]]
```
---
The basis states themselves can be displayed in the same way:
```wl
snegfermionoperators[c];
showbzmat[qsbasis[{c[1], c[2]}]]
```

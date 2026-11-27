---
keywords: matrix representation, occupation number representation, fast, exact diagonalization
see also: matrixrepresentationvc, matrixrepresentationvcsparse, makematricesbzvc, nonebasisvc
---
# matrixrepresentationvcfast

## Usage
- `matrixrepresentationvcfast[op, {v1, v2, ...}]` gives the matrix $\langle v_i | \hat O | v_j \rangle$ like `matrixrepresentationvc`, using a faster algorithm.
- `matrixrepresentationvcfast[op, {u1, ...}, {v1, ...}]` gives the rectangular matrix $\langle u_i | \hat O | v_j \rangle$.

## Details
- The result is the same as that of `matrixrepresentationvc`; see that page for the conventions.
- The fast algorithm applies the operator once to each ket, $\hat O |v_j\rangle = \sum_k a_{kj} |w_k\rangle$, and reads off the coefficients of the bra vectors, instead of computing all scalar products. It is used when every bra is a single occupation number vector with a coefficient, `x vc[...]`, the bras are distinct, and they contain no kets. Otherwise `matrixrepresentationvc` is called.
- For the Fock space of four spin-1/2 levels (256 states), the Hubbard Hamiltonian takes about a third of the time of `matrixrepresentationvc`.
- The result is not simplified.

## Examples
### Basic Examples
Hamiltonian of the Hubbard dimer in the Fock space of 16 states:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
fock = nonebasisvc[{c[1], c[2]}][[1, 2]];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];
m = matrixrepresentationvcfast[H, fock];
Dimensions[m]
```
The result agrees with `matrixrepresentationvc`:
```wl
m == matrixrepresentationvc[H, fock]
```

### Scope
Rectangular matrices, here $c^\dagger_{1\uparrow}$ from the vacuum and the one-electron states to the one-electron states:
```wl
snegfermionoperators[c];
makebasis[{c[1]}];
matrixrepresentationvcfast[c[CR, 1, UP], {vc[1, 0], vc[0, 1]}, {vc[0, 0], vc[1, 0], vc[0, 1]}]
```
---
Bases of superpositions use the general algorithm of `matrixrepresentationvc`:
```wl
matrixrepresentationvcfast[spinx[c[1]], {(vc[1, 0] + vc[0, 1])/Sqrt[2], (vc[1, 0] - vc[0, 1])/Sqrt[2]}]
```

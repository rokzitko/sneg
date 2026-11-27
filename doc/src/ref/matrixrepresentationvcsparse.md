---
keywords: sparse matrix, matrix representation, occupation number representation, Lanczos, Arnoldi, exact diagonalization
see also: matrixrepresentationvc, matrixrepresentationvcfast, makematricesbzvc, nonebasisvc, SparseArray
---
# matrixrepresentationvcsparse

## Usage
- `matrixrepresentationvcsparse[op, {v1, v2, ...}]` gives the matrix $\langle v_i | \hat O | v_j \rangle$ of the operator *op* as a `SparseArray`.

## Details
- The matrix elements are those of `matrixrepresentationvc[op, states]`; see that page for the conventions.
- For bases of distinct single occupation number vectors with coefficients, `x vc[...]`, without kets, the nonzero elements are collected directly from $\hat O |v_j\rangle$. Otherwise the dense matrix of `matrixrepresentationvc` is converted to a `SparseArray`.
- Hamiltonians of lattice models with local interactions are sparse in the occupation number basis. Sparse matrices can be diagonalized partially with `Eigenvalues[m, k]` and `Method -> "Arnoldi"`.
- Only the square form with a single list of states is defined; `matrixrepresentationvcsparse[op, bras, kets]` stays unevaluated.

## Examples
### Basic Examples
Hamiltonian of the Hubbard chain with three sites in the Fock space of 64 states, for $t = 1$, $U = 4$:
```wl
snegfermionoperators[c];
fock = nonebasisvc[{c[1], c[2], c[3]}][[1, 2]];
H = -hop[c[1], c[2]] - hop[c[2], c[3]] + 4 Total[hubbard[{c[1], c[2], c[3]}]];
m = matrixrepresentationvcsparse[H, fock]
```
Fraction of nonzero elements and the lowest eigenvalues:
```wl
{N[Length[m["NonzeroValues"]]/64^2], Sort[Eigenvalues[N[m]]][[1 ;; 4]]}
```

### Properties & Relations
`Normal` gives the dense matrix of `matrixrepresentationvc`:
```wl
Normal[m] == matrixrepresentationvc[H, fock]
```

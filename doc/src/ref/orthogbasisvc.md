---
keywords: orthogonalization, basis, invariant subspaces, occupation number representation
see also: orthogbasisop, orthogvc, mapbasis, transformbasis, dropemptysubspaces
---
# orthogbasisvc

## Usage
- `orthogbasisvc[bz1, bz]` orthonormalizes the states of every subspace of the basis *bz1* with respect to the states of the corresponding subspace of the basis *bz*; both bases are in the occupation number representation.

## Details
- `orthogbasisvc[bz1, bz]` is `mapbasis[bz1, bz, orthogvc]`: for each subspace, `orthogvc[states1, states]` is computed. The subspaces of *bz1* and *bz* must have the same quantum numbers in the same order; otherwise the message `mapbasis::compat` is issued and `Null` is returned.
- Typically, *bz1* is obtained from the orthonormal basis *bz* by applying an operator (projector, symmetrizer) to the states. The result is an orthonormal basis of the transformed states in each subspace; components outside the span of the original subspace are discarded. Subspaces can become empty; remove them with `dropemptysubspaces`.
- `orthogbasisop` is the corresponding function for the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
bz = qszbasisvc[{d[]}];
bz1 = applybasis[bz, 2 # &];
orthogbasisvc[bz1, bz] // InputForm
```

### Applications
Projection of a single level onto the states with zero or double occupancy; the singly occupied subspaces become empty:
```wl
snegfermionoperators[d];
bz = qszbasisvc[{d[]}];
bz1 = applybasis[bz, ap[projector02[d[]], #] &];
dropemptysubspaces[orthogbasisvc[bz1, bz]] // InputForm
```

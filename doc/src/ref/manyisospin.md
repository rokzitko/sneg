---
keywords: total isospin, charge SU(2), eta pairing, bipartite lattice, isospin multiplet
see also: isospin, manyspin, isospinplus, transformQStoIS
---
# manyisospin

## Usage
- `manyisospin[{c1, c2, ...}, {n1, n2, ...}]` gives the list $\{I^x, I^y, I^z, \mathbf I^2\}$ of the components of the total isospin $\mathbf I = \sum_k \mathbf I_k(n_k)$ of the fermion levels *ck* with the phases $(-1)^{n_k}$, and its square.
- `manyisospin[{c1, c2, ...}]` uses $n_k = 0$ for all levels.

## Details
- $\mathbf I_k(n_k)$ is `isospin[ck, nk]`; see `isospin` for the definition and the sign conventions. All results are expanded.
- On a bipartite lattice, choose $n_k$ even on one sublattice and odd on the other; then the total isospin commutes with the hopping between the sublattices, see `isospin`.
- If the two lists have different lengths, the message `manyisospin::unequallength` is issued and `Null` is returned.

## Examples
### Basic Examples
Total isospin of two levels with alternating phases:
```wl
snegfermionoperators[c];
{ix, iy, iz, ii} = manyisospin[{c[1], c[2]}, {0, 1}];
ix
```
The components obey the su(2) algebra and commute with $\mathbf I^2$:
```wl
Expand[{komutator[ix, iy] - I iz, komutator[ii, ix], komutator[ii, iz]}]
```

### Applications
Isospin multiplets of the Hubbard dimer: $\mathbf I^2$ in the subspaces of charge $Q$ and spin $S_z$. The states with $Q = \pm 2$ and the $Q = 0$, $S_z = 0$ state with $I(I+1) = 2$ form an isospin triplet:
```wl
snegfermionoperators[c];
ii = manyisospin[{c[1], c[2]}, {0, 1}][[4]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[ii, qszbasisvc[{c[1], c[2]}]]
```

### Properties & Relations
The total isospin commutes with the total spin:
```wl
snegfermionoperators[c];
Expand[Outer[komutator, Most[manyisospin[{c[1], c[2]}, {0, 1}]], Most[manyspin[{c[1], c[2]}]]]]
```

### Possible Issues
The lists of levels and phases must have equal lengths:
```wl
snegfermionoperators[c];
manyisospin[{c[1], c[2]}, {0}]
```

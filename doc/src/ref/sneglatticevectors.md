---
keywords: lattice vector, primitive vector, Bravais lattice, displacement, square lattice, honeycomb lattice, graphene
see also: sneglatticeindexes, snegmomentumindexes, blochphase, blochphaseexp, blochhamiltonian, fourier, listlatticevectors
---
# sneglatticevectors

## Usage
- `sneglatticevectors[a1, a2, ...]` declares *a1*, *a2*, ... as lattice vectors that can be used as displacements of lattice indexes, as in `c[R + a1]`.

## Details
- Lattice vectors are symbolic displacements such as the primitive vectors $\mathbf a_1, \mathbf a_2$ of a two-dimensional lattice. They are declared real constants and appended to `listlatticevectors`. They are not summation indexes: `latticeindexQ[a1]` is `False`.
- In combinations with lattice indexes, lattice vectors count as lattice quantities: `sumSimplifyKD` solves $\sum_R \delta_{R', R+a_1} f(R) = f(R'-a_1)$, and `sumShift` accepts shifts $R \to R + a_1$.
- Bloch phases $e^{i k\cdot a_1}$ appear as `blochphase[k, a1]`. Explicit components are substituted with `blochphaseexp`, for example `blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, 0}, a2 -> {0, 1}}]`. On finite clusters, `sumCluster` takes the components in lattice coordinates.
- In one dimension, integer displacements `c[R + 1]` can be used without declaring lattice vectors.
- A lattice vector is a symbol that may be zero for SNEG: $\delta_{R+a_1,R}$ is reduced to $\delta_{a_1,0}$, not to 0.

## Examples
### Basic Examples
```wl
sneglatticevectors[a1, a2];
{Conjugate[a1], latticeindexQ[a1], listlatticevectors}
```

### Applications
Square lattice with nearest-neighbor hopping, $\epsilon_{\mathbf k} = -2t(\cos k_x + \cos k_y)$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
H = -t sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}];
h = blochhamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]
```
```wl
Simplify[ExpToTrig[blochphaseexp[h, {k -> {kx, ky}, a1 -> {1, 0}, a2 -> {0, 1}}]]]
```

### Possible Issues
A delta of a lattice vector is not evaluated, because the symbol might be zero:
```wl
sneglatticeindexes[{R}, Ns];
sneglatticevectors[a1];
sumSimplifyKD[KroneckerDelta[R + a1, R]]
```

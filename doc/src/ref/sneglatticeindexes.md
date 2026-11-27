---
keywords: lattice index, unit cell, Bravais lattice, periodic boundary conditions, site index, translation invariance
see also: snegmomentumindexes, sneglatticevectors, latticesize, latticeindexQ, fourier, sum, sumSimplifyKD, snegfreeindexes
tech notes:
---
# sneglatticeindexes

## Usage
- `sneglatticeindexes[{R1, R2, ...}, Ns]` declares *R1*, *R2*, ... as indexes that label the unit cells of a periodic lattice with *Ns* unit cells.

## Details
- A lattice index $R$ labels the unit cells (Bravais lattice vectors) of a lattice with periodic boundary conditions. Site labels are defined modulo the lattice period, so the shifts $R \to \pm R + R_0$ are bijections of the index set. SNEG uses this to solve Kronecker deltas such as $\sum_R \delta_{R', R+1} f(R) = f(R'-1)$, to shift summation variables (`sumShift`), and to perform lattice sums $\sum_R e^{iK\cdot R} = N_s \delta_{K,0}$ (`sumSimplifyKD`).
- In one dimension, displacements can be written with integers, `c[R + 1]`. In higher dimensions, declare the primitive vectors with `sneglatticevectors` and write `c[R + a1]`. The site index is the first index after `CR`/`AN` of an operator; orbital indexes follow, and the spin is last: `c[CR, R, "A", UP]`.
- The declaration makes each index a free index (`snegfreeindexes`) and a real quantity (`Conjugate[R]` is `R`), sets `latticeindextype[R]` to `"Lattice"` and `latticesize[R]` to *Ns*, and appends the index to `listlatticeindexes`.
- *Ns* is a positive integer or an expression built from symbols, such as `Ns` or `L1 L2`; the symbols are declared real constants. Other values give the message `sneglatticeindexes::size`, and nothing is declared.
- A sum over a lattice index that does not appear in the summand evaluates to *Ns*: `sum[1, {R}]` is *Ns*.
- Indexes created by the automatic renaming of dummy indexes (`R1`, `R2`, ...) inherit the declaration.
- Generic indexes (`snegfreeindexes`) are not assumed to be periodic: for them, deltas of linear combinations are not solved, because on an open chain $\sum_i \delta_{j,i+1} f(i) = f(j-1)$ fails at the boundary.
- Lattice and momentum indexes with the same *Ns* describe the same lattice. Momentum indexes are declared with `snegmomentumindexes`.
- A repeated declaration overwrites the size.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R}, Ns];
{latticeindexQ[R], latticesize[R], Conjugate[R], sum[1, {R}]}
```

### Scope
Integer lattice sizes and sizes given as products:
```wl
sneglatticeindexes[{n}, 4];
sneglatticeindexes[{Rs}, L1 L2];
{sum[1, {n}], sum[1, {Rs}], Conjugate[L1]}
```

### Applications
Tight-binding chain $H = -t \sum_{R\sigma} (c^\dagger_{R\sigma} c_{R+1,\sigma} + \mathrm{h.c.})$ and its Fourier transform, $H = \sum_{k\sigma} (-t e^{ik} - t e^{-ik}) n_{k\sigma}$:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = -t sum[hop[c[R], c[R + 1]], {R}];
fourier[H, c -> ck, R -> k]
```
---
Kronecker deltas of shifted lattice indexes are solved, $\sum_R \delta_{R', R+1} f(R) = f(R'-1)$:
```wl
sneglatticeindexes[{R, Rp}, Ns];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[Rp, R + 1] f[R], {R}]]
```
For generic indexes, the delta is kept:
```wl
snegfreeindexes[i, j];
sumSimplifyKD[sum[KroneckerDelta[j, i + 1] f[i], {i}]]
```

### Possible Issues
Invalid lattice sizes are rejected:
```wl
sneglatticeindexes[{Rb}, -3]
```
```wl
latticeindexQ[Rb]
```
---
The indexes must be given as a list:
```wl
sneglatticeindexes[R, Ns]
```
---
The lattice size is declared real, but not positive; the complex conjugate of $N_s^{-1/2}$ is not simplified:
```wl
sneglatticeindexes[{R}, Ns];
Conjugate[1/Sqrt[Ns]]
```

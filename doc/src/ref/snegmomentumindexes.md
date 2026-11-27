---
keywords: momentum index, crystal momentum, Brillouin zone, reciprocal lattice, Umklapp, momentum conservation, periodic boundary conditions
see also: sneglatticeindexes, sneglatticevectors, latticesize, momentumindexQ, fourier, blochhamiltonian, sumSimplifyKD, sumShift, sum
---
# snegmomentumindexes

## Usage
- `snegmomentumindexes[{k1, k2, ...}, Ns]` declares *k1*, *k2*, ... as indexes that label the crystal momenta of a periodic lattice with *Ns* unit cells.

## Details
- A momentum index $k$ labels the $N_s$ crystal momenta in the Brillouin zone. Momenta are defined modulo reciprocal-lattice vectors, so Umklapp processes are included implicitly and the shifts $k \to \pm k + q$ are bijections of the index set. SNEG uses this to solve Kronecker deltas such as $\sum_k \delta_{p,k+q} f(k) = f(p-q)$ (momentum conservation), to shift summation variables (`sumShift`, `sumMomentumCanonical`), and to perform the sums $\sum_k e^{ik\cdot X} = N_s \delta_{X,0}$ (`sumSimplifyKD`).
- Symbolic momenta are abstract labels; in more than one dimension they stand for vectors. Explicit components are substituted only when Bloch phases are evaluated with `blochphaseexp`, and integer labels on finite clusters are used by `sumCluster`.
- The declaration makes each index a free index (`snegfreeindexes`) and a real quantity, sets `latticeindextype[k]` to `"Momentum"` and `latticesize[k]` to *Ns*, and appends the index to `listmomentumindexes`. *Ns* is a positive integer or an expression built from symbols, as for `sneglatticeindexes`; invalid sizes give the message `sneglatticeindexes::size`.
- `sum[1, {k}]` is *Ns*. Indexes created by the automatic renaming of dummy indexes (`k1`, `k2`, ...) inherit the declaration.
- The momentum label is the first index after `CR`/`AN` of an operator: `ck[CR, k, UP]`, `fk[AN, k, "A", 0]`.
- Deltas whose arguments mix momenta and lattice quantities are not solved, and coefficients other than $\pm 1$ are not solved ($\delta_{2k,q}$ has several solutions).

## Examples
### Basic Examples
```wl
snegmomentumindexes[{k, p, q}, Ns];
{momentumindexQ[k], latticesize[k], sum[1, {k}]}
```

### Applications
Momentum conservation in a commutator: $[c_{p\uparrow}, \sum_k c^\dagger_{k+q\uparrow} c_{k\uparrow}] = c_{p-q,\uparrow}$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, p, q}, Ns];
sumSimplifyKD[komutator[ck[AN, p, UP], sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}]]]
```
---
Fourier transform of the Hubbard interaction $U \sum_R n_{R\uparrow} n_{R\downarrow}$. The result conserves the total momentum, with one free momentum transfer per two-particle scattering:
```wl
snegfermionoperators[c, ck];
snegrealconstants[U];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[U sum[hubbard[c[R]], {R}], c -> ck, R -> k]
```

### Properties & Relations
Renamed dummy indexes are momentum indexes:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k}, Ns];
nc[sum[ck[CR, k, UP], {k}], sum[ck[AN, k, UP], {k}]]
```
```wl
{momentumindexQ[k1], listmomentumindexes}
```

### Possible Issues
Momentum deltas with coefficients other than $\pm 1$ are not solved:
```wl
snegmomentumindexes[{k, q}, Ns];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[2 k, q] f[k], {k}]]
```

---
keywords: Kronecker delta, sum over delta, momentum conservation, lattice sum, Bloch phase, symbolic sum simplification
see also: sum, sumExpand, sumCollect, sumFullSimplify, sumCollectSimplifyKD, SimplifyKD, blochphase, sneglatticeindexes, snegmomentumindexes, fourier
---
# sumSimplifyKD

## Usage
- `sumSimplifyKD[expr]` performs the sums over Kronecker deltas in *expr*, $\sum_i \delta_{ij} f_i = f_j$, splits sums into products of independent sums, and performs lattice sums over Bloch phases.

## Details
- For any declared indexes, a delta whose argument is a summation index $i$ is eliminated: $\sum_i (\delta_{i,x} a_i + b_i) = a_x + \sum_i b_i$, where *x* does not contain $i$. Deltas inside the scalar marker `scalar[...]` are handled in the same way.
- Sums that factorize are split, $\sum_{ij} a_i b_j = (\sum_i a_i)(\sum_j b_j)$, and summands of the form $a_i + b_j$ give $\sum_j 1 \sum_i a_i + \sum_i 1 \sum_j b_j$.
- For lattice and momentum indexes (`sneglatticeindexes`, `snegmomentumindexes`), on which a shift $k \to \pm k + q$ is a bijection, `sumSimplifyKD` additionally:
- solves deltas of linear combinations with coefficients $\pm 1$: $\sum_k \delta_{p,k+q} f(k) = f(p-q)$, $\sum_k \delta_{p,q-k} f(k) = f(q-p)$; if several summation indexes can be eliminated, the last one in the index list is used;
- performs the lattice sums $\sum_R e^{i K\cdot R} = N_s \delta_{K,0}$ and $\sum_k e^{i k\cdot X} = N_s \delta_{X,0}$ over Bloch phases `blochphase[k, R]` that enter the summand multiplicatively;
- reduces deltas whose arguments differ by a combination of lattice quantities, also outside sums: $\delta_{k,k+q} = \delta_{q,0}$, $\delta_{R,R+1} = 0$.
- Deltas are not solved if the coefficient of the summation index is not $\pm 1$ ($\delta_{2k,q}$ has several solutions on the torus), if lattice and momentum quantities are mixed, or, for generic indexes, if the summation index occurs in a linear combination: on an open chain, $\sum_i \delta_{j,i+1} f(i) = f(j-1)$ fails at the boundary.
- The rules are applied until the expression no longer changes (at most 50 iterations).
- The summands are not expanded. Deltas in products such as $\epsilon_k (\delta_{kp} c^\dagger_k - \ldots)$ are eliminated only after `sumExpand`. `sumCollectSimplifyKD` collects the deltas in each summand first.
- Expressions without `KroneckerDelta` and `blochphase` are returned unchanged; in particular, sums are then not split into products.
- `SimplifyKD` simplifies products and powers of deltas and step functions outside sums.

## Developer Notes
- The generic rules are in the internal list `rulesumSimplifyKD`; the lattice step uses the internal `rulelatticeKDreduce` (reduction of $\delta_{a,b}$ with a common lattice index in $a$ and $b$) and the elimination and phase-sum routines `snegKDsumterm` and `snegphasesumterm`.

## Examples
### Basic Examples
```wl
snegfreeindexes[i, j];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[i, j] f[i], {i}]]
```
---
A delta of spin indexes, $\sum_\sigma \delta_{\sigma\uparrow} c^\dagger_{i\sigma} = c^\dagger_{i\uparrow}$:
```wl
snegfermionoperators[c];
snegfreeindexes[i, sigma];
sumSimplifyKD[sum[KroneckerDelta[sigma, UP] c[CR, i, sigma], {sigma}]]
```

### Scope
Only the term with the delta is summed; the factor that does not depend on the eliminated index is split off:
```wl
snegfreeindexes[i, j, l];
snegrealfunctions[f, g];
{sumSimplifyKD[sum[KroneckerDelta[i, j] f[i] + g[i], {i}]], sumSimplifyKD[sum[KroneckerDelta[i, j] f[i] g[l], {i, l}]]}
```
---
Chains of deltas:
```wl
snegfreeindexes[i, j, l];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[i, j] KroneckerDelta[j, l] f[i], {i, j}]]
```
---
Deltas of linear combinations of momenta:
```wl
snegmomentumindexes[{k, p, q}, Ns];
snegrealfunctions[f];
{sumSimplifyKD[sum[KroneckerDelta[p, k + q] f[k], {k}]], sumSimplifyKD[sum[KroneckerDelta[p, q - k] f[k], {k}]]}
```
With two summation indexes, the last one is eliminated:
```wl
sumSimplifyKD[sum[KroneckerDelta[q, k - p] f[k, p], {k, p}]]
```
---
Lattice sums over Bloch phases, $\sum_R e^{i(k-p)R} = N_s \delta_{kp}$ and $\sum_k e^{ik(R-R')} = N_s \delta_{RR'}$:
```wl
sneglatticeindexes[{R, Rp}, Ns];
snegmomentumindexes[{k, p}, Ns];
{sumSimplifyKD[sum[blochphase[k - p, R], {R}]], sumSimplifyKD[sum[blochphase[k, R - Rp], {k}]]}
```
---
Deltas of lattice quantities are reduced also outside sums:
```wl
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k, q}, Ns];
{KroneckerDelta[R, R + 1], sumSimplifyKD[KroneckerDelta[R, R + 1]], sumSimplifyKD[KroneckerDelta[k, k + q]]}
```

### Applications
Momentum conservation. The density operator $\rho_q = \sum_k c^\dagger_{k+q\uparrow} c_{k\uparrow}$ transfers the momentum $q$: $[c_{p\uparrow}, \rho_q] = c_{p-q,\uparrow}$:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, p, q}, Ns];
sumSimplifyKD[komutator[ck[AN, p, UP], sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}]]]
```
---
Canonical anticommutator of Fourier-transformed operators, $\{c_{R\uparrow}, c^\dagger_{R'\uparrow}\} = \frac{1}{N_s} \sum_{k} e^{ik(R-R')} = \delta_{RR'}$:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R, Rp}, Ns];
snegmomentumindexes[{k, p}, Ns];
a = antikomutator[fourier[c[AN, R, UP], c -> ck, R -> k], fourier[c[CR, Rp, UP], c -> ck, R -> p]]
```
The delta $\delta_{kp}$ and the lattice sum are performed after expanding; the operator terms cancel after `Expand`:
```wl
Expand[sumSimplifyKD[sumExpand[a]]]
```
---
Equation of motion of a band electron, $[H, c^\dagger_{p\uparrow}] = \epsilon_p c^\dagger_{p\uparrow}$ for $H = \sum_{k\sigma} \epsilon_k n_{k\sigma}$:
```wl
snegfermionoperators[c];
snegfreeindexes[l, m, sigma];
snegrealfunctions[eps];
sumSimplifyKD[sumExpand[komutator[sum[eps[l] number[c[l], sigma], {l, sigma}], c[CR, m, UP]]]]
```

### Properties & Relations
`sumSimplifyKD` does not expand the summands; use `sumExpand` first or `sumCollectSimplifyKD`:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f];
s = sum[(1 + KroneckerDelta[i, j]) f[i], {i}];
{sumSimplifyKD[s], sumSimplifyKD[sumExpand[s]]}
```

### Possible Issues
For generic indexes, deltas with the summation index in a linear combination are not solved, because the shift is not exact on an open chain:
```wl
snegfreeindexes[n, j];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[j, n + 1] f[n], {n}]]
```
Declare lattice indexes for periodic lattices:
```wl
sneglatticeindexes[{R, Rp}, Ns];
sumSimplifyKD[sum[KroneckerDelta[Rp, R + 1] f[R], {R}]]
```
---
Deltas with coefficients other than $\pm 1$ are not solved:
```wl
snegmomentumindexes[{k, q}, Ns];
snegrealfunctions[f];
sumSimplifyKD[sum[KroneckerDelta[2 k, q] f[k], {k}]]
```
---
Lattice vectors are symbols that may be zero, so a delta of a lattice vector is not reduced to 0:
```wl
sneglatticeindexes[{R}, Ns];
sneglatticevectors[a1];
sumSimplifyKD[KroneckerDelta[R + a1, R]]
```
---
Without deltas or Bloch phases the expression is returned unchanged, and separable sums are not split:
```wl
snegfreeindexes[i, l];
snegrealfunctions[f, g];
sumSimplifyKD[sum[f[i] g[l], {i, l}]]
```
Use `rulesumFactor`:
```wl
sum[f[i] g[l], {i, l}] /. rulesumFactor
```

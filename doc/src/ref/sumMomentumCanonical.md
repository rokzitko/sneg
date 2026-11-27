---
keywords: momentum shift, canonical form, dummy index, symbolic sum, electron-phonon coupling, collect sums
see also: sumShift, sumCollect, sumFullSimplify, sumAbstractIndex, snegmomentumindexes, sneglatticeindexes, fourier
---
# sumMomentumCanonical

## Usage
- `sumMomentumCanonical[expr]` shifts the summed lattice and momentum indexes in every sum of *expr* so that the first operator argument that depends on each index is the bare index, and renames automatically renamed dummy indexes back to their root names.

## Details
- For each summed lattice or momentum index $m$, the operator arguments in the summand (all indexes after the first, `CR`/`AN`, index) are searched in order; the first argument $e = c\,m + r$ that depends on $m$, with $c = \pm 1$, is made the bare index by the shift $m \to c\,(m - r)$, applied to the whole summand. Example: $\sum_k c^\dagger_{k+q} c_k \to \sum_k c^\dagger_k c_{k-q}$.
- Dummy indexes created by the automatic renaming of `sum` (`k1`, `k2`, ... for the root `k`) are renamed to the root name if the root is a declared index of the same type that does not occur in the sum.
- After this step, equivalent sums often have identical summands, so that they cancel or can be combined by `sumCollect`. `sumFullSimplify` uses `sumCollect[sumMomentumCanonical[sumExpand[expr]]]` as one of its transformations.
- The shifts are exact for lattice and momentum indexes, see `sumShift`. Generic indexes are not changed. Sums that contain `SEA`-ordered fermion operators are left unchanged.
- The shift is applied to the whole summand; apply `sumExpand` first to canonicalize each term separately.
- Only automatically generated names are renamed. Sums that differ by the choice of the dummy name, such as $\sum_k n_k$ and $\sum_p n_p$, are not identified; use `sumAbstractIndex` and `sumNameIndex`.

## Examples
### Basic Examples
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumMomentumCanonical[sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}]]
```

### Scope
Shifts with a negative coefficient:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumMomentumCanonical[sum[nc[ck[CR, q - k, UP], ck[AN, k, UP]], {k}]]
```
---
Bloch phases and other functions of the index are shifted together with the operators:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
sumMomentumCanonical[sum[blochphase[k, 1] number[ck[k + q], UP], {k}]]
```

### Applications
Commutator of density operators on a lattice, $[\rho_q, \rho_{-q}] = 0$ with $\rho_q = \sum_p c^\dagger_{p+q} c_p$. After the Kronecker deltas are summed, two sums that differ by a shift and by the dummy name remain:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{p, q}, Ns];
rho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];
c2 = sumSimplifyKD[sumExpand[komutator[rho[q], rho[-q]]]]
```
```wl
sumMomentumCanonical[c2]
```
---
Electron-phonon coupling $H = g \sum_R n_R (b^\dagger_R + b_R)$ in momentum space. The Fourier transformation of electrons and phonons gives sums with the automatically renamed index `k1`:
```wl
snegfermionoperators[c, ck];
snegbosonoperators[b, bk];
snegrealconstants[g];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k, q}, Ns];
Hep = g sum[nc[number[c[R]], b[CR, R] + b[AN, R]], {R}];
Hq = fourier[fourier[Hep, c -> ck, R -> k], b -> bk, R -> q]
```
In the canonical form, $H = \frac{g}{\sqrt{N_s}} \sum_{kq\sigma} c^\dagger_{k\sigma} (c_{k+q,\sigma} b^\dagger_q + c_{k-q,\sigma} b_q)$:
```wl
sumCollect[sumMomentumCanonical[sumExpand[Hq]]]
```

### Possible Issues
Sums with different dummy names chosen by the user are not identified:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, p, q}, Ns];
sumCollect[sumMomentumCanonical[sum[number[ck[k + q], UP], {k}] - sum[number[ck[p], UP], {p}]]]
```
```wl
sumCollect[sumMomentumCanonical[sum[number[ck[k + q], UP], {k}] - (sum[number[ck[p], UP], {p}] /. p -> k)]]
```

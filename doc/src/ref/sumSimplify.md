---
keywords: simplify symbolic sums, Simplify, Kronecker delta
see also: sumFullSimplify, sum, sumExpand, sumCollect, sumSimplifyKD, SimplifyKD, Simplify
---
# sumSimplify

## Usage
- `sumSimplify[expr]` simplifies an expression with symbolic sums by `Simplify` with additional transformation functions for sums.

## Details
- The transformation functions are `SimplifyKD`, combinations of `Expand`, `sumExpand`, `Simplify`, `sumCollect`, and `sumSimplifyKD`, and the individual rules of `sumExpand`, `sumCollect`, and `sumSimplifyKD`. `Simplify` keeps the form with the smallest complexity.
- Unlike `sumFullSimplify`, `sumSimplify` uses `Simplify` instead of `FullSimplify`, and it does not shift momenta or rename dummy indexes (`sumMomentumCanonical`). It is faster but finds fewer simplifications.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
snegrealfunctions[eps];
sumSimplify[komutator[sum[eps[k] number[c[k], UP], {k}], c[CR, p, UP]]]
```

### Scope
Sums over Kronecker deltas:
```wl
snegfreeindexes[i, j];
snegrealfunctions[f];
sumSimplify[sum[KroneckerDelta[i, j] f[i], {i}]]
```

### Properties & Relations
Sums that differ only in the name of the dummy index are not combined; `sumFullSimplify` renames the automatically generated index `p1` back to `p`:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{p, q}, Ns];
rho[qq_] := sum[nc[ck[CR, p + qq, UP], ck[AN, p, UP]], {p}];
c2 = komutator[rho[q], rho[-q]];
{sumSimplify[c2], sumFullSimplify[c2]}
```

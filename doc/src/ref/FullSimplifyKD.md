---
keywords: Kronecker delta, UnitStep, step function, Fermi sea, FullSimplify
see also: SimplifyKD, sumSimplifyKD, sumFullSimplify, vevwick, FullSimplify
---
# FullSimplifyKD

## Usage
- `FullSimplifyKD[expr]` simplifies expressions with Kronecker deltas and step functions by `FullSimplify` with the identities used by `SimplifyKD`.

## Details
- `FullSimplifyKD` applies the same identities as `SimplifyKD` ($\delta^n = \delta$, $\theta(k) + \theta(-k) = 1$, $\theta(k)\theta(-k) = 0$, $\delta_{kl}\theta(k)\theta(-l) = 0$, ...), but within `FullSimplify`, which tries more transformations and is slower.
- As for `SimplifyKD`, the arguments of the step functions are assumed to be nonzero.

## Examples
### Basic Examples
```wl
FullSimplifyKD[KroneckerDelta[i, j]^4 + UnitStep[k] + UnitStep[-k]]
```

### Applications
Density-density correlation $\langle n_k n_l \rangle$ in the Fermi sea (`SEA` ordering, $n_k = \theta(-k)$). The exchange term $\delta_{kl} n_k (1 - n_l)$ vanishes:
```wl
snegfermionoperators[cs];
ordering[cs] = SEA;
snegfreeindexes[k, l];
v = vevwick[nc[cs[CR, k, UP], cs[AN, k, UP], cs[CR, l, UP], cs[AN, l, UP]]]
```
```wl
FullSimplifyKD[v]
```
`UnitStep[-k, -l]` is $\theta(-k)\theta(-l)$. `SimplifyKD` does not find this form:
```wl
SimplifyKD[v]
```

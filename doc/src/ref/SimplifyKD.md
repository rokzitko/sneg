---
keywords: Kronecker delta, UnitStep, step function, Fermi sea, occupation numbers, simplify
see also: FullSimplifyKD, sumSimplifyKD, sumSimplify, vev, ordering, Simplify
---
# SimplifyKD

## Usage
- `SimplifyKD[expr]` simplifies expressions with Kronecker deltas `KroneckerDelta` and step functions `UnitStep` in *expr*.

## Details
- `SimplifyKD` calls `Simplify` with an additional transformation function that applies the identities
- $\delta^n = \delta$ and $\theta^n = \theta$ for positive integer powers $n$;
- $\theta(k) + \theta(-k) = 1$, $1 - \theta(-k) = \theta(k)$, $\theta(-k) - 1 = -\theta(k)$, $\theta(k)\theta(-k) = 0$;
- $\delta_{kl}\,\theta(k)\theta(-l) = \delta_{kl}\,\theta(-k)\theta(l) = 0$.
- Here $\theta(x)$ is `UnitStep[x]`. With `SEA` ordering (see `ordering`), momenta are measured from the Fermi level, and $\theta(-k)$ and $\theta(k)$ are the occupation of the state $k$ in the Fermi sea and its complement.
- Since `UnitStep[0]` is 1, the identities with $\theta(k)$ and $\theta(-k)$ hold only for $k \ne 0$; `SimplifyKD` assumes that the arguments of the step functions are nonzero.
- `SimplifyKD` does not perform sums; use `sumSimplifyKD`. `FullSimplifyKD` uses `FullSimplify` instead of `Simplify`.

## Examples
### Basic Examples
```wl
{SimplifyKD[UnitStep[k] + UnitStep[-k]], SimplifyKD[1 - UnitStep[-k]], SimplifyKD[KroneckerDelta[a, b]^2]}
```

### Scope
A delta with opposite occupations vanishes:
```wl
SimplifyKD[KroneckerDelta[k, l] UnitStep[k] UnitStep[-l]]
```
`Simplify` alone does not use this identity:
```wl
Simplify[KroneckerDelta[k, l] UnitStep[k] UnitStep[-l]]
```

### Applications
Expectation values in the Fermi sea. With `SEA` ordering, $\langle c_k c^\dagger_k \rangle = 1 - n_k = \theta(k)$:
```wl
snegfermionoperators[cs];
ordering[cs] = SEA;
snegfreeindexes[k, l];
vev[nc[cs[AN, k, UP], cs[CR, k, UP]]]
```
```wl
SimplifyKD[%]
```
---
Density-density correlation in the Fermi sea from Wick's theorem, $\langle n_k n_l \rangle = n_k n_l + \delta_{kl} n_k (1 - n_l)$ with $n_k = \theta(-k)$:
```wl
snegfermionoperators[cs];
ordering[cs] = SEA;
snegfreeindexes[k, l];
v = vevwick[nc[cs[CR, k, UP], cs[AN, k, UP], cs[CR, l, UP], cs[AN, l, UP]]]
```
```wl
SimplifyKD[v]
```

### Properties & Relations
Negative powers are not simplified:
```wl
SimplifyKD[{KroneckerDelta[a, b]^-1, UnitStep[k]^-1}]
```

### Possible Issues
`Simplify` may factor the expression before the identities are applied, so that some terms are not recognized. The last term of the density-density correlation above vanishes, $\delta_{kl}\theta(-k)\theta(l) = 0$, which `FullSimplifyKD` finds:
```wl
snegfermionoperators[cs];
ordering[cs] = SEA;
snegfreeindexes[k, l];
v = vevwick[nc[cs[CR, k, UP], cs[AN, k, UP], cs[CR, l, UP], cs[AN, l, UP]]];
{SimplifyKD[v], FullSimplifyKD[v]}
```

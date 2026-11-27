---
keywords: real function, dispersion relation, band energy, declaration, parameter function, c-number
see also: snegcomplexfunctions, snegrealconstants, snegfreeindexes, isnumericQ, sum
---
# snegrealfunctions

## Usage
- `snegrealfunctions[f, g, ...]` declares *f*, *g*, ... as real-valued functions.

## Details
- After the declaration, every application `f[args]` is a scalar (see `isnumericQ`), whatever the arguments, and it is real: `Conjugate[f[args]]` is `f[args]`.
- Typical uses are dispersion relations $\epsilon_k$, momentum-dependent real couplings $V_k$, $J(q)$, and distribution functions.
- The declaration sets `snegnonopQ[f] = True`, the upvalue `Conjugate[f[args___]] = f[args]`, and `isnumericQ[f[___]] = True`. The function symbols are not added to any list.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[eps];
{nc[c[AN, k, UP], eps[k] c[CR, k, UP]], Conjugate[eps[k]]}
```

### Applications
Band Hamiltonian $H = \sum_{k\sigma} \epsilon_k n_{k\sigma}$ and its commutator with a creation operator:
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
snegrealfunctions[eps];
H = sum[eps[k] number[c[k]], {k}];
sumSimplifyKD[sumExpand[komutator[H, c[CR, p, UP]]]]
```

### Properties & Relations
A complex function is conjugated:
```wl
snegrealfunctions[eps];
snegcomplexfunctions[V];
{Conjugate[eps[k]], Conjugate[V[k]]}
```

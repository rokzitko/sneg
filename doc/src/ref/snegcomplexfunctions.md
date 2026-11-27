---
keywords: complex function, hybridization, form factor, declaration, parameter function, c-number
see also: snegrealfunctions, snegcomplexconstants, snegfreeindexes, isnumericQ, sum
---
# snegcomplexfunctions

## Usage
- `snegcomplexfunctions[f, g, ...]` declares *f*, *g*, ... as complex-valued functions.

## Details
- After the declaration, every application `f[args]` and its conjugate `Conjugate[f[args]]` are scalars (see `isnumericQ`). `conj[f[args]]` is `Conjugate[f[args]]`.
- Typical uses are momentum-dependent hybridizations $V_k$, form factors, and complex couplings.
- The declaration sets `snegnonopQ[f] = True` and `isnumericQ[f[___]] = isnumericQ[Conjugate[f[___]]] = True`. The function symbols are not added to any list.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
snegfreeindexes[k];
snegcomplexfunctions[V];
conj[V[k] nc[c[CR, k, UP], d[AN, UP]]]
```

### Applications
The hybridization term of the Anderson impurity model, $H_\mathrm{hyb} = \sum_{k\sigma}(V_k c^\dagger_{k\sigma} d_\sigma + V_k^* d^\dagger_\sigma c_{k\sigma})$, is Hermitian:
```wl
snegfermionoperators[c, d];
snegfreeindexes[k];
snegcomplexfunctions[V];
Hhyb = sum[V[k] nc[c[CR, k, UP], d[AN, UP]] + V[k] nc[c[CR, k, DO], d[AN, DO]], {k}];
Hhyb = Hhyb + conj[Hhyb];
Expand[conj[Hhyb] - Hhyb]
```
The equation of motion of the impurity operator, $[d_\uparrow, H_\mathrm{hyb}] = \sum_k V_k^* c_{k\uparrow}$:
```wl
sumSimplifyKD[sumExpand[komutator[d[AN, UP], Hhyb]]]
```

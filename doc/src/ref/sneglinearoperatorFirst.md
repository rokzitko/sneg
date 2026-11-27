---
keywords: linear function, expectation value, linear functional, extending SNEG
see also: sneglinearoperator, snegsesquilinearoperator, isnumericQ, vev, avg
---
# sneglinearoperatorFirst

## Usage
- `sneglinearoperatorFirst[f]` makes the function *f* linear in its first argument, with $f(z) = z$ for scalars $z$.

## Details
- After `sneglinearoperatorFirst[f]`, the following rules hold for scalars $z$ (see `isnumericQ`), Grassmann numbers $\xi$, and arbitrary expressions $a, b$:
  - $f(a + b, \ldots) = f(a, \ldots) + f(b, \ldots)$;
  - $f(z\, a, \ldots) = z\, f(a, \ldots)$;
  - $f(z) = z$, and $f(z, b, \ldots) = z\, f(b, \ldots)$;
  - $f(\xi) = \xi$ and $f(\xi a) = \xi f(a)$ for a product `nc[ξ, a]` starting with a Grassmann number.
- These are the rules of a normalized linear functional, such as an expectation value $\langle A\rangle$ with $\langle 1\rangle = 1$. In SNEG, `vev`, `vevwick`, `avg`, and `zeroonvac` are declared in this way.
- The further arguments, if any, are parameters (e.g. a state or a temperature) and are not expanded.
- The rules are added as definitions of *f*; the values on operator products are given by further definitions. `sneglinearoperatorFirst` returns `{Null}`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[t];
sneglinearoperatorFirst[ev];
ev[3 + t number[c[1]]]
```

### Applications
A thermal average for free electrons, $\langle n_{k\sigma}\rangle = f(\epsilon_k)$, as a linear functional with the values on the number operators. Sums are handled by an additional rule:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealfunctions[eps, nF];
sneglinearoperatorFirst[ev];
ev[nc[c[CR, i_, s_], c[AN, i_, s_]]] := nF[eps[i]];
ev[sum[a_, it_List]] := sum[ev[a], it];
ev[sum[eps[k] number[c[k]], {k}]]
```

### Properties & Relations
`vev` is declared with `sneglinearoperatorFirst`:
```wl
snegfermionoperators[c];
snegrealconstants[t];
vev[t + 2 nc[c[AN, 1, UP], c[CR, 1, UP]]]
```
---
Compare with `sneglinearoperator`, which removes scalar arguments:
```wl
sneglinearoperator[g];
sneglinearoperatorFirst[h];
{g[2], h[2], g[2, b], h[2, b]}
```

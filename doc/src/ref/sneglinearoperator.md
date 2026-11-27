---
keywords: linear function, multilinear, distributive, factoring out constants, extending SNEG
see also: sneglinearoperatorFirst, snegsesquilinearoperator, isnumericQ, nc, dd
---
# sneglinearoperator

## Usage
- `sneglinearoperator[f]` makes the function *f* linear in each of its arguments.

## Details
- After `sneglinearoperator[f]`, the following rules hold for scalars $z$ (see `isnumericQ`) and arbitrary expressions $a, b$:
  - $f(\ldots, a + b, \ldots) = f(\ldots, a, \ldots) + f(\ldots, b, \ldots)$;
  - $f(\ldots, z\, a, \ldots) = z\, f(\ldots, a, \ldots)$;
  - $f(\ldots, z, \ldots) = z\, f(\ldots, \ldots)$: an argument that is a scalar is factored out and removed.
- The last rule corresponds to the convention for products, where a scalar factor is a product with the identity: `nc[a, 2, b]` is `2 nc[a, b]`. It makes `sneglinearoperator` suitable for product-like functions with a variable number of arguments, such as `nc` and `dd`, which are declared in this way.
- The rules are added as definitions of *f*; further definitions for the values on operators can be given before or after the declaration. `sneglinearoperator` returns `{Null}`.

## Developer Notes
- In SNEG, `nc`, `dd`, `wick`, `invertspin`, and `vc2ops` are declared with `sneglinearoperator`; `vev`, `vevwick`, `avg`, and `zeroonvac` with `sneglinearoperatorFirst`; `scalarproduct` and `scalarproductvc` with `snegsesquilinearoperator`.
- The definitions are DownValues with the patterns `f[a___, z_?isnumericQ, b___]`, `f[a___, z_?isnumericQ b_, c___]`, and `f[a___, b1_ + b2_, c___]`. Use `isnumericQ` (and declarations of constants) to control what counts as a scalar.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[t];
snegcomplexconstants[z];
sneglinearoperator[tensor];
tensor[t c[CR, 1, UP] + 2 c[CR, 2, UP], z c[AN, 1, UP]]
```

### Scope
A scalar argument is factored out and removed:
```wl
snegrealconstants[t];
sneglinearoperator[f];
{f[3, a], f[a, t, b], f[t]}
```

### Applications
A correlation function $\langle\langle A; B\rangle\rangle$ that is to be decomposed into the correlation functions of operator monomials. The spin-spin correlation function of a level reduces to density-density correlation functions:
```wl
snegfermionoperators[d];
sneglinearoperator[corr];
corr[spinz[d[]], spinz[d[]]]
```

### Properties & Relations
`nc` is declared with `sneglinearoperator`:
```wl
snegfermionoperators[c];
snegrealconstants[t];
{nc[a, 2, b], nc[a, t b + c[CR, 1, UP]]}
```

### Possible Issues
A scalar argument is removed, which is inappropriate for functions with a fixed number of slots. Here the constant part of the first argument removes the slot:
```wl
sneglinearoperator[f];
f[1/2 + a, b]
```
For functions of one argument with $f(1) = 1$, such as expectation values, use `sneglinearoperatorFirst`:
```wl
sneglinearoperator[g];
sneglinearoperatorFirst[h];
{g[2], h[2]}
```

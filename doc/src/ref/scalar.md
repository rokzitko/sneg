---
keywords: scalar marker, c-number wrapper, coefficient, symbolic sums, performance
see also: isnumericQ, nc, sum, vev, conj
---
# scalar

## Usage
- `scalar[x]` marks *x* as a scalar (c-number) coefficient, which is factored out of operator products without further tests.

## Details
- `nc` always factors `scalar[x]` out of products. Products and sums of `scalar` wrappers are combined: `scalar[a] scalar[b]` is `scalar[a b]`, `scalar[a] + scalar[b]` is `scalar[a + b]`, and a factor *z* with `isnumericQ[z]` is absorbed, `z scalar[b]` is `scalar[z b]`.
- `scalar[0]` is 0. `conj[scalar[x]]` is `scalar[conj[x]]`, and `vev[scalar[x]]` is *x*. Symbolic sums are pulled out: `scalar[a sum[b, {k}]]` is `sum[scalar[a b], {k}]`.
- `scalar` is used to speed up calculations with long coefficients, in particular in symbolic sums, where the test `isnumericQ` of every factor is expensive, and by functions such as `scalarproductvc`.
- The replacement rules `rulestripscalar` (`scalar[x] :> x`) and `rulesimplifyscalar` (`scalar[x] :> scalar[Simplify[x]]`) remove or simplify the wrappers.
- `isnumericQ[scalar[x]]` is `False`; the wrapper is recognized by explicit rules of `nc`, `conj`, `vev`, and the sum functions.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[t];
nc[c[AN, 1, UP], scalar[t] c[CR, 1, UP]]
```
The wrappers are combined:
```wl
{scalar[t] + scalar[2], 3 scalar[t], scalar[t] scalar[t]}
```

### Scope
Conjugation and vacuum expectation values:
```wl
snegfermionoperators[c];
snegrealconstants[t];
{conj[scalar[I t] c[CR, 1, UP]], vev[scalar[t] nc[c[AN, 1, UP], c[CR, 1, UP]]]}
```
---
The wrapper is removed with `rulestripscalar`:
```wl
snegfermionoperators[c];
snegrealconstants[t];
nc[c[AN, 1, UP], scalar[t] c[CR, 1, UP]] /. rulestripscalar
```

### Possible Issues
The content of `scalar` is not tested: an operator inside the wrapper is treated as a number:
```wl
snegfermionoperators[c];
nc[c[AN, 1, UP], scalar[c[CR, 2, UP]]]
```

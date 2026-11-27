---
keywords: scalar marker, remove scalar, symbolic sum
see also: rulesimplifyscalar, sumSimplifyKD, isnumericQ, sum
---
# rulestripscalar

## Usage
- `expr /. rulestripscalar` removes the scalar markers `scalar[x]` from *expr*, replacing them by *x*.

## Details
- `scalar[x]` is a marker that declares *x* as a scalar (non-operator) quantity. It is moved in front of `nc` products, absorbs other scalar factors (`a scalar[x]` becomes `scalar[a x]`), and adds up, `scalar[x] + scalar[y]` = `scalar[x + y]`. It is used for speed in expressions with symbolic sums; `sumSimplifyKD` eliminates Kronecker deltas also inside `scalar[...]`.
- `rulestripscalar` is `{scalar[x_] :> x}`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegfreeindexes[k, p];
snegrealfunctions[eps];
e = sumSimplifyKD[sum[scalar[eps[k] KroneckerDelta[k, p]] c[CR, k, UP], {k}]]
```
```wl
e /. rulestripscalar
```

### Properties & Relations
Scalar markers absorb scalar factors and combine:
```wl
snegrealconstants[t];
2 scalar[t] + 3 scalar[t^2]
```

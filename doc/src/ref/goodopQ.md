---
keywords: operator in basis, occupation number representation, BASIS
see also: op2ndx, ndx2op, makebasis, ap
---
# goodopQ

## Usage
- `goodopQ[c[t, i, σ]]` gives `True` if the operator $c_{i\sigma}$ belongs to the single-particle basis `BASIS` defined by `makebasis`, and `False` otherwise.

## Details
- The creation/annihilation index `t` is disregarded.
- `ap` applies an operator to a `vc` vector only if `goodopQ` gives `True`; `op2ndx` returns a position only for such operators.
- `goodopQ` remains unevaluated for expressions without indexes, such as the level `c[]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
makebasis[{c[1]}];
{goodopQ[c[CR, 1, UP]], goodopQ[c[AN, 1, DO]], goodopQ[c[CR, 2, UP]], goodopQ[d[CR, UP]]}
```

### Properties & Relations
Operators outside the basis are not applied by `ap`:
```wl
snegfermionoperators[c];
makebasis[{c[1]}];
{ap[c[CR, 1, UP], vacuum[]], ap[c[CR, 2, UP], vacuum[]]}
```

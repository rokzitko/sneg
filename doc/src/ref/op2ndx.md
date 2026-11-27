---
keywords: operator index, position in basis, occupation number representation, BASIS
see also: ndx2op, goodopQ, makebasis, vc, ap
---
# op2ndx

## Usage
- `op2ndx[c[t, i, σ]]` gives the position of the operator $c_{i\sigma}$ in the single-particle basis `BASIS` defined by `makebasis`.

## Details
- The position is the index of the occupation number of the state $c^\dagger_{i\sigma}|0\rangle$ in the `vc` vectors.
- The creation/annihilation index `t` (`CR` or `AN`) is disregarded: `op2ndx[c[AN, i, σ]]` equals `op2ndx[c[CR, i, σ]]`.
- If the operator is not in `BASIS`, `op2ndx` returns unevaluated; see `goodopQ`.
- `op2ndx` is `Listable`. The inverse function is `ndx2op`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
makebasis[{c[], d[]}];
{op2ndx[c[CR, UP]], op2ndx[c[AN, DO]], op2ndx[d[CR, UP]], op2ndx[d[AN, DO]]}
```

### Scope
`op2ndx` threads over lists:
```wl
snegfermionoperators[c, d];
makebasis[{c[], d[]}];
op2ndx[BASIS]
```
---
Operators that are not in the basis remain unevaluated:
```wl
op2ndx[c[CR, 1, UP]]
```

### Properties & Relations
`ndx2op` is the inverse function:
```wl
snegfermionoperators[c, d];
makebasis[{c[], d[]}];
ndx2op[op2ndx[d[AN, DO]]]
```

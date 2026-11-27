---
keywords: operator index, position in basis, occupation number representation, BASIS
see also: op2ndx, goodopQ, makebasis, vc
---
# ndx2op

## Usage
- `ndx2op[n]` gives the creation operator at position *n* of the single-particle basis `BASIS` defined by `makebasis`.

## Details
- `ndx2op[n]` is `BASIS[[n]]`. It always returns a creation operator.
- `ndx2op` is `Listable`. The inverse function is `op2ndx`.
- For *n* larger than the length of `BASIS`, the `Part::partw` message is issued.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
makebasis[{c[], d[]}];
ndx2op[3]
```
The operators corresponding to the occupation numbers of a `vc` vector:
```wl
ndx2op[Range[4]]
```

### Properties & Relations
The occupied states of a `vc` vector:
```wl
snegfermionoperators[c, d];
makebasis[{c[], d[]}];
v = vc[1, 0, 0, 1];
ndx2op[Flatten[Position[List @@ v, 1]]]
```
---
The result is a creation operator even if the index was obtained from an annihilation operator:
```wl
ndx2op[op2ndx[c[AN, DO]]]
```

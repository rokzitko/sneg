---
keywords: basis, combine bases, subspace-wise operation, symmetrization
see also: orthogbasisvc, orthogbasisop, applybasis, transformtoLR, mergebasis
---
# mapbasis

## Usage
- `mapbasis[bz1, bz2, f]` gives the basis with the quantum numbers of *bz1* and the states `f[states1, states2]` of corresponding subspaces of *bz1* and *bz2*.
- `mapbasis[bz1, bz2]` uses `f = (states1 + states2)/2`.

## Details
- *bz1* and *bz2* must have identical lists of quantum numbers, in the same order. Otherwise, the message `mapbasis::compat` is issued and `Null` is returned.
- *f* receives the two lists of states of a subspace and must return a list of states.
- `mapbasis` is used to construct parity-adapted bases in `transformtoLR`: with the mirror-transformed basis $P\,bz$, the combinations $(bz + P\,bz)/2$ and $(bz - P\,bz)/2$ contain the even and odd states, which are then orthonormalized with `orthogbasisop`.

## Examples
### Basic Examples
```wl
mapbasis[{{{0}, {a, b}}, {{1}, {c}}}, {{{0}, {x, y}}, {{1}, {z}}}]
```
```wl
mapbasis[{{{0}, {a, b}}, {{1}, {c}}}, {{{0}, {x, y}}, {{1}, {z}}}, Subtract]
```

### Applications
States of two levels that are even and odd under the exchange $c_1 \leftrightarrow c_2$:
```wl
snegfermionoperators[c];
bz = qszbasis[{c[1], c[2]}];
swapped = bz /. {c[t_, 1, s_] :> c[t, 2, s], c[t_, 2, s_] :> c[t, 1, s]};
even = dropemptysubspaces[orthogbasisop[mapbasis[bz, swapped], bz]];
Select[even, First[#] == {-1, 1/2} &]
```

### Possible Issues
The quantum numbers of the two bases must agree:
```wl
mapbasis[{{{0}, {a}}}, {{{1}, {x}}}]
```

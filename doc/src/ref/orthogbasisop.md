---
keywords: orthogonalization, basis, invariant subspaces, creation operator representation
see also: orthogbasisvc, orthogop, mapbasis, transformtoLR, dropemptysubspaces
---
# orthogbasisop

## Usage
- `orthogbasisop[bz1, bz]` orthonormalizes the states of every subspace of the basis *bz1* with respect to the states of the corresponding subspace of the basis *bz*; both bases are in the creation operator representation.

## Details
- `orthogbasisop[bz1, bz]` is `mapbasis[bz1, bz, orthogop]`; see `orthogbasisvc` for the conventions. The subspaces of *bz1* and *bz* must have the same quantum numbers in the same order.
- `transformtoLR` uses `orthogbasisop` to orthonormalize the symmetric and antisymmetric combinations of the basis states under the mirror symmetry.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
bz = qszbasis[{d[]}];
orthogbasisop[applybasis[bz, 3 # &], bz]
```

### Applications
States of two levels that are odd under the exchange $c_1 \leftrightarrow c_2$:
```wl
snegfermionoperators[c];
bz = qszbasis[{c[1], c[2]}];
swapped = bz /. {c[t_, 1, s_] :> c[t, 2, s], c[t_, 2, s_] :> c[t, 1, s]};
odd = dropemptysubspaces[orthogbasisop[mapbasis[bz, swapped, Subtract], bz]];
{First[#], Length[Last[#]]} & /@ odd
```

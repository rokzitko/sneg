---
keywords: tensor product, product basis, composite system, additive quantum numbers, symmetry-adapted basis, spin kets
see also: directproductbasis, SU2basistensorproduct, ketbratensorproduct, mergebasis, quickDBL, spinbasis, qszbasis
---
# basistensorproduct

## Usage
- `basistensorproduct[bz1, bz2, f]` gives the tensor product of the bases *bz1* and *bz2*: the products `nc[s1, s2]` of all states, with the quantum numbers `f[qn1, qn2]`; subspaces with equal quantum numbers are merged.
- `basistensorproduct[bz1, bz2]` uses `f = List`.

## Details
- *bz1* and *bz2* are bases `{{qn, states}, ...}` in the creation operator representation (operator strings and kets). The state of the first basis stands to the left in the product.
- *f* combines the quantum number lists of the two subspaces. For additive quantum numbers such as $(Q, S_z)$, `f = Plus` adds them componentwise. The default `List` gives the nested quantum numbers `{qn1, qn2}`.
- The result is merged and sorted with `mergebasis`. `directproductbasis` is the same construction without merging.
- Product bases are used to add non-fermionic degrees of freedom (spin kets, phonon kets) to fermionic bases, and to build bases of large systems from bases of subsystems. For SU(2) quantum numbers, the product of highest-weight states is not a highest-weight basis of the total spin; use `SU2basistensorproduct`, which couples the multiplets with Clebsch-Gordan coefficients.
- `basistensorproduct` does not call `makebasis`. Before converting the result to the occupation number representation, define `BASIS` for all levels.

## Examples
### Basic Examples
The $(Q, S_z)$ basis of two levels from the bases of single levels:
```wl
snegfermionoperators[c];
bz = basistensorproduct[qszbasis[{c[1]}], qszbasis[{c[2]}], Plus]
```
It coincides with `qszbasisvc` of the two levels:
```wl
makebasis[{c[1], c[2]}];
bzop2bzvc[bz] === qszbasisvc[{c[1], c[2]}]
```

### Scope
The default combination function gives nested quantum numbers:
```wl
snegfermionoperators[c];
basistensorproduct[qbasis[{c[1]}], qbasis[{c[2]}]] // Length
```
---
Product basis of two spins 1/2 labeled by the total $S_z$:
```wl
basistensorproduct[spinbasis[1/2, {1, 0}], spinbasis[1/2, {0, 1}], Plus]
```

### Applications
Anderson impurity with an additional local spin 1/2 coupled by the exchange interaction $J\, \mathbf S \cdot \mathbf s_d$: the fermionic $(Q, S_z)$ basis is extended with spin kets, and $S_z$ is replaced by the total $S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[J];
bz = basistensorproduct[qszbasis[{d[]}], spinbasis[1/2], {#1[[1]], #1[[2]] + #2[[1]]} &];
makebasis[{d[]}];
H = J Expand[spinxyz[d[]] ~inner~ spinketbraXYZ[1/2]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bzop2bzvc[bz]]
```

### Properties & Relations
`directproductbasis` gives the same states without merging the subspaces:
```wl
snegfermionoperators[c];
Length /@ {directproductbasis[qbasis[{c[1]}], qbasis[{c[2]}], Plus], basistensorproduct[qbasis[{c[1]}], qbasis[{c[2]}], Plus]}
```

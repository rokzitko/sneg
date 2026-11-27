---
keywords: pair hopping, two-electron hopping, pair tunneling, isospin exchange, negative-U, Josephson coupling
see also: hop, chargecharge, isospinx, bcs, anhop
related links: [A. Horvat, R. Žitko, and J. Mravlje, Phys. Rev. B 94, 165140 (2016) (three-orbital Kanamori impurity)](https://doi.org/10.1103/PhysRevB.94.165140)
---
# twohop

## Usage
- `twohop[a[i], b[j]]` gives the two-electron (pair) hopping operator $a^\dagger_{i\downarrow} a^\dagger_{i\uparrow} b_{j\downarrow} b_{j\uparrow} + b^\dagger_{j\downarrow} b^\dagger_{j\uparrow} a_{i\downarrow} a_{i\uparrow}$.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes.
- With the pair operators $B^\dagger_i = a^\dagger_{i\uparrow} a^\dagger_{i\downarrow}$ (the doubly occupied state $|2\rangle = B^\dagger|0\rangle$, as in `bcs`), `twohop[a, b]` is $-(B_a^\dagger B_b + B_b^\dagger B_a)$. The matrix element between the doubly occupied states $|2_a 0_b\rangle$ and $|0_a 2_b\rangle$ is $-1$ in this convention.
- In terms of the isospin operators, `twohop[a, b]` is $-2 (I^x_a I^x_b + I^y_a I^y_b)$, the transverse part of an isospin exchange (with the isospin phases $n = 0$ for both levels).
- The pair hopping conserves the total charge and the total spin. It arises, e.g., in second order in the hopping between negative-$U$ centers and in multi-orbital Kanamori Hamiltonians.
- `twohop` is `Listable`. `SnegSimplify` recognizes the pair-hopping operator.

## Examples
### Basic Examples
```wl
snegfermionoperators[a, b];
twohop[a[], b[]]
```
Matrix element between the doubly occupied states $a^\dagger_\uparrow a^\dagger_\downarrow|0\rangle$ and $b^\dagger_\uparrow b^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[twohop[a[], b[]], {nc[a[CR, UP], a[CR, DO]], nc[b[CR, UP], b[CR, DO]]}]
```

### Properties & Relations
Relation with the isospin exchange:
```wl
snegfermionoperators[a, b];
Expand[twohop[a[], b[]] + 2 (nc[isospinx[a[]], isospinx[b[]]] + nc[isospiny[a[]], isospiny[b[]]])]
```
---
The pair hopping conserves the charge and the spin:
```wl
{Sx, Sy, Sz, S2} = manyspin[{a[], b[]}];
Expand[{komutator[twohop[a[], b[]], number[a[]] + number[b[]]], komutator[twohop[a[], b[]], Sx], komutator[twohop[a[], b[]], Sz]}]
```
---
`SnegSimplify` recognizes the operator:
```wl
SnegSimplify[Expand[2 twohop[a[], b[]]]]
```

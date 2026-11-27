---
keywords: anomalous hopping, pairing between levels, triplet pairing, pair creation, superconducting proximity, Bogoliubov-de Gennes
see also: anhop, genanhop, bcs, holehop, hop
---
# anomaloushop

## Usage
- `anomaloushop[a[i], b[j]]` gives the anomalous hopping operator $\sum_\sigma (a^\dagger_{i\sigma} b^\dagger_{j\bar\sigma} + b_{j\bar\sigma} a_{i\sigma}) = a^\dagger_{i\uparrow} b^\dagger_{j\downarrow} + a^\dagger_{i\downarrow} b^\dagger_{j\uparrow} + \mathrm{h.c.}$
- `anomaloushop[a[i], b[j], σ]` gives $a^\dagger_{i\sigma} b^\dagger_{j\bar\sigma} + b_{j\bar\sigma} a_{i\sigma}$.
- `anomaloushop[fn1, fn2]` gives the anomalous hopping between abstract orbitals given as pure functions.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes; $\bar\sigma$ is the spin opposite to $\sigma$.
- The pair $a^\dagger_\uparrow b^\dagger_\downarrow + a^\dagger_\downarrow b^\dagger_\uparrow$ created by `anomaloushop` is the $S_z = 0$ component of a spin triplet. The operator commutes with the total $S^z$ but not with $S^x$, $S^y$. For singlet pairing $a^\dagger_\uparrow b^\dagger_\downarrow - a^\dagger_\downarrow b^\dagger_\uparrow + \mathrm{h.c.}$, which conserves the total spin, use `anhop`.
- `anomaloushop[a[i], a[i]]` vanishes. Local (on-site) pairing is given by `bcs`.
- `anomaloushop[p, h]` equals `holehop[h, p]`: it is the hopping between a particle level `p` and a level `h` described by hole operators.
- Abstract orbitals: `fn` is a pure function of the type and spin indexes such that `fn[CR, σ]` is the creation operator of the orbital. In the two-argument form, one argument may be an ordinary level.
- `anomaloushop` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[a, b];
anomaloushop[a[], b[]]
```
Acting on the vacuum, it creates the $S_z = 0$ triplet pair:
```wl
matrixrepresentationop[anomaloushop[a[], b[]], {1, nc[a[CR, UP], b[CR, DO]], nc[a[CR, DO], b[CR, UP]]}]
```

### Scope
Only one spin component:
```wl
snegfermionoperators[a, b];
anomaloushop[a[], b[], UP]
```

### Properties & Relations
`anomaloushop` commutes with the total $S^z$ but not with $S^x$:
```wl
snegfermionoperators[a, b];
{Sx, Sy, Sz, S2} = manyspin[{a[], b[]}];
Expand[{komutator[anomaloushop[a[], b[]], Sz], komutator[anomaloushop[a[], b[]], Sx]}]
```
---
On a single level, the triplet pair vanishes:
```wl
anomaloushop[a[], a[]]
```
---
Relation with the singlet pairing `anhop`, which has the opposite sign of the $\downarrow\uparrow$ term:
```wl
Expand[anomaloushop[a[], b[], UP] - anomaloushop[a[], b[], DO] - anhop[a[], b[]]]
```

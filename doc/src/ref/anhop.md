---
keywords: anomalous hopping, singlet pairing, nonlocal pairing, crossed Andreev reflection, superconducting proximity effect, Cooper pair splitting, Wilson chain with pairing
see also: genanhop, anomaloushop, bcs, hop, twohop
---
# anhop

## Usage
- `anhop[a[i], b[j]]` gives the singlet anomalous hopping operator $a^\dagger_{i\uparrow} b^\dagger_{j\downarrow} - a^\dagger_{i\downarrow} b^\dagger_{j\uparrow} + \mathrm{h.c.}$
- `anhop[a[i], b[j], σ]` gives $a^\dagger_{i\sigma} b^\dagger_{j\bar\sigma} + b_{j\bar\sigma} a_{i\sigma}$, the same as `anomaloushop[a[i], b[j], σ]`.
- `anhop[fn1, fn2]` gives the singlet anomalous hopping between abstract orbitals given as pure functions.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes; $\bar\sigma$ is the spin opposite to $\sigma$.
- `anhop[a, b]` is `anhop[a, b, UP] - anhop[a, b, DO]`. The pair $a^\dagger_\uparrow b^\dagger_\downarrow - a^\dagger_\downarrow b^\dagger_\uparrow$ is a spin singlet, so `anhop` commutes with all components of the total spin. It differs from `anomaloushop` (triplet, $S_z = 0$) by the sign of the $\sigma = \downarrow$ term.
- `anhop[a, b]` is symmetric in its arguments, and `anhop[a, a]` is `2 bcs[a]`.
- With a complex amplitude, use `genanhop[t, a, b]`; `anhop[a, b]` is `genanhop[1, a, b]`.
- Abstract orbitals: `fn` is a pure function of the type and spin indexes such that `fn[CR, σ]` is the creation operator of the orbital. In the two-argument form, one argument may be an ordinary level.
- `anhop` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[a, b];
anhop[a[], b[]]
```
Acting on the vacuum, it creates the singlet pair $(a^\dagger_\uparrow b^\dagger_\downarrow - a^\dagger_\downarrow b^\dagger_\uparrow)|0\rangle$:
```wl
matrixrepresentationop[anhop[a[], b[]], {1, nc[a[CR, UP], b[CR, DO]], nc[a[CR, DO], b[CR, UP]]}]
```

### Scope
Pairing between neighboring sites of a chain, as in a Wilson chain of a superconductor with nonlocal pairing:
```wl
snegfermionoperators[f];
snegrealconstants[kappa];
kappa Total[anhop[{f[0], f[1]}, {f[1], f[2]}]]
```

### Properties & Relations
`anhop` conserves the total spin:
```wl
snegfermionoperators[a, b];
{Sx, Sy, Sz, S2} = manyspin[{a[], b[]}];
H = anhop[a[], b[]];
Expand[{komutator[H, Sx], komutator[H, Sy], komutator[H, Sz]}]
```
It changes the charge by $\pm 2$, $[N, H] = 2(P^\dagger - P)$ with the pair creation part $P^\dagger$:
```wl
Expand[komutator[number[a[]] + number[b[]], H]]
```
---
Symmetry in the arguments and the on-site limit:
```wl
{Expand[anhop[a[], b[]] - anhop[b[], a[]]], Expand[anhop[a[], a[]] - 2 bcs[a[]]]}
```
---
Relation with `genanhop`:
```wl
anhop[a[], b[]] == genanhop[1, a[], b[]]
```

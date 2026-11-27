---
keywords: spin-flip hopping, spin-orbit coupling, spin-nonconserving tunneling, sigma_x hopping
see also: hop, spinfliphopphi, matrixhop, PauliX, manyspin, spinx
---
# spinfliphop

## Usage
- `spinfliphop[a[i], b[j]]` gives the spin-flip hopping operator $\sum_\sigma (a^\dagger_{i\sigma} b_{j\bar\sigma} + b^\dagger_{j\sigma} a_{i\bar\sigma})$, where $\bar\sigma$ is the spin opposite to $\sigma$.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes.
- In matrix notation, `spinfliphop[a, b]` is $a^\dagger \sigma_x b + b^\dagger \sigma_x a$ with the Pauli matrix $\sigma_x$; it equals `matrixhop[PauliX, a, b]`. Other spin-dependent hopping matrices, e.g. $\sigma_y$ for Rashba-type spin-orbit coupling, are given by `matrixhop`.
- The spin-flip hopping breaks the spin SU(2) symmetry. It conserves the charge and commutes with the total $S^x$ of the two levels.
- `spinfliphop[a[i], a[i]]` is $4 S^x_i$.
- `spinfliphop` is `Listable`. For a spin-flip hopping with a phase, see `spinfliphopphi`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
spinfliphop[c[], d[]]
```

### Scope
`spinfliphop` threads over lists of levels:
```wl
snegfermionoperators[c];
Total[spinfliphop[{c[1], c[2]}, {c[2], c[3]}]]
```

### Applications
A dimer with spin-conserving hopping $t$ and spin-flip hopping $\lambda$. The single-particle levels are $\pm t \pm \lambda$, since $t + \lambda\sigma_x$ has the eigenvalues $t \pm \lambda$:
```wl
snegfermionoperators[c];
snegrealconstants[t, lambda];
H = -t hop[c[1], c[2]] - lambda spinfliphop[c[1], c[2]];
Eigenvalues[matrixrepresentationop[H, {c[CR, 1, UP], c[CR, 1, DO], c[CR, 2, UP], c[CR, 2, DO]}]]
```

### Properties & Relations
The spin-flip hopping commutes with the total $S^x$, but not with $S^y$ and $S^z$:
```wl
snegfermionoperators[c, d];
{Sx, Sy, Sz, S2} = manyspin[{c[], d[]}];
H = spinfliphop[c[], d[]];
Expand[{komutator[H, Sx], komutator[H, Sz]}]
```
---
Relation with `matrixhop` and with the on-site spin:
```wl
{spinfliphop[c[], d[]] == matrixhop[PauliX, c[], d[]], Expand[spinfliphop[c[], c[]] - 4 spinx[c[]]]}
```

---
keywords: projector, empty state, zero occupancy, hole
see also: projector, PROJ0, projectorUP, projectorDO, projector2, projector1, projector02
---
# projector0

## Usage
- `projector0[c[i]]` gives the projector $P_0 = (1 - n_{i\uparrow})(1 - n_{i\downarrow})$ onto the empty state of the level `c[i]`.
- `projector0[c]` is `projector0[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projector0[c[i]]` is `projector[c[i], PROJ0]`. See `projector` for the complete set of local projectors and their relations.
- In normal-ordered form, $P_0 = 1 - n + n_\uparrow n_\downarrow$.
- `projector0` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projector0[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projector0[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projector0` threads over lists of levels:
```wl
snegfermionoperators[c];
projector0[{c[1], c[2]}] == {projector0[c[1]], projector0[c[2]]}
```

### Applications
In the limit $U \to \infty$ of the Hubbard model, double occupancy is excluded. The hopping projected onto the subspace without double occupancy of the two sites, $P\, \mathrm{hop}\, P$ with $P = (1 - P_2(1))(1 - P_2(2))$, contains the factors $(1 - n_{i,-\sigma})$ of the $t$-$J$ model; for spin up, the projected hopping is $c^\dagger_{1\uparrow} c_{2\uparrow}$ multiplied by the projectors onto the empty or spin-up states of the other spin:
```wl
snegfermionoperators[c];
P = nc[1 - projector2[c[1]], 1 - projector2[c[2]]];
Expand[nc[P, hop[c[1], c[2], UP], P] - (nc[projector0[c[1]] + projectorUP[c[1]], c[CR, 1, UP], c[AN, 2, UP], projector0[c[2]] + projectorUP[c[2]]] + nc[projector0[c[2]] + projectorUP[c[2]], c[CR, 2, UP], c[AN, 1, UP], projector0[c[1]] + projectorUP[c[1]]])]
```

### Properties & Relations
In normal-ordered form, $P_0 = 1 - n + n_\uparrow n_\downarrow$:
```wl
snegfermionoperators[c];
Expand[projector0[c[]] - (1 - number[c[]] + hubbard[c[]])]
```
---
`projector0` is idempotent:
```wl
Expand[nc[projector0[c[]], projector0[c[]]] - projector0[c[]]]
```

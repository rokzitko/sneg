---
keywords: projector, empty or doubly occupied, charge fluctuation, isospin, even occupancy
see also: projector, PROJ02, projector0, projectorUP, projectorDO, projector2, projector1
---
# projector02

## Usage
- `projector02[c[i]]` gives the projector $P_{02} = P_0 + P_2 = (n_i - 1)^2$ onto the empty and the doubly occupied states of the level `c[i]`.
- `projector02[c]` is `projector02[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projector02[c[i]]` is `projector[c[i], PROJ02]`. See `projector` for the complete set of local projectors and their relations.
- $P_{02} = 1 - P_1$ is the projector onto the states with the isospin $1/2$ (charge doublet) of the level; it equals `isozsq[c[i]]`.
- `projector02` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projector02[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projector02[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projector02` threads over lists of levels:
```wl
snegfermionoperators[c];
projector02[{c[1], c[2]}] == {projector02[c[1]], projector02[c[2]]}
```

### Properties & Relations
Relation with `projector1` and `isozsq`:
```wl
snegfermionoperators[c];
Expand[{projector02[c[]] + projector1[c[]] - 1, projector02[c[]] - isozsq[c[]]}]
```
---
`projector02` is idempotent:
```wl
Expand[nc[projector02[c[]], projector02[c[]]] - projector02[c[]]]
```

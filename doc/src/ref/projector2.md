---
keywords: projector, double occupancy, doubly occupied state, Hubbard interaction
see also: projector, PROJ2, projector0, projectorUP, projectorDO, projector1, projector02
---
# projector2

## Usage
- `projector2[c[i]]` gives the projector $P_2 = n_{i\uparrow} n_{i\downarrow}$ onto the doubly occupied state of the level `c[i]`.
- `projector2[c]` is `projector2[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projector2[c[i]]` is `projector[c[i], PROJ2]`. See `projector` for the complete set of local projectors and their relations.
- $P_2$ equals the Hubbard interaction operator `hubbard[c[i]]`.
- `projector2` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projector2[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projector2[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projector2` threads over lists of levels:
```wl
snegfermionoperators[c];
projector2[{c[1], c[2]}] == {projector2[c[1]], projector2[c[2]]}
```

### Properties & Relations
$P_2$ is the Hubbard interaction:
```wl
snegfermionoperators[c];
projector2[c[]] == hubbard[c[]]
```
---
`projector2` is idempotent:
```wl
Expand[nc[projector2[c[]], projector2[c[]]] - projector2[c[]]]
```

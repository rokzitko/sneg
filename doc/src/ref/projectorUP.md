---
keywords: projector, spin up, single occupancy, local moment
see also: projector, PROJUP, projector0, projectorDO, projector2, projector1, projector02
---
# projectorUP

## Usage
- `projectorUP[c[i]]` gives the projector $P_\uparrow = n_{i\uparrow}(1 - n_{i\downarrow})$ onto the singly occupied state with spin up of the level `c[i]`.
- `projectorUP[c]` is `projectorUP[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projectorUP[c[i]]` is `projector[c[i], PROJUP]`. See `projector` for the complete set of local projectors and their relations.
- $P_\uparrow - P_\downarrow = 2 S^z$ and $P_\uparrow + P_\downarrow = P_1$ (`projector1`).
- `projectorUP` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projectorUP[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorUP[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projectorUP` threads over lists of levels:
```wl
snegfermionoperators[c];
projectorUP[{c[1], c[2]}] == {projectorUP[c[1]], projectorUP[c[2]]}
```

### Properties & Relations
Relation with the spin and with `projector1`:
```wl
snegfermionoperators[c];
Expand[{projectorUP[c[]] - projectorDO[c[]] - 2 spinz[c[]], projectorUP[c[]] + projectorDO[c[]] - projector1[c[]]}]
```
---
`projectorUP` is idempotent:
```wl
Expand[nc[projectorUP[c[]], projectorUP[c[]]] - projectorUP[c[]]]
```

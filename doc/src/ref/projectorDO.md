---
keywords: projector, spin down, single occupancy, local moment
see also: projector, PROJDO, projector0, projectorUP, projector2, projector1, projector02
---
# projectorDO

## Usage
- `projectorDO[c[i]]` gives the projector $P_\downarrow = n_{i\downarrow}(1 - n_{i\uparrow})$ onto the singly occupied state with spin down of the level `c[i]`.
- `projectorDO[c]` is `projectorDO[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes; other levels are not affected.
- `projectorDO[c[i]]` is `projector[c[i], PROJDO]`. See `projector` for the complete set of local projectors and their relations.
- $P_\uparrow - P_\downarrow = 2 S^z$ and $P_\uparrow + P_\downarrow = P_1$ (`projector1`).
- `projectorDO` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projectorDO[c[]]
```
The matrix in the basis $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, $c^\dagger_\downarrow|0\rangle$, $c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorDO[c[]], {1, c[CR, UP], c[CR, DO], nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
`projectorDO` threads over lists of levels:
```wl
snegfermionoperators[c];
projectorDO[{c[1], c[2]}] == {projectorDO[c[1]], projectorDO[c[2]]}
```

### Properties & Relations
The spin-flip operator $S^-$ maps the spin-up onto the spin-down state, $S^- P_\uparrow = P_\downarrow S^-$:
```wl
snegfermionoperators[c];
Expand[nc[spinminus[c[]], projectorUP[c[]]] - nc[projectorDO[c[]], spinminus[c[]]]]
```
---
`projectorDO` is idempotent:
```wl
Expand[nc[projectorDO[c[]], projectorDO[c[]]] - projectorDO[c[]]]
```

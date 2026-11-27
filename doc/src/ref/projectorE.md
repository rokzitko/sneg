---
keywords: projector, BCS state, superposition of empty and doubly occupied states, zero-bandwidth model
see also: projectorEphi, projectorO, projector02, projector, bcs
---
# projectorE

## Usage
- `projectorE[c[i]]` gives the projector onto the state $(|0\rangle + |2\rangle)/\sqrt2$ of the level `c[i]`, with $|2\rangle = c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}|0\rangle$.
- `projectorE[c]` is `projectorE[c[]]`.

## Details
- The operator is $P_E = \frac12 [P_0 + P_2 + c_\downarrow c_\uparrow + c^\dagger_\uparrow c^\dagger_\downarrow] = [P_0 + P_2 + \mathrm{bcs}(c)]/2$, with $P_0$ and $P_2$ the projectors onto the empty and doubly occupied states.
- $(|0\rangle + |2\rangle)/\sqrt2$ is the ground state of the local pairing term $-\Delta\, \mathrm{bcs}(c)$ with $\Delta > 0$.
- `projectorE[c[i]]` is `projectorEphi[c[i], 0]`; `projectorO` is the projector onto the orthogonal even state. See `projectorEphi` for details.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projectorE[c[]]
```
Its matrix in the basis $|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorE[c[]], {1, nc[c[CR, UP], c[CR, DO]]}]
```

### Properties & Relations
Relation with the pairing operator and with `projectorO`:
```wl
snegfermionoperators[c];
Expand[{projectorE[c[]] - (projector02[c[]] + bcs[c[]])/2, projectorE[c[]] + projectorO[c[]] - projector02[c[]]}]
```

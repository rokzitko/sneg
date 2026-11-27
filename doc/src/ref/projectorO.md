---
keywords: projector, superposition of empty and doubly occupied states, zero-bandwidth model
see also: projectorOphi, projectorE, projector02, projector, bcs
---
# projectorO

## Usage
- `projectorO[c[i]]` gives the projector onto the state $(|0\rangle - |2\rangle)/\sqrt2$ of the level `c[i]`, with $|2\rangle = c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}|0\rangle$.
- `projectorO[c]` is `projectorO[c[]]`.

## Details
- The operator is $P_O = \frac12 [P_0 + P_2 - c_\downarrow c_\uparrow - c^\dagger_\uparrow c^\dagger_\downarrow] = [P_0 + P_2 - \mathrm{bcs}(c)]/2$.
- `projectorO[c[i]]` is `projectorOphi[c[i], 0]`, and $P_E + P_O = P_0 + P_2$ with $P_E$ = `projectorE[c[i]]`. See `projectorEphi` for details.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
projectorO[c[]]
```
Its matrix in the basis $|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorO[c[]], {1, nc[c[CR, UP], c[CR, DO]]}]
```

### Properties & Relations
`projectorO` is orthogonal to `projectorE`:
```wl
snegfermionoperators[c];
Expand[nc[projectorO[c[]], projectorE[c[]]]]
```

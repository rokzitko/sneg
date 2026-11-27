---
keywords: projector, BCS state, superposition of empty and doubly occupied states, superconducting phase, zero-bandwidth model
see also: projectorEphi, projectorO, projectorE, projector02, bcs
---
# projectorOphi

## Usage
- `projectorOphi[c[i], φ]` gives the projector $|O_\phi\rangle\langle O_\phi|$ onto the state $|O_\phi\rangle = (|0\rangle - e^{-i\phi}|2\rangle)/\sqrt2$ of the level `c[i]`, with $|2\rangle = c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}|0\rangle$.
- `projectorOphi[c, φ]` is `projectorOphi[c[], φ]`.

## Details
- The operator is $P_O(\phi) = \frac12 [P_0 + P_2 - e^{i\phi} c_\downarrow c_\uparrow - e^{-i\phi} c^\dagger_\uparrow c^\dagger_\downarrow] = [P_0 + P_2 - \mathrm{bcs}(c, -\phi)]/2$.
- It is the complement of `projectorEphi` in the even subspace of the level: $P_E(\phi) + P_O(\phi) = P_0 + P_2$. $|O_{-\Phi}\rangle$ is the excited even state of the local pairing term $-\Delta\, \mathrm{bcs}(c, \Phi)$, at the energy $+\Delta$.
- `projectorOphi[c[i], 0]` is `projectorO[c[i]]`. See `projectorEphi` for details and applications.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[phi];
projectorOphi[c[], phi]
```
Its matrix in the basis $|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorOphi[c[], phi], {1, nc[c[CR, UP], c[CR, DO]]}]
```

### Properties & Relations
$P_O$ is idempotent and complements $P_E$ in the even subspace:
```wl
snegfermionoperators[c];
snegrealconstants[phi];
PO = projectorOphi[c[], phi];
Expand[{nc[PO, PO] - PO, PO + projectorEphi[c[], phi] - projector02[c[]]}]
```

---
keywords: projector, BCS state, pair coherent state, superposition of empty and doubly occupied states, superconducting phase, zero-bandwidth model, atomic limit of superconductor
see also: projectorOphi, projectorE, projectorO, projector02, projector, bcs, hopphi, isospinx
related links: [Á. Bácsi, T. Iličin, and R. Žitko, Phys. Rev. B 113, 064501 (2026) (Schrieffer-Wolff approach to Josephson junctions)](https://doi.org/10.1103/jfcl-7pzt)
---
# projectorEphi

## Usage
- `projectorEphi[c[i], φ]` gives the projector $|E_\phi\rangle\langle E_\phi|$ onto the state $|E_\phi\rangle = (|0\rangle + e^{-i\phi}|2\rangle)/\sqrt2$ of the level `c[i]`, with $|2\rangle = c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}|0\rangle$.
- `projectorEphi[c, φ]` is `projectorEphi[c[], φ]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes. $|0\rangle$ and $|2\rangle$ are the empty and the doubly occupied states of the level; the other levels are not affected.
- The operator is
$$P_E(\phi) = \frac12 [P_0 + P_2 + e^{i\phi} c_\downarrow c_\uparrow + e^{-i\phi} c^\dagger_\uparrow c^\dagger_\downarrow],$$
with the projectors $P_0 = (1 - n_\uparrow)(1 - n_\downarrow)$ and $P_2 = n_\uparrow n_\downarrow$ (`projector0`, `projector2`).
- `projectorOphi[c[i], φ]` is the projector onto the orthogonal state $|O_\phi\rangle = (|0\rangle - e^{-i\phi}|2\rangle)/\sqrt2$; $P_E + P_O = P_0 + P_2$ (`projector02`).
- In terms of the pairing operator, $P_{E,O}(\phi) = [P_0 + P_2 \pm \mathrm{bcs}(c, -\phi)]/2$. $|E_\phi\rangle$ is therefore the ground state of the local pairing term $-\Delta\, \mathrm{bcs}(c, \Phi)$ ($\Delta > 0$) for $\phi = -\Phi$, i.e. the BCS state $(|0\rangle + e^{i\Phi}|2\rangle)/\sqrt2$ of a superconductor with the phase $\Phi$ in the zero-bandwidth (atomic) limit, and $|O_\phi\rangle$ is the excited even state at the energy $+\Delta$.
- `projectorEphi[c[i], 0]` is `projectorE[c[i]]`.
- `projectorEphi` is not `Listable`.

## Examples
### Basic Examples
The projector onto $(|0\rangle + e^{-i\phi}|2\rangle)/\sqrt2$:
```wl
snegfermionoperators[c];
snegrealconstants[phi];
projectorEphi[c[], phi]
```
Its matrix in the basis $|0\rangle$, $|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$:
```wl
matrixrepresentationop[projectorEphi[c[], phi], {1, nc[c[CR, UP], c[CR, DO]]}]
```

### Scope
A bare symbol is a level without indexes:
```wl
snegfermionoperators[f];
snegrealconstants[phi];
projectorEphi[f, phi] === projectorEphi[f[], phi]
```

### Applications
Ground state of a superconducting level in the zero-bandwidth limit. For $H = -\Delta\, \mathrm{bcs}(f, \Phi)$, $H P_E(-\Phi) = -\Delta P_E(-\Phi)$ and $H P_O(-\Phi) = +\Delta P_O(-\Phi)$:
```wl
snegfermionoperators[f];
snegrealconstants[Delta, Phi];
H = -Delta bcs[f[], Phi];
Expand[{nc[H, projectorEphi[f[], -Phi]] + Delta projectorEphi[f[], -Phi], nc[H, projectorOphi[f[], -Phi]] - Delta projectorOphi[f[], -Phi]}]
```
---
Spectral decomposition of the local pairing term in the even subspace, $\mathrm{bcs}(c, \Phi) = P_E(-\Phi) - P_O(-\Phi)$:
```wl
snegfermionoperators[c];
snegrealconstants[Phi];
Expand[bcs[c[], Phi] - (projectorEphi[c[], -Phi] - projectorOphi[c[], -Phi])]
```

### Properties & Relations
$P_E$ is an idempotent Hermitian operator, orthogonal to $P_O$, and $P_E + P_O = P_0 + P_2$:
```wl
snegfermionoperators[c];
snegrealconstants[phi];
PE = projectorEphi[c[], phi];
PO = projectorOphi[c[], phi];
Expand[{nc[PE, PE] - PE, conj[PE] - PE, nc[PE, PO], PE + PO - projector02[c[]]}]
```
---
Relation with the pairing operator:
```wl
Expand[{PE - (projector02[c[]] + bcs[c[], -phi])/2, PO - (projector02[c[]] - bcs[c[], -phi])/2}]
```
---
At zero phase:
```wl
{projectorEphi[c[], 0] == projectorE[c[]], projectorOphi[c[], 0] == projectorO[c[]]}
```

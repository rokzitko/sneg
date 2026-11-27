---
keywords: symmetry-adapted basis, spin projection, U(1) spin symmetry, occupation number representation
see also: szbasis, qszbasisvc, sbasisvc, qbasisvc, makematricesbzvc
---
# szbasisvc

## Usage
- `szbasisvc[{c[1], c[2], ...}]` gives the basis of the Fock space of the levels `c[1]`, `c[2]`, ... in the occupation number representation, organized in subspaces with well-defined spin projection $S_z$.

## Details
- `szbasisvc` constructs the $(Q, S_z)$ basis with `qszbasisvc`, drops $Q$, and merges the subspaces with equal $S_z$. The result is `{{{Sz}, {states}}, ...}`.
- The basis is appropriate for Hamiltonians that conserve $S_z$ but not the particle number, e.g. with superconducting pairing and a magnetic field along $z$.
- `szbasis` gives the same basis in the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
szbasisvc[{d[]}] // InputForm
```

### Applications
Superconducting level in a magnetic field, $H = \epsilon\, n + \Delta (d^\dagger_\uparrow d^\dagger_\downarrow + \mathrm{h.c.}) - B S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[eps, Delta, B];
H = eps number[d[]] + Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]) - B spinz[d[]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, szbasisvc[{d[]}]]
```

### Properties & Relations
Dimensions for two levels:
```wl
snegfermionoperators[c];
{First[#], Length[Last[#]]} & /@ szbasisvc[{c[1], c[2]}]
```

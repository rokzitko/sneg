---
keywords: spin-polarized basis, spinless fermions, projection, spin-up subspace
see also: qbasis, qszbasis, mergebasis, dropemptysubspaces, transformbasis
---
# spinlessbasis

## Usage
- `spinlessbasis[bz]` projects the basis *bz* in the creation operator representation onto the states without spin-down electrons.

## Details
- All creation operators with the spin index `DO` are set to 0; the states that vanish are removed, subspaces with equal quantum numbers are merged, and empty subspaces are dropped.
- The quantum numbers are not changed: $Q$ remains the charge relative to half filling of the spinful levels, $Q = N - N_\mathrm{levels}$.
- The result describes fully spin-polarized electrons, i.e. effectively spinless fermions represented by the spin-up operators.
- *bz* must be in the creation operator representation (e.g. from `qbasis` or `qszbasis`).

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinlessbasis[qbasis[{c[1], c[2]}]]
```

### Applications
Spin-polarized electrons on two sites with hopping and nearest-neighbor repulsion $V n_1 n_2$:
```wl
snegfermionoperators[c];
snegrealconstants[t, V];
bz = spinlessbasis[qbasis[{c[1], c[2]}]];
H = -t hop[c[1], c[2]] + V nc[number[c[1]], number[c[2]]];
makematricesbzop[H, bz]
```

### Properties & Relations
For a $(Q, S_z)$ basis, only the subspaces with $S_z = (N_\mathrm{levels} + Q)/2$ remain:
```wl
snegfermionoperators[c];
spinlessbasis[qszbasis[{c[1], c[2]}]]
```

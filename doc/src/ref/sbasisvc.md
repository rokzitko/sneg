---
keywords: symmetry-adapted basis, total spin, SU(2) symmetry, spin multiplet, occupation number representation
see also: sbasis, qsbasisvc, szbasisvc, qbasisvc, makematricesbzvc
---
# sbasisvc

## Usage
- `sbasisvc[{c[1], c[2], ...}]` gives a basis of the Fock space of the spin-1/2 levels `c[1]`, `c[2]`, ... in the occupation number representation, organized in subspaces with well-defined total spin $S$; each subspace contains the states with $S_z = S$.

## Details
- `sbasisvc` constructs the $(Q, S)$ basis with `qsbasisvc`, drops $Q$, and merges the subspaces with equal $S$. The result is `{{{S}, {states}}, ...}`; each state represents a multiplet of $2S+1$ states.
- The basis is appropriate for spin-symmetric Hamiltonians that do not conserve the particle number, e.g. with superconducting pairing. The operators must be spin-1/2 fermions; see `qsbasisvc` for the conventions.
- `sbasisvc[{}]` is `{{{0}, {vc[]}}}`. `sbasis` gives the same basis in the creation operator representation.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
sbasisvc[{d[]}] // InputForm
```

### Applications
Superconducting impurity: a level with pairing, $H = \epsilon\, n + U n_\uparrow n_\downarrow + \Delta (d^\dagger_\uparrow d^\dagger_\downarrow + \mathrm{h.c.})$. The singlet subspace contains the empty and the doubly occupied state; the doublet is not affected by the pairing:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U, Delta];
H = eps number[d[]] + U hubbard[d[]] + Delta (nc[d[CR, UP], d[CR, DO]] + nc[d[AN, DO], d[AN, UP]]);
makematricesbzvc[H, sbasisvc[{d[]}]]
```

### Properties & Relations
Dimensions for two levels; with the multiplet degeneracies, $5 \cdot 1 + 4 \cdot 2 + 1 \cdot 3 = 16$ states:
```wl
snegfermionoperators[c];
{First[#], Length[Last[#]]} & /@ sbasisvc[{c[1], c[2]}]
```

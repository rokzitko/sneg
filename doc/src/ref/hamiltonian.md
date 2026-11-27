---
keywords: Hubbard model, Hubbard chain, model Hamiltonian
see also: hubbard, hop, number, makematricesbzvc
---
# hamiltonian

## Usage
- `hamiltonian[Hubbard, c[j], n, U, t, ε]` gives the Hamiltonian of the open Hubbard chain of *n* sites $\sum_{i=1}^{n} (\epsilon\, n_i + U n_{i\uparrow} n_{i\downarrow}) + t \sum_{i=1}^{n-1} \sum_\sigma (c^\dagger_{i\sigma} c_{i+1,\sigma} + \mathrm{h.c.})$.

## Details
- `hamiltonian` is a stub for model Hamiltonians; only the type `Hubbard` is defined. For other types, `hamiltonian` remains unevaluated.
- The site operators are `c[i, j]`, $i = 1, \ldots, n$: the site index is inserted as the first index of the level `c[j]`, which may have further indexes, e.g. a channel.
- The hopping enters with the sign $+t$: the usual convention $-t \sum$ `hop` corresponds to the argument $-t$.
- The chain has open boundary conditions. For periodic chains, other lattices, or longer-range terms, build the Hamiltonian from `hop`, `number`, and `hubbard` directly.

## Examples
### Basic Examples
The Hubbard dimer:
```wl
snegfermionoperators[c];
snegrealconstants[U, t, eps];
hamiltonian[Hubbard, c[], 2, U, t, eps]
```

### Applications
Three-site Hubbard chain with $U = 4$, hopping $-1$ (argument $t = -1$), and $\epsilon = -U/2$ (particle-hole symmetric point). The lowest energies in the subspaces of charge $Q = N - 3$ and spin $S_z \ge 0$; the ground state is the $S = 1/2$ doublet at half filling ($Q = 0$):
```wl
snegfermionoperators[c];
H = hamiltonian[Hubbard, c[], 3, 4, -1, -2];
mats = makematricesbzvc[H, qszbasisvc[{c[1], c[2], c[3]}]];
{First[#], Min[Eigenvalues[N[Last[#]]]]} & /@ Select[mats, First[#][[2]] >= 0 &]
```

### Properties & Relations
Equivalent construction with `hop`, `number`, and `hubbard`:
```wl
snegfermionoperators[c];
snegrealconstants[U, t, eps];
hamiltonian[Hubbard, c[], 3, U, t, eps] == Sum[eps number[c[i]] + U hubbard[c[i]], {i, 3}] + t Sum[hop[c[i], c[i + 1]], {i, 2}]
```

### Possible Issues
The site index is inserted before the other indexes of the level:
```wl
snegfermionoperators[c];
snegrealconstants[U, t, eps];
hamiltonian[Hubbard, c["a"], 2, U, t, eps] == hamiltonian[Hubbard, c[], 2, U, t, eps] /. c[k_, i_, s_] :> c[k, i, "a", s]
```

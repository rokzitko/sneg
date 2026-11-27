---
keywords: isospin, charge SU(2), pseudospin, SO(4) symmetry, particle-hole symmetry, Hubbard model, eta pairing, symmetry-adapted basis, invariant subspaces
see also: isospinminus, isospinxyz, qsbasisvc, qszbasisvc, quickISObasis, nnop, makematricesbzvc, isozsq, bzvc2bzop
related links: [C. N. Yang, Phys. Rev. Lett. 63, 2144 (1989)](https://doi.org/10.1103/PhysRevLett.63.2144); [C. N. Yang and S. C. Zhang, Mod. Phys. Lett. B 4, 759 (1990)](https://doi.org/10.1142/S0217984990000933)
---
# transformQStoIS

## Usage
- `transformQStoIS[bz, Tm]` transforms the basis *bz* with the charge $Q$ as the first quantum number, e.g. $(Q, S)$, into a basis with the isospin $I$ instead of $Q$, e.g. $(I, S)$, using the isospin lowering operator *Tm*.
- `transformQStoIS[bz]` uses the global value of `Tminus` as the isospin lowering operator.

## Details
- The isospin (charge SU(2), pseudospin) of a spin-1/2 level $i$ has the components $I^z_i = (n_i - 1)/2$, $I^+_i = (-1)^{\phi_i} c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}$, $I^-_i = (I^+_i)^\dagger$, with a phase $(-1)^{\phi_i}$ given by `isospinminus[c[i], phi]`. The total isospin $\mathbf I = \sum_i \mathbf I_i$ has $I^z = Q/2$, where $Q = N - N_\mathrm{levels}$ is the charge of the basis.
- For particle-hole symmetric Hamiltonians on bipartite lattices, e.g. the Hubbard model $-t \sum (c^\dagger_{i\sigma} c_{j\sigma} + \mathrm{h.c.}) + \frac{U}{2} \sum_i (n_i - 1)^2$, the isospin is conserved if the phases alternate between the sublattices, $\phi_i = 0$ on one and $\phi_i = 1$ on the other. Together with the spin SU(2), this gives the SO(4) symmetry of the Hubbard model.
- *bz* is a basis `{{qn, states}, ...}` (see `makematricesbzvc`) in the occupation number representation whose first quantum number is $Q$; the further quantum numbers (e.g. $S$ or $S_z$) are kept. Suitable bases are `qsbasisvc`, `qszbasisvc`, and `qbasisvc`.
- The result contains only the highest-weight states of the isospin multiplets, $I^z = I$, as the $(Q, S)$ basis contains only the states with $S_z = S$. The first quantum number is $I$; each state represents a multiplet of $2I+1$ states with $Q = 2I, 2I-2, \ldots, -2I$. In an $(I, S)$ basis, an eigenvalue in the subspace $(I, S)$ is $(2I+1)(2S+1)$-fold degenerate.
- The subspaces are processed in the order of decreasing $Q$: the states with the largest $Q$ are highest-weight states; $I^-$ generates the other members of their multiplets, which are projected out of the subspaces with lower $Q$. The remaining states are the highest-weight states of the next multiplets. Empty subspaces are removed; the result is ordered by decreasing $Q = 2I$ of the original subspaces.
- *Tm* must be the total isospin lowering operator of all levels in the basis, e.g. `isospinminus[c[1], 0] + isospinminus[c[2], 1]`.
- If `Tminus` has no value, `transformQStoIS[bz]` issues the message `transformQStoIS::Tminus` and returns `$Failed`.
- `quickISObasis`, `quickISOSZbasis`, and `quickSU2basis` construct the lowering operator with the phases given by `nnop` and call `transformQStoIS`.

## Examples
### Basic Examples
The $(I, S)$ basis of a single level: the empty and doubly occupied states form an isospin doublet ($I = 1/2$, $S = 0$), the singly occupied states a spin doublet ($I = 0$, $S = 1/2$):
```wl
snegfermionoperators[c];
transformQStoIS[qsbasisvc[{c[1]}], isospinminus[c[1]]]
```

### Scope
The global `Tminus` is used if the lowering operator is not given:
```wl
snegfermionoperators[c];
Tminus = isospinminus[c[1], 0] + isospinminus[c[2], 1];
transformQStoIS[qsbasisvc[{c[1], c[2]}]]
```
---
An $(I, S_z)$ basis from a $(Q, S_z)$ basis:
```wl
{First[#], Length[Last[#]]} & /@ transformQStoIS[qszbasisvc[{c[1], c[2]}], Tminus]
```

### Applications
SO(4) symmetry of the half-filled Hubbard dimer. With alternating phases, the isospin commutes with the Hamiltonian in the particle-hole symmetric form:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
tm = isospinminus[c[1], 0] + isospinminus[c[2], 1];
H = -t hop[c[1], c[2]] + U/2 Total[isozsq[{c[1], c[2]}]];
Expand[komutator[H, tm]]
```
In the $(I, S)$ basis, the Hamiltonian splits into blocks of dimension at most 2; the state with $(I, S) = (1, 0)$ represents the isospin triplet formed by the empty state, the fully occupied state, and a combination of the states with one doubly occupied site; $(1/2, 1/2)$ represents the doublets with one or three electrons, $(0, 1)$ the spin triplet, and $(0, 0)$ the two remaining singlets at half filling:
```wl
bz = transformQStoIS[qsbasisvc[{c[1], c[2]}], tm];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bz]
```
The multiplicities $(2I+1)(2S+1)$ account for all 16 states:
```wl
Total[(2 #[[1, 1]] + 1) (2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz]
```

### Properties & Relations
The states have the isospin $\mathbf I^2 = I(I+1)$:
```wl
snegfermionoperators[c];
tm = isospinminus[c[1], 0] + isospinminus[c[2], 1];
bz = transformQStoIS[qsbasisvc[{c[1], c[2]}], tm];
ii = Expand[inner[isospinxyz[c[1], 0] + isospinxyz[c[2], 1], isospinxyz[c[1], 0] + isospinxyz[c[2], 1]]];
{First[First[#]], Union[Diagonal[Last[#]]]} & /@ makematricesbzvc[ii, bz]
```

### Possible Issues
Without the global `Tminus`, the one-argument form fails:
```wl
snegfermionoperators[c];
Clear[Tminus];
transformQStoIS[qsbasisvc[{c[1]}]]
```

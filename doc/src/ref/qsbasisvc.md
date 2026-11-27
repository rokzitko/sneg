---
keywords: symmetry-adapted basis, total spin, SU(2) symmetry, charge conservation, Clebsch-Gordan coefficients, spin multiplet, highest-weight state, invariant subspace, exact diagonalization, NRG
see also: qsbasis, qszbasisvc, sbasisvc, quickISObasis, transformQStoIS, SU2basistensorproduct, makebasis, makematricesbzvc, spindown, vc
related links: [H. R. Krishna-murthy, J. W. Wilkins, K. G. Wilson, Phys. Rev. B 21, 1003 (1980)](https://doi.org/10.1103/PhysRevB.21.1003)
---
# qsbasisvc

## Usage
- `qsbasisvc[{c[1], c[2], ...}]` gives a basis of the Fock space of the spin-1/2 levels `c[1]`, `c[2]`, ... in the occupation number representation, organized in subspaces with well-defined charge $Q$ and total spin $S$; each subspace contains the states with $S_z = S$.

## Details
- The result is a list `{{{Q, S}, {states}}, ...}` of invariant subspaces of the group $U(1)_\mathrm{charge} \times SU(2)_\mathrm{spin}$; the states are linear combinations of `vc` vectors. `qsbasisvc` calls `makebasis` with its argument.
- The charge is measured from half filling, $Q = N - N_\mathrm{levels}$.
- Only one state of each spin multiplet is kept: the highest-weight state with $S_z = S$. Every state in the subspace $(Q, S)$ represents a multiplet of $2S + 1$ degenerate states, which are obtained by applying the total spin-lowering operator $S^-$. Hence $\sum_{(Q,S)} (2S+1)\, d_{Q,S} = 4^{N_\mathrm{levels}}$, where $d_{Q,S}$ is the number of states in the subspace.
- Matrices computed in this basis (`makematricesbzvc`) are the reduced matrices of operators that commute with all components of the total spin, $[H, \mathbf S] = 0$: each eigenvalue in the subspace $(Q, S)$ is $(2S+1)$-fold degenerate. For operators that do not commute with $S^\pm$, such as a Zeeman term, use `qszbasisvc`.
- Construction: the levels are added one at a time, in the order of the list. If $|a\rangle$ is a state of the first levels with quantum numbers $(q, s)$ and $S_z = s$, adding the level $c$ gives the states
$$(q-1, s)\colon |a\rangle, \quad (q+1, s)\colon c^\dagger_\downarrow c^\dagger_\uparrow |a\rangle, \quad (q, s+\tfrac12)\colon c^\dagger_\uparrow |a\rangle,$$
$$(q, s-\tfrac12)\colon \sqrt{\frac{2s}{2s+1}}\, c^\dagger_\downarrow |a\rangle - \sqrt{\frac{1}{2s+1}}\, c^\dagger_\uparrow |a'\rangle \quad (s \ge \tfrac12),$$
where $|a'\rangle = S^- |a\rangle / \Vert S^- |a\rangle \Vert$ has $S_z = s - 1$. The coefficients are the Clebsch-Gordan coefficients for coupling the spin $s$ of the existing states with the spin 1/2 of the added electron, with the Condon-Shortley phase convention; the operator of the added level stands to the left of the operators of the previous levels. This fixes the phases of the basis states; for example, the one-level states are $|0\rangle$, $c^\dagger_\uparrow|0\rangle$, and $c^\dagger_\downarrow c^\dagger_\uparrow|0\rangle = -$`vc[1, 1]`.
- The operators must be spin-1/2 fermion operators (declared with `snegfermionoperators`). Otherwise the message `qsbasisvc::spin` is issued and `$Failed` is returned. For other fermions, use `qszbasisvc`, `qbasisvc`, or `nonebasisvc`.
- `qsbasisvc[{}]` is `{{{0, 0}, {vc[]}}}`.
- The subspaces are sorted by $(Q, S)$. The states are simplified with `Simplify`.
- `qsbasis` gives the same basis in the creation operator representation; `sbasisvc` merges the subspaces with different $Q$. Bases with isospin are obtained with `quickISObasis` and `transformQStoIS`.

## Developer Notes
- The recursion is implemented by the private functions `ADDvc`, `ADDQSvc`, and `ADDONEvc` and uses `spindownvc` to construct $|a'\rangle$; `qsbasis` uses the analogous functions `ADD`, `ADDQS`, `ADDONE` with `spindown` in the creation operator representation. Subspaces with equal quantum numbers are merged with `keymerge`.
- The construction follows the iterative construction of $(Q, S)$ states in the numerical renormalization group (Krishna-murthy, Wilkins, Wilson). NRG Ljubljana uses `bzvc2bzop[qsbasisvc[basisops]]` as the starting point for the symmetry types `QS`, `QSLR`, `ISO`, `ISO2`, `ISOLR`, `ISO2LR`, and stores $S$ as $2S + 1$.

## Examples
### Basic Examples
The basis of a single level. The subspace $(0, 1/2)$ contains only the state with $S_z = +1/2$:
```wl
snegfermionoperators[d];
qsbasisvc[{d[]}] // InputForm
```
Two levels: the singlet and triplet states with one electron per level appear in the subspaces $(0, 0)$ and $(0, 1)$:
```wl
snegfermionoperators[c];
qsbasisvc[{c[1], c[2]}] // InputForm
```

### Scope
Dimensions of the subspaces for three levels:
```wl
snegfermionoperators[c];
dims = {First[#], Length[Last[#]]} & /@ qsbasisvc[{c[1], c[2], c[3]}]
```
Counting the multiplet degeneracies gives the dimension of the Fock space, $4^3 = 64$:
```wl
Total[(2 #[[1, 2]] + 1) #[[2]] & /@ dims]
```
---
Levels of different operators:
```wl
snegfermionoperators[d, c];
{First[#], Length[Last[#]]} & /@ qsbasisvc[{d[], c[1]}]
```

### Applications
Hubbard dimer, $H = -t \sum_\sigma (c^\dagger_{1\sigma} c_{2\sigma} + \mathrm{h.c.}) + U \sum_i n_{i\uparrow} n_{i\downarrow}$. At half filling, the ground state is the singlet with $E = (U - \sqrt{U^2 + 16t^2})/2$, the triplet has $E = 0$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, qsbasisvc[{c[1], c[2]}]]
```
---
Anderson impurity $d$ coupled to one bath level $c$ at the particle-hole symmetric point, $H = -\frac{U}{2} n_d + U n_{d\uparrow} n_{d\downarrow} + V \sum_\sigma (d^\dagger_\sigma c_\sigma + \mathrm{h.c.})$. The ground state is the singlet with $E = -(U + \sqrt{U^2 + 64 V^2})/4$:
```wl
snegfermionoperators[d, c];
snegrealconstants[U, V];
H = -U/2 number[d[]] + U hubbard[d[]] + V hop[d[], c[]];
eig = {First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, qsbasisvc[{d[], c[]}]]
```
For $U \gg V$, the singlet-triplet splitting is the Kondo exchange coupling $J = 8V^2/U$ of the Schrieffer-Wolff transformation:
```wl
Series[-U/2 - (-U - Sqrt[U^2 + 64 V^2])/4, {V, 0, 2}, Assumptions -> U > 0]
```
---
Two-site Heisenberg model from the half-filled Hubbard dimer: projecting onto single occupancy with `transformbasis` leaves the singlet and the triplet:
```wl
snegfermionoperators[c];
bz = transformbasis[qsbasisvc[{c[1], c[2]}], nc[projector1[c[1]], projector1[c[2]]]]
```
```wl
makematricesbzvc[spinspin[c[1], c[2]], bz]
```

### Properties & Relations
The states are eigenstates of $\mathbf S^2$ with eigenvalue $S(S+1)$ and of $S_z$ with eigenvalue $S$:
```wl
snegfermionoperators[c];
bz = qsbasisvc[{c[1], c[2], c[3]}];
{sx, sy, sz, ss} = manyspin[{c[1], c[2], c[3]}];
And @@ Flatten[Table[{Expand[ap[ss, v] - sub[[1, 2]] (sub[[1, 2]] + 1) v] === 0, Expand[ap[sz, v] - sub[[1, 2]] v] === 0}, {sub, bz}, {v, sub[[2]]}]]
```
---
`qsbasis` gives the basis in the creation operator representation:
```wl
snegfermionoperators[c];
Expand[bzop2bzvc[qsbasis[{c[1], c[2]}]]] === Expand[qsbasisvc[{c[1], c[2]}]]
```
---
Compared with the $(Q, S_z)$ basis, the subspaces are smaller:
```wl
snegfermionoperators[c];
{Length /@ Last /@ qsbasisvc[{c[1], c[2], c[3]}], Length /@ Last /@ qszbasisvc[{c[1], c[2], c[3]}]}
```

### Possible Issues
`qsbasisvc` requires spin-1/2 fermions:
```wl
snegspinlessfermionoperators[a];
qsbasisvc[{a[1], a[2]}]
```
---
Only the states with $S_z = S$ are included. Operators that do not conserve $S^\pm$, such as a magnetic field along $z$, give matrices whose eigenvalues are only those of the $S_z = S$ members of the multiplets:
```wl
snegfermionoperators[c];
snegrealconstants[B];
makematricesbzvc[-B (spinz[c[1]] + spinz[c[2]]), qsbasisvc[{c[1], c[2]}]]
```

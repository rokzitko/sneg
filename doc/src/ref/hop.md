---
keywords: hopping, tight-binding, kinetic energy, hybridization, electron transfer, tunneling, Hubbard model, Anderson model, chain, ring, band structure
see also: number, hubbard, matrixhop, genhop, hopphi, current, spinfliphop, holehop, twohop, anomaloushop, fourier, blochhamiltonian, clusterhop
---
# hop

## Usage
- `hop[a[i], b[j]]` gives the hopping operator $\sum_\sigma (a^\dagger_{i\sigma} b_{j\sigma} + b^\dagger_{j\sigma} a_{i\sigma})$ between the levels `a[i]` and `b[j]`.
- `hop[a[i], b[j], σ]` gives the hopping operator $a^\dagger_{i\sigma} b_{j\sigma} + b^\dagger_{j\sigma} a_{i\sigma}$ for the spin projection *σ* only.
- `hop[fn1, fn2]` gives the hopping between abstract orbitals given as pure functions.

## Details
- The levels are specified without the creation/annihilation and spin indexes: `a[i]` stands for `a[CR, i, σ]` and `a[AN, i, σ]`. The two levels may belong to the same or to different operator families, e.g. `hop[d[], c[0]]` for the hybridization of an impurity level with a conduction-band site.
- `hop` is the bare hopping with amplitude $+1$. The tight-binding Hamiltonian with hopping integral $t$ is $H = -t \sum_{\langle ij\rangle} \mathrm{hop}(c_i, c_j)$; with this sign the band $\epsilon_k = -2t\cos k$ of a chain has its minimum at $k = 0$ for $t > 0$.
- Each bond is entered once: $\sum_{\langle ij\rangle}$ runs over unordered pairs, since `hop` already contains the Hermitian conjugate.
- For spin-1/2 fermions the sum runs over $\sigma \in \{\uparrow, \downarrow\}$, for fermions with spin $s$ over $\sigma = -s, \ldots, s$, and spinless fermions have the spin index 0. Both levels must have the same spin; otherwise `hop` remains unevaluated.
- `hop[a[i], a[i]]` is $2 n_i$.
- Abstract orbitals: `fn` is a pure function of the type and spin indexes such that `fn[CR, σ]` is the creation operator of the orbital. The annihilation operator is obtained as `conj[fn[CR, σ]]`. In `hop[fn1, fn2]`, one argument may be an ordinary level `a[i]`, which is converted to `a[#1, i, #2] &`; the spin-resolved form `hop[fn1, fn2, σ]` requires two functions.
- Complex hopping amplitudes: `genhop[t, a[i], b[j]]` gives $t\, a^\dagger b + t^* b^\dagger a$, and `hopphi[a[i], b[j], φ]` the Peierls-phase form $e^{i\phi} a^\dagger b + e^{-i\phi} b^\dagger a$. Spin-dependent hopping $a^\dagger T b + \mathrm{h.c.}$ with a $2\times2$ matrix $T$ is given by `matrixhop`.
- The particle current associated with `hop[a[i], b[j]]` is `current[a[i], b[j]]` $= i[\mathrm{hop}, n_a]$.
- `hop` is `Listable`: `hop[{a1, a2}, {b1, b2}]` gives `{hop[a1, b1], hop[a2, b2]}`, and `hop[{a1, a2}, b]` gives `{hop[a1, b], hop[a2, b]}`.
- `SnegSimplify` rewrites expanded hopping terms as `hop[...]`.

## Examples
### Basic Examples
Hopping between two sites:
```wl
snegfermionoperators[c];
hop[c[1], c[2]]
```
Spin-up part only:
```wl
hop[c[1], c[2], UP]
```

### Scope
Hybridization between an impurity level `d[]` and the first site of a conduction-band chain `f[0]`:
```wl
snegfermionoperators[d, f];
snegrealconstants[v];
v hop[d[], f[0]]
```
---
Levels with site and orbital indexes:
```wl
snegfermionoperators[c];
hop[c[1, "a"], c[2, "b"], DO]
```
---
`hop` threads over lists. The bonds of an open three-site chain:
```wl
Total[hop[{c[1], c[2]}, {c[2], c[3]}]]
```
---
Spin-1 fermions:
```wl
snegfermionoperators[{f, 1}];
hop[f[1], f[2]]
```
---
Spinless fermions:
```wl
snegspinlessfermionoperators[a];
hop[a[1], a[2]]
```
---
Hopping from a level `c[3]` to the bonding orbital $(c^\dagger_{1\sigma} + c^\dagger_{2\sigma})/\sqrt{2}$, given as a pure function of the type and spin indexes:
```wl
snegfermionoperators[c];
bond = (c[#1, 1, #2] + c[#1, 2, #2])/Sqrt[2] &;
hop[bond, c[3]]
```
---
Translation-invariant hopping on a lattice, written as a symbolic sum over a declared lattice index:
```wl
snegfermionoperators[c];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
H = -t sum[hop[c[R], c[R + 1]], {R}]
```

### Applications
Bonding and antibonding states of a dimer, $E = \mp t$. The single-particle Hamiltonian matrix in the basis $c^\dagger_{1\uparrow}|0\rangle, c^\dagger_{2\uparrow}|0\rangle$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
h = matrixrepresentationop[-t hop[c[1], c[2]], {c[CR, 1, UP], c[CR, 2, UP]}]
```
```wl
Eigensystem[h]
```
---
Open tight-binding chain with $N = 5$ sites. The single-particle levels are $-2t\cos(\pi m/(N+1))$, $m = 1, \ldots, N$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
H = -t Sum[hop[c[j], c[j + 1]], {j, 4}];
h = matrixrepresentationop[H, Table[c[CR, j, UP], {j, 5}]]
```
```wl
Sort[Eigenvalues[h]] == Sort[Table[-2 t Cos[Pi m/6], {m, 5}]]
```
---
Ring of $N = 6$ sites with periodic boundary conditions. The levels are $-2t\cos k$ with $k = 2\pi m/N$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
H = -t Sum[hop[c[j], c[Mod[j, 6] + 1]], {j, 6}];
Sort[Eigenvalues[matrixrepresentationop[H, Table[c[CR, j, UP], {j, 6}]]]]
```
---
Band structure of the infinite chain from the Fourier transform. The Bloch Hamiltonian (rows and columns `{UP, DO}`) is $-2t\cos k$:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
h = blochhamiltonian[fourier[-t sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, k];
ExpToTrig[blochphaseexp[h]]
```
---
Many-particle spectrum of the dimer in the subspaces of charge $Q$ (measured from half filling) and spin $S_z$. For two electrons ($Q = 0$), the ground state has both electrons in the bonding orbital, $E = -2t$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
Map[{First[#], Eigenvalues[Last[#]]} &, makematricesbzvc[-t hop[c[1], c[2]], qszbasisvc[{c[1], c[2]}]]]
```
---
Continuity equation on a three-site chain $H = -t\,(\mathrm{hop}_{12} + \mathrm{hop}_{23})$. The rate of change of the occupancy of the middle site, $\dot n_2 = i[H, n_2]$ ($\hbar = 1$), equals the particle current into site 2 from both neighbors, $J_{1\to2} + J_{3\to2}$ with $J_{j\to i} = -t\,$`current[c[i], c[j]]`:
```wl
snegfermionoperators[c];
snegrealconstants[t];
H = -t (hop[c[1], c[2]] + hop[c[2], c[3]]);
Expand[I komutator[H, number[c[2]]] - (-t current[c[2], c[1]] - t current[c[2], c[3]])]
```

### Properties & Relations
`hop` is Hermitian and symmetric in its arguments:
```wl
snegfermionoperators[c];
{conj[hop[c[1], c[2]]] == hop[c[1], c[2]], hop[c[1], c[2]] == hop[c[2], c[1]]}
```
---
Hopping from a level to itself is twice the number operator:
```wl
Expand[hop[c[1], c[1]] - 2 number[c[1]]]
```
---
The hopping conserves the total charge and the total spin:
```wl
{Sx, Sy, Sz, S2} = manyspin[{c[1], c[2]}];
H = hop[c[1], c[2]];
Expand[{komutator[H, number[c[1]] + number[c[2]]], komutator[H, Sx], komutator[H, Sy], komutator[H, Sz]}]
```
---
On a bipartite lattice, the hopping also conserves the total isospin (charge SU(2) symmetry) if the isospin phases of the two sublattices differ:
```wl
{Ix, Iy, Iz, I2} = manyisospin[{c[1], c[2]}, {0, 1}];
Expand[{komutator[H, Ix], komutator[H, Iy], komutator[H, Iz]}]
```
With equal phases, only $I_z$ is conserved:
```wl
{Ix, Iy, Iz, I2} = manyisospin[{c[1], c[2]}, {0, 0}];
Expand[{komutator[H, Ix], komutator[H, Iz]}]
```
---
Relation with the generalized forms:
```wl
{genhop[1, c[1], c[2]], hopphi[c[1], c[2], 0], matrixhop[IdentityMatrix[2], c[1], c[2]]} == ConstantArray[hop[c[1], c[2]], 3]
```
---
`SnegSimplify` recognizes the hopping operator:
```wl
SnegSimplify[Expand[3 hop[c[1], c[2]]]]
```

### Possible Issues
Unlike `number`, `hop` does not accept bare symbols as levels:
```wl
snegfermionoperators[c, d];
hop[c, d]
```
```wl
hop[c[], d[]]
```
---
Levels with different spin are not connected:
```wl
snegfermionoperators[{f, 1}];
hop[c[1], f[1]]
```
---
On a ring of two sites, the bonds to the left and to the right neighbor are the same bond. Summing over both doubles the hopping:
```wl
snegrealconstants[t];
Expand[-t Sum[hop[c[j], c[Mod[j, 2] + 1]], {j, 2}] + 2 t hop[c[1], c[2]]]
```

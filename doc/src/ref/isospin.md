---
keywords: isospin, charge SU(2), pseudospin, eta pairing, Nambu spinor, particle-hole symmetry, bipartite lattice, superconducting pairing
see also: isospinxyz, isospinx, isospiny, isospinz, isospinplus, isospinminus, manyisospin, nambu, isozsq, spinxyz, transformQStoIS
related links: [C. N. Yang, Phys. Rev. Lett. 63, 2144 (1989)](https://doi.org/10.1103/PhysRevLett.63.2144)
---
# isospin

## Usage
- `isospin[c[i]]` gives the list $\{I^x_i, I^y_i, I^z_i\}$ of the isospin (charge pseudospin) operators of the spin-1/2 fermion level `c[i]`.
- `isospin[c[i], n]` gives the isospin operators with the phase factor $(-1)^n$.

## Details
- With the Nambu spinor $\Psi_i = (c_{i\uparrow},\, (-1)^n c^\dagger_{i\downarrow})^T$, the isospin operators are
  $$I^a_i = \frac12 \Psi_i^\dagger\, \sigma^a\, \Psi_i, \qquad a = x, y, z,$$
  i.e. the spin operators of the Nambu spinor. `nambu[c[i], n]` gives $\Psi_i^\dagger = (c^\dagger_{i\uparrow}, (-1)^n c_{i\downarrow})$.
- Explicitly,
  $$I^z = \frac12 (n_\uparrow + n_\downarrow - 1), \quad I^+ = (-1)^n c^\dagger_\uparrow c^\dagger_\downarrow, \quad I^- = (-1)^n c_\downarrow c_\uparrow,$$
  with $I^\pm = I^x \pm i I^y$, $I^x = \frac12 (I^+ + I^-)$, $I^y = \frac1{2i} (I^+ - I^-)$. For $n = 0$, $I^x = \frac12 (c^\dagger_\uparrow c^\dagger_\downarrow + c_\downarrow c_\uparrow)$.
- The operators obey $[I^a, I^b] = i \varepsilon_{abc} I^c$ and commute with the spin operators of the level. The empty and doubly occupied states form an isospin doublet ($I = 1/2$, $I^z = \mp 1/2$); the singly occupied states are isospin singlets ($I = 0$) and spin doublets. Hence $\mathbf I^2 + \mathbf S^2 = 3/4$ on a single level.
- The phase index *n* is an integer; only its parity matters. Its default value is 0.
- On a bipartite lattice, the total isospin $\mathbf I = \sum_i \mathbf I_i(n_i)$ is formed with $n_i$ even on one sublattice and odd on the other, so that $I^+ = \sum_i (-1)^{n_i} c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}$ is Yang's $\eta$-pairing operator at momentum $(\pi, \pi, \ldots)$. With this choice, $\mathbf I$ commutes with the hopping between the two sublattices, and the Hubbard model at half filling in the particle-hole symmetric form $H = -t \sum_{\langle ij\rangle\sigma} c^\dagger_{i\sigma} c_{j\sigma} + U \sum_i (n_{i\uparrow} - \frac12)(n_{i\downarrow} - \frac12)$ has the charge SU(2) symmetry. In quantum impurity models the phases alternate along the Wilson chain, $n_i = i$.
- `isospin` and `isospinxyz` are synonyms. The individual components are given by `isospinx`, `isospiny`, `isospinz`, `isospinplus`, `isospinminus`; the total isospin of several levels and its square by `manyisospin`.
- The level is given without the creation/annihilation and spin indexes, `c[i]` or `c[]`; unlike `isospinx`, ..., `isospin` does not accept the bare symbol `c`.
- `isospin` is `Listable` in both arguments: `isospin[{c[1], c[2]}, {0, 1}]` gives the isospin operators of the two levels with alternating phases.

## Examples
### Basic Examples
Isospin operators of the level `c[]`:
```wl
snegfermionoperators[c];
isospin[c[]]
```
They satisfy the su(2) algebra:
```wl
{ix, iy, iz} = isospin[c[]];
Expand[{komutator[ix, iy] - I iz, komutator[iy, iz] - I ix, komutator[iz, ix] - I iy}]
```

### Scope
The phase factor $(-1)^n$ changes the sign of $I^x$ and $I^y$; $I^z$ is unchanged:
```wl
snegfermionoperators[c];
isospin[c[1], 1]
```
---
`isospin` threads over levels and phases:
```wl
snegfermionoperators[c];
isospin[{c[1], c[2]}, {0, 1}][[All, 3]]
```

### Applications
Charge SU(2) symmetry of the half-filled Hubbard dimer. The total isospin with alternating phases commutes with the Hamiltonian in the particle-hole symmetric form:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U Sum[nc[number[c[i], UP] - 1/2, number[c[i], DO] - 1/2], {i, 2}];
Expand[komutator[H, #] & /@ manyisospin[{c[1], c[2]}, {0, 1}]]
```
With equal phases, the isospin does not commute with the hopping:
```wl
Expand[komutator[H, isospinplus[c[1]] + isospinplus[c[2]]]]
```
---
Yang's $\eta$-pairing operator $\eta^\dagger = \sum_i (-1)^i c^\dagger_{i\uparrow} c^\dagger_{i\downarrow}$ is an eigenoperator of the Hubbard Hamiltonian $H = -t \sum c^\dagger c + U \sum n_\uparrow n_\downarrow$: $[H, \eta^\dagger] = U \eta^\dagger$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
eta = isospinplus[c[1], 1] + isospinplus[c[2], 2];
Expand[komutator[H, eta] - U eta]
```
---
The local interaction $\frac{U}{2}(n-1)^2 = 2U (I^z)^2$ is a function of the isospin; `isozsq` gives $(n-1)^2$:
```wl
snegfermionoperators[d];
Expand[isozsq[d[]] - 4 nc[isospinz[d[]], isospinz[d[]]]]
```

### Properties & Relations
On a single level, $\mathbf I^2 + \mathbf S^2 = 3/4$:
```wl
snegfermionoperators[c];
{ix, iy, iz} = isospin[c[]];
Expand[nc[ix, ix] + nc[iy, iy] + nc[iz, iz] + spinss[c[]] - 3/4]
```
---
Isospin and spin operators commute:
```wl
snegfermionoperators[c];
Expand[Outer[komutator, isospin[c[]], spinxyz[c[]]]]
```
---
Relation with the Nambu spinor:
```wl
snegfermionoperators[c];
{nambu[c[], 1], isospinplus[c[], 1]}
```
---
`isospin` and `isospinxyz` are synonyms:
```wl
snegfermionoperators[c];
isospin[c[2], 1] === isospinxyz[c[2], 1]
```

### Possible Issues
The isospin operators are defined for spin-1/2 fermions. There is no check of the spin: for a spin-1 fermion, `UP` and `DO` (internally 1 and 0) select the components $m = 1$ and $m = 0$, and $m = -1$ is ignored:
```wl
snegfermionoperators[{f, 1}];
isospinz[f[]]
```
---
`isospin` requires the level with brackets; the bare symbol is not accepted:
```wl
snegfermionoperators[c];
{isospin[c], isospin[c[]] === isospinxyz[c[]]}
```

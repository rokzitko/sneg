---
keywords: spin-spin interaction, exchange interaction, Heisenberg model, scalar product of spins, superexchange, Kondo exchange, singlet, triplet
see also: spinspinz, spinspinxy, spinspinpm, spinspinmp, spinspinsymmetric, spinxyz, manyspin, snegspinoperators, spinclustermatrices, chargecharge
related links: [W. Heisenberg, Z. Phys. 49, 619 (1928)](https://doi.org/10.1007/BF01328601)
---
# spinspin

## Usage
- `spinspin[a[i], b[j]]` gives the scalar product $\mathbf S_{a,i} \cdot \mathbf S_{b,j}$ of the spin operators of the fermion levels `a[i]` and `b[j]`.
- `spinspin[S[i], S[j]]` gives $\mathbf S_i \cdot \mathbf S_j = \sum_{a} S^a_i S^a_j$ for abstract spin operators declared with `snegspinoperators`.
- `spinspin[a, b]` is `spinspin[a[], b[]]`.

## Details
- For fermion levels, $\mathbf S_{a} \cdot \mathbf S_{b} = S^x_a S^x_b + S^y_a S^y_b + S^z_a S^z_b$ with the spin operators of `spinxyz`. For spin-1/2 fermions,
  $$\mathbf S_a \cdot \mathbf S_b = S^z_a S^z_b + \frac12 (S^+_a S^-_b + S^-_a S^+_b) = \frac14 \sum_{\sigma\sigma'\tau\tau'} \boldsymbol\sigma_{\sigma\sigma'} \cdot \boldsymbol\sigma_{\tau\tau'}\, c^\dagger_{a\sigma} c_{a\sigma'} c^\dagger_{b\tau} c_{b\tau'}.$$
  The result is normal ordered.
- The longitudinal and transverse parts are given by `spinspinz` and `spinspinxy`, with `spinspin[a, b] == spinspinz[a, b] + spinspinxy[a, b]`.
- The levels can have different spins, e.g. a spin-1 and a spin-1/2 fermion level.
- For abstract spin operators `S`, the result is `nc[S[i, 1], S[j, 1]] + nc[S[i, 2], S[j, 2]] + nc[S[i, 3], S[j, 3]]` in Cartesian components (component indexes 1, 2, 3 for $x, y, z$). For $i = j$ and a declared spin size *s* this reduces to $s(s+1)$ (for a symbolic *s* after `Expand`). Use `spincomponents` to rewrite it in terms of $S^z$ and $S^\pm$. The two arguments can be different spin symbols.
- A level can also be given as a function `f` with `f[t, σ]` the operator of type *t* (`CR` or `AN`) and spin σ, e.g. `(c[#1, 1, #2] + c[#1, 2, #2])/Sqrt[2] &`; see `spinxyz`.
- `spinspin` of a fermion level and an abstract spin is not defined and remains unevaluated. Write the Kondo exchange $J\, \mathbf S \cdot \mathbf s$ with the components, e.g. `Sum[nc[S[a], spinxyz[c[]][[a]]], {a, 3}]`.
- In the Heisenberg model $H = J \sum_{\langle ij\rangle} \mathbf S_i \cdot \mathbf S_j$, $J > 0$ is antiferromagnetic. For two spins 1/2, $\mathbf S_1 \cdot \mathbf S_2 = -3/4$ in the singlet and $1/4$ in the triplet.

## Examples
### Basic Examples
Exchange interaction between two fermion levels:
```wl
snegfermionoperators[c];
spinspin[c[1], c[2]]
```
Exchange interaction between abstract spins:
```wl
snegspinoperators[S];
spinspin[S[1], S[2]]
```

### Scope
Levels of different operator families and without indexes:
```wl
snegfermionoperators[a, b];
spinspin[a, b] == spinspin[a[], b[]]
```
---
A spin-1 and a spin-1/2 fermion level:
```wl
snegfermionoperators[{f, 1}, c];
spinspin[f[], c[]]
```
---
For spins with a declared size, $\mathbf S_i \cdot \mathbf S_i = s(s+1)$; for a symbolic size this requires `Expand`:
```wl
snegspinoperators[{S, 1/2}, {T, 1}, {M, s}];
{spinspin[S[1], S[1]], spinspin[T[1], T[1]], Expand[spinspin[M[1], M[1]]]}
```
---
Two different spin symbols:
```wl
snegspinoperators[{S, 1/2}, {T, 1}];
spinspin[S[1], T[1]]
```
---
In terms of $S^z$ and $S^\pm$:
```wl
snegspinoperators[S];
spincomponents[spinspin[S[1], S[2]]]
```
---
A level given as a function of the operator type and the spin: the bonding orbital $b_\sigma = (c_{2\sigma} + c_{3\sigma})/\sqrt2$. The singlet formed by site 1 and the bonding orbital is an eigenstate of $\mathbf S_1 \cdot \mathbf S_b$ with eigenvalue $-3/4$:
```wl
snegfermionoperators[c];
b = Function[{t, σ}, (c[t, 2, σ] + c[t, 3, σ])/Sqrt[2]];
singlet = nc[c[CR, 1, UP], b[CR, DO]] - nc[c[CR, 1, DO], b[CR, UP]];
Expand[nc[spinspin[c[1], b], singlet, VACUUM] + 3/4 nc[singlet, VACUUM]]
```

### Applications
Singlet and triplet states of two electrons on two sites:
```wl
snegfermionoperators[c];
singlet = nc[c[CR, 1, UP], c[CR, 2, DO]] - nc[c[CR, 1, DO], c[CR, 2, UP]];
triplet = nc[c[CR, 1, UP], c[CR, 2, DO]] + nc[c[CR, 1, DO], c[CR, 2, UP]];
{Expand[nc[spinspin[c[1], c[2]], singlet, VACUUM] + 3/4 nc[singlet, VACUUM]], Expand[nc[spinspin[c[1], c[2]], triplet, VACUUM] - 1/4 nc[triplet, VACUUM]]}
```
---
Heisenberg dimer of two spins 1/2: the eigenvalues in the subspaces of total $S^z$ are the triplet $J/4$ and the singlet $-3J/4$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
{First[#], Eigenvalues[Last[#]]} & /@ spinclustermatrices[J spinspin[S[1], S[2]], {S[1], S[2]}]
```
---
Antiferromagnetic Heisenberg ring of four spins 1/2: the ground state has the energy $-2J$ and total $S_z = 0$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
H = J Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];
{First[#], Min[Eigenvalues[Last[#]] /. J -> 1]} & /@ spinclustermatrices[H, {S -> Range[4]}]
```
---
Two spins 1 with bilinear and biquadratic exchange, $H = \mathbf S_1 \cdot \mathbf S_2 + \beta (\mathbf S_1 \cdot \mathbf S_2)^2$. For total spin $S_t$, $\mathbf S_1 \cdot \mathbf S_2 = S_t(S_t+1)/2 - 2 = -2, -1, 1$:
```wl
snegspinoperators[{T, 1}];
snegrealconstants[β];
ss = spinspin[T[1], T[2]];
Union[Eigenvalues[spinclustermatrices[ss + β nc[ss, ss], {T[1], T[2]}, "Blocks" -> False]]]
```
---
Kondo exchange $J\, \mathbf S \cdot \mathbf s$ of an impurity spin $\mathbf S$ (here without a site index, $S^a$ = `S[a]`) with the spin density $\mathbf s$ of the conduction-band level `c[]`. The total spin is conserved; `spincomponents` is needed to combine Cartesian and ladder components:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
snegrealconstants[J];
HK = J Sum[nc[S[a], spinxyz[c[]][[a]]], {a, 3}];
spincomponents[komutator[HK, #]] & /@ {S[3] + spinz[c[]], S[4] + spinplus[c[]]}
```

### Properties & Relations
Decomposition into the longitudinal and transverse parts:
```wl
snegfermionoperators[c];
Expand[spinspin[c[1], c[2]] - spinspinz[c[1], c[2]] - spinspinxy[c[1], c[2]]]
```
---
$\mathbf S_1 \cdot \mathbf S_2 = \frac12 [(\mathbf S_1 + \mathbf S_2)^2 - \mathbf S_1^2 - \mathbf S_2^2]$:
```wl
snegfermionoperators[c];
Expand[spinspin[c[1], c[2]] - (manyspin[{c[1], c[2]}][[4]] - spinss[c[1]] - spinss[c[2]])/2]
```
---
The exchange interaction conserves the total spin:
```wl
snegfermionoperators[c];
Expand[komutator[spinspin[c[1], c[2]], #] & /@ manyspin[{c[1], c[2]}]]
```
---
For abstract spins without a declared size, the commutator with the total spin vanishes after `spincomponents`:
```wl
snegspinoperators[G];
spincomponents[Expand[komutator[spinspin[G[1], G[2]], G[1, 4] + G[2, 4]]]]
```
---
The fermion and the abstract spin-1/2 representations give the same spectrum on singly occupied sites, see `abrikosov`:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
abrikosov[spinspin[S[1], S[2]], S -> c] == Expand[spinspin[c[1], c[2]]]
```

### Possible Issues
`S[0]` is the spin with component index 0 and evaluates to 0, so site 0 cannot be written as `S[0]`:
```wl
snegspinoperators[S];
spinspin[S[0], S[1]]
```
Use explicit components, or start the site numbering at 1:
```wl
Sum[nc[S[0, a], S[1, a]], {a, 3}]
```
---
A fermion level and an abstract spin cannot be combined with `spinspin`:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
spinspin[S[1], c[1]]
```

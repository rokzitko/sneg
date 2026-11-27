---
keywords: total spin, total spin squared, spin multiplet, Casimir operator
see also: spinxyz, spinall, spinss, spinspin, manyisospin, qsbasis
---
# manyspin

## Usage
- `manyspin[{a[i], b[j], ...}]` gives the list $\{S^x, S^y, S^z, \mathbf S^2\}$ of the components of the total spin $\mathbf S = \mathbf S_a + \mathbf S_b + \cdots$ of the fermion levels and its square.

## Details
- The components are the sums of `spinxyz` of the levels; $\mathbf S^2 = (S^x)^2 + (S^y)^2 + (S^z)^2$. All results are expanded.
- The levels are given without the creation/annihilation and spin indexes. Levels of different fermion families and spins can be combined.
- For abstract spin operators, form the sums directly, e.g. `Sum[S[i, 3], {i, n}]`.

## Examples
### Basic Examples
Total spin of two levels:
```wl
snegfermionoperators[c];
{sx, sy, sz, ss} = manyspin[{c[1], c[2]}];
sz
```
The components obey the su(2) algebra and commute with $\mathbf S^2$:
```wl
Expand[{komutator[sx, sy] - I sz, komutator[ss, sx], komutator[ss, sz]}]
```

### Applications
Total spin of the two-site Hubbard model in the subspaces of charge $Q$ and $S_z$. In the half-filled $S_z = 0$ subspace, the eigenvalues $S(S+1) = 0, 2$ show three singlets and one triplet:
```wl
snegfermionoperators[c];
ss = manyspin[{c[1], c[2]}][[4]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[ss, qszbasisvc[{c[1], c[2]}]]
```
---
The Hubbard dimer conserves the total spin:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
Expand[komutator[H, #] & /@ manyspin[{c[1], c[2]}]]
```

### Properties & Relations
The total spin commutes with the total isospin:
```wl
snegfermionoperators[c];
Expand[Outer[komutator, Most[manyspin[{c[1], c[2]}]], Most[manyisospin[{c[1], c[2]}]]]]
```

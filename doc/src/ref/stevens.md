---
keywords: Stevens operators, crystal field, single-ion anisotropy, zero-field splitting, magnetic anisotropy, operator equivalents, rare earth, transition metal
see also: spintensor, spinclustermatrices, snegspinoperators, spincomponents, rotatespin
related links: [K. W. H. Stevens, Proc. Phys. Soc. A 65, 209 (1952)](https://doi.org/10.1088/0370-1298/65/3/308); [R. Žitko, R. Peters, and T. Pruschke, Phys. Rev. B 78, 224404 (2008) (anisotropic magnetic impurities)](https://doi.org/10.1103/PhysRevB.78.224404)
---
# stevens

## Usage
- `stevens[k, q, S[j]]` gives the Stevens operator $O^q_k$ of rank *k* and component *q* for the spin `S[j]` with a declared size.
- `stevens[k, q, S -> j]` is the same.

## Details
- The Stevens operators are Hermitian polynomials in the spin components that transform like the tesseral harmonics. The normalization is that of the tables of Abragam and Bleaney; for example, with $X = s(s+1)$,
  $$O^0_2 = 3 (S^z)^2 - X, \quad O^2_2 = (S^x)^2 - (S^y)^2 = \frac12 [(S^+)^2 + (S^-)^2], \quad O^{-2}_2 = \frac{1}{2i} [(S^+)^2 - (S^-)^2],$$
  $$O^1_2 = \frac14 [S^z (S^+ + S^-) + (S^+ + S^-) S^z], \quad O^0_4 = 35 (S^z)^4 - 30 X (S^z)^2 + 25 (S^z)^2 - 6X + 3X^2, \quad O^4_4 = \frac12 [(S^+)^4 + (S^-)^4].$$
- They are constructed from the spherical tensors $T^k_q$ of `spintensor`: $O^0_k \propto T^k_0$ and, for $q > 0$, $O^{\pm q}_k \propto (-1)^q T^k_q \pm T^k_{-q}$. The normalization fixes the coefficient of $(S^z)^{k-q} (S^+)^q$ to $L/2$ ($L/(2i)$ for $-q$), and that of $(S^z)^k$ to $L$ for $q = 0$, where $L$ is the leading coefficient of the primitive integer polynomial proportional to $d^q P_k(z)/dz^q$, with the Legendre polynomial $P_k$.
- The results are expanded in $S^z$ and $S^\pm$ and reduced to the normal form for the declared spin size, numeric or symbolic. The operators vanish for $k > 2s$.
- A crystal-field Hamiltonian is $H = \sum_{kq} B^q_k O^q_k$. The single-ion anisotropy $D (S^z)^2 + E [(S^x)^2 - (S^y)^2]$ is $\frac{D}{3} O^0_2 + E\, O^2_2$ up to a constant.
- The spin must have a declared size (`snegspinoperators[{S, s}]`); otherwise the message `stevens::size` is issued and `$Failed` is returned. *k* and *q* must be integers with $k \ge 0$ and $|q| \le k$ (message `spintensor::range`). `stevens[0, 0, S[j]]` is 1.

## Examples
### Basic Examples
The rank-2 Stevens operators of a spin 1:
```wl
snegspinoperators[{T, 1}];
Table[stevens[2, q, T[1]], {q, -2, 2}]
```

### Scope
Symbolic spin size:
```wl
snegspinoperators[{M, s}];
stevens[2, 0, M[1]]
```
```wl
stevens[4, 0, M[1]]
```
---
Site given as a rule:
```wl
snegspinoperators[{T, 1}];
stevens[2, 2, T -> 0]
```
---
Operators with $k > 2s$ vanish:
```wl
snegspinoperators[{S, 1/2}, {T, 1}];
{stevens[2, 0, S[1]], stevens[4, 0, T[1]]}
```

### Applications
Axial crystal field $B^0_2 O^0_2$ of a spin 5/2 (e.g. $\mathrm{Mn}^{2+}$, $\mathrm{Fe}^{3+}$): three Kramers doublets $m = \pm1/2, \pm3/2, \pm5/2$ with the energies $-8, -2, 10$ in units of $B^0_2$:
```wl
snegspinoperators[{F, 5/2}];
Sort[Eigenvalues[spinclustermatrices[stevens[2, 0, F[1]], {F[1]}, "Blocks" -> False]]]
```
---
Zero-field splitting of a spin 1 with axial and rhombic anisotropy, $H = D (S^z)^2 + E [(S^x)^2 - (S^y)^2]$. The levels are $0$ and $D \pm E$:
```wl
snegspinoperators[{T, 1}];
snegrealconstants[Dz, E2];
H = Dz/3 (stevens[2, 0, T[1]] + 2) + E2 stevens[2, 2, T[1]];
Eigenvalues[spinclustermatrices[H, {T[1]}, "Blocks" -> False]]
```
---
Cubic crystal field $B_4 (O^0_4 + 5 O^4_4)$ of a spin 2 splits the quintuplet into a doublet ($E_g$) and a triplet ($T_{2g}$):
```wl
snegspinoperators[{Q, 2}];
H = stevens[4, 0, Q[1]] + 5 stevens[4, 4, Q[1]];
Tally[Eigenvalues[spinclustermatrices[H, {Q[1]}, "Blocks" -> False]]]
```

### Properties & Relations
$O^2_2 = (S^x)^2 - (S^y)^2$:
```wl
snegspinoperators[{Q, 3/2}];
Expand[stevens[2, 2, Q[1]] - spincomponents[pow[Q[1, 1], 2] - pow[Q[1, 2], 2]]]
```
---
The Stevens operators are Hermitian:
```wl
snegspinoperators[{Q, 2}];
Expand[Table[conj[stevens[4, q, Q[1]]] - stevens[4, q, Q[1]], {q, -4, 4}]]
```
---
$O^0_2$ is proportional to the spherical tensor $T^2_0$:
```wl
snegspinoperators[{Q, 3/2}];
Expand[stevens[2, 0, Q[1]] - Sqrt[6] spintensor[2, 0, Q[1]]]
```

### Possible Issues
A declared spin size is required:
```wl
snegspinoperators[G];
stevens[2, 0, G[1]]
```
---
The real constant `E` would conflict with the built-in `E`; also `D` is protected. Use other names for anisotropy constants, as `Dz` and `E2` above.
```wl
Attributes[D]
```

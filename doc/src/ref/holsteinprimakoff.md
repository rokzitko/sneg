---
keywords: Holstein-Primakoff transformation, spin waves, magnons, linear spin-wave theory, ferromagnet, antiferromagnet, 1/s expansion, bosonization of spins
see also: jordanwigner, schwingerbosons, abrikosov, rotatespin, fourier, blochhamiltonian, bdghamiltonian, bogoliubov, snegspinoperators, snegbosonoperators
related links: [T. Holstein and H. Primakoff, Phys. Rev. 58, 1098 (1940)](https://doi.org/10.1103/PhysRev.58.1098)
---
# holsteinprimakoff

## Usage
- `holsteinprimakoff[expr, S -> b, n]` applies the Holstein-Primakoff transformation to the spin operators `S` in *expr* and keeps the terms with at most *n* boson operators.

## Details
- The transformation is
  $$S^z_j = s - b^\dagger_j b_j, \qquad S^+_j = \sqrt{2s - b^\dagger_j b_j}\; b_j, \qquad S^-_j = b^\dagger_j \sqrt{2s - b^\dagger_j b_j},$$
  with the fully polarized state $S^z = s$ as the boson vacuum. The boson of the site `S[j...]` is `b[j...]`, i.e. `b[CR, j...]` and `b[AN, j...]`.
- `S` must be declared with a spin size, `snegspinoperators[{S, s}]`, numeric or symbolic; otherwise the message `holsteinprimakoff::size` is issued. `b` must be a boson operator (`snegbosonoperators`), otherwise `holsteinprimakoff::op`. In both cases `$Failed` is returned.
- The expression is first rewritten in terms of $S^z$ and $S^\pm$ (`spincomponents`). Each normal-ordered product $(S^z_j)^m (S^\pm_j)^a$ on one site is then expanded exactly in normal-ordered boson monomials, using the Newton series $h(b^\dagger b) = \sum_m \frac{\Delta^m h(0)}{m!} (b^\dagger)^m b^m$ with the forward difference $\Delta$. The coefficients are exact in $s$, e.g.
  $$S^+ = \sqrt{2s}\, b + (\sqrt{2s-1} - \sqrt{2s})\, b^\dagger b\, b + \ldots .$$
  This differs from the $1/s$ expansion $\sqrt{2s}\,(1 - b^\dagger b/(4s))\, b$ of the square root, which is recovered for large $s$.
- Terms with more than *n* boson operators are dropped, also inside sums `sum[...]`. For linear spin-wave theory, $n = 2$. The commutator of two images truncated at *n* is exact up to $n - 2$ boson operators.
- For an antiferromagnet, rotate the spins of one sublattice with `rotatespin` first, so that the classical ground state is ferromagnetic in the local frames. The resulting quadratic boson Hamiltonian contains anomalous terms $b^\dagger b^\dagger$ and is diagonalized with `bdghamiltonian` and `bogoliubov`.
- `holsteinprimakoff` does not thread over lists; map it over the elements (see Possible Issues).
- Translation-invariant Hamiltonians written as sums over a lattice index (`sneglatticeindexes`) can be transformed to momentum space with `fourier`.

## Examples
### Basic Examples
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b];
holsteinprimakoff[#, S -> b, 3] & /@ {S[1, 3], S[1, 4], S[1, 5]}
```

### Scope
Numeric spin size:
```wl
snegspinoperators[{T, 1}];
snegbosonoperators[b];
holsteinprimakoff[T[1, 4], T -> b, 3]
```
---
Same-site products are expanded as a whole, not factor by factor. For $S^z S^+ = (s - b^\dagger b) \sqrt{2s - b^\dagger b}\, b$:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b];
holsteinprimakoff[nc[S[1, 3], S[1, 4]], S -> b, 3]
```
---
The exchange interaction to second order in the bosons:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b];
holsteinprimakoff[spinspin[S[1], S[2]], S -> b, 2]
```
---
Including the quartic (magnon-magnon) terms:
```wl
holsteinprimakoff[spinspin[S[1], S[2]], S -> b, 4]
```

### Applications
Linear spin waves of the ferromagnetic Heisenberg chain $H = -J \sum_R \mathbf S_R \cdot \mathbf S_{R+1}$ in momentum space. The constant is the classical ground-state energy $-J N s^2$:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b, bk];
snegrealconstants[J];
sneglatticeindexes[{R}, N0];
snegmomentumindexes[{k}, N0];
hk = fourier[holsteinprimakoff[-J sum[spinspin[S[R], S[R + 1]], {R}], S -> b, 2], b -> bk, R -> k]
```
The magnon dispersion $\omega_k = 2Js(1 - \cos k)$:
```wl
ExpToTrig[blochphaseexp[blochhamiltonian[hk, {bk}, k], {k -> x}]]
```
---
Linear spin waves of the antiferromagnetic chain $H = J \sum_R (\mathbf S_{R,A} \cdot \mathbf S_{R,B} + \mathbf S_{R,B} \cdot \mathbf S_{R+1,A})$ with a two-site unit cell. The spins of sublattice B are rotated by $\pi$ about $x$; the quadratic Hamiltonian contains anomalous terms:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b, bk];
snegrealconstants[J];
sneglatticeindexes[{R}, N0];
snegmomentumindexes[{k}, N0];
h = J sum[spinspin[S[R, "A"], S[R, "B"]] + spinspin[S[R, "B"], S[R + 1, "A"]], {R}];
h = rotatespin[h, S, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
{mk, e0} = bdghamiltonian[fourier[holsteinprimakoff[h, S -> b, 2], b -> bk, R -> k], {bk["A"], bk["B"]}, k];
e0
```
The Bogoliubov transformation gives the magnon energy $\omega_k^2 = 2 J^2 s^2 (1 - \cos k)$, i.e. $\omega_k = 2Js |\sin(k/2)|$ for the lattice constant of the two-site cell:
```wl
Union[Simplify[ExpToTrig[blochphaseexp[bogoliubov[mk, "Boson", "Momentum" -> True][[1]]^2, {k -> x}]]]]
```

### Properties & Relations
The su(2) algebra is reproduced exactly when the truncation order is high enough:
```wl
snegspinoperators[{T, 1}];
snegbosonoperators[b];
Expand[holsteinprimakoff[nc[T[1, 4], T[1, 5]] - (2 + T[1, 3] - nc[T[1, 3], T[1, 3]]), T -> b, 8]]
```
---
For a large spin, the coefficient of $b^\dagger b b$ in $S^+$ approaches the $1/s$ expansion $-\sqrt{2s}/(4s)$:
```wl
Series[Sqrt[2 s - 1] - Sqrt[2 s], {s, Infinity, 1}]
```

### Possible Issues
A declared spin size is required:
```wl
snegspinoperators[G];
snegbosonoperators[b];
holsteinprimakoff[G[1, 3], G -> b, 2]
```
---
The target operator must be a boson:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
holsteinprimakoff[S[1, 3], S -> c, 2]
```
---
`holsteinprimakoff` is not `Listable`. A list with more than one element silently gives 0; map the function over the list instead:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b];
{holsteinprimakoff[{S[1, 3], S[1, 4]}, S -> b, 2], holsteinprimakoff[#, S -> b, 2] & /@ {S[1, 3], S[1, 4]}}
```

---
keywords: Bogoliubov transformation, Bogoliubov-Valatin transformation, quasiparticles, BdG, coherence factors, BCS, Majorana zero mode, Kitaev chain, Colpa, paraunitary, bosonic Bogoliubov, squeezing, spin waves, magnons, ground-state energy
see also: quadraticform, bdghamiltonian, bogoliubovrules, gaussianaverage, quadraticoperator, meanfieldsolve
related links: [J. H. P. Colpa, Physica A 93, 327 (1978)](https://doi.org/10.1016/0378-4371%2878%2990160-7); [J. Bardeen, L. N. Cooper, and J. R. Schrieffer, Phys. Rev. 108, 1175 (1957)](https://doi.org/10.1103/PhysRev.108.1175); [A. Yu. Kitaev, Phys.-Usp. 44, 131 (2001)](https://doi.org/10.1070/1063-7869/44/10S/S29)
---
# bogoliubov

## Usage
- `bogoliubov[M, "Fermion"]` diagonalizes the fermionic Bogoliubov-de Gennes (BdG) matrix *M* and returns `{eps, T}`, the quasiparticle energies $\varepsilon_m$ and the transformation matrix $T$ with $\Psi = T \Phi$.
- `bogoliubov[M, "Boson"]` does the same for a bosonic BdG matrix (paraunitary transformation).
- `bogoliubov[H, {a1, a2, ...}]` diagonalizes the quadratic Hamiltonian *H* of the modes $a_i$ and returns `{eps, T, Egs}`, including the ground-state energy $E_\mathrm{gs}$.
- `bogoliubov[Hk, type, "Momentum" -> True]` diagonalizes a Bloch BdG matrix $H(k)$ from `bdghamiltonian`.

## Details
- The BdG matrix refers to $H = \frac12 \Psi^\dagger M \Psi + E_0$ with $\Psi = (a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)^T$, as returned by `quadraticform`. The quasiparticle spinor is $\Phi = (\gamma_1, \ldots, \gamma_n, \gamma_1^\dagger, \ldots, \gamma_n^\dagger)^T$, and $T$ has the blocks $U$ (upper left), $V^*$ (upper right), $V$ (lower left), and $U^*$ (lower right), i.e. $a_i = \sum_m (U_{im} \gamma_m + V_{im}^* \gamma_m^\dagger)$. Column $m$ of $T$ contains the coefficients $(U_{1m}, \ldots, U_{nm}, V_{1m}, \ldots, V_{nm})$ of the quasiparticle $\gamma_m$.
- Fermions: $T$ is unitary and $T^\dagger M T = \mathrm{diag}(\varepsilon_1, \ldots, \varepsilon_n, -\varepsilon_1, \ldots, -\varepsilon_n)$. Then $H = \sum_m \varepsilon_m \gamma_m^\dagger \gamma_m + E_\mathrm{gs}$ with $E_\mathrm{gs} = E_0 - \frac12 \sum_m \varepsilon_m$.
- Bosons: $T$ is paraunitary, $T^\dagger \eta T = \eta$ with $\eta = \mathrm{diag}(1, \ldots, 1, -1, \ldots, -1)$, which preserves the commutation relations, and $T^\dagger M T = \mathrm{diag}(\varepsilon_1, \ldots, \varepsilon_n, \varepsilon_1, \ldots, \varepsilon_n)$. Then $H = \sum_m \varepsilon_m \gamma_m^\dagger \gamma_m + E_\mathrm{gs}$ with $E_\mathrm{gs} = E_0 + \frac12 \sum_m \varepsilon_m$.
- For numerical matrices, the quasiparticle energies are non-negative and sorted in ascending order.
- Numerical fermionic matrices are diagonalized as Hermitian matrices; the eigenvectors $x_m$ with positive eigenvalues define the quasiparticles, and the remaining columns are $C x_m = \tau_x x_m^*$. Eigenvalues with $|\lambda| \le$ `"Tolerance"` $\times \max(1, \max|\lambda|)$ are zero modes; they are combined into fermions with $\varepsilon = 0$ in a basis invariant under $C$, so that $T$ keeps the BdG structure. At $T = 0$ these modes are half occupied in `gaussianaverage`.
- Numerical bosonic matrices are diagonalized by Colpa's method: Cholesky decomposition $M = K^\dagger K$ and diagonalization of $K \eta K^\dagger$. This requires $M$ to be positive definite. Otherwise (dynamical instability, or zero modes such as Goldstone modes) the message `bogoliubov::posdef` is given and `$Failed` is returned.
- Symbolic matrices are supported if the graph of nonzero matrix elements decomposes into blocks of at most two of the $2n$ components of $\Psi$. This covers singlet pairing ($c_{\uparrow}$ with $c_{\downarrow}^\dagger$), two-mode squeezing ($a$ with $b^\dagger$), single-mode squeezing ($b$ with $b^\dagger$), and the hybridization of two levels. Larger blocks give the message `bogoliubov::symbolic`. The symbolic results assume real diagonal elements and real quasiparticle energies; the energies are listed in the order of the modes and are not sorted, and their sign is not checked.
- *M* must have the particle-hole symmetry of a BdG matrix, $\tau_x M^* \tau_x = -M$ (fermions) or $M$ (bosons), where $\tau_x$ exchanges the two halves of $\Psi$; otherwise `bogoliubov::phs` is given.
- `bogoliubov[H, modes]` computes `quadraticform[H, modes]` and diagonalizes the matrix; the modes are specified as in `quadraticform`. The result is `{eps, T, Egs}`.
- With `"Momentum" -> True`, *M* is a Bloch BdG matrix $H(k)$ for $\Psi_k = (c_k, c_{-k}^\dagger)$, whose particle-hole symmetry relates $k$ and $-k$. All $2n$ columns of $T$ are then eigenvectors of $H(k)$ (fermions, $T$ unitary) or of $\eta H(k)$ (bosons, $T$ paraunitary), and `eps` lists all $2n$ diagonal elements of $T^\dagger H(k) T$: the energies of the quasiparticles $\gamma_k$ followed by $-\varepsilon(-k)$ (fermions) or $\varepsilon(-k)$ (bosons). For numerical fermionic matrices, the first $n$ entries are the largest eigenvalues, with zero modes assigned to the particle columns first; if there are more than $n$ positive or negative eigenvalues, `bogoliubov::count` is given.
- The following options can be given:

| `"Tolerance"` | `10^-10` | relative tolerance for zero modes (fermions) and for the positive definiteness of $M$ (bosons) |
| `"Momentum"` | `False` | whether *M* is a Bloch BdG matrix $H(k)$ |

- Inexact or exact numerical matrices are diagonalized numerically; exact input is converted with `N`.

## Developer Notes
- Internally, the conjugate matrix $T^*$ is also computed (`snegbogoliubov` returns `{eps, T, Tconj}`); for symbolic matrices it is formed with the assumption of real diagonal elements and real energies, which avoids unevaluated `Conjugate` of square roots. `gaussianaverage` uses this triple to form the correlation matrix.

## Examples
### Basic Examples
On-site singlet pairing, $H = \xi (n_\uparrow + n_\downarrow) - \Delta (c_\uparrow^\dagger c_\downarrow^\dagger + c_\downarrow c_\uparrow)$. The quasiparticle energy is $E = \sqrt{\xi^2 + \Delta^2}$ and the ground-state energy $\xi - E$:
```wl
snegfermionoperators[c];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{eps, T, Egs} = bogoliubov[HS, {c[1]}];
{eps, Egs}
```
The first column of $T$ gives $c_\uparrow = u \gamma_1 + \ldots$ and $c_\downarrow^\dagger = v \gamma_1 + \ldots$ with the coherence factors $u^2 = \frac12 (1 + \xi/E)$ and $v^2 = \frac12 (1 - \xi/E)$:
```wl
E1 = Sqrt[xi^2 + Dl^2];
Simplify[{T[[1, 1]]^2 - (1 + xi/E1)/2, T[[4, 1]]^2 - (1 - xi/E1)/2}]
```
---
The Kitaev chain with three sites at $t = \Delta = 1$, $\mu = 0$ has a Majorana zero mode:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
M = First[quadraticform[H, {f[1], f[2], f[3]}]];
First[bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, "Fermion"]]
```

### Scope
#### Fermions
A numerical transformation is unitary and diagonalizes $M$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
M = First[quadraticform[H, {f[1], f[2], f[3]}]] /. {t -> 1, Dl -> 1/2, mu -> 1/3};
{eps, T} = bogoliubov[M, "Fermion"];
eps
```
```wl
Chop[{ConjugateTranspose[T] . T - IdentityMatrix[6], ConjugateTranspose[T] . M . T - DiagonalMatrix[Join[eps, -eps]]}] // Flatten // Union
```
#### Bosons
Single-mode squeezing, $H = A\, b^\dagger b + \frac{B}{2} (b^\dagger b^\dagger + b b)$, has the energy $\sqrt{A^2 - B^2}$ for $|B| < A$, and the ground-state energy $(\sqrt{A^2 - B^2} - A)/2$:
```wl
snegbosonoperators[b];
snegrealconstants[A, B];
{eps, T, Egs} = bogoliubov[A number[b[]] + B/2 (nc[b[CR], b[CR]] + nc[b[AN], b[AN]]), {b}];
{eps, Egs}
```
---
Two coupled bosons with hopping and pairing; the numerical transformation is paraunitary:
```wl
snegbosonoperators[b];
H = 2 number[b[1]] + 3 number[b[2]] + (nc[b[CR, 1], b[AN, 2]] + nc[b[CR, 2], b[AN, 1]]) + (nc[b[CR, 1], b[CR, 2]] + nc[b[AN, 2], b[AN, 1]]);
{eps, T, Egs} = bogoliubov[H, {b[1], b[2]}]
```
```wl
eta = DiagonalMatrix[{1, 1, -1, -1}];
Chop[ConjugateTranspose[T] . eta . T - eta]
```
#### Bloch BdG matrices
BCS quasiparticles from the Bloch BdG matrix of `bdghamiltonian`; the list contains the energies of $\gamma_{k\uparrow}$, $\gamma_{k\downarrow}$ followed by $-E_{-k}$ twice:
```wl
snegfermionoperators[ck];
snegrealconstants[Dl];
snegrealfunctions[xi];
snegmomentumindexes[{k}, Ns];
hk = First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]];
First[bogoliubov[hk /. xi[-k] -> xi[k], "Fermion", "Momentum" -> True]]
```

### Options
#### Momentum
For a Bloch BdG matrix without inversion symmetry, $h(k) \neq h(-k)$, the particle and hole energies at a given $k$ differ. Here a numerical $H(k)$ of a chain with a $\sin k$ term in $h(k)$:
```wl
hk = {{-2 Cos[0.7] + 0.5 Sin[0.7] - 0.3, 0.4 I Sin[0.7]}, {-0.4 I Sin[0.7], 2 Cos[0.7] + 0.5 Sin[0.7] + 0.3}};
bogoliubov[hk, "Fermion", "Momentum" -> True][[1]]
```
Without the option, the matrix is rejected:
```wl
bogoliubov[hk, "Fermion"]
```
#### Tolerance
Eigenvalues below the tolerance are treated as zero modes:
```wl
{First[bogoliubov[{{10^-6, 0}, {0, -10^-6}}, "Fermion"]], First[bogoliubov[{{10^-6, 0}, {0, -10^-6}}, "Fermion", "Tolerance" -> 10^-3]]}
```

### Applications
#### Majorana end states of the Kitaev chain
In the topological phase $|\mu| < 2t$, the open Kitaev chain has a quasiparticle with an energy that vanishes exponentially with the length $L$. Its weight $|U_{jm}|^2 + |V_{jm}|^2$ is localized at the two ends of the chain:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
L = 8;
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu Sum[number[f[j]], {j, L}];
M = First[quadraticform[H, Table[f[j], {j, L}]]];
{eps, T} = bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 1/2}, "Fermion"];
eps
```
```wl
Table[Abs[T[[j, 1]]]^2 + Abs[T[[L + j, 1]]]^2, {j, L}]
```
In the trivial phase the lowest excitation is gapped and extended:
```wl
{eps, T} = bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 3}, "Fermion"];
{First[eps], Table[Abs[T[[j, 1]]]^2 + Abs[T[[L + j, 1]]]^2, {j, L}]}
```
#### Two-mode squeezing
$H = \omega (a^\dagger a + b^\dagger b) + \lambda (a^\dagger b^\dagger + b a)$ decomposes into the blocks $(a, b^\dagger)$ and $(b, a^\dagger)$ and is diagonalized symbolically; the energies are $\sqrt{\omega^2 - \lambda^2}$:
```wl
snegbosonoperators[a, b];
snegrealconstants[w, lam];
{eps, T, Egs} = bogoliubov[w (number[a[]] + number[b[]]) + lam (nc[a[CR], b[CR]] + nc[b[AN], a[AN]]), {a, b}];
{eps, Egs}
```
#### Spin waves of a finite antiferromagnet
The Heisenberg ring with four spins 1/2 in a staggered field, $H = J \sum_i \mathbf S_i \cdot \mathbf S_{i+1} - h \sum_i (-1)^{i+1} S^z_i$, expanded with Holstein-Primakoff bosons around the Néel state (sublattice B rotated by $\pi$ about $x$). The magnon energies are $\sqrt{(1 + h)^2 - \cos^2 k}$, $k = 0, \pm\pi/2, \pi$, in units of $J$, and $E_\mathrm{gs}$ includes the zero-point energy:
```wl
snegspinoperators[{S, 1/2}];
snegbosonoperators[b];
snegrealconstants[hs];
h = Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}] - hs Sum[(-1)^(i + 1) S[i, 3], {i, 4}];
h = rotatespin[h, S, If[EvenQ[First[{##}]], RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
hb = holsteinprimakoff[h, S -> b, 2];
{eps, T, Egs} = bogoliubov[hb /. hs -> 1/10, Table[b[i], {i, 4}]];
{eps, Egs}
```
```wl
N[Sqrt[(1 + 1/10)^2 - Cos[{0, Pi/2, Pi/2, Pi}]^2]]
```

### Properties & Relations
The many-body spectrum is $E_\mathrm{gs} + \sum_m n_m \varepsilon_m$, $n_m \in \{0, 1\}$. The ground-state energy agrees with exact diagonalization in the Fock space (`thermalaverage` of $H$ at $T = 0$):
```wl
snegspinlessfermionoperators[f];
H = Sum[-hop[f[j], f[j + 1]] + 1/2 (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 3}] - 1/3 Sum[number[f[j]], {j, 4}];
modes = Table[f[j], {j, 4}];
{eps, T, Egs} = bogoliubov[H, modes];
{Egs, thermalaverage[H, H, modes, 0]}
```
---
`bogoliubovrules` rewrites the Hamiltonian in terms of the quasiparticle operators:
```wl
snegspinlessfermionoperators[g];
Chop[Expand[H /. bogoliubovrules[T, modes, g]] - Sum[eps[[m]] number[g[m]], {m, 4}] - Egs]
```
---
`quadraticoperator` of the diagonal matrix reproduces the constant $-\frac12 \sum_m \varepsilon_m$:
```wl
snegspinlessfermionoperators[g];
snegrealconstants[e1, e2];
quadraticoperator[DiagonalMatrix[{e1, e2, -e1, -e2}], {g[1], g[2]}]
```

### Possible Issues
A bosonic matrix that is not positive definite cannot be diagonalized by a paraunitary transformation. The Heisenberg ring without a staggered field has Goldstone zero modes:
```wl
snegspinoperators[{S, 1/2}];
snegbosonoperators[b];
h = Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];
h = rotatespin[h, S, If[EvenQ[First[{##}]], RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
bogoliubov[holsteinprimakoff[h, S -> b, 2], Table[b[i], {i, 4}]]
```
---
Symbolic matrices are limited to blocks of at most two components; the Kitaev chain couples all components:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
bogoliubov[First[quadraticform[H, {f[1], f[2], f[3]}]], "Fermion"]
```
---
Symbolic energies are not checked for their sign. For a single level with energy $\epsilon$, the result is $\varepsilon = \epsilon$, which is the quasiparticle energy only for $\epsilon > 0$; for $\epsilon < 0$ the roles of $\gamma$ and $\gamma^\dagger$ are exchanged and $E_\mathrm{gs} = E_0 - \epsilon/2$ is not the ground-state energy:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[e];
bogoliubov[e number[f[1]], {f[1]}]
```
```wl
bogoliubov[-number[f[1]], {f[1]}]
```
---
Exact zero modes of real BdG matrices can make the numerical diagonalization fail with `bogoliubov::phs`, although the matrix is a valid BdG matrix. This happens for the Kitaev chain with five sites at $t = \Delta$, $\mu = 0$: the zero-mode eigenvectors returned by `Eigensystem` are odd under the particle-hole conjugation $C$, and the construction of the $C$-invariant basis fails:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
L = 5;
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu Sum[number[f[j]], {j, L}];
M = First[quadraticform[H, Table[f[j], {j, L}]]];
bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, "Fermion"]
```
The same failure can occur for other real BdG matrices with exact zero modes. The energies are still available from the eigenvalues $\pm\varepsilon_m$ of $M$, and `gaussianaverage`, which does not construct $T$ for numerical fermionic matrices, gives the averages; here the occupation of the end sites:
```wl
Sort[Chop[Eigenvalues[N[M /. {t -> 1, Dl -> 1, mu -> 0}]]]]
```
```wl
gaussianaverage[{number[f[1]], number[f[3]]}, M /. {t -> 1, Dl -> 1, mu -> 0}, Table[f[j], {j, L}], 0]
```

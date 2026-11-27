---
keywords: quadratic Hamiltonian, bilinear form, Bogoliubov-de Gennes, BdG matrix, Nambu spinor, anomalous terms, pairing, hopping matrix, single-particle Hamiltonian, squeezing, linear boson terms
see also: quadraticoperator, bogoliubov, bdghamiltonian, gaussianaverage, meanfield, blochhamiltonian, matrixhop
---
# quadraticform

## Usage
- `quadraticform[H, {a1, a2, ...}]` returns `{M, E0}`, the Bogoliubov-de Gennes (BdG) matrix $M$ and the constant $E_0$ with $H = \frac12 \Psi^\dagger M \Psi + E_0$, $\Psi = (a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)^T$, for a Hamiltonian *H* quadratic in the modes $a_i$.
- `quadraticform[H, modes, "BdG" -> False]` returns `{h, C}` with $H = \sum_{ij} a_i^\dagger h_{ij} a_j + C$ for a number-conserving *H*.
- `quadraticform[H, modes, "Linear" -> True]` returns `{M, f, E0}`, including the coefficients $f_i$ of linear boson terms $\sum_i (f_i b_i^\dagger + f_i^* b_i)$.

## Details
- A general quadratic Hamiltonian of $n$ fermion or boson modes,
$$H = \sum_{ij} h_{ij} a_i^\dagger a_j + \frac12 \sum_{ij} (\Delta_{ij} a_i^\dagger a_j^\dagger + \Delta_{ij}^* a_j a_i) + C,$$
is written in the BdG (Nambu) form $H = \frac12 \Psi^\dagger M \Psi + E_0$. The Nambu spinor $\Psi$ lists all annihilation operators first, followed by the creation operators in the same order.
- For fermions, the $2n \times 2n$ matrix $M$ has the blocks $h$ (upper left), $\Delta$ (upper right), $\Delta^\dagger$ (lower left), and $-h^T$ (lower right), with $h = h^\dagger$ and $\Delta^T = -\Delta$. The constant is $E_0 = C + \frac12 \mathrm{Tr}\, h$.
- For bosons, the blocks are $A$, $B$, $B^*$, $A^*$, with $A = A^\dagger$ and $B^T = B$, and $E_0 = C - \frac12 \mathrm{Tr}\, A$.
- The matrix elements are computed as $M_{pq} = \eta_p \eta_q [\Psi_p, [H, \Psi_q^\dagger]]_\mp$, with the anticommutator for fermions and the commutator for bosons; $\eta_p = 1$ for fermions, and $\eta = (1, \ldots, 1, -1, \ldots, -1)$ for bosons. $M$ is therefore always in the symmetrized form above, independently of how the terms of $H$ are ordered or split, e.g. $\Delta\, c_1^\dagger c_2^\dagger$ and $-\Delta\, c_2^\dagger c_1^\dagger$ give the same $M$.
- The modes are specified in one of the following forms:

| `c` or `c[j...]` | a fermion level or a boson; for fermions with spin, the spin index is added as the last index and varies fastest, e.g. `{c[1], c[2]}` gives $(c_{1\uparrow}, c_{1\downarrow}, c_{2\uparrow}, c_{2\downarrow})$; spinless fermions get the index `0` |
| `Function[t, expr]` | a mode given by its operator for `t = CR` and `t = AN`, e.g. a bonding orbital $(c_1 + c_2)/\sqrt2$ |

- All modes must be of the same statistics (all fermions or all bosons).
- The following options can be given:

| `"BdG"` | `True` | if `False`, return the hopping matrix $h$ ($n \times n$) and the constant $C$ instead of $M$ and $E_0$ |
| `"Linear"` | `False` | if `True`, return the linear boson coefficients $f_i$ as the middle element |
| `"Check"` | `True` | check that $M$ is Hermitian (and $g_i = f_i^*$ for the linear terms) |

- With `"BdG" -> False`, anomalous terms ($\Delta \neq 0$) are not allowed. For fermions $C = E_0 - \frac12 \mathrm{Tr}\, h$, for bosons $C = E_0 + \frac12 \mathrm{Tr}\, A$.
- For fermions, `"Linear" -> True` returns a list of zeros as $f$; terms linear in the fermion operators are always rejected.
- `quadraticform` fails with a message and returns `$Failed` if $H$ contains terms of higher order (`quadraticform::order`), operators that are not among the modes (`quadraticform::rest`), linear fermion terms (`quadraticform::odd`), linear boson terms without `"Linear" -> True` (`quadraticform::linear`), anomalous terms with `"BdG" -> False` (`quadraticform::anom`), or symbolic sums (`quadraticform::sum`). A non-Hermitian $M$ gives the warning `quadraticform::herm`, and the result is still returned.
- Symbolic sums are not supported. For translation-invariant Hamiltonians in momentum space use `bdghamiltonian` or `blochhamiltonian`; finite clusters can be written explicitly with `sumCluster`.
- Coefficients must be numbers in the sense of `isnumericQ`: declare parameters with `snegrealconstants` or `snegcomplexconstants`. Mean-field averages `avg[...]` are treated as numbers, so mean-field Hamiltonians from `meanfield` can be passed directly.
- Inexact coefficients are allowed; rounding errors of order $10^{-10}$ are removed with `Chop`.

## Developer Notes
- The modes are converted to functions `t -> operator`; the same parsing is used by `quadraticoperator`, `bogoliubov`, `bogoliubovrules`, `gaussianaverage`, `meanfieldsolve`, `thermalaverage`, and `lehmann`.
- After $M$ is computed from double (anti)commutators, the remainder $H - \frac12 \Psi^\dagger M \Psi$ must be a c-number (plus linear boson terms), which detects all non-quadratic content.

## Examples
### Basic Examples
The Kitaev chain with three sites, $H = \sum_{j=1}^{2} [-t(f_j^\dagger f_{j+1} + \mathrm{h.c.}) + \Delta (f_j^\dagger f_{j+1}^\dagger + f_{j+1} f_j)] - \mu \sum_{j=1}^{3} n_j$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
{M, E0} = quadraticform[H, {f[1], f[2], f[3]}];
MatrixForm[M]
```
The constant is $E_0 = \frac12 \mathrm{Tr}\, h = -3\mu/2$:
```wl
E0
```
`quadraticoperator` reconstructs the Hamiltonian:
```wl
Expand[quadraticoperator[M, {f[1], f[2], f[3]}, E0] - H]
```

### Scope
#### Number-conserving Hamiltonians
For a tight-binding ring of four sites, `"BdG" -> False` gives the hopping matrix $h$; its eigenvalues are the band energies $-2t\cos k$, $k = 0, \pm\pi/2, \pi$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
H = -t Sum[hop[f[j], f[Mod[j, 4] + 1]], {j, 4}];
{h, C0} = quadraticform[H, Table[f[j], {j, 4}], "BdG" -> False];
{MatrixForm[h], C0, Eigenvalues[h]}
```
---
For spin-1/2 fermions, the spin index varies fastest; the basis is $(c_{1\uparrow}, c_{1\downarrow}, c_{2\uparrow}, c_{2\downarrow})$:
```wl
snegfermionoperators[c];
snegrealconstants[t, B];
quadraticform[-t hop[c[1], c[2]] + B (spinz[c[1]] + spinz[c[2]]), {c[1], c[2]}, "BdG" -> False]
```
#### Spin-singlet pairing
On-site singlet pairing $-\Delta (c_\uparrow^\dagger c_\downarrow^\dagger + c_\downarrow c_\uparrow)$ appears in the off-diagonal blocks; the antisymmetric $\Delta$ block has $\Delta_{\uparrow\downarrow} = -\Delta$:
```wl
snegfermionoperators[c];
snegrealconstants[xi, Dl];
HS = xi number[c[1]] - Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{M, E0} = quadraticform[HS, {c[1]}];
{MatrixForm[M], E0}
```
#### Modes given as functions
Bonding and antibonding orbitals $(f_1 \pm f_2)/\sqrt2$ diagonalize the hopping $-t (f_1^\dagger f_2 + \mathrm{h.c.})$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
bond = Function[x, (f[x, 1, 0] + f[x, 2, 0])/Sqrt[2]];
anti = Function[x, (f[x, 1, 0] - f[x, 2, 0])/Sqrt[2]];
quadraticform[-t hop[f[1], f[2]], {bond, anti}, "BdG" -> False]
```
#### Bosons
A single squeezed boson mode $H = A\, b^\dagger b + \frac{B}{2} (b^\dagger b^\dagger + b b)$; the constant is $E_0 = -A/2$:
```wl
snegbosonoperators[b];
snegrealconstants[A, B];
quadraticform[A number[b[]] + B/2 (nc[b[CR], b[CR]] + nc[b[AN], b[AN]]), {b}]
```

### Options
#### BdG
With `"BdG" -> False`, the constant is the c-number part $C$ of the normal-ordered Hamiltonian, not $E_0$:
```wl
snegfermionoperators[c];
snegrealconstants[eps];
{quadraticform[eps number[c[1]], {c[1]}], quadraticform[eps number[c[1]], {c[1]}, "BdG" -> False]}
```
#### Linear
A displaced harmonic oscillator $H = \omega b^\dagger b + \lambda (b^\dagger + b)$ has linear terms with $f = \lambda$:
```wl
snegbosonoperators[b];
snegrealconstants[w, lam];
quadraticform[w number[b[]] + lam (b[CR] + b[AN]), {b}, "Linear" -> True]
```
Without the option, the linear terms are rejected:
```wl
quadraticform[w number[b[]] + lam (b[CR] + b[AN]), {b}]
```
#### Check
A non-Hermitian operator gives a warning; the matrix is returned:
```wl
snegspinlessfermionoperators[f];
quadraticform[nc[f[CR, 1, 0], f[AN, 2, 0]], {f[1], f[2]}, "BdG" -> False]
```
With `"Check" -> False`, no check is made:
```wl
quadraticform[nc[f[CR, 1, 0], f[AN, 2, 0]], {f[1], f[2]}, "BdG" -> False, "Check" -> False]
```

### Applications
#### Kitaev chain: Majorana zero mode
At $t = \Delta$, $\mu = 0$, the open Kitaev chain has an exact zero-energy mode formed by the Majorana operators at the two ends. The quasiparticle energies follow from the BdG matrix with `bogoliubov`:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
L = 4;
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, L - 1}] - mu Sum[number[f[j]], {j, L}];
{M, E0} = quadraticform[H, Table[f[j], {j, L}]];
Chop[First[bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, "Fermion"]]]
```
In the trivial phase $|\mu| > 2t$, all excitations are gapped:
```wl
First[bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 3}, "Fermion"]]
```
#### Mean-field Hamiltonians
Averages `avg[...]` are numbers for `quadraticform`. The Hartree-Fock-Bogoliubov decoupling of the Hubbard interaction gives a BdG matrix with the anomalous average $\langle c_\downarrow c_\uparrow \rangle$ in the off-diagonal blocks:
```wl
snegfermionoperators[c];
snegrealconstants[U, eps];
Hmf = meanfield[eps number[c[1]] + U hubbard[c[1]], {"Hartree", "Pairing"}];
{M, E0} = quadraticform[Hmf, {c[1]}];
MatrixForm[M]
```
#### Polaron shift
For the displaced oscillator, completing the square, $\omega b^\dagger b + \lambda (b^\dagger + b) = \omega \tilde b^\dagger \tilde b - \lambda^2/\omega$ with $\tilde b = b + \lambda/\omega$. The ground-state energy follows from $E_0$ and the linear coefficients:
```wl
snegbosonoperators[b];
snegrealconstants[w, lam];
{M, fl, E0} = quadraticform[w number[b[]] + lam (b[CR] + b[AN]), {b}, "Linear" -> True, "BdG" -> False];
E0 - First[fl]^2/M[[1, 1]]
```

### Properties & Relations
The BdG matrix is Hermitian and has the particle-hole symmetry $\tau_x M^* \tau_x = -M$ (fermions), so its eigenvalues come in pairs $\pm E$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
M = First[quadraticform[H, {f[1], f[2], f[3]}]];
taux = ArrayFlatten[{{0, IdentityMatrix[3]}, {IdentityMatrix[3], 0}}];
{M === ConjugateTranspose[M], Expand[taux . Conjugate[M] . taux + M] === ConstantArray[0, {6, 6}]}
```
```wl
Sort[Eigenvalues[M /. {t -> 1, Dl -> 1/2, mu -> 1/3}] // N]
```
---
The result does not depend on how the operators are ordered in $H$. Here the pairing term is written as $-\Delta (f_2^\dagger f_1^\dagger + f_1 f_2)$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[Dl];
quadraticform[Dl (nc[f[CR, 1, 0], f[CR, 2, 0]] + nc[f[AN, 2, 0], f[AN, 1, 0]]), {f[1], f[2]}] === quadraticform[-Dl (nc[f[CR, 2, 0], f[CR, 1, 0]] + nc[f[AN, 1, 0], f[AN, 2, 0]]), {f[1], f[2]}]
```
---
For a number-conserving fermion Hamiltonian the two forms are related by $E_0 = C + \frac12 \mathrm{Tr}\, h$:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu];
H = -t hop[f[1], f[2]] - mu (number[f[1]] + number[f[2]]) + 1;
{{M, E0}, {h, C0}} = {quadraticform[H, {f[1], f[2]}], quadraticform[H, {f[1], f[2]}, "BdG" -> False]};
E0 == C0 + Tr[h]/2
```

### Possible Issues
Parameters must be declared as constants. An undeclared symbol is treated as an operator, and the Hamiltonian is not quadratic:
```wl
snegspinlessfermionoperators[f];
quadraticform[g hop[f[1], f[2]], {f[1], f[2]}]
```
---
All operators of $H$ must be among the modes:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
quadraticform[-t hop[f[1], f[2]], {f[1]}]
```
---
Interaction terms are rejected; decouple them first with `meanfield`:
```wl
snegfermionoperators[c];
snegrealconstants[U];
quadraticform[U hubbard[c[1]], {c[1]}]
```
---
Symbolic sums are not supported:
```wl
snegspinlessfermionoperators[f];
snegfreeindexes[j];
quadraticform[sum[number[f[j]], {j}], {f[1]}]
```

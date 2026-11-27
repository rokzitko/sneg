---
keywords: expectation value, thermal average, ground-state average, Gaussian state, Wick's theorem, correlation matrix, Fermi function, Bose function, BCS, anomalous average, quantum depletion, Friedel oscillations
see also: quadraticform, bogoliubov, bdghamiltonian, meanfield, meanfieldsolve, thermalaverage, avg, vevwick
---
# gaussianaverage

## Usage
- `gaussianaverage[expr, M, {a1, ..., an}, T]` gives the expectation value of the operator expression *expr* in the thermal state at temperature *T* of the quadratic Hamiltonian with the Bogoliubov-de Gennes matrix *M* of the modes $a_i$.
- `gaussianaverage[expr, H, {a1, ..., an}, T]` uses the quadratic Hamiltonian *H* itself.
- `gaussianaverage[expr, Hk, {ck[a], ...}, T, "Momentum" -> k]` uses the Bloch BdG matrix $H(k)$ (or a momentum-space Hamiltonian) of a translation-invariant system; *expr* contains operators with momenta $k$ and $-k$.
- `gaussianaverage[{expr1, expr2, ...}, ...]` gives a list of expectation values.

## Details
- The state is the Gibbs state $\rho = e^{-H/T}/\mathrm{Tr}\, e^{-H/T}$ of the quadratic Hamiltonian ($k_B = 1$). For `T = 0` it is the ground state. The chemical potential, if any, must be included in $H$; the average is grand canonical.
- The modes are specified as in `quadraticform`: `c` or `c[j...]` (for fermions with spin, the spin index is added as the last index and varies fastest), or `Function[t, expr]`. The matrix *M* refers to the Nambu spinor $\Psi = (a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)^T$; if *H* is given instead, $M$ is computed with `quadraticform`.
- The averages are computed from the correlation matrix $G_{pq} = \langle \Psi_p \Psi_q^\dagger \rangle$ with Wick's theorem. *expr* may be any polynomial in the mode operators, with products of any number of operators; terms with an odd number of operators give 0, and c-numbers are kept.
- For numerical fermionic matrices, $G = (1 + e^{-M/T})^{-1}$ is computed directly from the eigenvectors of $M$; at $T = 0$, eigenvalues with $|\lambda| \le$ `"Tolerance"` (zero modes) are half occupied, which is the average over the degenerate ground states.
- Otherwise $G = T_B D T_B^\dagger$ with the transformation $T_B$ of `bogoliubov` and the quasiparticle occupations: Fermi function $1/(e^{\varepsilon/T} + 1)$ or Bose function $1/(e^{\varepsilon/T} - 1)$. Symbolic matrices are therefore restricted to blocks of at most two components, and bosonic matrices must be positive definite. At $T = 0$, the symbolic quasiparticle energies are assumed to be positive.
- With `"Momentum" -> k`, the modes are orbital templates as in `bdghamiltonian`, and the averages are taken in the thermal state of $\frac12 \Psi_k^\dagger H(k) \Psi_k$ with $\Psi_k = (c_{k a}, \ldots, c_{-k a}^\dagger, \ldots)$. The operators in *expr* must be components of $\Psi_k$ or $\Psi_k^\dagger$, i.e. $c_{k a}$, $c_{-k a}$ and their conjugates. For fermions, the occupations of the $2n$ columns are $1 - f(\varepsilon)$ for the particle and hole branches at $T > 0$, which accounts for the energies $-\varepsilon(-k)$ of the hole branch.
- The following options can be given:

| `"Momentum"` | `None` | momentum index *k* for Bloch BdG matrices |
| `"IndexPosition"` | `1` | position of the momentum among the indexes after `CR`/`AN`, as in `bdghamiltonian` |
| `"Tolerance"` | `10^-10` | tolerance for zero modes and for the diagonalization |

- Messages: `gaussianaverage::op` if *expr* contains operators that are not among the modes or terms that are not products of operators, `gaussianaverage::sum` for symbolic sums, `gaussianaverage::index` if the momentum is not a declared momentum index, and `quadraticoperator::dim` if the dimension of *M* does not match the modes. Failures of `quadraticform`, `bdghamiltonian`, and `bogoliubov` are passed on. In all these cases the result is `$Failed`.

## Developer Notes
- `meanfieldsolve` uses the same correlation matrix and contraction functions (`snegcorrelation`, `snegcontraction`, `snegwick`) for its self-consistency loop.

## Examples
### Basic Examples
Ground-state averages for a tight-binding dimer with one electron per spin: the occupation of site 1 and the bond order $\langle c_{1\uparrow}^\dagger c_{2\uparrow} \rangle = 1/2$ of the bonding orbital:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]];
gaussianaverage[{number[c[1]], nc[c[CR, 1, UP], c[AN, 2, UP]]}, H, {c[1], c[2]}, 0]
```
---
A single level at temperature $T$ is occupied according to the Fermi function:
```wl
snegfermionoperators[d];
snegrealconstants[eps, T];
gaussianaverage[number[d[], UP], eps number[d[]], {d}, T]
```

### Scope
#### Bosons
The Bose function for a harmonic oscillator, and the quantum depletion $\langle b^\dagger b \rangle = \frac12 (A/\varepsilon - 1)$, $\varepsilon = \sqrt{A^2 - B^2}$, of a squeezed mode at $T = 0$:
```wl
snegbosonoperators[b];
snegrealconstants[w, A, B, T];
{gaussianaverage[number[b[]], w number[b[]], {b}, T], gaussianaverage[number[b[]], {{A, B}, {B, A}}, {b}, 0]}
```
#### Anomalous averages and Wick's theorem
For on-site singlet pairing, the occupation, the pair amplitude, and the double occupancy. By Wick's theorem, $\langle n_\uparrow n_\downarrow \rangle = \langle n_\uparrow \rangle \langle n_\downarrow \rangle + |\langle c_\downarrow c_\uparrow \rangle|^2 = v^4 + u^2 v^2 = v^2$:
```wl
snegfermionoperators[c];
H = 4/5 number[c[1]] - 3/5 (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
gaussianaverage[{number[c[1], UP], nc[c[AN, 1, DO], c[AN, 1, UP]], hubbard[c[1]]}, H, {c[1]}, 0]
```
#### BdG matrices
The matrix from `quadraticform` can be given instead of the Hamiltonian, which avoids recomputing it:
```wl
snegspinlessfermionoperators[f];
H = Sum[-hop[f[j], f[j + 1]] + 1/2 (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}];
M = First[quadraticform[H, {f[1], f[2], f[3]}]];
gaussianaverage[{number[f[1]], nc[f[AN, 2, 0], f[AN, 1, 0]], nc[number[f[1]], number[f[3]]]}, M, {f[1], f[2], f[3]}, 1/2]
```
#### Momentum space
BCS averages from the Bloch BdG matrix at $\xi_k = 2/5$, $\Delta = 3/10$, $E_k = 1/2$: $\langle c_{-k\downarrow} c_{k\uparrow} \rangle = \Delta/(2E_k) = 3/10$ and $\langle n_{k\uparrow} \rangle = v_k^2 = 1/10$:
```wl
snegfermionoperators[ck];
snegrealconstants[Dl];
snegrealfunctions[xi];
snegmomentumindexes[{k}, Ns];
hk = First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> xi[k];
Simplify[gaussianaverage[{nc[ck[AN, -k, DO], ck[AN, k, UP]], number[ck[k], UP]}, hk, {ck}, 0, "Momentum" -> k] /. {xi[k] -> 2/5, Dl -> 3/10}]
```

### Options
#### Momentum
With `"Momentum" -> k`, the second argument may also be the momentum-space Hamiltonian, and the operators with momenta $k$ and $-k$ are recognized as components of $\Psi_k$ and $\Psi_k^\dagger$:
```wl
snegspinlessfermionoperators[fk];
snegmomentumindexes[{k}, Ns];
gaussianaverage[{number[fk[k]], number[fk[-k]]}, sum[-number[fk[k]], {k}], {fk}, 0, "Momentum" -> k]
```
#### Tolerance
A level with energy $10^{-6}$ is empty at $T = 0$; with a larger tolerance it is treated as a zero mode and half occupied:
```wl
snegspinlessfermionoperators[f];
{gaussianaverage[number[f[1]], {{10^-6, 0}, {0, -10^-6}}, {f[1]}, 0], gaussianaverage[number[f[1]], {{10^-6, 0}, {0, -10^-6}}, {f[1]}, 0, "Tolerance" -> 10^-3]}
```

### Applications
#### Friedel oscillations
The density of spinless fermions in an open tight-binding chain of 12 sites below half filling ($\mu = -1$) oscillates with the wave vector $2k_F$ near the ends:
```wl
snegspinlessfermionoperators[f];
L = 12;
H = -Sum[hop[f[j], f[j + 1]], {j, L - 1}] + Sum[number[f[j]], {j, L}];
dens = gaussianaverage[Table[number[f[j]], {j, L}], H, Table[f[j], {j, L}], 0];
NumberForm[dens, 3]
```
#### BCS gap equation at finite temperature
The thermal anomalous average $F_k = \langle c_{-k\downarrow} c_{k\uparrow} \rangle = \frac{\Delta}{2E_k} \tanh\frac{E_k}{2T}$ gives the gap equation $\Delta = \frac{g}{N_s} \sum_k F_k$. Here it is solved for a band with four momenta, $\xi_k = -2\cos k + 1/2$, and $g = 3$, at $T = 1/10$:
```wl
snegfermionoperators[ck];
snegrealconstants[Dl, x, T];
snegrealfunctions[xi];
snegmomentumindexes[{k}, Ns];
hk = First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> xi[k];
F = FullSimplify[ExpToTrig[gaussianaverage[nc[ck[AN, -k, DO], ck[AN, k, UP]], hk, {ck}, T, "Momentum" -> k] /. xi[k] -> x], Dl > 0 && T > 0 && Element[x, Reals]]
```
```wl
FindRoot[Dl == 3/4 Sum[F /. {x -> -2 Cos[q] + 1/2, T -> 1/10}, {q, {0, Pi/2, Pi, 3 Pi/2}}], {Dl, 1}]
```
The same gap follows from the self-consistent mean-field solution of the attractive Hubbard ring with `meanfieldsolve`.
#### Quantum depletion in Bogoliubov theory
For weakly interacting bosons, $H = \sum_k [(\epsilon_k + n_0 U) b_k^\dagger b_k + \frac{n_0 U}{2} (b_k^\dagger b_{-k}^\dagger + b_{-k} b_k)]$; the number of bosons outside the condensate in the mode $k$ at $T = 0$ is $\frac12 [(\epsilon_k + n_0 U)/E_k - 1]$ with the Bogoliubov dispersion $E_k = \sqrt{\epsilon_k (\epsilon_k + 2 n_0 U)}$:
```wl
snegbosonoperators[bk];
snegrealconstants[n0U];
snegrealfunctions[ek];
snegmomentumindexes[{k}, Ns];
hb = sum[(ek[k] + n0U) number[bk[k]] + n0U/2 (nc[bk[CR, k], bk[CR, -k]] + nc[bk[AN, -k], bk[AN, k]]), {k}];
hk = First[bdghamiltonian[hb, {bk}, k]] /. ek[-k] -> ek[k];
nk = gaussianaverage[number[bk[k]], hk, {bk}, 0, "Momentum" -> k];
Simplify[nk - ((ek[k] + n0U)/Sqrt[ek[k] (ek[k] + 2 n0U)] - 1)/2, ek[k] > 0 && n0U > 0]
```

### Properties & Relations
For quadratic Hamiltonians, the result agrees with the exact thermal average of `thermalaverage`, which diagonalizes the Hamiltonian in the Fock space:
```wl
snegfermionoperators[c];
H = -hop[c[1], c[2]] + 1/3 number[c[1]];
obs = nc[number[c[1], UP], number[c[2], DO]] + nc[c[CR, 1, UP], c[AN, 2, UP]];
{gaussianaverage[obs, H, {c[1], c[2]}, 1/2], thermalaverage[obs, H, {c[1], c[2]}, 1/2]}
```
---
The average of a mean-field Hamiltonian, with the averages taken in the same Gaussian state, equals the Wick average of the interacting Hamiltonian:
```wl
snegfermionoperators[c];
H0 = -hop[c[1], c[2]] + 1/5 number[c[1]];
Hint = 2 (hubbard[c[1]] + hubbard[c[2]]);
mf = meanfield[Hint];
rules = Map[# -> gaussianaverage[First[#], H0, {c[1], c[2]}, 1/3] &, Union[Cases[mf, _avg, Infinity]]];
{gaussianaverage[mf /. rules, H0, {c[1], c[2]}, 1/3], gaussianaverage[Hint, H0, {c[1], c[2]}, 1/3]}
```

### Possible Issues
At $T = 0$, symbolic quasiparticle energies are assumed to be positive. For a level with symbolic energy $\epsilon$ the result is 0, which is correct only for $\epsilon > 0$:
```wl
snegfermionoperators[d];
snegrealconstants[eps];
{gaussianaverage[number[d[], UP], eps number[d[]], {d}, 0], gaussianaverage[number[d[], UP], -number[d[]], {d}, 0]}
```
---
The state is grand canonical: the particle number is controlled by the chemical potential included in $H$. For the tight-binding dimer without chemical potential, only the bonding level at $-1$ is occupied (two electrons); with $\mu = 3/2$, all four states are filled:
```wl
snegfermionoperators[c];
{gaussianaverage[number[c[1]] + number[c[2]], -hop[c[1], c[2]], {c[1], c[2]}, 0], gaussianaverage[number[c[1]] + number[c[2]], -hop[c[1], c[2]] - 3/2 (number[c[1]] + number[c[2]]), {c[1], c[2]}, 0]}
```
---
All operators must be among the modes:
```wl
snegfermionoperators[c];
gaussianaverage[number[c[3]], -hop[c[1], c[2]], {c[1], c[2]}, 0]
```

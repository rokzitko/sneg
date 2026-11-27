---
keywords: Bogoliubov-de Gennes Hamiltonian, BdG, Bloch Hamiltonian, Nambu spinor, momentum space, superconductivity, BCS, Kitaev chain, topological superconductor, spin waves, magnons, antiferromagnet, Holstein-Primakoff
see also: blochhamiltonian, quadraticform, bogoliubov, gaussianaverage, fourier, holsteinprimakoff, snegmomentumindexes
related links: [J. Bardeen, L. N. Cooper, and J. R. Schrieffer, Phys. Rev. 108, 1175 (1957)](https://doi.org/10.1103/PhysRev.108.1175); [A. Yu. Kitaev, Phys.-Usp. 44, 131 (2001)](https://doi.org/10.1070/1063-7869/44/10S/S29)
---
# bdghamiltonian

## Usage
- `bdghamiltonian[H, {ck[a], ck[b], ...}, k]` returns `{Hk, E0}`, the Bloch Bogoliubov-de Gennes matrix $H(k)$ and the constant $E_0$ with $H = \frac12 \sum_k \Psi_k^\dagger H(k) \Psi_k + E_0$, for a translation-invariant quadratic Hamiltonian *H* in momentum space, with the Nambu spinor $\Psi_k = (c_{k a}, c_{k b}, \ldots, c_{-k a}^\dagger, c_{-k b}^\dagger, \ldots)^T$.

## Details
- *H* is a symbolic sum over the momentum index *k* (declared with `snegmomentumindexes`), typically obtained with `fourier` from a lattice Hamiltonian. It may contain anomalous terms $c_k^\dagger c_{-k}^\dagger$, $c_{-k} c_k$ (fermions) or $b_k^\dagger b_{-k}^\dagger$, $b_{-k} b_k$ (bosons), which `blochhamiltonian` does not describe.
- The orbitals are given as templates, as in `blochhamiltonian`: `ck` or `ck[a...]`. The momentum is inserted at the position given by `"IndexPosition"` (by default as the first index after `CR`/`AN`); for fermions with spin, the spin index is added as the last index and varies fastest ($\uparrow, \downarrow$ for spin 1/2, `0` for spinless fermions). For example, `{ck}` with spin-1/2 fermions gives $\Psi_k = (c_{k\uparrow}, c_{k\downarrow}, c_{-k\uparrow}^\dagger, c_{-k\downarrow}^\dagger)$.
- For $n$ orbital components, $H(k)$ is a $2n \times 2n$ matrix. For fermions, its blocks are $h(k)$ (upper left), $\Delta(k)$ (upper right), $\Delta(k)^\dagger$ (lower left), and $-h(-k)^T$ (lower right), where $h(k)$ is the Bloch Hamiltonian of the normal part. For bosons, the lower right block is $+h(-k)^T$.
- The matrix elements are $H_{pq}(k) = \eta_p \eta_q [\Psi_{k,p}, [H, \Psi_{k,q}^\dagger]]_\mp$ (anticommutator for fermions, commutator for bosons, $\eta = 1$ for fermions and $\eta = (1, \ldots, 1, -1, \ldots, -1)$ for bosons), evaluated with the lattice sums performed by `sumSimplifyKD`.
- The constant is $E_0 = C + \frac12 \sum_k \mathrm{Tr}\, h(k)$ for fermions and $E_0 = C - \frac12 \sum_k \mathrm{Tr}\, h(k)$ for bosons, where $C$ is the c-number part of the normal-ordered *H*. Since $\Psi_k$ and $\Psi_{-k}$ contain the same operators, every mode appears twice in the sum over $k$, which accounts for the factor $\frac12$.
- In contrast to the real-space BdG matrix of `quadraticform`, $H(k)$ relates $k$ and $-k$: the particle-hole symmetry reads $\tau_x H(k)^* \tau_x = -H(-k)$ (fermions) or $H(-k)$ (bosons). To diagonalize $H(k)$ use `bogoliubov[Hk, "Fermion", "Momentum" -> True]` or `"Boson"`; averages are computed with `gaussianaverage[..., "Momentum" -> k]`.
- Functions of $k$ such as `eps[k]` stay as given; $h(-k)$ contains `eps[-k]`. For inversion-symmetric dispersions replace `eps[-k] -> eps[k]` before diagonalization.
- The following options can be given:

| `"IndexPosition"` | `1` | position of the momentum among the indexes after `CR`/`AN` |
| `"Check"` | `True` | check translation invariance and Hermiticity |

- With `"Check" -> True`, terms that do not conserve momentum give the message `bdghamiltonian::transl` and `$Failed`; a non-Hermitian $H(k)$ gives the warning `bdghamiltonian::herm`.
- Other failures: *k* is not a declared momentum index (`bdghamiltonian::index`), *H* contains terms with an odd number of mode operators (`bdghamiltonian::linear`), terms of higher order or other operators (`bdghamiltonian::rest`), modes of mixed statistics (`bdghamiltonian::stat`), or fermions declared with an ordering other than the default `EMPTY` (`bdghamiltonian::ordering`), for which $E_0$ cannot be determined.

## Examples
### Basic Examples
The Kitaev chain, $H = \sum_R [-t (f_R^\dagger f_{R+1} + \mathrm{h.c.}) - \mu n_R + \Delta (f_R^\dagger f_{R+1}^\dagger + f_{R+1} f_R)]$, in momentum space:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, mu, Dl];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}];
Hk = fourier[H, f -> fk, R -> k];
{hk, E0} = bdghamiltonian[Hk, {fk}, k]
```
In terms of trigonometric functions, $H(k) = -(2t\cos k + \mu)\tau_z - 2\Delta \sin k\, \tau_y$:
```wl
Simplify[ExpToTrig[blochphaseexp[hk, {k -> q}]]] // MatrixForm
```
The quasiparticle energies are $\pm\sqrt{(2t\cos k + \mu)^2 + 4\Delta^2 \sin^2 k}$:
```wl
Simplify[ExpToTrig[blochphaseexp[Eigenvalues[hk]^2, {k -> q}]] - ((2 t Cos[q] + mu)^2 + 4 Dl^2 Sin[q]^2)]
```

### Scope
#### Spin-singlet pairing
The BCS reduced Hamiltonian with a gap $\Delta$, $H = \sum_{k\sigma} \xi_k n_{k\sigma} - \Delta \sum_k (c_{k\uparrow}^\dagger c_{-k\downarrow}^\dagger + c_{-k\downarrow} c_{k\uparrow})$. The constant is $E_0 = \sum_k \xi_k$:
```wl
snegfermionoperators[ck];
snegrealconstants[Dl];
snegrealfunctions[xi];
snegmomentumindexes[{k}, Ns];
HB = sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}];
{hk, E0} = bdghamiltonian[HB, {ck}, k];
{MatrixForm[hk], E0}
```
With $\xi_{-k} = \xi_k$, the quasiparticle energies are $E_k = \sqrt{\xi_k^2 + \Delta^2}$, each twofold degenerate:
```wl
bogoliubov[hk /. xi[-k] -> xi[k], "Fermion", "Momentum" -> True][[1]]
```
#### Several orbitals
A two-orbital chain with a staggered potential and intra-orbital pairing; the Nambu spinor is $(c_{kA}, c_{kB}, c_{-kA}^\dagger, c_{-kB}^\dagger)$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, m, Dl];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[-t (hop[f[R, "A"], f[R, "B"]] + hop[f[R, "B"], f[R + 1, "A"]]) + m (number[f[R, "A"]] - number[f[R, "B"]]) + Dl (nc[f[CR, R, "A", 0], f[CR, R + 1, "A", 0]] + nc[f[AN, R + 1, "A", 0], f[AN, R, "A", 0]]), {R}];
{hk, E0} = bdghamiltonian[fourier[H, f -> fk, R -> k], {fk["A"], fk["B"]}, k];
MatrixForm[hk]
```
#### Bosons
Linear spin waves of the Heisenberg antiferromagnetic chain, $H = J \sum_R (\mathbf S_{RA} \cdot \mathbf S_{RB} + \mathbf S_{RB} \cdot \mathbf S_{R+1,A})$, with a two-site unit cell. The spins of sublattice B are rotated by $\pi$ about the $x$ axis, so that the classical Néel state is ferromagnetic in the rotated frame, and the Holstein-Primakoff expansion is truncated at second order:
```wl
snegspinoperators[{S, s}];
snegbosonoperators[b, bk];
snegrealconstants[J];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
h = J sum[spinspin[S[R, "A"], S[R, "B"]] + spinspin[S[R, "B"], S[R + 1, "A"]], {R}];
h = rotatespin[h, S, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
hk = fourier[holsteinprimakoff[h, S -> b, 2], b -> bk, R -> k];
{Mk, E0} = bdghamiltonian[hk, {bk["A"], bk["B"]}, k];
{MatrixForm[Mk], E0}
```
The constant $E_0 = -2JN_s s(s+1)$ includes the classical energy $-2JN_s s^2$. The magnon energies from the paraunitary diagonalization are $\omega_k = J s\sqrt{2 - 2\cos k} = 2Js|\sin(k/2)|$, where $k$ refers to the two-site cell:
```wl
w = bogoliubov[Mk, "Boson", "Momentum" -> True][[1]];
Simplify[ExpToTrig[blochphaseexp[w^2, {k -> q}]]]
```

### Options
#### IndexPosition
With `"IndexPosition" -> 2`, the momentum is the second index after `CR`/`AN`, after an orbital label:
```wl
snegspinlessfermionoperators[fk];
snegrealconstants[t];
snegmomentumindexes[{k}, Ns];
bdghamiltonian[sum[-2 t Cos[k] nc[fk[CR, "x", k, 0], fk[AN, "x", k, 0]], {k}], {fk["x"]}, k, "IndexPosition" -> 2]
```
#### Check
Terms that do not conserve momentum are detected:
```wl
snegspinlessfermionoperators[fk];
snegmomentumindexes[{k}, Ns];
bdghamiltonian[sum[nc[fk[CR, k, 0], fk[AN, -k, 0]], {k}], {fk}, k]
```
With `"Check" -> False`, no message is given and the result is meaningless:
```wl
bdghamiltonian[sum[nc[fk[CR, k, 0], fk[AN, -k, 0]], {k}], {fk}, k, "Check" -> False]
```

### Applications
#### Topological phase transition of the Kitaev chain
The bulk gap of the Kitaev chain closes at $k = 0$ for $\mu = -2t$ and at $k = \pi$ for $\mu = 2t$; these are the boundaries of the topological phase $|\mu| < 2t$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, mu, Dl];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}];
hk = First[bdghamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]];
e2 = Simplify[ExpToTrig[blochphaseexp[Max[Eigenvalues[hk]]^2, {k -> q}]]];
Factor[{e2 /. q -> 0, e2 /. q -> Pi}]
```
#### BCS gap equation
The anomalous average $\langle c_{-k\downarrow} c_{k\uparrow} \rangle = \frac{\Delta}{2E_k} \tanh\frac{E_k}{2T}$ at temperature $T$ follows from $H(k)$ with `gaussianaverage`. With the attractive interaction $-\frac{g}{N_s} \sum_{kp} c_{k\uparrow}^\dagger c_{-k\downarrow}^\dagger c_{-p\downarrow} c_{p\uparrow}$ and $\Delta = \frac{g}{N_s} \sum_k \langle c_{-k\downarrow} c_{k\uparrow} \rangle$, this gives the BCS gap equation $1 = \frac{g}{N_s} \sum_k \tanh(E_k/2T)/(2E_k)$:
```wl
snegfermionoperators[ck];
snegrealconstants[Dl, x, T];
snegrealfunctions[xi];
snegmomentumindexes[{k}, Ns];
hk = First[bdghamiltonian[sum[xi[k] number[ck[k]] - Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}], {ck}, k]] /. xi[-k] -> xi[k];
F = gaussianaverage[nc[ck[AN, -k, DO], ck[AN, k, UP]], hk, {ck}, T, "Momentum" -> k];
FullSimplify[ExpToTrig[F /. xi[k] -> x], Dl > 0 && T > 0 && Element[x, Reals]]
```

### Properties & Relations
For a number-conserving Hamiltonian, the upper left block of $H(k)$ is the Bloch Hamiltonian $h(k)$ of `blochhamiltonian`, and the lower right block is $-h(-k)^T$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, t2];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hk = fourier[sum[-t hop[f[R], f[R + 1]] + I t2 (nc[f[CR, R + 2, 0], f[AN, R, 0]] - nc[f[CR, R, 0], f[AN, R + 2, 0]]), {R}], f -> fk, R -> k];
{First[bdghamiltonian[Hk, {fk}, k]], blochhamiltonian[Hk, {fk}, k]}
```
---
The real-space analog for finite systems is `quadraticform`, which uses the spinor $(a_1, \ldots, a_n, a_1^\dagger, \ldots, a_n^\dagger)$ of the same modes.

### Possible Issues
$H(k)$ is not the BdG matrix of the modes $(c_k, c_k^\dagger)$, so `bogoliubov` without `"Momentum" -> True` rejects it when $h(k) \neq h(-k)$:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, mu, Dl];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] + Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}];
hk = First[bdghamiltonian[fourier[H, f -> fk, R -> k], {fk}, k]];
num = blochphaseexp[hk /. {t -> 1, Dl -> 3/5, mu -> 3/10}, {k -> 9/10}];
bogoliubov[num, "Fermion"]
```
```wl
bogoliubov[num, "Fermion", "Momentum" -> True][[1]]
```
---
The momentum must be declared with `snegmomentumindexes`; a lattice index is rejected:
```wl
snegspinlessfermionoperators[fk];
sneglatticeindexes[{R}, Ns];
bdghamiltonian[sum[number[fk[R]], {R}], {fk}, R]
```
---
Interaction terms are rejected; decouple them first with `meanfield`:
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k}, Ns];
bdghamiltonian[sum[nc[number[ck[k], UP], number[ck[k], DO]], {k}], {ck}, k]
```

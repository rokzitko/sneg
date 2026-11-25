# Quadratic forms, Bogoliubov transformations, and mean-field theory

SNEG extracts the coefficient matrices of quadratic (bilinear) Hamiltonians,
including anomalous terms, in real space and in momentum space,
diagonalizes them by Bogoliubov transformations for fermions and bosons,
computes thermal and ground-state expectation values with Wick's theorem,
and performs mean-field decouplings with self-consistent solutions on finite
sets of modes.

## Conventions

A quadratic Hamiltonian of the modes $a_1, \dots, a_n$ is written in the
Bogoliubov-de Gennes (BdG) form

$$H = \tfrac12 \Psi^\dagger M \Psi + E_0, \qquad
\Psi = (a_1, \dots, a_n, a_1^\dagger, \dots, a_n^\dagger)^T,$$

with

$$M = \begin{pmatrix} h & \Delta \\ \Delta^\dagger & -h^T \end{pmatrix}
\ (\text{fermions},\ \Delta^T = -\Delta), \qquad
M = \begin{pmatrix} A & B \\ B^* & A^* \end{pmatrix}
\ (\text{bosons},\ B^T = B).$$

For $H = \sum_{ij} h_{ij} a_i^\dagger a_j + \frac12\sum_{ij}(\Delta_{ij}
a_i^\dagger a_j^\dagger + \mathrm{h.c.}) + C$ one has
$E_0 = C + \frac12 \mathrm{Tr}\, h$ for fermions and
$E_0 = C - \frac12 \mathrm{Tr}\, A$ for bosons. The matrix elements are
obtained as $M_{pq} = \eta_p\eta_q [\Psi_p, [H, \Psi_q^\dagger]]_\mp$
(anticommutator for fermions, commutator for bosons, $\eta = (1, \dots, 1,
-1, \dots, -1)$ for bosons and $\eta = 1$ for fermions), so that $M$ is
always in the symmetrized form above, independently of how $H$ is written.

Modes are given as for `blochhamiltonian`: templates `c` or `c[j...]`, where
fermions get the spin index as the last index, varying fastest ($\uparrow,
\downarrow$ for spin 1/2, `0` for spinless fermions), or functions
`Function[t, ...]` returning the operator for `t = CR, AN` (e.g. bonding
orbitals).

A Bogoliubov transformation $\Psi = T\Phi$ with
$\Phi = (\gamma_1, \dots, \gamma_n, \gamma_1^\dagger, \dots,
\gamma_n^\dagger)^T$ and $T = \begin{pmatrix} U & V^* \\ V & U^*
\end{pmatrix}$ gives

$$H = \sum_m \varepsilon_m \gamma_m^\dagger \gamma_m + E_\mathrm{gs},\qquad
E_\mathrm{gs} = E_0 \mp \tfrac12 \sum_m \varepsilon_m,$$

with the upper sign for fermions ($T$ unitary, $T^\dagger M T =
\mathrm{diag}(\varepsilon, -\varepsilon)$) and the lower sign for bosons
($T^\dagger \eta T = \eta$, $T^\dagger M T = \mathrm{diag}(\varepsilon,
\varepsilon)$).

## Quadratic forms in real space

The Kitaev chain with three sites,
$H = \sum_j [-t(f_j^\dagger f_{j+1} + \mathrm{h.c.}) + \Delta (f_j^\dagger
f_{j+1}^\dagger + f_{j+1} f_j)] - \mu\sum_j n_j$:

```wl
Needs["sneg`"]
snegspinlessfermionoperators[f]; snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] +
      nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
{M, E0} = quadraticform[H, {f[1], f[2], f[3]}]   (* E0 = -3 mu/2 *)
quadraticoperator[M, {f[1], f[2], f[3]}, E0] - H // Expand   (* 0 *)
```

`quadraticform[H, modes, "BdG" -> False]` returns $\{h, C\}$ with
$H = a^\dagger h a + C$ for number-conserving Hamiltonians. Linear boson
terms $\sum_i (f_i b_i^\dagger + f_i^* b_i)$, as in Holstein-Primakoff
expansions around non-collinear states, are returned with
`"Linear" -> True` as $\{M, f, E_0\}$. Higher-order terms, operators that are
not among the modes, and sums are reported with messages.

## Bogoliubov transformations

```wl
bogoliubov[M /. {t -> 1, Dl -> 1, mu -> 0}, "Fermion"][[1]]
(* {0, 2., 2.}: the Majorana zero mode at t = Delta, mu = 0 *)
bogoliubov[H /. {t -> 1, Dl -> 1, mu -> 0}, {f[1], f[2], f[3]}][[{1, 3}]]
(* {{0, 2., 2.}, -2.}: energies and ground-state energy *)
```

Numerical fermionic matrices are diagonalized as Hermitian matrices whose
positive-energy eigenvectors $x_m$ define the quasiparticles; the remaining
columns are $C x_m = \tau_x x_m^*$. Zero modes are paired into fermions in a
basis invariant under $C$. Bosonic matrices are diagonalized by Colpa's
method [Physica A 93, 327 (1978)], which requires $M$ to be positive
definite; otherwise (dynamical instability, Goldstone zero modes) a message
is given.

Symbolic matrices are supported if they decompose into blocks of at most two
modes, which covers singlet pairing, two-mode squeezing, and two-level
hybridization. For on-site singlet pairing
$H = \xi (n_\uparrow + n_\downarrow) - \Delta (c_\uparrow^\dagger
c_\downarrow^\dagger + c_\downarrow c_\uparrow)$:

```wl
snegfermionoperators[c]; snegrealconstants[xi];
HS = xi (number[c[1], UP] + number[c[1], DO]) -
  Dl (nc[c[CR, 1, UP], c[CR, 1, DO]] + nc[c[AN, 1, DO], c[AN, 1, UP]]);
{eps, T, Egs} = bogoliubov[HS, {c[1]}]
(* eps = {E, E}, E = Sqrt[Dl^2 + xi^2], Egs = xi - E,
   T[[1, 1]]^2 = u^2 = (1 + xi/E)/2, T[[4, 1]]^2 = v^2 = (1 - xi/E)/2 *)
```

The symbolic results assume real diagonal elements and real quasiparticle
energies; the ground-state energy and the $T = 0$ averages below assume
$\varepsilon_m > 0$ for the branch given.

`bogoliubovrules[T, modes, g]` returns the rules
$a_i \to \sum_m (T_{im}\gamma_m + T_{i,n+m}\gamma_m^\dagger)$ and their
conjugates, which rewrite any expression in quasiparticle operators `g`
(spinless fermions or bosons):

```wl
snegspinlessfermionoperators[g];
Expand[HS /. bogoliubovrules[T, {c[1]}, g]] /. {Dl -> 3/5, xi -> 4/5} // Simplify
(* -1/5 + g^dagger_1 g_1 + g^dagger_2 g_2 *)
```

## Momentum space

`bdghamiltonian[H, {ck[a], ...}, k]` returns $\{H(k), E_0\}$ with
$H = \frac12\sum_k \Psi_k^\dagger H(k)\Psi_k + E_0$ and
$\Psi_k = (c_{k,a}, \dots, c_{-k,a}^\dagger, \dots)$, with the same orbital
templates, translation check, and Hermiticity check as `blochhamiltonian`.
`blochhamiltonian` itself now stops with a message if $H$ contains anomalous
or linear terms, which $h(k)$ does not describe.

```wl
snegspinlessfermionoperators[fk];
sneglatticeindexes[{R}, Ns]; snegmomentumindexes[{k}, Ns];
Hk = fourier[sum[-t hop[f[R], f[R + 1]] - mu number[f[R]] +
    Dl (nc[f[CR, R, 0], f[CR, R + 1, 0]] + nc[f[AN, R + 1, 0], f[AN, R, 0]]), {R}], f -> fk, R -> k];
{Hkm, E0k} = bdghamiltonian[Hk, {fk}, k]
(* H(k) = {{-mu - 2 t cos k, 2 i Dl sin k}, {-2 i Dl sin k, mu + 2 t cos k}}, E0 = -mu Ns/2 *)
```

The eigenvalues are $\pm\sqrt{(2t\cos k + \mu)^2 + 4\Delta^2\sin^2 k}$.

The particle-hole symmetry of $H(k)$ relates $k$ and $-k$,
$\tau_x H(k)^* \tau_x = \mp H(-k)$, so $H(k)$ is in general not a BdG
matrix of the modes $(c_k, c_k^\dagger)$; `bogoliubov` then stops with a
message. With `"Momentum" -> True` it diagonalizes $H(k)$ without this
assumption and returns all $2n$ diagonal elements of $T^\dagger H(k) T$:
the energies of $\gamma_k$ followed by $-\varepsilon(-k)$ (fermions) or
$\varepsilon(-k)$ (bosons).

```wl
snegfermionoperators[ck]; snegrealfunctions[eps];
HB = sum[eps[k] (number[ck[k], UP] + number[ck[k], DO]) -
    Dl (nc[ck[CR, k, UP], ck[CR, -k, DO]] + nc[ck[AN, -k, DO], ck[AN, k, UP]]), {k}];
{HBk, E0B} = bdghamiltonian[HB, {ck}, k];   (* E0B = sum[eps[k], {k}] *)
HBk = HBk /. eps[-k] -> eps[k];
bogoliubov[HBk, "Fermion", "Momentum" -> True][[1]]
(* {E, E, -E, -E} with E = Sqrt[Dl^2 + eps[k]^2] *)
```

Linear spin waves of the Heisenberg antiferromagnet follow from
Holstein-Primakoff bosons on a two-site unit cell, with the B sublattice
rotated by $\pi$ about $x$ (see [`README_spin.md`](README_spin.md)):

```wl
snegspinoperators[{S, s}]; snegbosonoperators[b, bk]; snegrealconstants[J];
h = J sum[spinspin[S[R, "A"], S[R, "B"]] + spinspin[S[R, "B"], S[R + 1, "A"]], {R}];
h = rotatespin[h, S, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &];
hk = fourier[holsteinprimakoff[h, S -> b, 2], b -> bk, R -> k];
{Mk, E0} = bdghamiltonian[hk, {bk["A"], bk["B"]}, k];  (* E0 = -2 J Ns s (s + 1) *)
bogoliubov[Mk, "Boson", "Momentum" -> True][[1]]
(* four times J s Sqrt[2 - 2 cos k] = 2 J s |sin(k/2)| *)
```

## Gaussian averages

`gaussianaverage[expr, M, modes, T]` returns the expectation value of `expr`
in the thermal state of the quadratic Hamiltonian (BdG matrix `M` or the
Hamiltonian itself) at temperature `T` ($k_B = 1$; `T = 0` gives the ground
state; fermionic zero modes are then half occupied). It uses the correlation
matrix $G = \langle \Psi\Psi^\dagger\rangle$ and Wick's theorem, so
products of any number of mode operators can be averaged; `expr` may be a
list. With `"Momentum" -> k`, `M` is a Bloch BdG matrix and `expr` contains
the operators with momenta $k$ and $-k$:

```wl
gaussianaverage[{nc[ck[AN, -k, DO], ck[AN, k, UP]], number[ck[k], UP]}, HBk, {ck}, 0,
  "Momentum" -> k] /. {eps[k] -> 2/5, Dl -> 3/10} // Simplify
(* {3/10, 1/10}: Delta/(2E) and v^2 = (1 - eps/E)/2 with E = 1/2 *)
```

At $T > 0$ the anomalous average is $\Delta\tanh(E/2T)/(2E)$.

## Mean-field decoupling

`meanfield[H, channels]` replaces every product of $2m \ge 4$ fermion or
boson operators $o_1 \cdots o_{2m}$ by

$$\sum_{i<j} s_{ij}\, W(\text{rest})\, o_i o_j - (m - 1)\, W(o_1\cdots o_{2m}),$$

where $W$ is the Wick expansion with the contractions
$\langle o_a o_b\rangle$, written as `avg[nc[oa, ob]]`, and $s_{ij}$ is the
fermionic sign of moving $o_i o_j$ to the front. For four operators this is
the usual $AB \to \langle A\rangle B + A\langle B\rangle -
\langle A\rangle\langle B\rangle$ summed over the three pairings. The
average of the mean-field Hamiltonian in any Gaussian state equals the Wick
average of $H$, and its stationarity gives the Hartree-Fock-Bogoliubov
equations.

The channels select the contractions: `"Hartree"` ($\langle a^\dagger a
\rangle$ of the same mode, i.e. equal operator labels), `"Fock"`
($\langle a^\dagger b\rangle$ of different modes, including spin-flip
averages), `"Pairing"` ($\langle ab\rangle$, $\langle a^\dagger
b^\dagger\rangle$), `"Normal"` (Hartree and Fock), `"All"` (default), a list
of these, or a function `f[o1, o2]` returning `True` for allowed
contractions. The Hartree/Fock distinction refers to the operator labels: in
momentum space the direct term of a density-density interaction appears as
$\langle c_{k+q}^\dagger c_k\rangle$, so use `"Normal"` there and impose
momentum conservation on the averages afterwards.

```wl
snegrealconstants[U];
meanfield[U nc[number[c[1], UP], number[c[1], DO]], "Hartree"]
(* U <n_dn> n_up + U <n_up> n_dn - U <n_up> <n_dn> *)
meanfield[-U/Ns sum[nc[ck[CR, k, UP], ck[CR, -k, DO], ck[AN, -p, DO], ck[AN, p, UP]], {k, p}], "Pairing"]
(* BCS reduction with <c_-p,dn c_p,up> and <c^dagger_k,up c^dagger_-k,dn> *)
```

`avg[x]` is linear, acts on the canonically ordered operator expression
(e.g. `avg[nc[c, c^dagger]] = 1 - avg[nc[c^dagger, c]]`),
$\langle x\rangle^* = \langle x^\dagger\rangle$, and it is a number in
operator products, so mean-field Hamiltonians can be passed to
`quadraticform` and `bdghamiltonian`.

## Self-consistent solutions

`meanfieldsolve[Hmf, modes, {avg[...] -> x0, ...}]` iterates
$\langle\cdot\rangle \to$ quadratic form $\to$ correlation matrix $\to$
new averages with linear mixing (`"Mixing" -> 1/2`) until the largest change
is below `"Tolerance"` (`10^-10`), at most `"MaxIterations"` (1000) times.
Unspecified averages start from zero; initial values may be given for either
of two conjugate averages and in the canonically ordered form returned by
`avg` (e.g. `-avg[...]`). The option `"Temperature"` (default 0) sets the
thermal state, and `"ParticleNumber" -> N` fixes the fermion number by
adjusting the chemical potential (filling the lowest levels for
number-conserving Hamiltonians at $T = 0$, where a partially filled
degenerate level is occupied equally with a message). The result is an
`Association` with the keys `"Averages"`, `"Energy"` ($\langle H_\mathrm{MF}
\rangle$, which equals the Hartree-Fock energy $\langle H\rangle$),
`"QuasiparticleEnergies"`, `"ChemicalPotential"`, `"Iterations"`, and
`"Converged"`. Different initial values may converge to different (e.g.
paramagnetic or broken-symmetry) solutions.

For the Hubbard dimer $H = -t\sum_\sigma(c_{1\sigma}^\dagger c_{2\sigma} +
\mathrm{h.c.}) + U\sum_i n_{i\uparrow}n_{i\downarrow}$ at half filling,
$U = 4t$:

```wl
h = -hop[c[1], c[2]] + 4 Sum[nc[number[c[i], UP], number[c[i], DO]], {i, 2}];
sol = meanfieldsolve[meanfield[h], {c[1], c[2]},
  {avg[number[c[1], UP]] -> 0.9, avg[number[c[2], DO]] -> 0.9}, "ParticleNumber" -> 2];
sol["Energy"]   (* -0.5 *)
```

The broken-symmetry solution has the staggered moment
$m = n_{1\uparrow} - n_{1\downarrow} = \sqrt3/2$, from
$m = (Um/2)/\sqrt{t^2 + (Um/2)^2}$, and the energy $-t/2$, above the exact
ground-state energy $(U - \sqrt{U^2 + 16t^2})/2 \approx -0.83t$. For
$U < 2t$ the iteration converges to the paramagnetic solution with
$E = -2t + U/2$.

The solver works with finite sets of modes. Momentum-space problems can be
treated on finite clusters with `sumCluster`, or by evaluating the
self-consistency conditions obtained from `bdghamiltonian` and
`gaussianaverage`.

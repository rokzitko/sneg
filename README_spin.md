# Spin algebras and Hubbard X-operators

SNEG treats spin operators as abstract operators with the su(2) algebra,
optionally of a given spin size $s$. They can be combined with fermions and
bosons, mapped to bosonic or fermionic representations, diagonalized on
finite clusters, and used to build crystal-field (Stevens) operators. Hubbard
X-operators $X^{ab} = |a\rangle\langle b|$ cover the atomic limit, t-J
models, and SU(N) spins in the fundamental representation.

## Conventions

A spin operator `S[j..., t]` carries the site indexes `j...` and, as its last
index, the component `t = 1, 2, 3, 4, 5` for $S^x, S^y, S^z, S^+, S^-$
(`SPININDEXx`, ..., `SPININDEXm`). Operators on different sites commute, and
on the same site

$$[S^a, S^b] = i\varepsilon_{abc} S^c,\qquad [S^z, S^\pm] = \pm S^\pm,\qquad
[S^+, S^-] = 2 S^z .$$

Spin operators commute with all other operators. Products are ordered with
spins on the left, then bosons, Majorana and Dirac fermions; different spin
symbols are sorted alphabetically and operators of one symbol by site and
component ($x < y < z < + < -$). $S^\dagger_x = S_x$ and $(S^+)^\dagger = S^-$.

Because the last index is the component, `S[0]` evaluates to zero and `S[-1]`
to `-S[1]`. Functions that take lists of sites therefore also accept
`S -> {0, 1, ...}` instead of `{S[0], S[1], ...}`.

## Declarations and the spin size

```wl
Needs["sneg`"]
snegspinoperators[G];               (* su(2) algebra, size unspecified *)
snegspinoperators[{S, 1/2}, {T, 1}]; (* spin 1/2 and spin 1 *)
snegspinoperators[{M, s}];           (* symbolic size; s becomes a real constant *)
```

Without a declared size only the Lie algebra is used; this holds in every
representation. With a declared size, products of operators on the same site
are reduced automatically to the normal form

$$\sum_{a,b} c_{ab}\, (S^z)^b (S^\pm)^a .$$

* For every size, $S^+S^- = s(s+1) - (S^z)^2 + S^z$, and $S^x$, $S^y$ in such
  products are written in terms of $S^\pm$. In particular
  $\mathbf S_j\cdot\mathbf S_j = s(s+1)$.
* For numeric $s$, also $(S^z)^b (S^\pm)^a$ with $a + b > 2s$ is reduced
  (e.g. $(S^\pm)^{2s+1} = 0$). The remaining $(2s+1)^2$ monomials are linearly
  independent, so the normal form is unique and operator identities reduce
  to zero.
* For $s = 1/2$, products of Cartesian components follow the Pauli rule
  $S^aS^b = \delta_{ab}/4 + (i/2)\varepsilon_{abc}S^c$ and stay Cartesian.

```wl
{nc[S[1, 1], S[1, 2]], nc[S[1, 4], S[1, 5]], spinspin[S[1], S[1]]}
(* {I/2 S[1, 3], 1/2 + S[1, 3], 3/4} *)
{nc[T[1, 4], T[1, 4], T[1, 4]], nc[T[1, 3], T[1, 3], T[1, 3]]}
(* {0, T[1, 3]} *)
nc[M[1, 5], M[1, 4]]
(* s (1 + s) - M[1, 3] - nc[M[1, 3], M[1, 3]] *)
```

`spincomponents[expr]` rewrites all components in terms of $S^z, S^\pm$
(`spincomponents[expr, "XYZ"]` uses $S^x, S^y, S^z$). Together with the
automatic ordering this gives a unique form also for spins without a
declared size, e.g.

```wl
spincomponents[Expand[komutator[spinspin[G[1], G[2]], G[1, 4] + G[2, 4]]]]
(* 0 *)
```

For sizes above 1/2, Cartesian products are reduced back to $S^z, S^\pm$, so
the `"XYZ"` form is stable only for spin 1/2 and for spins without a size.

Symbolic Cartesian components are declared with
`snegspincomponentindexes[a, b]`. Then
$[S^a_i, S^b_j] = i\delta_{ij}\sum_c \varepsilon_{abc} S^c_j$, with
`levicivita[a, b, c]`, and `spincomponents` expands sums over component
indexes into $x, y, z$, e.g. `sum[nc[S[1, a], S[2, a]], {a}]` becomes
$\mathbf S_1\cdot\mathbf S_2$.

## Kondo impurity

```wl
snegfermionoperators[c]; snegrealconstants[J];
HK = J (nc[S[3], spinz[c[]]] + (nc[S[4], spinminus[c[]]] + nc[S[5], spinplus[c[]]])/2);
Expand[komutator[HK, S[3] + spinz[c[]]]]   (* 0 *)
```

## Exact diagonalization

Pure spin Hamiltonians are diagonalized with `spinclustermatrices`, which
builds sparse matrices from Kronecker products of spin matrices in the
product basis $|m_1,\ldots,m_N\rangle$ ($m$ from $+s$ to $-s$, first site
varying slowest), split into total-$S^z$ subspaces:

```wl
H = J Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}];
{First[#], Min[Eigenvalues[Last[#] /. J -> 1]]} & /@ spinclustermatrices[H, {S -> Range[4]}]
(* {{{-2}, 1}, {{-1}, -1}, {{0}, -2}, {{1}, -1}, {{2}, 1}} *)
```

The four-site Heisenberg ring has the ground-state energy $-2J$ with $S = 0$.

For spins coupled to fermions, `spinketbrarules[{S[1], ...}]` maps the spin
operators to the bra-ket representation, with site $k$ in the $k$-th ket
slot, and `QSZaddspinket[bvc, {s1, s2, ...}]` adds the spin kets to a
fermionic basis (total $S^z$ includes the spins). Each operator includes the
identity of the other slots, so the cost grows with the number of spins.

```wl
bz = QSZaddspinket[qszbasisvc[{c[]}], {1/2}];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[Expand[HK /. spinketbrarules[{S[]}]], bz]
(* ..., {{0, 0}, {-3 J/4, J/4}}, ...: singlet and triplet at single occupancy *)
```

## Representations

* `jordanwigner[expr, S -> f, {j1, j2, ...}]`: spin 1/2,
  $S^+_k = f^\dagger_k\prod_{l<k}(1-2n_l)$, $S^z_k = n_k - 1/2$, for the
  sites in the given order; `f` is a spinless fermion.
* `holsteinprimakoff[expr, S -> b, n]`: $S^z = s - b^\dagger b$,
  $S^+ = \sqrt{2s - b^\dagger b}\, b$. Each same-site product
  $(S^z)^m (S^\pm)^a$ is expanded exactly in normal-ordered boson operators
  (Newton series $h(b^\dagger b) = \sum_m \Delta^m h(0)/m!\,
  (b^\dagger)^m b^m$), and terms with more than `n` boson operators are
  dropped. The coefficients are exact in $s$; e.g.
  $S^+ = \sqrt{2s}\,b + (\sqrt{2s-1} - \sqrt{2s})\,b^\dagger b b + \ldots$.
  The commutator of two images truncated at `n` is exact up to `n - 2`
  boson operators.
* `schwingerbosons[expr, S -> {a, b}]`: $S^+ = a^\dagger b$,
  $S^- = b^\dagger a$, $S^z = (n_a - n_b)/2$, with the constraint
  $n_a + n_b = 2s$.
* `abrikosov[expr, S -> f]`: $\mathbf S = f^\dagger \mathbf S f$ with
  fermions of the same spin (`snegfermionoperators[{f, s}]`); faithful for one
  fermion per site, e.g. with `projector[f[j], PROJ1]` for spin 1/2.

Linear spin waves of the ferromagnetic chain combine Holstein-Primakoff with
the Fourier transformation (see [`README_momentum.md`](README_momentum.md)):

```wl
snegbosonoperators[b, bk];
sneglatticeindexes[{R}, Ns]; snegmomentumindexes[{k}, Ns];
hk = fourier[holsteinprimakoff[-J sum[spinspin[M[R], M[R + 1]], {R}], M -> b, 2], b -> bk, R -> k]
(* -J Ns s^2 + sum[(2 J s - J s blochphase[-k, 1] - J s blochphase[k, 1]) bk^dagger_k bk_k, {k}] *)
blochhamiltonian[hk, {bk}, k]    (* {{2 J s (1 - Cos[k])}} after blochphaseexp *)
```

For the antiferromagnet, rotate one sublattice by $\pi$ about $x$ first,
`rotatespin[H, M, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &]`;
the quadratic Hamiltonian then contains anomalous terms
$b^\dagger b^\dagger$. `bdghamiltonian` and
`bogoliubov[..., "Boson", "Momentum" -> True]` give
$\omega = 2Js|\sin(k/2)|$ for the two-site unit cell (see
[`README_quadratic.md`](README_quadratic.md)).

## Spherical tensors, Stevens operators, and rotations

`spintensor[k, q, S[j]]` returns the spherical tensor operators with
$T_k^k = (-S^+/\sqrt2)^k$ and $T_k^{q-1} = [S^-, T_k^q]/\sqrt{(k+q)(k-q+1)}$,
so that $T_1^0 = S^z$, $T_1^{\pm1} = \mp S^\pm/\sqrt2$ and
$T_2^0 = (3(S^z)^2 - \mathbf S^2)/\sqrt6$.

`stevens[k, q, S[j]]` returns the Stevens operators $O_k^q$ in the
normalization of Abragam and Bleaney, e.g. $O_2^0 = 3(S^z)^2 - s(s+1)$,
$O_2^2 = (S_x)^2 - (S_y)^2 = ((S^+)^2 + (S^-)^2)/2$,
$O_2^{-2} = ((S^+)^2 - (S^-)^2)/(2i)$,
$O_4^0 = 35 S_z^4 - 30 X S_z^2 + 25 S_z^2 - 6X + 3X^2$ with $X = s(s+1)$.
They vanish for $k > 2s$. A spin with a declared size is required.

```wl
snegspinoperators[{F, 5/2}]; snegrealconstants[Dz];
Sort[Eigenvalues[spinclustermatrices[stevens[2, 0, F[1]], {F[1]}, "Blocks" -> False]]]
(* {-8, -8, -2, -2, 10, 10}: doublets m = +-1/2, +-3/2, +-5/2, in units of Dz *)
```

`rotatespin[expr, S, R]` rotates the spin vectors, $\mathbf S_j \to
R\,\mathbf S_j$, with a $3\times3$ matrix `R` or a function of the site
indexes returning it (sublattice rotations, spirals, local frames). The
result is in Cartesian components. Dzyaloshinskii-Moriya couplings
$\mathbf D\cdot(\mathbf S_1\times\mathbf S_2)$ can be written with
`mcross`.

## Hubbard X-operators

```wl
snegxoperators[X, "HubbardAtom"];          (* states "0", "UP", "DO", "2" *)
snegxoperators[Y, {1, 2, 3}, {0, 0, 0}];  (* three bosonic states: SU(3) *)
```

`X[j..., a, b]` is $|a\rangle\langle b|$ on site $j$. The local states carry
fermion parities; $X^{ab}$ is fermionic when the parities of $a$ and $b$
differ. On the same site $X^{ab}X^{cd} = \delta_{bc}X^{ad}$; on different
sites X-operators commute or, if both are fermionic, anticommute. They are
ordered to the right of spins and to the left of bosons and fermions,
anticommuting with fermions when fermionic. X-operators are independent of
the other operators, so they should not be mixed with fermions of the same
site.

```wl
{nc[X[1, "0", "UP"], X[1, "UP", "2"]], komutator[X[1, "UP", "DO"], X[1, "DO", "UP"]]}
(* {X[1, "0", "2"], X[1, "UP", "UP"] - X[1, "DO", "DO"]} *)
```

For the Hubbard atom, $|{\uparrow}\rangle = c^\dagger_\uparrow|0\rangle$,
$|{\downarrow}\rangle = c^\dagger_\downarrow|0\rangle$ and
$|2\rangle = c^\dagger_\uparrow c^\dagger_\downarrow|0\rangle$;
`xoperator[c[j], a, b]` gives $X^{ab}$ in terms of fermions, e.g.
$X^{0\uparrow} = c_\uparrow(1 - n_\downarrow)$, and
`xtofermions[expr, X -> c]` converts expressions for exact diagonalization
in the usual fermionic bases. The t-J dimer,
$H = -t\sum_\sigma (X_1^{\sigma0}X_2^{0\sigma} + \mathrm{h.c.}) +
J(\mathbf S_1\cdot\mathbf S_2 - n_1n_2/4)$, gives the two-electron singlet
at $-J$ and the triplet at $0$, the large-$U$ limit of the Hubbard dimer
with $J = 4t^2/U$.

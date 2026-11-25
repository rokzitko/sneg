# Momentum space and lattices

SNEG can transform lattice Hamiltonians to momentum space, apply momentum
conservation and lattice sums, extract Bloch Hamiltonians, and specialize
symbolic results to finite periodic clusters for exact diagonalization.

## Conventions

A periodic lattice has `Ns` unit cells at lattice vectors `R` (periodic
boundary conditions). Site and momentum operators are related by

$$c_{R\alpha\sigma} = N_s^{-1/2} \sum_k e^{i k\cdot R}\, c_{k\alpha\sigma},
\qquad
c_{k\alpha\sigma} = N_s^{-1/2} \sum_R e^{-i k\cdot R}\, c_{R\alpha\sigma},$$

so that $\sum_R e^{i K\cdot R} = N_s\,\delta_{K,0}$ and
$\sum_k e^{i k\cdot X} = N_s\,\delta_{X,0}$. The site or momentum label is
the first index after `CR`/`AN`; orbital indexes follow, and the spin index
is last, as elsewhere in SNEG.

Momentum labels are defined modulo reciprocal-lattice vectors (Umklapp
processes are included implicitly) and site labels modulo the lattice
period. On these index sets a shift `k -> ±k + q` is a bijection. This makes
two operations exact: eliminating Kronecker deltas of linear combinations,
such as $\sum_k \delta_{p,k+q} f(k) = f(p-q)$, and shifting summation
variables. Both hold only for declared lattice and momentum indexes:

* Generic indexes keep the bare-index delta rules, because on an open chain
  $\sum_i \delta_{j,i+1} f(i) = f(j-1)$ fails at the boundary.
* Deltas with coefficients other than ±1, such as $\delta_{2k,q}$, have
  several solutions on the torus and are left unevaluated.
* Sums containing `SEA`-ordered operators are never shifted. For linearized
  (unbounded) bands the shift is not a symmetry of the Fermi sea; this is the
  origin of the anomalous commutator of density operators in the
  Tomonaga-Luttinger model.

Symbolic momenta are abstract labels (vectors in more than one dimension).
Explicit components are substituted only when Bloch phases are evaluated
with `blochphaseexp`.

## Declarations

```wl
Needs["sneg`"]
snegfermionoperators[c, ck, d];
snegspinlessfermionoperators[f, fk];
snegrealconstants[t, U, V];
sneglatticeindexes[{R}, Ns];       (* unit cells; Ns becomes a real constant *)
snegmomentumindexes[{k, p, q}, Ns]; (* crystal momenta *)
sneglatticevectors[a1, a2];        (* displacements, e.g. c[R + a1] *)
```

Indexes created by automatic renaming (`k1`, `k2`, ...) inherit these
declarations. Sums over a declared index that does not appear in the
summand evaluate to `Ns`.

## Hubbard chain

```wl
H = -t sum[hop[c[R], c[R + 1]], {R}] + U sum[hubbard[c[R]], {R}];
Hk = fourier[H, c -> ck, R -> k]
```

The result is, written out in conventional notation,

$$H = \sum_{k\sigma} \left(-t\,e^{ik} - t\,e^{-ik}\right) c^\dagger_{k\sigma} c_{k\sigma}
 + \frac{U}{N_s} \sum_{k,k_2,k_3}
 c^\dagger_{k\uparrow} c^\dagger_{k_2+k_3-k,\downarrow} c_{k_2\downarrow} c_{k_3\uparrow},$$

where the Bloch phases appear as `blochphase[k, 1]`. The Bloch Hamiltonian of
the kinetic term is

```wl
blochhamiltonian[fourier[-t sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, k]
(* {{-t blochphase[-k, 1] - t blochphase[k, 1], 0}, {0, ...}} *)
blochphaseexp[%] // ExpToTrig   (* -2 t Cos[k] IdentityMatrix[2] *)
```

with rows and columns ordered `{UP, DO}`. Commutators with symbolic sums use
the same momentum algebra, e.g.

```wl
snegsumreduce[komutator[ck[AN, p, UP], sum[nc[ck[CR, k + q, UP], ck[AN, k, UP]], {k}]]]
(* ck[AN, p - q, UP] *)
```

## Graphene

With two sublattices `A` and `B` in each unit cell and the neighbors of an
`A` site at `R`, `R - a1`, and `R - a2`:

```wl
Hg = -t sum[hop[f[R, "A"], f[R, "B"]] + hop[f[R, "A"], f[R - a1, "B"]] +
    hop[f[R, "A"], f[R - a2, "B"]], {R}];
h = blochhamiltonian[fourier[Hg, f -> fk, R -> k], {fk["A"], fk["B"]}, k]
(* {{0, -t - t blochphase[-k, a1] - t blochphase[-k, a2]},
    {-t - t blochphase[k, a1] - t blochphase[k, a2], 0}} *)
blochphaseexp[h, {k -> {4 Pi/3, 0}, a1 -> {1, 0}, a2 -> {1/2, Sqrt[3]/2}}] // Simplify
(* {{0, 0}, {0, 0}}: the Dirac point K *)
```

`blochhamiltonian` computes $h_{\alpha\beta}(k) = \{c_{k\alpha},
[H, c^\dagger_{k\beta}]\}$ (commutators for bosons). It reports Hamiltonians
that are not translation invariant for the chosen unit cell (for example a
staggered potential without a doubled cell) and non-Hermitian results, and
it stops with a message for anomalous or linear terms, which are treated by
`bdghamiltonian` (see [`README_quadratic.md`](README_quadratic.md)). The
option `"OrbitalPositions" -> {tau_A, tau_B}` returns the Bloch matrix in the
convention $c_{R\alpha} = N_s^{-1/2}\sum_k e^{ik\cdot(R+\tau_\alpha)}
c_{k\alpha}$, $h^{II}_{\alpha\beta} = e^{-ik\cdot\tau_\alpha}
h_{\alpha\beta} e^{ik\cdot\tau_\beta}$. Spin-dependent hopping can be
written with `matrixhop`; for example a Rashba chain with
`matrixhop[-t IdentityMatrix[2] + I alpha PauliY, c[R], c[R + 1]]` gives
$h(k) = -2t\cos k - 2\alpha \sin k\,\sigma_y$.

## Anderson impurity

An impurity coupled to the site at the origin hybridizes with all momenta:

```wl
fourier[V hop[d[], c[0]], c -> ck, R -> k]
(* sum[V/Sqrt[Ns] (ck†_kσ d_σ + d†_σ ck_kσ), {k}] *)
```

## Momentum shifts

`sumShift[expr, k -> k + q]` shifts a summation variable explicitly.
`sumMomentumCanonical` shifts each summed momentum so that the first
operator argument depending on it is the bare index, and renames
automatically renamed dummy indexes back to their root names, so that
`sumCollect` can combine equivalent sums. `sumFullSimplify` uses this step.
For example, $[\rho_q, \rho_{-q}]$ with $\rho_q = \sum_p c^\dagger_{p+q}
c_p$ simplifies to zero on a lattice.

## Finite periodic clusters

Sites of an $L_1\times L_2\times\cdots$ cluster are integer vectors in lattice
coordinates, $n_a = 0,\ldots,L_a-1$ (plain integers in 1D). Momentum labels
are integer vectors $m$ with $k_a = (2\pi m_a + \theta_a)/L_a$. Twisted
boundary conditions are $c_{n+L_a e_a} = e^{i\theta_a} c_n$.

* `fourierrules[c -> ck, {L1, ...}, "Twist" -> theta]` gives the explicit
  unitary transformation.
* `clusterhop[T, c[a], d[b], delta, {L1, ...}, "Twist" -> theta]` builds
  $\sum_n c^\dagger_{n a} T d_{n+\delta, b} + \mathrm{h.c.}$ with a scalar or
  2×2 spin matrix `T`. On a two-site ring the bonds to the left and right
  neighbor coincide, so the hopping is doubled, consistent with
  $-2t\cos k$ at $k = 0, \pi$.
* `sumCluster[expr, {L1, ...}, {a1 -> {1, 0}, ...}]` specializes a symbolic
  result to the cluster. Site and momentum labels are folded modulo $L$ and
  Kronecker deltas are evaluated modulo $L$ before any other evaluation.
  Twisted boundary conditions are not supported here, because a twist shifts
  single-particle momenta but not momentum transfers; use `fourierrules`
  and `clusterhop` instead.
* `momentumbasis[bz, {L1, ...}]` splits basis subspaces by the total crystal
  momentum $K = \sum m \bmod L$, which is appended to the quantum numbers.

Exact diagonalization of the half-filled four-site Hubbard ring in
momentum space:

```wl
Hk = fourier[-t sum[hop[c[R], c[R + 1]], {R}] + U sum[hubbard[c[R]], {R}],
  c -> ck, R -> k];
HK = sumCluster[Hk, {4}];
ops = Table[ck[m], {m, 0, 3}];
makebasis[ops];
bz = Select[momentumbasis[qszbasis[ops], {4}], Take[First[#], 2] == {0, 0} &];
mats = makematricesbzvc[HK /. {t -> 1, U -> 4}, bzop2bzvc[bz, vacuum[]]];
{First[#], Min[Eigenvalues[N[Last[#]]]]} & /@ mats
(* {{{0, 0, 0}, -1.80642}, {{0, 0, 1}, -0.828427},
    {{0, 0, 2}, -2.10275}, {{0, 0, 3}, -0.828427}} *)
```

The quantum numbers are $(Q, S_z, K)$ with $Q$ measured from half filling;
the ground state lies in the $K = 2$ ($k = \pi$) sector. The same spectrum is
obtained in real space from
`clusterhop[-t, c, c, 1, {4}] + U Sum[hubbard[c[n]], {n, 0, 3}]`, and
`Expand[HR /. fourierrules[c -> ck, {4}]]` reproduces `HK` exactly.

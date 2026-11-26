# Schrieffer-Wolff transformation

`schriefferwolff` derives effective low-energy Hamiltonians by the canonical
Schrieffer-Wolff transformation to arbitrary order, e.g. Kondo models from
Anderson models, Heisenberg and t-J models from Hubbard models, and
superexchange or ring-exchange couplings of clusters. The results are
operator expressions that can be converted to spin operators.

## Conventions

The Hamiltonian $H = H_0 + V$ is block-diagonalized with respect to a
projector $P$ ($Q = 1 - P$) by

$$H_\mathrm{eff} = e^{S} H e^{-S},\qquad S = S_1 + S_2 + \dots,$$

where $S$ is anti-Hermitian and block-off-diagonal ($PSP = QSQ = 0$) and
$S_n$ is of order $V^n$. This canonical generator is unique. Order by order,
with $R_n$ the part of order $n$ of $e^{S'} H e^{-S'}$,
$S' = S_1 + \dots + S_{n-1}$,

$$[H_0, S_n] = P R_n Q + Q R_n P,\qquad H_\mathrm{eff}^{(n)} = P R_n P .$$

In particular $H_\mathrm{eff}^{(2)} = \frac12 P[S_1, V]P$ with
$[H_0, S_1] = PVQ + QVP$, which for degenerate $P$ (energy $E_P$) equals
$P V Q (E_P - H_0)^{-1} Q V P$.

`schriefferwolff[H0, V, P, n]` returns the list
$\{P H_0 P, PVP, P H_\mathrm{eff}^{(2)} P, \dots,
P H_\mathrm{eff}^{(n)} P\}$; `"Generator" -> True` returns
`{list, {S1, ..., Sn}}`. The coefficients are simplified with the function
given by `"Simplify"` (default `Simplify`).

**Requirements.**

* $H_0$ is diagonal in the occupation numbers of the *local* modes: the
  fermion modes (and X-operator sites) that appear in products of number
  operators in $H_0$ or in $P$, e.g. $\epsilon_d n_d + U n_\uparrow
  n_\downarrow$. All other operators in $V$ must be eigenoperators of the
  rest of $H_0$, $[H_0, o] = \lambda o$: band energies
  $\sum_k \epsilon_k n_k$, linear bosons $\omega b^\dagger b$, Zeeman terms
  $B S^z$, atomic Hamiltonians $\sum_a E_a X^{aa}$.
* $P$ is written with number operators of the local modes (or diagonal
  X-operators), e.g. `projector1[d[]]` for single occupancy, and
  $P^2 = P$. It then commutes with $H_0$.
* Components of $V$ (and of the higher-order remainders) that connect $P$
  and $Q$ must have nonzero energy denominators; otherwise
  `schriefferwolff::res` is reported. Symbolic denominators such as
  $\epsilon_k - \epsilon_d$ are only rejected if they vanish identically.

The method splits every normal-ordered operator monomial $M$ into
eigenoperators of $H_0$: for $f(n)$ a polynomial in occupation numbers,
$f(n) M = M f(n + \Delta)$, so $[H_0, M] = M g(n)$ with a polynomial $g$ in
the occupations of the modes not contained in $M$, and $M$ is resolved with
projectors onto their values. `eigenoperators[O, H0]` gives this
decomposition, $O = \sum_\omega O_\omega$ with
$[H_0, O_\omega] = \omega O_\omega$:

```wl
Needs["sneg`"]
snegfermionoperators[c]; snegrealconstants[t, U];
H0 = U Sum[nc[number[c[i], UP], number[c[i], DO]], {i, 2}];
eigenoperators[nc[c[CR, 1, UP], c[AN, 2, UP]], H0][[All, 1]]   (* {0, U, -U} *)
```

## Superexchange in the Hubbard dimer

```wl
snegxoperators[X, "HubbardAtom"]; snegspinoperators[{S, 1/2}];
P = nc[projector1[c[1]], projector1[c[2]]];
heff = schriefferwolff[H0, -t hop[c[1], c[2]], P, 4];
Expand[xtospin[fermionstox[#, c -> X], X -> S]] & /@ heff
(* {0, 0, (4 t^2/U)(S1.S2 - 1/4), 0, -(16 t^4/U^3)(S1.S2 - 1/4)} *)
```

`fermionstox[expr, c -> X]` expresses the fermions in Hubbard-atom
X-operators ($c^\dagger_\uparrow = X^{\uparrow 0} + X^{2\downarrow}$,
$c^\dagger_\downarrow = X^{\downarrow 0} - X^{2\uparrow}$, the inverse of
`xtofermions`), and `xtospin[expr, X -> S]` maps the X-operators of singly
occupied sites to spin-1/2 operators ($X^{\uparrow\uparrow} = 1/2 + S^z$,
$X^{\uparrow\downarrow} = S^+$, ...). The result reproduces the expansion of
the exact singlet-triplet splitting $(\sqrt{U^2 + 16t^2} - U)/2 =
4t^2/U - 16t^4/U^3 + \dots$. The same result follows with an atomic $H_0 =
U\sum_i X_i^{22}$ and $V$ written in X-operators.

## Anderson model to Kondo model

```wl
snegfermionoperators[c, d]; snegrealconstants[ed, U]; snegrealfunctions[eps, v];
snegfreeindexes[k];
H0 = sum[eps[k] (number[c[k], UP] + number[c[k], DO]), {k}] +
  ed (number[d[], UP] + number[d[], DO]) + U nc[number[d[], UP], number[d[], DO]];
V = sum[v[k] hop[c[k], d[]], {k}];
heff = schriefferwolff[H0, V, projector1[d[]], 2];
xtospin[fermionstox[heff[[3]], d -> X], X -> S]
```

The second-order term is a double sum over $k, k_1$ containing the exchange
interaction $\sum_{kk'} J_{kk'}\, \mathbf S\cdot\mathbf s_{kk'}$ with
$\mathbf s_{kk'} = \frac12\sum_{\sigma\sigma'} c^\dagger_{k\sigma}
\boldsymbol\sigma_{\sigma\sigma'} c_{k'\sigma'}$ and

$$J_{kk'} = V_k V_{k'}\left[\frac{1}{\epsilon_k - \epsilon_d} +
\frac{1}{\epsilon_d + U - \epsilon_k} + \frac{1}{\epsilon_{k'} - \epsilon_d}
+ \frac{1}{\epsilon_d + U - \epsilon_{k'}}\right],$$

which is antiferromagnetic ($J_{kk'} > 0$) for
$\epsilon_d < \epsilon_k, \epsilon_{k'} < \epsilon_d + U$, together with
potential scattering and a constant energy shift. Automatically renamed
summation indexes are given canonical names ($k, k_1, \dots$) and sums that
differ only by the names of their indexes are combined.

## Lattices: Heisenberg and t-J models

For translation-invariant Hamiltonians written as sums over a declared
lattice index, $P$ can be given as a rule `{c, f}`: the local projector
`f[c[R]]` on every site. The commutators then only involve the relative
offsets of overlapping terms. This route is limited to second order, where
it is exact; higher orders need explicit clusters.

```wl
sneglatticeindexes[{R}, Ns];
H0 = U sum[nc[number[c[R], UP], number[c[R], DO]], {R}];
V = -t sum[hop[c[R], c[R + 1]], {R}];
heff = schriefferwolff[H0, V, {c, projector1}, 2];
Expand[xtospin[fermionstox[heff[[3]], c -> X], X -> S]]
(* -Ns t^2/U + sum[(4 t^2/U)(S_R . S_(R+1)), {R}] *)
```

With the rule `{c, 1 - nc[number[#, UP], number[#, DO]] &}` (no double
occupancy) the result is the t-J model including the three-site
correlated-hopping terms (Chao, Spałek, and Oleś, J. Phys. C 10, L271
(1977)); its specialization to a four-site ring with `sumCluster` agrees
with the explicit cluster calculation.

## Clusters and matrices

For explicit clusters, $P$ is a product of local projectors and the order is
arbitrary. The runtime grows quickly with the order and the number of
sites: the dimer to fourth order takes less than a second, the four-site
ring to second order a few seconds, and the 2x2 plaquette to fourth order
(ring exchange) about four minutes; see
[`examples/schriefferwolff-plaquette.m`](examples/schriefferwolff-plaquette.m),
which compares the effective spectrum with exact diagonalization (errors
scale as $t^4/U^3$ and $t^6/U^5$ at second and fourth order).

`schriefferwolffmatrix[H0, V, pind, n]` performs the same canonical
transformation for matrices, with $P$ spanned by the basis states `pind`,
and returns the $P$ blocks of $H_\mathrm{eff}^{(m)}$. $H_0$ must not couple
$P$ and $Q$; numerical non-diagonal $H_0$ are diagonalized within the
blocks. It is useful when $H_0$ is not diagonal in occupation numbers and as
an independent check of the operator results.

## Output and limitations

* The results are $P H_\mathrm{eff} P$ as operators, including the
  projector factors; with the lattice rule, the projectors of sites not
  touched by a term are omitted (they are 1 within $P$).
* Products are formed in an internal representation of short operator
  monomials times polynomials in occupation numbers, so projectors do not
  generate long operator strings; the final conversion to SNEG expressions
  can still produce large expressions for clusters.
* $H_0$ with nonlinear boson or spin terms (e.g. $U n(n-1)$, $D (S^z)^2$)
  and projectors involving bosons or spins are not supported.

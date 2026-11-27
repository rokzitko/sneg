---
keywords: eigenoperator, ladder operator, energy transfer, Liouvillian, Schrieffer-Wolff transformation, Hubbard operators, projector, superoperator
see also: schriefferwolff, schriefferwolffmatrix, komutator, liouvillian, projector1, snegxoperators
related links: [Á. Bácsi, T. Iličin, and R. Žitko, Phys. Rev. B 113, 064501 (2026) (Schrieffer-Wolff approach to Josephson junctions)](https://doi.org/10.1103/jfcl-7pzt)
---
# eigenoperators

## Usage
- `eigenoperators[O, H0]` decomposes the operator *O* into eigenoperators of the commutator with *H0* and returns `{{omega1, O1}, {omega2, O2}, ...}` with $O = \sum_\omega O_\omega$ and $[H_0, O_\omega] = \omega\, O_\omega$.

## Details
- $O_\omega$ changes the energy of $H_0$ by $\omega$: if $|n\rangle$ is an eigenstate of $H_0$ with energy $E_n$, then $O_\omega |n\rangle$ is an eigenstate with energy $E_n + \omega$ (or zero). In the eigenbasis of $H_0$, $O_\omega$ contains the matrix elements $\langle m|O|n\rangle$ with $E_m - E_n = \omega$.
- $H_0$ must be diagonal in the occupation numbers of the local modes: polynomials in fermion number operators (e.g. $\epsilon_d n_d + U n_{d\uparrow} n_{d\downarrow}$) or diagonal X-operators ($\sum_a E_a X^{aa}$). All other operators in *O* must be eigenoperators of the remaining part of $H_0$, $[H_0, o] = \lambda o$: band energies $\sum_k \epsilon_k n_k$, linear bosons $\omega\, b^\dagger b$, Zeeman terms $B S^z$.
- For a normal-ordered monomial $M$ and a polynomial $f(n)$ of the occupation numbers, $f(n) M = M f(n + \Delta)$, where $\Delta$ is the change of the occupations by $M$. Hence $[H_0, M] = M g(n)$ with a polynomial $g$ in the occupations of the local modes not contained in $M$ ("spectators"), and $M$ is split with projectors onto the values of the spectator occupations. The components $O_\omega$ therefore contain products of number operators $n$ and $1 - n$.
- The energies $\omega$ are expressions in the parameters of $H_0$. The components are grouped by $\omega$; components that vanish are dropped. The order of the list is not sorted.
- The result is the basis of the Schrieffer-Wolff transformation in `schriefferwolff`: the first-order generator is $S_1 = \sum_{\omega \neq 0} V_\omega/\omega$ for the block-off-diagonal components of $V$.
- Messages and failures (`$Failed`): `eigenoperators::diag` if $H_0$ is not diagonal or an operator is not an eigenoperator of $H_0$; `eigenoperators::sum` for symbolic sums (decompose the summand instead); `eigenoperators::term` for terms that are not products of operators; `eigenoperators::ordering` if local fermions are declared with an ordering other than the default `EMPTY`.
- Coefficients may contain real functions such as `eps[k]` declared with `snegrealfunctions`.

## Developer Notes
- Internally, occupations are represented by commuting indicators `snegswind[mode, value]`, and operators by short monomials times polynomials in these indicators (`snegswcontext`, `snegswterms`, `snegswcomponents`). `schriefferwolff` uses the same representation.

## Examples
### Basic Examples
The hopping of a spin-up electron in the Hubbard dimer with $H_0 = U (n_{1\uparrow} n_{1\downarrow} + n_{2\uparrow} n_{2\downarrow})$ changes the energy by $0$ or $\pm U$:
```wl
snegfermionoperators[c];
snegrealconstants[U];
H0 = U (hubbard[c[1]] + hubbard[c[2]]);
e = eigenoperators[nc[c[CR, 1, UP], c[AN, 2, UP]], H0];
e[[All, 1]]
```
The component with $\omega = U$ creates a doublon on site 1 and does not destroy one on site 2, $c_{1\uparrow}^\dagger c_{2\uparrow} n_{1\downarrow} (1 - n_{2\downarrow})$:
```wl
Expand[e[[2, 2]] - nc[c[CR, 1, UP], c[AN, 2, UP], number[c[1], DO], 1 - number[c[2], DO]]]
```
The components add up to the operator and are eigenoperators:
```wl
{Expand[Total[e[[All, 2]]] - nc[c[CR, 1, UP], c[AN, 2, UP]]], Union[Map[Expand[komutator[H0, #[[2]]] - #[[1]] #[[2]]] &, e]]}
```

### Scope
#### Anderson impurity
Adding an electron to the impurity level costs $\epsilon_d$ if the level is empty and $\epsilon_d + U$ if it is singly occupied:
```wl
snegfermionoperators[d];
snegrealconstants[ed, U];
eigenoperators[d[CR, UP], ed number[d[]] + U hubbard[d[]]]
```
#### Bands, bosons, and spins
Operators that are eigenoperators of a band term, a boson, or a Zeeman term:
```wl
snegfermionoperators[a, d];
snegbosonoperators[b];
snegspinoperators[{S, 1/2}];
snegrealconstants[ed, w, B, e1];
eigenoperators[nc[S[1, 4], a[CR, 1, DO], a[AN, 1, UP]] + nc[b[CR], d[AN, UP]], B S[1, 3] + w number[b[]] + ed number[d[]] + e1 number[a[1]]]
```
---
Momentum-dependent energies of a band, declared with `snegrealfunctions`:
```wl
snegfermionoperators[c, d];
snegrealconstants[ed, U];
snegrealfunctions[eps];
snegfreeindexes[k];
eigenoperators[nc[c[CR, k, UP], d[AN, UP]], eps[k] number[c[k]] + ed number[d[]] + U hubbard[d[]]]
```
#### Hubbard X-operators
For an atomic Hamiltonian $\sum_a E_a X^{aa}$, the X-operator $X^{ab}$ changes the energy by $E_a - E_b$:
```wl
snegxoperators[X, "HubbardAtom"];
snegrealconstants[ed, U];
eigenoperators[X[1, "0", "UP"] + X[1, "UP", "2"], U X[1, "2", "2"] + ed X[1, "UP", "UP"]]
```

### Applications
#### First-order Schrieffer-Wolff generator
For the Hubbard dimer, the components of the hopping with $\omega = \pm U$ connect the singly occupied subspace with the states with a doublon. The generator $S_1 = \sum_{\omega \neq 0} V_\omega/\omega$ agrees with the generator returned by `schriefferwolff`:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H0 = U (hubbard[c[1]] + hubbard[c[2]]);
V = -t hop[c[1], c[2]];
S1 = Total[Map[If[#[[1]] === 0, 0, #[[2]]/#[[1]]] &, eigenoperators[V, H0]]];
gen = Last[schriefferwolff[H0, V, nc[projector1[c[1]], projector1[c[2]]], 2, "Generator" -> True]];
Expand[S1 - First[gen]]
```

### Properties & Relations
`eigenoperators` decomposes $O$ according to the spectrum of the Liouvillian $\mathcal L_0 O = [H_0, O]$. Applying $\mathcal L_0$ twice to the impurity operator gives the components multiplied by $\omega^2$:
```wl
snegfermionoperators[d];
snegrealconstants[ed, U];
H0 = ed number[d[]] + U hubbard[d[]];
e = eigenoperators[d[CR, UP], H0];
Expand[komutator[H0, komutator[H0, d[CR, UP]]] - Total[Map[#[[1]]^2 #[[2]] &, e]]]
```

### Possible Issues
$H_0$ must be diagonal in the occupation numbers; a hopping term in $H_0$ is rejected:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
eigenoperators[c[CR, 1, UP], U hubbard[c[1]] + t hop[c[1], c[2]]]
```
---
Symbolic sums are not decomposed; decompose the summand:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
eigenoperators[sum[c[CR, k, UP], {k}], 0]
```

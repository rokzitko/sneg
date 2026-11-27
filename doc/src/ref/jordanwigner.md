---
keywords: Jordan-Wigner transformation, spin chain, XXZ chain, XY model, spinless fermions, string operator, fermionization, Ising chain
see also: holsteinprimakoff, schwingerbosons, abrikosov, snegspinlessfermionoperators, spinclustermatrices, quadraticform
related links: [P. Jordan and E. Wigner, Z. Phys. 47, 631 (1928)](https://doi.org/10.1007/BF01331938)
---
# jordanwigner

## Usage
- `jordanwigner[expr, S -> f, {j1, j2, ...}]` applies the Jordan-Wigner transformation to the spin-1/2 operators `S` of the sites *j1*, *j2*, ... in *expr*, giving an expression in the spinless fermion operators `f`.

## Details
- For the sites in the given order $j_1, j_2, \ldots$, with $k$ the position of the site in the list,
  $$S^+_k = f^\dagger_k \prod_{l<k} (1 - 2 n_l), \qquad S^-_k = \prod_{l<k} (1 - 2 n_l)\, f_k, \qquad S^z_k = n_k - \frac12,$$
  with $n_l = f^\dagger_l f_l$, and $S^x = (S^+ + S^-)/2$, $S^y = (S^+ - S^-)/(2i)$. The string $\prod_{l<k}(1 - 2n_l) = \prod_{l<k} e^{i\pi n_l}$ makes spins on different sites commute.
- The spin-up state $|{\uparrow}\rangle$ corresponds to an occupied fermion level, the spin-down state to an empty one.
- `S` must be a spin declared with `snegspinoperators[{S, 1/2}]`, otherwise the message `jordanwigner::spin` is issued; `f` must be a spinless fermion operator (`snegspinlessfermionoperators`), otherwise `jordanwigner::op`. In both cases `$Failed` is returned.
- The fermion of the site `S[j]` is `f[j]`, i.e. `f[CR, j]` and `f[AN, j]` (internally with the spin index 0). Sites with several indexes are given as lists, e.g. `{{1, "a"}, {1, "b"}}`.
- Only the operators of the listed sites are transformed; the result is expanded.
- Nearest-neighbor terms in the chosen ordering give local fermion terms: the XX chain maps to free fermions with the hopping $J/2$, and the $S^z S^z$ coupling to a density-density interaction. Terms between distant sites in the ordering contain the string operators.

## Examples
### Basic Examples
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
jordanwigner[{S[1, 4], S[2, 4], S[2, 3]}, S -> f, {1, 2}]
```

### Scope
The ordering of the sites determines the strings:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
{jordanwigner[S[1, 4], S -> f, {1, 2}], jordanwigner[S[1, 4], S -> f, {2, 1}]}
```
---
Nearest-neighbor spin flips become hopping terms:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
jordanwigner[nc[S[1, 4], S[2, 5]], S -> f, {1, 2}]
```
---
Exchange between sites 1 and 3 contains the string of site 2:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
jordanwigner[spinspin[S[1], S[3]], S -> f, {1, 2, 3}]
```

### Applications
XXZ chain of four sites, $H = \sum_i [\frac{J}{2} (S^+_i S^-_{i+1} + S^-_i S^+_{i+1}) + J_z S^z_i S^z_{i+1}]$. The transverse part gives nearest-neighbor hopping, the longitudinal part a nearest-neighbor interaction:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
snegrealconstants[J, Jz];
H = Sum[J/2 (nc[S[i, 4], S[i + 1, 5]] + nc[S[i, 5], S[i + 1, 4]]) + Jz nc[S[i, 3], S[i + 1, 3]], {i, 3}];
Hf = jordanwigner[H, S -> f, Range[4]]
```
The spectrum agrees with the exact diagonalization of the spin model:
```wl
e1 = Sort[Flatten[Eigenvalues /@ makematricesbzop[Hf /. {J -> 1, Jz -> 1/2}, qszbasis[Table[f[i], {i, 4}]]][[All, 2]]]];
e2 = Sort[Flatten[Eigenvalues /@ spinclustermatrices[H /. {J -> 1, Jz -> 1/2}, {S -> Range[4]}][[All, 2]]]];
Chop[N[e1 - e2]]
```
---
The XX chain ($J_z = 0$) is a free-fermion model. Its single-particle Hamiltonian is the tight-binding matrix with the hopping $J/2$, with the eigenvalues $J \cos(\pi m/5)$, $m = 1, \ldots, 4$:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
snegrealconstants[J];
Hxx = Sum[J/2 (nc[S[i, 4], S[i + 1, 5]] + nc[S[i, 5], S[i + 1, 4]]), {i, 3}];
{h, e0} = quadraticform[jordanwigner[Hxx, S -> f, Range[4]], Table[f[i], {i, 4}], "BdG" -> False];
{h // MatrixForm, Simplify[Sort[Eigenvalues[h]] - Sort[Table[J Cos[Pi m/5], {m, 4}]]]}
```

### Properties & Relations
The transformation preserves the spin algebra: $[S^+_2, S^-_2] = 2 S^z_2$, and operators on different sites commute:
```wl
snegspinoperators[{S, 1/2}];
snegspinlessfermionoperators[f];
jw = jordanwigner[#, S -> f, Range[3]] &;
Expand[{komutator[jw[S[2, 4]], jw[S[2, 5]]] - 2 jw[S[2, 3]], komutator[jw[S[1, 4]], jw[S[3, 5]]], komutator[jw[S[1, 1]], jw[S[1, 2]]] - I jw[S[1, 3]]}]
```

### Possible Issues
The transformation is defined for spin 1/2 only:
```wl
snegspinoperators[{T, 1}];
snegspinlessfermionoperators[f];
jordanwigner[T[1, 3], T -> f, {1}]
```
---
The target operator must be a spinless fermion:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
jordanwigner[S[1, 3], S -> c, {1}]
```

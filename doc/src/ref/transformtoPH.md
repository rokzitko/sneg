---
keywords: phonons, local phonon mode, Holstein model, Anderson-Holstein model, electron-phonon coupling, polaron, Lang-Firsov, boson cutoff, product basis, kets
see also: phononnumber, phononplus, phononminus, phononx, phononbasis, ket, bra, qsbasis, makematricesbzvc, transformtoLR
---
# transformtoPH

## Usage
- `transformtoPH[bz, {nph}]` gives the product of the basis *bz* with the states $|n\rangle$, $n = 0, \ldots, n_\mathrm{ph}$, of a local phonon mode.
- `transformtoPH[bz, {n1, n2, ...}]` includes several phonon modes with the cutoffs $n_1, n_2, \ldots$; the phonon states are `ket[k1, k2, ...]`.

## Details
- `PH` stands for phonons. The phonon (boson) states are represented by kets: `ket[n]` is $|n\rangle = (b^\dagger)^n |0\rangle / \sqrt{n!}$, truncated at $n_\mathrm{ph}$ quanta.
- *bz* is a basis `{{qn, states}, ...}` (see `makematricesbzvc`) in either representation. Each state of each subspace is multiplied by all phonon states, and the quantum numbers are not changed; the dimension of every subspace grows by the factor $\prod_i (n_i + 1)$. In the creation operator representation, the states are `nc[f, ket[n]]`; in the occupation number representation, `vc[..., ket[n]]`.
- Within a subspace, the phonon state is the outer index: first all fermion states with $|0\rangle$, then all with $|1\rangle$, and so on.
- The phonon operators in the truncated space are `phononnumber[nph]` ($b^\dagger b$), `phononplus[nph]` ($b^\dagger$), `phononminus[nph]` ($b$), and `phononx[nph]` ($b + b^\dagger$), written with `ket` and `bra`; for several modes, `phononplus[i, {n1, n2, ...}]` etc. act on mode *i*.
- The truncation is exact for operators that conserve the phonon number. With electron-phonon coupling, results converge with increasing cutoff; check the convergence. The truncated operators violate $[b, b^\dagger] = 1$ in the highest state.
- The cutoffs must be given as a list of integers; otherwise `transformtoPH` stays unevaluated.
- To combine phonons with the parity of `transformtoLR`, add the phonons first and give the transformation of the kets under the reflection in the rules of `transformtoLR`.

## Examples
### Basic Examples
Single level with a phonon mode with at most two quanta:
```wl
snegfermionoperators[d];
transformtoPH[qsbasis[{d[]}], {2}]
```

### Scope
Basis in the occupation number representation; the kets are appended to the vectors:
```wl
snegfermionoperators[d];
transformtoPH[qszbasisvc[{d[]}], {1}]
```
---
Two phonon modes with one quantum each:
```wl
snegfermionoperators[d];
keyselect[transformtoPH[qsbasis[{d[]}], {1, 1}], {-1, 0}]
```

### Applications
Anderson-Holstein impurity: a level coupled to a local phonon of frequency $\omega$, $H = \frac{U}{2}(n-1)^2 + \omega\, b^\dagger b + g (n - 1)(b + b^\dagger)$:
```wl
snegfermionoperators[d];
snegrealconstants[U, omega, g];
H[nph_] := U/2 isozsq[d[]] + omega phononnumber[nph] + g nc[number[d[]] - 1, phononx[nph]];
makematricesbzvc[H[2], transformtoPH[qsbasisvc[{d[]}], {2}]]
```
The Lang-Firsov transformation gives the exact ground state energy $U/2 - g^2/\omega$ for the empty and the doubly occupied level and $0$ for single occupancy; the effective interaction is $U_\mathrm{eff} = U - 2 g^2/\omega$. Convergence of the lowest energy in the subspace $Q = -1$ with the cutoff, for $U = 1$, $\omega = 1$, $g = 1/2$ (exact value 1/4):
```wl
Table[{nph, Min[Eigenvalues[N[keyselect[makematricesbzvc[H[nph] /. {U -> 1, omega -> 1, g -> 1/2}, transformtoPH[qsbasisvc[{d[]}], {nph}]], {-1, 0}][[1, 2]]]]]}, {nph, {2, 4, 8, 12}}]
```

### Properties & Relations
The dimension of the product basis is the product of the dimensions:
```wl
snegfermionoperators[c];
Total[Length /@ transformtoPH[qszbasisvc[{c[1], c[2]}], {3}][[All, 2]]] == 16*4
```

### Possible Issues
The cutoff must be a list:
```wl
snegfermionoperators[d];
transformtoPH[qszbasisvc[{d[]}], 2]
```

---
keywords: Fourier transformation, momentum space, Bloch states, tight-binding, band structure, Hubbard model, electron-phonon coupling, Holstein model, Anderson impurity, plane waves
see also: blochhamiltonian, blochphase, blochphaseexp, sneglatticeindexes, snegmomentumindexes, sneglatticevectors, sumSimplifyKD, sumMomentumCanonical, fourierrules, sumCluster, bdghamiltonian
---
# fourier

## Usage
- `fourier[expr, c -> ck, R -> k]` transforms the site operators `c` in *expr* to the momentum operators `ck`, $c_R = N_s^{-1/2} \sum_k e^{ik\cdot R} c_k$.
- `fourier[expr, c -> ck, R -> k, "Inverse" -> True]` transforms the momentum operators `ck` back to the site operators `c`, $c_k = N_s^{-1/2} \sum_R e^{-ik\cdot R} c_R$.

## Details
- *R* must be a lattice index (`sneglatticeindexes`) and *k* a momentum index (`snegmomentumindexes`); $N_s$ is `latticesize[k]` (`latticesize[R]` for the inverse transformation). `c` and `ck` are fermion or boson operators.
- The convention is
$$c_{R\alpha\sigma} = \frac{1}{\sqrt{N_s}} \sum_k e^{ik\cdot R} c_{k\alpha\sigma}, \qquad c^\dagger_{R\alpha\sigma} = \frac{1}{\sqrt{N_s}} \sum_k e^{-ik\cdot R} c^\dagger_{k\alpha\sigma},$$
so that $\sum_R e^{iK\cdot R} = N_s \delta_{K,0}$ and $\sum_k e^{ik\cdot X} = N_s \delta_{X,0}$. The transformation is unitary: canonical (anti)commutation relations and $\sum_R n_R = \sum_k n_k$ are preserved.
- The site or momentum label is the first index after `CR`/`AN` (option `"IndexPosition"`). Orbital indexes and the spin index are kept: `c[CR, R + a1, "A", UP]` becomes a sum over `ck[CR, k, "A", UP]` with the Bloch phase `blochphase[-k, R + a1]`. Every operator `c[CR | AN, ...]` is transformed, whatever its site label: `c[0]`, `c[R + 1]`, `c[R + a1]`, or a free label.
- Bloch phases are represented by `blochphase[k, X]`, $e^{ik\cdot X}$. Phases with a common lattice argument are combined and sums in the lattice argument are split, so $e^{-ik\cdot R} e^{ik'\cdot(R+1)} = e^{i(k'-k)\cdot R} e^{ik'}$.
- With `"Simplify" -> True` (default), the result is simplified by repeated `sumExpand`, `Expand`, `sumSimplifyKD`, and `sumCollect`: the lattice sums over $R$ are performed, which gives momentum conservation, and the Kronecker deltas are eliminated. Automatically renamed dummy indexes with the root *k* (`k1`, ...) are renamed back to *k* where possible.
- To transform several operator families, call `fourier` repeatedly with different momentum indexes, e.g. `k` for electrons and `q` for phonons.
- The result is a sum over momenta; `blochhamiltonian` extracts the Bloch matrix $h(k)$ of a bilinear Hamiltonian, and `sumCluster` specializes the result to a finite periodic cluster.

| `"IndexPosition"` | `1` | position of the site or momentum label among the indexes after `CR`/`AN` |
| `"Inverse"` | `False` | whether to transform the momentum operators `ck` to the site operators `c` |
| `"Simplify"` | `True` | whether to perform the lattice sums and simplify the result |

| `fourier::index` | *R* is not a lattice index or *k* is not a momentum index; returns `$Failed` |
| `fourier::op` | `c` (`ck` for the inverse) is not a fermion or boson operator; returns `$Failed` |
| `fourier::capture` | *expr* already contains *k* (*R* for the inverse), which would be captured by the new sum; returns `$Failed` |

## Examples
### Basic Examples
Hubbard chain, $H = -t \sum_{R\sigma} (c^\dagger_{R\sigma} c_{R+1,\sigma} + \mathrm{h.c.}) + U \sum_R n_{R\uparrow} n_{R\downarrow}$:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t, U];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
H = -t sum[hop[c[R], c[R + 1]], {R}] + U sum[hubbard[c[R]], {R}];
fourier[H, c -> ck, R -> k]
```
In conventional notation,
$$H = \sum_{k\sigma} (-t e^{ik} - t e^{-ik}) c^\dagger_{k\sigma} c_{k\sigma} + \frac{U}{N_s} \sum_{k k_2 k_3} c^\dagger_{k\uparrow} c^\dagger_{k_2+k_3-k,\downarrow} c_{k_2\downarrow} c_{k_3\uparrow}.$$

### Scope
A single operator:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[c[AN, R, UP], c -> ck, R -> k]
```
---
The local density $n_R$ is not diagonal in momentum; the total density is, $\sum_R n_R = \sum_k n_k$:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
{fourier[number[c[R]], c -> ck, R -> k], fourier[sum[number[c[R]], {R}], c -> ck, R -> k]}
```
---
Boson operators. Dispersionless (Einstein) phonons, $\omega_0 \sum_R b^\dagger_R b_R = \omega_0 \sum_q b^\dagger_q b_q$:
```wl
snegbosonoperators[b, bk];
snegrealconstants[w0];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{q}, Ns];
fourier[w0 sum[nc[b[CR, R], b[AN, R]], {R}], b -> bk, R -> q]
```
---
Two-dimensional lattices with lattice vectors; square lattice:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1, a2];
fourier[-t sum[hop[f[R], f[R + a1]] + hop[f[R], f[R + a2]], {R}], f -> fk, R -> k]
```
---
Orbital indexes. The Su-Schrieffer-Heeger chain with intracell hopping $v$ and intercell hopping $w$ between the sublattices A and B:
```wl
snegspinlessfermionoperators[f, fk];
snegrealconstants[v, w];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hssh = sum[v hop[f[R, "A"], f[R, "B"]] + w hop[f[R + 1, "A"], f[R, "B"]], {R}];
fourier[Hssh, f -> fk, R -> k]
```

### Options
#### IndexPosition
The site label is the second index after `CR`/`AN`:
```wl
snegspinlessfermionoperators[f, fk];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[sum[hop[f["x", R], f["x", R + 1]], {R}], f -> fk, R -> k, "IndexPosition" -> 2]
```
#### Inverse
Back to real space:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
Hk = sum[-t (blochphase[k, 1] + blochphase[-k, 1]) number[ck[k]], {k}];
fourier[Hk, c -> ck, R -> k, "Inverse" -> True]
```
The result describes the same hopping term as $-t \sum_R (c^\dagger_R c_{R+1} + \mathrm{h.c.})$:
```wl
sumFullSimplify[% - (-t sum[hop[c[R], c[R + 1]], {R}])]
```
#### Simplify
Without simplification, the operators are only replaced by their expansions:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[sum[number[c[R], UP], {R}], c -> ck, R -> k, "Simplify" -> False]
```

### Applications
Tight-binding band of a chain, $\epsilon_k = -2t\cos k$, from the Bloch Hamiltonian:
```wl
snegfermionoperators[c, ck];
snegrealconstants[t];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
h = blochhamiltonian[fourier[-t sum[hop[c[R], c[R + 1]], {R}], c -> ck, R -> k], {ck}, k];
ExpToTrig[blochphaseexp[h]]
```
---
Electron-phonon coupling of the Holstein model, $g \sum_R n_R (b^\dagger_R + b_R)$. The electrons and the phonons are transformed with different momentum indexes; the result conserves momentum:
```wl
snegfermionoperators[c, ck];
snegbosonoperators[b, bk];
snegrealconstants[g];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k, q}, Ns];
Hep = g sum[nc[number[c[R]], b[CR, R] + b[AN, R]], {R}];
Hq = fourier[fourier[Hep, c -> ck, R -> k], b -> bk, R -> q]
```
`sumMomentumCanonical` gives the standard form $\frac{g}{\sqrt{N_s}} \sum_{kq\sigma} c^\dagger_{k\sigma}(c_{k+q,\sigma} b^\dagger_q + c_{k-q,\sigma} b_q)$:
```wl
sumCollect[sumMomentumCanonical[sumExpand[Hq]]]
```
---
Anderson impurity coupled to the site $R = 0$ of the host lattice; the impurity hybridizes with all Bloch states with $V_k = V/\sqrt{N_s}$:
```wl
snegfermionoperators[c, ck, d];
snegrealconstants[V];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[V hop[d[], c[0]], c -> ck, R -> k]
```

### Properties & Relations
The transformed operators obey the canonical anticommutation relations, $\{c_{R\uparrow}, c^\dagger_{R'\uparrow}\} = \delta_{RR'}$:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R, Rp}, Ns];
snegmomentumindexes[{k, p}, Ns];
Expand[sumSimplifyKD[sumExpand[antikomutator[fourier[c[AN, R, UP], c -> ck, R -> k], fourier[c[CR, Rp, UP], c -> ck, R -> p]]]]]
```

### Possible Issues
The momentum index must not occur in the expression; transform a second operator family with a different index:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[number[ck[k]], c -> ck, R -> k]
```
---
The indexes must be declared:
```wl
snegfermionoperators[c, ck];
snegmomentumindexes[{k}, Ns];
fourier[number[c[1]], c -> ck, n -> k]
```
---
Majorana operators are not transformed:
```wl
snegmajoranaoperators[gm];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
fourier[gm[R], gm -> gk, R -> k]
```
---
`Ns` is declared real but not positive, so the conjugate of $N_s^{-1/2}$ is not simplified by `conj`. Transform the creation operator directly:
```wl
snegfermionoperators[c, ck];
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
{conj[fourier[c[AN, R, UP], c -> ck, R -> k]], fourier[c[CR, R, UP], c -> ck, R -> k]}
```

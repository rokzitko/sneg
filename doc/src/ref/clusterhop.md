---
keywords: hopping on a finite cluster, ring, periodic boundary conditions, twisted boundary conditions, flux, exact diagonalization, spin-orbit coupling
see also: fourierrules, sumCluster, momentumbasis, hop, matrixhop
---
# clusterhop

## Usage
- `clusterhop[T, c[a], d[b], delta, {L1, ...}]` gives the hopping $\sum_n c^\dagger_{n a}\, T\, d_{n+\delta, b} + \mathrm{h.c.}$ on a finite periodic cluster with $L_1 \times \cdots$ sites.
- `clusterhop[T, c, d, delta, {L1, ...}]` is the same for operators without orbital indexes.
- `clusterhop[T, c[a], d[b], delta, {L1, ...}, "Twist" -> theta]` uses twisted boundary conditions.

## Details
- Sites are labeled as in `fourierrules`: integers $n = 0, \ldots, L - 1$ in one dimension, integer vectors $n = (n_1, n_2, \ldots)$ in lattice coordinates otherwise. The displacement *delta* is an integer (1D) or an integer vector of the same dimension; otherwise `clusterhop::dims` is issued and `$Failed` returned. The target site $n + \delta$ is folded into the cluster modulo $L_a$.
- The site label is inserted as the first index after `CR`/`AN`, before the orbital indexes *a* and *b*; for fermions, the spin index is appended last: `c[a]` gives `c[CR, n, a, σ]`.
- *T* is a number or a symbol (times the identity in spin space), or a matrix in the spin indexes, e.g. a 2×2 matrix for spin-1/2 fermions with rows and columns ordered $(\uparrow, \downarrow)$, as in `matrixhop`. `c` and `d` can be fermions (with spin), spinless fermions, or bosons.
- With `"Twist" -> theta`, the bonds that cross the boundary in direction $a$ are multiplied by $e^{i\theta_a w_a}$, where $w_a$ is the number of windings, corresponding to $c_{n + L_a e_a} = e^{i\theta_a} c_n$. A scalar twist applies to all directions. A twist describes a magnetic flux $\theta$ through a ring and is used for twist-averaged boundary conditions. The default `Automatic` means periodic boundary conditions.
- Every site contributes one bond, so on a ring with $L = 2$ the bonds to the left and to the right neighbor coincide and the hopping is doubled; this is consistent with the band energies $-2t\cos k$ at $k = 0, \pi$. Likewise, for $L_a = 1$ the hopping in direction $a$ becomes an on-site term.

| `"Twist"` | `Automatic` | twist angles $\theta_a$ of the boundary conditions |

## Examples
### Basic Examples
Hopping on a three-site ring of spinless fermions:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
clusterhop[-t, f, f, 1, {3}]
```

### Scope
On a two-site ring the hopping is doubled:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
clusterhop[-t, f, f, 1, {2}]
```
---
Hopping between orbitals A and B in the same unit cell ($\delta = 0$) on a two-cell chain:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
clusterhop[-t, f["A"], f["B"], 0, {2}]
```
---
Two-dimensional cluster, hopping in direction $a_1$ on a $2 \times 2$ cluster:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t];
clusterhop[-t, f, f, {1, 0}, {2, 2}]
```
---
Spin-dependent hopping with a $2 \times 2$ matrix, here the spin-flip term $\alpha \sigma_x$ on a two-site ring:
```wl
snegfermionoperators[c];
snegrealconstants[alpha];
clusterhop[alpha PauliMatrix[1], c, c, 1, {2}]
```

### Options
#### Twist
A flux $\theta$ through a three-site ring; the bond between sites 2 and 0 carries the phase:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, theta];
clusterhop[-t, f, f, 1, {3}, "Twist" -> theta]
```

### Applications
Persistent current. The single-particle energies of a ring threaded by the flux $\theta$ are $-2t\cos((2\pi m + \theta)/L)$; the ground-state energy of one electron depends on the flux:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, theta];
H = clusterhop[-t, f, f, 1, {3}, "Twist" -> theta];
h = Table[braketop[f[CR, i, 0], H, f[CR, j, 0]], {i, 0, 2}, {j, 0, 2}];
Simplify[Eigenvalues[h /. {t -> 1, theta -> Pi/2}] - Table[-2 Cos[(2 Pi m + Pi/2)/3], {m, 0, 2}]]
```
---
Spectrum of the half-filled Hubbard ring with four sites in the sector $S_z = 0$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
ops = Table[c[n], {n, 0, 3}];
H = clusterhop[-t, c, c, 1, {4}] + U Sum[hubbard[c[n]], {n, 0, 3}];
makebasis[ops];
bz = Select[qszbasis[ops], First[#] == {0, 0} &];
Min[Eigenvalues[N[matrixrepresentationop[H /. {t -> 1, U -> 4}, bz[[1, 2]]]]]]
```

### Possible Issues
The displacement must have the dimension of the cluster:
```wl
snegspinlessfermionoperators[f];
clusterhop[-1, f, f, 1, {2, 2}]
```

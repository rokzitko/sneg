---
keywords: charge-charge interaction, density-density interaction, intersite Coulomb repulsion, extended Hubbard model, nearest-neighbor interaction V, charging energy
see also: number, hubbard, isozsq, spinspin, twohop, hop
---
# chargecharge

## Usage
- `chargecharge[a[i], b[j]]` gives the charge-charge (density-density) interaction $n_{a,i}\, n_{b,j}$ between the levels `a[i]` and `b[j]`.

## Details
- `chargecharge[a[i], b[j]]` is `nc[number[a[i]], number[b[j]]]`, with $n = \sum_\sigma c^\dagger_\sigma c_\sigma$. The result is normal ordered.
- The levels are fermion levels given without the creation/annihilation and spin indexes.
- The extended Hubbard model contains the intersite repulsion $V \sum_{\langle ij\rangle} n_i n_j$, written as `V chargecharge[c[i], c[j]]` for each bond.
- For the same level, `chargecharge[c[i], c[i]]` is $n_i^2 = n_i + 2 n_{i\uparrow} n_{i\downarrow}$, which includes the self-interaction term $n_i$. The number of electron pairs is $n_i(n_i - 1)/2 = n_{i\uparrow} n_{i\downarrow}$ (`hubbard`).
- `chargecharge` conserves the charge and the spin.
- `chargecharge` is not `Listable`. Bare symbols are not accepted as levels.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
chargecharge[c[], d[]]
```

### Applications
Extended Hubbard dimer, $H = -t\,\mathrm{hop}_{12} + U \sum_i n_{i\uparrow} n_{i\downarrow} + V n_1 n_2$. In the half-filled subspace ($Q = 0$, $S_z = 0$), the intersite repulsion $V$ competes with $U$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U, V];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]) + V chargecharge[c[1], c[2]];
mats = makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]];
Eigenvalues[Select[mats, First[#] == {0, 0} &][[1, 2]]]
```
The spin-triplet state with both electrons on different sites has the energy $V$:
```wl
Eigenvalues[Select[mats, First[#] == {0, 1} &][[1, 2]]]
```

### Properties & Relations
On the same level, $n^2 = n + 2 n_\uparrow n_\downarrow$:
```wl
snegfermionoperators[c, d];
Expand[chargecharge[c[], c[]] - number[c[]] - 2 hubbard[c[]]]
```
---
The interaction is symmetric and commutes with the total spin:
```wl
{Sx, Sy, Sz, S2} = manyspin[{c[], d[]}];
{Expand[chargecharge[c[], d[]] - chargecharge[d[], c[]]], Expand[komutator[chargecharge[c[], d[]], S2]]}
```
---
`SnegSimplify` recognizes the charge-charge interaction:
```wl
SnegSimplify[Expand[chargecharge[c[], d[]]]]
```

---
keywords: product basis, two channels, separately conserved quantum numbers, spin projection, isospin, symmetry-adapted basis, NRG
see also: quickDBL, directproductbasis, mergebasis, qszbasis, quickISOSZbasis, makebasis
---
# quickDBLSZ

## Usage
- `quickDBLSZ[ops1, ops2, fnc]` gives the product basis of two subsystems with the levels *ops1* and *ops2*, with the quantum numbers `{X1, X2, Sz}`, where `{X1, Sz1}` and `{X2, Sz2}` are the quantum numbers of the bases `fnc[ops1]` and `fnc[ops2]`, and $S_z = S_{z1} + S_{z2}$.

## Details
- *fnc* is a basis generator in the creation operator representation with two quantum numbers, the second being $S_z$: `qszbasis` (quantum numbers $Q_1, Q_2, S_z$, the `DBLQSZ` symmetry type of NRG Ljubljana) or `quickISOSZbasis` ($I_1, I_2, S_z$, `DBLISOSZ`).
- The product states are `nc[state1, state2]`; the subspaces with equal `{X1, X2, Sz}` are merged with `mergebasis`.
- `quickDBLSZ` calls `makebasis[Join[ops1, ops2]]` at the end, so that `bzop2bzvc` can be applied to the result.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
bz = quickDBLSZ[{c[1]}, {c[2]}, qszbasis];
{First[#], Length[Last[#]]} & /@ bz
```

### Applications
Two quantum dots with separately conserved charges, coupled by the exchange interaction $J\, \mathbf S_1 \cdot \mathbf S_2$, in a magnetic field $B$. With one electron on each dot ($Q_1 = Q_2 = 0$), the subspace $S_z = 0$ contains the singlet and the $S_z = 0$ triplet state; the triplet states with $S_z = \pm 1$ are shifted by $\mp B$:
```wl
snegfermionoperators[c];
snegrealconstants[J, B];
bz = quickDBLSZ[{c[1]}, {c[2]}, qszbasis];
H = J spinspin[c[1], c[2]] - B (spinz[c[1]] + spinz[c[2]]);
Select[{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bzop2bzvc[bz]], #[[1, 1]] == 0 && #[[1, 2]] == 0 &]
```

### Properties & Relations
Merging the charges gives the $(Q, S_z)$ basis of the whole system:
```wl
snegfermionoperators[c];
bz = bzop2bzvc[quickDBLSZ[{c[1]}, {c[2]}, qszbasis]];
Map[Sort, mergebasis[{{#[[1, 1]] + #[[1, 2]], #[[1, 3]]}, #[[2]]} & /@ Expand[bz]], {2}] === Map[Sort, qszbasisvc[{c[1], c[2]}], {2}]
```

---
keywords: product basis, two channels, separately conserved quantum numbers, isospin, charge, symmetry-adapted basis, NRG
see also: quickDBLSZ, directproductbasis, basistensorproduct, quickSU2basis, qbasis, makebasis
---
# quickDBL

## Usage
- `quickDBL[ops1, ops2, fnc]` gives the product basis of two subsystems with the levels *ops1* and *ops2*, with the quantum numbers `{X1, X2}`, where `X1` and `X2` are the first quantum numbers of the bases `fnc[ops1]` and `fnc[ops2]`.

## Details
- *fnc* is a basis generator in the creation operator representation, such as `qbasis`, `qszbasis`, or `quickSU2basis`. Only the first quantum number of each subsystem basis is kept; the subspaces with equal first quantum numbers are merged before the product is formed.
- The product states are `nc[state1, state2]` (the operator strings of the first subsystem to the left), and the quantum numbers are joined: `{X1, X2}`. The subspaces are not merged further.
- `quickDBL` calls `makebasis[Join[ops1, ops2]]` at the end, so that `bzop2bzvc` can be applied to the result.
- The basis is appropriate for systems with two separately conserved quantities, e.g. the charges $Q_1$, $Q_2$ of two decoupled channels (`qbasis`), or the isospins $I_1$, $I_2$ of two particle-hole symmetric channels (`quickSU2basis`, the `DBLSU2` symmetry type of NRG Ljubljana).
- For SU(2) quantum numbers (isospin), the product of highest-weight states is the highest-weight state of the product of the two multiplets: each state represents $(2I_1+1)(2I_2+1)$ states. Generators whose subspaces contain only highest-weight states of a quantum number that is dropped (e.g. `qsbasis`, where $S$ is dropped) give incomplete bases; see Possible Issues.

## Examples
### Basic Examples
Two levels with separately conserved charges:
```wl
snegfermionoperators[c];
quickDBL[{c[1]}, {c[2]}, qbasis]
```

### Applications
Two Hubbard atoms coupled by the exchange interaction $J\, \mathbf S_1 \cdot \mathbf S_2$, which conserves the isospin of each site. With the basis of `quickSU2basis` for each site, the subspace $(I_1, I_2) = (0, 0)$ contains the four states with single occupancy on both sites:
```wl
snegfermionoperators[c];
snegrealconstants[J, U];
nnop[c[1]] = 0; nnop[c[2]] = 0;
bz = quickDBL[{c[1]}, {c[2]}, quickSU2basis];
H = U/2 (isozsq[c[1]] + isozsq[c[2]]) + J spinspin[c[1], c[2]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bzop2bzvc[bz]]
```

### Properties & Relations
With the multiplet degeneracies $(2I_1+1)(2I_2+1)$, the dimension of the Fock space is recovered:
```wl
Total[(2 #[[1, 1]] + 1) (2 #[[1, 2]] + 1) Length[#[[2]]] & /@ bz]
```
---
`quickDBL` restores `BASIS` for all levels:
```wl
BASIS
```

### Possible Issues
With `qsbasis`, the total spin $S$ of each subsystem is dropped, but the subspaces contain only the states with $S_z = S$; the product basis is incomplete:
```wl
snegfermionoperators[c];
Total[Length /@ Last /@ quickDBL[{c[1]}, {c[2]}, qsbasis]]
```

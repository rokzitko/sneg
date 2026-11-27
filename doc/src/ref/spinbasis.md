---
keywords: spin basis, total Sz, product basis, bra-ket representation, invariant subspaces
see also: spinket, spinbra, QSZaddspinket, spinketbraspinspin, spinketbrarules, makematricesbzop, basistensorproduct
---
# spinbasis

## Usage
- `spinbasis[s]` gives the basis of a spin *s* in the bra-ket representation, as a list of subspaces `{{m}, {ket[m]}}` labeled by the spin projection.
- `spinbasis[{s1, s2, ...}]` gives the product basis of several spins, `ket[m1, m2, ...]`, organized in subspaces of total $S_z = \sum_k m_k$.
- `spinbasis[s, pos]` places a single spin in the slots specified by the position array *pos*.

## Details
- The result has the form of the bases used by `makematricesbzop`: a list of `{{qn}, {states}}`.
- For a single spin, the subspaces are ordered from $m = s$ down to $m = -s$. For several spins, the subspaces are ordered by increasing total $S_z$; spin $k$ occupies the $k$-th slot of the kets.
- `QSZaddspinket` combines a spin basis with a fermionic basis with good charge and spin projection.
- All spins must be integers or half-integers.

## Examples
### Basic Examples
```wl
spinbasis[1]
```
Two spins 1/2:
```wl
spinbasis[{1/2, 1/2}]
```

### Scope
Three spins with different sizes:
```wl
Length /@ Last /@ spinbasis[{1/2, 1, 1/2}]
```

### Applications
Heisenberg dimer of a spin 1/2 and a spin 1 in the bra-ket representation:
```wl
snegspinoperators[{S, 1/2}, {T, 1}];
h = Expand[spinspin[S[1], T[1]] /. spinketbrarules[{S[1], T[1]}]];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzop[h, spinbasis[{1/2, 1}]]
```

### Properties & Relations
The dimension of the product space is $\prod_k (2 s_k + 1)$:
```wl
Total[Length /@ Last /@ spinbasis[{1/2, 1, 3/2}]] == 2 3 4
```

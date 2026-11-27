---
keywords: bra-ket representation, exchange interaction, two spins, singlet, triplet
see also: spinketbraXYZ, spinbasis, spinspin, spinketbrarules, makematricesbzop
---
# spinketbraspinspin

## Usage
- `spinketbraspinspin[{s1, s2}]` gives the exchange operator $\mathbf S_1 \cdot \mathbf S_2$ of two spins *s1* and *s2* in the bra-ket representation, acting on the kets `ket[m1, m2]`.

## Details
- The first spin occupies the first slot of the kets and the second spin the second slot, as in `spinbasis[{s1, s2}]`.
- The result is expanded.

## Examples
### Basic Examples
```wl
spinketbraspinspin[{1/2, 1/2}]
```
Matrices in the subspaces of total $S^z$: triplet $1/4$, singlet $-3/4$:
```wl
makematricesbzop[spinketbraspinspin[{1/2, 1/2}], spinbasis[{1/2, 1/2}]]
```

### Scope
Spin 1/2 coupled to spin 1: the eigenvalues are $1/2$ (total spin 3/2) and $-1$ (total spin 1/2):
```wl
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzop[spinketbraspinspin[{1/2, 1}], spinbasis[{1/2, 1}]]
```

### Properties & Relations
Agreement with `spinketbrarules` applied to the abstract exchange:
```wl
snegspinoperators[{S, 1/2}];
Expand[spinspin[S[1], S[2]] /. spinketbrarules[{S[1], S[2]}]] == spinketbraspinspin[{1/2, 1/2}]
```

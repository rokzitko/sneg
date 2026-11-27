---
keywords: bra-ket representation, spin operators, impurity spin, Kondo model, exact diagonalization, spins coupled to fermions, NRG
see also: QSZaddspinket, spinbasis, spinketbraX, spinclustermatrices, makematricesbzvc, snegspinoperators
---
# spinketbrarules

## Usage
- `spinketbrarules[{S[1], S[2], ...}]` gives replacement rules that map the abstract spin operators of the listed sites to their bra-ket representation.
- `spinketbrarules[{S -> {j1, j2, ...}}]` specifies the sites by their indexes.

## Details
- For each listed site and each component $t = 1, \ldots, 5$ ($S^x, S^y, S^z, S^+, S^-$) a rule `S[j..., t] -> op` is generated. The *k*-th site in the list corresponds to the *k*-th slot of the kets `ket[m1, m2, ...]`, as in `spinbasis[{s1, s2, ...}]` and `QSZaddspinket[bvc, {s1, s2, ...}]`.
- Each operator includes the identities `spinketbraI` of the other slots, so that it acts on the full kets. The number of terms therefore grows as $\prod_k (2s_k + 1)$.
- All spins must have a declared numeric size. A site without indexes is given as `S[]`, as for a single impurity spin with the operators `S[1]`, ..., `S[5]`. Site 0 must be given as `S -> {0}`, since `S[0]` evaluates to 0. Sites with several indexes are given as `S -> {{1, "a"}, ...}`.
- Apply the rules to an expanded Hamiltonian and compute its matrices with `makematricesbzvc` in a basis from `QSZaddspinket` (spins coupled to fermions) or with `makematricesbzop` in `spinbasis` (pure spin models; `spinclustermatrices` is faster for those).
- An invalid site specification or a spin without a declared numeric size gives a message and returns `$Failed`.

## Examples
### Basic Examples
A single spin 1/2 without site index:
```wl
snegspinoperators[{S, 1/2}];
spinketbrarules[{S[]}]
```

### Scope
Two sites; the operators include the identity of the other slot:
```wl
snegspinoperators[{S, 1/2}];
spinketbrarules[{S -> {1, 2}}][[3]]
```

### Applications
Kondo model of an impurity spin coupled to a single conduction-band level, $H = J\, \mathbf S \cdot \mathbf s$. At single occupancy of the level ($Q = 0$) the spectrum consists of the singlet $-3J/4$ and the triplet $J/4$:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
snegrealconstants[J];
HK = J (nc[S[3], spinz[c[]]] + (nc[S[4], spinminus[c[]]] + nc[S[5], spinplus[c[]]])/2);
bz = QSZaddspinket[qszbasisvc[{c[]}], {1/2}];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[Expand[HK /. spinketbrarules[{S[]}]], bz]
```
---
Two impurity spins coupled to each other and one of them in a magnetic field:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
snegrealconstants[B];
bz = QSZaddspinket[qszbasisvc[{c[]}], {1/2, 1/2}];
h = Expand[(spinspin[S[1], S[2]] + B S[1, 3]) /. spinketbrarules[{S[1], S[2]}]];
{First[#], Eigenvalues[Last[#]]} & /@ Select[makematricesbzvc[h, bz], First[First[#]] == -1 &]
```

### Properties & Relations
The rules reproduce the spin matrices:
```wl
snegspinoperators[{T, 1}];
matrixrepresentationop[T[1, 4] /. spinketbrarules[{T[1]}], spinket[1]] == spinmatrixP[1]
```

### Possible Issues
Only spin operators can be listed:
```wl
snegfermionoperators[c];
spinketbrarules[{c[1]}]
```

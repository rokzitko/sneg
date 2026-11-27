---
keywords: basis with impurity spin, spin ket, Kondo model, charge and spin projection, NRG Ljubljana, tensor product basis
see also: spinketbrarules, spinbasis, qszbasisvc, makematricesbzvc, spinket
---
# QSZaddspinket

## Usage
- `QSZaddspinket[bvc, s]` adds a spin *s* to the basis *bvc* with the quantum numbers $(Q, S_z)$, giving the tensor-product basis with the states `vc[..., ket[m]]`.
- `QSZaddspinket[bvc, {s1, s2, ...}]` adds several spins, with the states `vc[..., ket[m1, m2, ...]]`.

## Details
- *bvc* is a basis in the occupation-number representation with subspaces labeled `{Q, Sz}`, as given by `qszbasisvc`. The result has the subspaces `{Q, Sz + m}` with $m$ the total projection of the added spins, so that $S_z$ is the projection of the total spin (fermions and impurity spins). The order of the subspaces follows from `mergebasis`.
- The spins occupy the slots of the kets in the given order; operators on them are obtained with `spinketbrarules`, applied to an expression in abstract spin operators, or with `spinketbraX`, ... with position arrays.
- The key of each subspace must be of the form `{Q, Sz}`; the last element is taken as $S_z$. For bases with other quantum numbers the result is not meaningful.
- NRG Ljubljana uses this construction for the initial Hamiltonian of quantum impurity models with impurity spins, e.g. the Kondo model.

## Examples
### Basic Examples
A spin 1/2 added to the basis of one fermion level:
```wl
snegfermionoperators[c];
QSZaddspinket[qszbasisvc[{c[]}], 1/2]
```

### Scope
Two spins 1/2; the total dimension is $4 \times 2 \times 2$:
```wl
snegfermionoperators[c];
bz = QSZaddspinket[qszbasisvc[{c[]}], {1/2, 1/2}];
{First /@ bz, Total[Length[Last[#]] & /@ bz]}
```

### Applications
Kondo model with one conduction-band level, $H = J\, \mathbf S \cdot \mathbf s$. The total spin projection is a good quantum number; at $Q = 0$ the singlet $-3J/4$ and the triplet $J/4$ appear:
```wl
snegspinoperators[{S, 1/2}];
snegfermionoperators[c];
snegrealconstants[J];
HK = J (nc[S[3], spinz[c[]]] + (nc[S[4], spinminus[c[]]] + nc[S[5], spinplus[c[]]])/2);
bz = QSZaddspinket[qszbasisvc[{c[]}], 1/2];
{First[#], Eigenvalues[Last[#]]} & /@ makematricesbzvc[Expand[HK /. spinketbrarules[{S[]}]], bz]
```
---
Spin-1 impurity (underscreened Kondo model) coupled to one level: the eigenvalues at $Q = 0$ are $-J$ (total spin 1/2) and $J/2$ (total spin 3/2):
```wl
snegspinoperators[{T, 1}];
snegfermionoperators[c];
snegrealconstants[J];
HK = J (nc[T[3], spinz[c[]]] + (nc[T[4], spinminus[c[]]] + nc[T[5], spinplus[c[]]])/2);
bz = QSZaddspinket[qszbasisvc[{c[]}], 1];
Union[Flatten[Eigenvalues /@ Last /@ Select[makematricesbzvc[Expand[HK /. spinketbrarules[{T[]}]], bz], First[First[#]] == 0 &]]]
```

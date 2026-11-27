---
keywords: tensor product, ket-bra operators, composite system, spin operators, phonon operators, Null placeholder
see also: ket, bra, basistensorproduct, spinbasis, spinketbraZ, phononnumber
---
# ketbratensorproduct

## Usage
- `ketbratensorproduct[x1, x2]` gives the tensor product of the expressions *x1* and *x2* built from kets and bras, $X_1 \otimes X_2$.
- `ketbratensorproduct[x1, x2, x3, ...]` gives the tensor product of several factors.
- `ketbratensorproduct[x]` is *x*.

## Details
- The kets and bras of *x1* are padded on the right with `Null` entries for the slots of *x2*, and those of *x2* on the left with `Null` entries for the slots of *x1*; the two expressions are then multiplied with `nc`, which combines kets and bras with complementary `Null` patterns. The number of slots of each factor is the maximal number of entries of its kets and bras.
- Typical uses are operators on composite systems built from operators on the factors, such as $S^z \otimes 1$, or the operators of one phonon mode in a multi-mode phonon space (`phononnumber[i, {N1, N2, ...}]` is implemented this way).
- Each factor should be an operator (sums of ket-bra products) or a state on its own subsystem; the identity of a subsystem must be written explicitly (e.g. `spinketbraI[s]`).

## Examples
### Basic Examples
```wl
ketbratensorproduct[ket[1], ket[2]]
```
```wl
ketbratensorproduct[nc[ket[a], bra[b]], nc[ket[c], bra[d]]]
```

### Scope
Three factors:
```wl
ketbratensorproduct[ket[a], ket[b], ket[c]]
```
---
$S^z \otimes 1$ for two spins 1/2:
```wl
ketbratensorproduct[spinketbraZ[1/2], spinketbraI[1/2]]
```

### Applications
Heisenberg coupling of two spins 1/2, $J\, \mathbf S_1 \cdot \mathbf S_2 = J \sum_a S^a \otimes S^a$, in the product basis. The eigenvalues are $-3J/4$ (singlet) and $J/4$ (triplet):
```wl
snegrealconstants[J];
H = J Expand[Total[MapThread[ketbratensorproduct, {spinketbraXYZ[1/2], spinketbraXYZ[1/2]}]]];
states = Flatten[Last /@ spinbasis[{1/2, 1/2}]];
Eigenvalues[matrixrepresentationop[H, states]]
```
---
Phonon number operator of the second of two modes with at most one phonon each:
```wl
ketbratensorproduct[phononid[1], phononnumber[1]]
```

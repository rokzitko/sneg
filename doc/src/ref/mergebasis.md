---
keywords: basis, invariant subspaces, merge subspaces, quantum numbers, symmetry reduction
see also: applybasis, basistensorproduct, directproductbasis, appendquantumnumber, dropemptysubspaces, keymerge, bzQ, qszbasisvc
---
# mergebasis

## Usage
- `mergebasis[{{qn1, states1}, {qn2, states2}, ...}]` combines the states of all elements with equal quantum numbers into one subspace.

## Details
- The result is a list `{{qn, states}, ...}` with one element for each distinct quantum number `qn`, sorted in canonical order. The states of elements with the same `qn` are joined in the order of their appearance.
- The quantum numbers are compared with `SameQ`; they are usually lists `{Q, Sz}`, ..., but can be any expressions.
- Typical uses: removing a quantum number from a basis (e.g. obtaining the $Q$ basis from the $(Q, S_z)$ basis), replacing quantum numbers by functions of them (e.g. charge parity $(-1)^Q$), and combining the products of subsystem bases with additive quantum numbers.
- `keymerge` is an equivalent function.

## Examples
### Basic Examples
```wl
mergebasis[{{{0}, {a1}}, {{1}, {b1}}, {{0}, {a2, a3}}}]
```

### Scope
The quantum numbers are sorted:
```wl
mergebasis[{{{1, 1/2}, {x}}, {{-1, 0}, {y}}, {{1, -1/2}, {z}}}]
```

### Applications
Dropping $S_z$ from the $(Q, S_z)$ basis of a level gives the charge basis:
```wl
snegfermionoperators[d];
mergebasis[{{First[#[[1]]]}, #[[2]]} & /@ qszbasisvc[{d[]}]] // InputForm
```
---
Fermion parity $P = (-1)^N$ of two levels, $N = Q + 2$:
```wl
snegfermionoperators[c];
{First[#], Length[Last[#]]} & /@ mergebasis[{{(-1)^(#[[1, 1]] + 2)}, #[[2]]} & /@ qbasisvc[{c[1], c[2]}]]
```

### Properties & Relations
`basistensorproduct` uses `mergebasis` to combine products of subsystem states with equal quantum numbers:
```wl
snegfermionoperators[c];
basistensorproduct[qbasis[{c[1]}], qbasis[{c[2]}], Plus]
```

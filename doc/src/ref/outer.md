---
keywords: outer product, direct product, tensor product, noncommutative product, basis states
see also: inner, VMV, mcross, nc, Outer
---
# outer

## Usage
- `outer[a, b]` gives the outer product of the lists *a* and *b*, the matrix of all products `nc[ai, bj]`.
- `outer[a, b, n]` treats only the first *n* levels of *a* and *b* as lists, as in `Outer`.

## Details
- `outer[a, b, ...]` is `Outer[nc, a, b, ...]`. The factors keep their order, as needed for operators.
- `Flatten[outer[a, b]]` gives all products of the elements of *a* and *b*; this is used to form product bases, e.g. of fermion states and phonon or spin kets.

## Examples
### Basic Examples
```wl
outer[{a1, a2}, {b1, b2}]
```

### Applications
Single-particle density matrix operators $c^\dagger_{i\uparrow} c_{j\uparrow}$ of two sites:
```wl
snegfermionoperators[c];
outer[{c[CR, 1, UP], c[CR, 2, UP]}, {c[AN, 1, UP], c[AN, 2, UP]}]
```
---
Product states of the spin-up states of two levels and the phonon states with at most one quantum:
```wl
snegfermionoperators[c];
Flatten[outer[{c[CR, 1, UP], c[CR, 2, UP]}, {ket[0], ket[1]}]]
```

### Properties & Relations
The trace of the outer product is the inner product:
```wl
Tr[outer[{a1, a2}, {b1, b2}]] == inner[{a1, a2}, {b1, b2}]
```

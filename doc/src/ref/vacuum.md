---
keywords: vacuum state, empty state, occupation number representation, Fock vacuum
see also: vc, makebasis, ap, VACUUM, ops2vc, vc2ops
---
# vacuum

## Usage
- `vacuum[]` gives the vacuum state $|0\rangle$ in the occupation number representation, `vc[0, 0, ..., 0]`, for the single-particle basis `BASIS` defined by `makebasis`.

## Details
- The length of the vector is the number of single-particle states in `BASIS`: twice the number of spin-1/2 levels.
- `vacuum[]` is evaluated with the current `BASIS`; call it after `makebasis` or after a basis generator such as `qszbasisvc`, which calls `makebasis`.
- `ap[1, VACUUM]` and `ap[op, VACUUM]` also give `vacuum[]` and `ap[op, vacuum[]]`: the symbol `VACUUM` (the vacuum in the creation operator representation) is replaced by `vacuum[]` when an operator is applied with `ap`.
- If `BASIS` is empty, `vacuum[]` is `vc[]`.
- The old usage message refers to the help page `vacuumlc`; the function is `vacuum`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2]}];
vacuum[] // InputForm
```
Many-particle states are obtained by applying creation operators to the vacuum:
```wl
ap[nc[c[CR, 1, UP], c[CR, 2, DO]], vacuum[]] // InputForm
```

### Properties & Relations
`vacuum[]` corresponds to the identity operator in the creation operator representation:
```wl
snegfermionoperators[c];
makebasis[{c[1]}];
vc2ops[vacuum[]]
```
---
Annihilation operators give zero, and the vacuum is normalized:
```wl
{ap[c[AN, 1, UP], vacuum[]], scalarproductvc[vacuum[], vacuum[]]}
```
---
`ap[1, VACUUM]` is the vacuum vector:
```wl
ap[1, VACUUM] === vacuum[]
```

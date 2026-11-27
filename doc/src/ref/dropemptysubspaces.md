---
keywords: basis, invariant subspaces, empty subspaces
see also: transformbasis, transformtoLR, transformQStoIS, mergebasis, bzQ
---
# dropemptysubspaces

## Usage
- `dropemptysubspaces[bz]` removes the subspaces without states from the basis *bz*.

## Details
- *bz* is a list `{{qn, states}, ...}`; elements with `states === {}` are removed. The order of the remaining subspaces is preserved.
- Projections and symmetry transformations can leave subspaces empty. `transformbasis`, `transformtoLR`, `transformtoLRvc`, and `transformQStoIS` apply `dropemptysubspaces` to their results.

## Examples
### Basic Examples
```wl
dropemptysubspaces[{{{0}, {}}, {{1}, {a, b}}, {{2}, {}}}]
```

### Applications
Removing the states with a doubly occupied level from a basis leaves some subspaces empty:
```wl
snegfermionoperators[d];
bz = qszbasisvc[{d[]}];
bz2 = Map[{First[#], DeleteCases[Last[#], vc[1, 1]]} &, bz]
```
```wl
dropemptysubspaces[bz2]
```

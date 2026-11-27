---
keywords: declared lattice indexes, registry
see also: sneglatticeindexes, listmomentumindexes, listlatticevectors, listfreeindexes, latticeindexQ
---
# listlatticeindexes

## Usage
- `listlatticeindexes` is the list of the symbols declared as lattice indexes.

## Details
- Symbols are added by `sneglatticeindexes` and by the automatic renaming of dummy lattice indexes in products of sums. The list is sorted.
- The list is for inspection; the index properties are stored with the symbols, so removing a symbol from the list does not undo its declaration.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R, Rp}, Ns];
listlatticeindexes
```

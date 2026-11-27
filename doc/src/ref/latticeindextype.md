---
keywords: index type, lattice index, momentum index
see also: latticeindexQ, momentumindexQ, sneglatticeindexes, snegmomentumindexes, latticesize
---
# latticeindextype

## Usage
- `latticeindextype[x]` gives `"Lattice"` for a lattice index, `"Momentum"` for a momentum index, and `None` for all other expressions.

## Details
- The type is set by `sneglatticeindexes` and `snegmomentumindexes` as an upvalue of the index symbol, and copied to indexes created by the automatic renaming of dummy indexes.
- Lattice vectors (`sneglatticevectors`) have the type `None`, although they count as lattice quantities in Kronecker deltas and shifts.

## Examples
### Basic Examples
```wl
sneglatticeindexes[{R}, Ns];
snegmomentumindexes[{k}, Ns];
sneglatticevectors[a1];
snegfreeindexes[i];
latticeindextype /@ {R, k, a1, i}
```

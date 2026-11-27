---
keywords: declared momentum indexes, registry
see also: snegmomentumindexes, listlatticeindexes, listlatticevectors, listfreeindexes, momentumindexQ
---
# listmomentumindexes

## Usage
- `listmomentumindexes` is the list of the symbols declared as momentum indexes.

## Details
- Symbols are added by `snegmomentumindexes` and by the automatic renaming of dummy momentum indexes in products of sums. The list is sorted.
- The list is for inspection; the index properties are stored with the symbols.

## Examples
### Basic Examples
```wl
snegfermionoperators[ck];
snegmomentumindexes[{k, q}, Ns];
listmomentumindexes
```
The product of two sums creates the momentum index `k1`:
```wl
nc[sum[ck[CR, k, UP], {k}], sum[ck[AN, k, UP], {k}]];
listmomentumindexes
```

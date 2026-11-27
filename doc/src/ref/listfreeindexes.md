---
keywords: declared indexes, summation indexes, registry
see also: snegfreeindexes, listlatticeindexes, listmomentumindexes, sum, sumAutoRename
---
# listfreeindexes

## Usage
- `listfreeindexes` is the list of all symbols declared as summation indexes.

## Details
- Symbols are added by `snegfreeindexes`, `sneglatticeindexes`, `snegmomentumindexes`, `snegspincomponentindexes`, and by the automatic renaming of dummy indexes in products of sums (`k1`, `k2`, ...). The list is sorted.
- The list is for inspection; the index properties are stored with the symbols.

## Examples
### Basic Examples
```wl
snegfreeindexes[k, sigma];
sneglatticeindexes[{R}, Ns];
listfreeindexes
```
Automatic renaming declares new indexes:
```wl
snegfermionoperators[c];
nc[sum[c[CR, k, UP], {k}], sum[c[AN, k, UP], {k}]];
listfreeindexes
```

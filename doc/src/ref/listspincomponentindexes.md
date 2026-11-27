---
keywords: spin component index, declared indexes
see also: snegspincomponentindexes, spincomponentindexQ, listfreeindexes
---
# listspincomponentindexes

## Usage
- `listspincomponentindexes` is the list of the symbols declared as spin component indexes with `snegspincomponentindexes`.

## Details
- The list is empty when the package is loaded and is extended by each declaration; reloading the package keeps the existing list.
- The spin component indexes are also summation indexes and appear in `listfreeindexes`.

## Examples
### Basic Examples
```wl
snegspincomponentindexes[a, b];
listspincomponentindexes
```

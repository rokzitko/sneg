---
keywords: declared symbols, list of fermion operators, declaration
see also: snegfermionoperators, snegspinlessfermionoperators, fermionQ, listbosonoperators
---
# listfermionoperators

## Usage
- `listfermionoperators` is the list of the symbols declared as fermion operators.

## Details
- Symbols are added by `snegfermionoperators` and `snegspinlessfermionoperators`. The list is sorted and contains each symbol once.
- `listfermionoperators` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
snegspinlessfermionoperators[f];
listfermionoperators
```

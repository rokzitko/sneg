---
keywords: declared symbols, list of Majorana operators, declaration
see also: snegmajoranaoperators, majoranaQ, listfermionoperators
---
# listmajoranaoperators

## Usage
- `listmajoranaoperators` is the list of the symbols declared as Majorana operators.

## Details
- Symbols are added by `snegmajoranaoperators`. The list is sorted and contains each symbol once.
- `listmajoranaoperators` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
snegmajoranaoperators[g, h];
listmajoranaoperators
```

---
keywords: declared symbols, list of Grassmann numbers, declaration
see also: sneggrassmanconstants, grassmanQ
---
# listgrassmanconstants

## Usage
- `listgrassmanconstants` is the list of the symbols declared as Grassmann numbers.

## Details
- Symbols are added by `sneggrassmanconstants`. The list is sorted and contains each symbol once.
- `listgrassmanconstants` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
sneggrassmanconstants[x, y];
listgrassmanconstants
```

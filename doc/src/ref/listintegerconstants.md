---
keywords: declared symbols, list of integer constants, declaration
see also: snegintegerconstants, listrealconstants, isnumericQ
---
# listintegerconstants

## Usage
- `listintegerconstants` is the list of the symbols declared as integer constants.

## Details
- Symbols are added by `snegintegerconstants`. The list is sorted and contains each symbol once.
- `listintegerconstants` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.
- The lists of constants and `listfreeindexes` are used by `maskOp` to replace the declared constants temporarily by plain symbols.

## Examples
### Basic Examples
```wl
snegintegerconstants[n, m];
listintegerconstants
```

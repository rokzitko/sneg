---
keywords: declared symbols, list of complex constants, declaration
see also: snegcomplexconstants, listrealconstants, isnumericQ
---
# listcomplexconstants

## Usage
- `listcomplexconstants` is the list of the symbols declared as complex constants.

## Details
- Symbols are added by `snegcomplexconstants`. The list is sorted and contains each symbol once.
- `listcomplexconstants` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.
- The lists of constants and `listfreeindexes` are used by `maskOp` to replace the declared constants temporarily by plain symbols.

## Examples
### Basic Examples
```wl
snegcomplexconstants[z, w];
listcomplexconstants
```

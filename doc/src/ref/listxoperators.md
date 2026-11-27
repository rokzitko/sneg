---
keywords: declared symbols, list of Hubbard X-operators, declaration
see also: snegxoperators, xoperatorQ
---
# listxoperators

## Usage
- `listxoperators` is the list of the symbols declared as Hubbard X-operators.

## Details
- Symbols are added by `snegxoperators`. The list is sorted and contains each symbol once.
- `listxoperators` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
snegxoperators[X, "HubbardAtom"];
listxoperators
```

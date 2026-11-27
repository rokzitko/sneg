---
keywords: declared symbols, list of boson operators, declaration
see also: snegbosonoperators, bosonQ, listfermionoperators
---
# listbosonoperators

## Usage
- `listbosonoperators` is the list of the symbols declared as boson operators.

## Details
- Symbols are added by `snegbosonoperators`. The list is sorted and contains each symbol once.
- `listbosonoperators` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
snegbosonoperators[a, b];
listbosonoperators
```

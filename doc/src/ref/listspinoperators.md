---
keywords: declared symbols, list of spin operators, declaration
see also: snegspinoperators, spinQ, listfermionoperators
---
# listspinoperators

## Usage
- `listspinoperators` is the list of the symbols declared as spin operators.

## Details
- Symbols are added by `snegspinoperators`. The list is sorted and contains each symbol once.
- `listspinoperators` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.

## Examples
### Basic Examples
```wl
snegspinoperators[{S, 1/2}, T];
MemberQ[listspinoperators, #] & /@ {S, T}
```

### Possible Issues
The list also contains the internal symbol `Sneg`Private`snegstevensS`, which is declared when SNEG is loaded (it is used by `stevens`):
```wl
listspinoperators
```

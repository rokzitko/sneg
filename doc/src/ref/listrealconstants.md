---
keywords: declared symbols, list of real constants, declaration
see also: snegrealconstants, listcomplexconstants, listintegerconstants, isnumericQ
---
# listrealconstants

## Usage
- `listrealconstants` is the list of the symbols declared as real constants.

## Details
- Symbols are added by `snegrealconstants`, and the symbols in symbolic spin sizes (`snegspinoperators`) and lattice sizes (`sneglatticeindexes`), which are declared as real constants automatically. The list is sorted and contains each symbol once.
- `listrealconstants` is a global variable. It is initialized to `{}` when SNEG is loaded, unless it already has a value; it is not used to decide the type of an operator or constant (see the type predicates and `isnumericQ`), so changing it does not change any declaration.
- The lists of constants and `listfreeindexes` are used by `maskOp` to replace the declared constants temporarily by plain symbols.

## Examples
### Basic Examples
```wl
snegrealconstants[t, U];
MemberQ[listrealconstants, #] & /@ {t, U}
```

### Possible Issues
The list also contains the internal symbol `Sneg`Private`snegstevenss`, which is declared when SNEG is loaded (it is used by `stevens`):
```wl
listrealconstants
```

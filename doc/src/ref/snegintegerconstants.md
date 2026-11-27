---
keywords: integer constant, parameter, declaration
see also: snegrealconstants, snegfreeindexes, isnumericQ, listintegerconstants
---
# snegintegerconstants

## Usage
- `snegintegerconstants[n, m, ...]` declares *n*, *m*, ... as integer constants.

## Details
- Integer constants are scalars (see `isnumericQ`) and invariant under complex conjugation, like real constants (`snegrealconstants`).
- The declaration sets the upvalues `snegnonopQ[n] = True` and `Conjugate[n] = n`, and adds *n* to `listintegerconstants`.
- The integer property is not used in simplifications: no assumption `n ∈ Integers` is made. Use `Assumptions -> Element[n, Integers]` where needed.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegintegerconstants[n];
{nc[c[AN, 1, UP], n c[CR, 1, UP]], Conjugate[n], listintegerconstants}
```

### Possible Issues
The constant is not assumed to be an integer:
```wl
snegintegerconstants[n];
{Simplify[Sin[n Pi]], Simplify[Sin[n Pi], Element[n, Integers]]}
```

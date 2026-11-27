---
keywords: boson test, type predicate, declared operator
see also: operatorQ, fermionQ, majoranaQ, spinQ, snegbosonoperators
---
# bosonQ

## Usage
- `bosonQ[a]` gives `True` if *a* is a boson operator symbol or a boson operator `a[...]`.

## Details
- `bosonQ` is `True` for symbols declared with `snegbosonoperators` and for their applications `a[t, j, ...]`.
- For other expressions, `bosonQ` remains unevaluated; use `TrueQ[bosonQ[x]]` in conditions.

## Examples
### Basic Examples
```wl
snegbosonoperators[a];
{bosonQ[a], bosonQ[a[CR, 1]], TrueQ[bosonQ[u]]}
```

### Scope
Select the boson operators of an expression:
```wl
snegbosonoperators[a];
snegfermionoperators[d];
Union[Cases[nc[a[CR], d[CR, UP], d[AN, UP], a[AN]], _?bosonQ, Infinity]]
```

---
keywords: operator test, type predicate, declared operator
see also: fermionQ, bosonQ, majoranaQ, spinQ, xoperatorQ, grassmanQ, isnumericQ, snegfermionoperators
---
# operatorQ

## Usage
- `operatorQ[op]` gives `True` if *op* is a declared operator symbol or an operator `op[...]` of a declared family.

## Details
- `operatorQ` is `True` for symbols declared with `snegfermionoperators`, `snegspinlessfermionoperators`, `snegbosonoperators`, `snegmajoranaoperators`, `snegspinoperators`, and `snegxoperators`, and for their applications `op[j, ...]`.
- For other expressions, `operatorQ` remains unevaluated (it is not `False`), except for `operatorQ[nc]`, which is `False`. Use `TrueQ[operatorQ[x]]` in conditions.
- `operatorQ` tests a single operator; products, sums, and Grassmann numbers are not operators in this sense.
- The declaration functions set `operatorQ[op]` as an upvalue of `op`; for a new operator type, set `op /: operatorQ[op] = True`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegbosonoperators[a];
{operatorQ[c], operatorQ[c[CR, 1, UP]], operatorQ[a[AN]]}
```

### Possible Issues
For undeclared symbols and composite expressions, `operatorQ` remains unevaluated; use `TrueQ`:
```wl
snegfermionoperators[c];
{operatorQ[u], operatorQ[nc[c[CR, 1, UP], c[AN, 1, UP]]], TrueQ[operatorQ[u]]}
```

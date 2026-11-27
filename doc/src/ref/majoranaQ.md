---
keywords: Majorana test, type predicate, declared operator
see also: operatorQ, fermionQ, snegmajoranaoperators
---
# majoranaQ

## Usage
- `majoranaQ[g]` gives `True` if *g* is a Majorana operator symbol or a Majorana operator `g[...]`.

## Details
- `majoranaQ` is `True` for symbols declared with `snegmajoranaoperators` and for their applications `g[j, ...]`.
- For other expressions, `majoranaQ` remains unevaluated; use `TrueQ[majoranaQ[x]]` in conditions. Majorana operators are not `fermionQ`.

## Examples
### Basic Examples
```wl
snegmajoranaoperators[g];
snegfermionoperators[c];
{majoranaQ[g], majoranaQ[g[1]], TrueQ[majoranaQ[c]]}
```

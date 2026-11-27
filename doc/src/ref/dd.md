---
keywords: normal-ordered product, double dots, Wick's theorem, normal ordering
see also: wick, normalorder, normalorderwick, vev, vevwick, contraction
---
# dd

## Usage
- `dd[expr]` represents the normal-ordered product ${:}\mathit{expr}{:}$.

## Details
- `dd` is a formal placeholder for normal-ordered products. It is returned by `wick`; the operators inside are kept in the order produced by `nc`. To compute an explicit normal-ordered operator, use `normalorder`, `normalorderwick`, or `wick`.
- The normal order refers to the vacuum set by `ordering` (empty state or Fermi sea).
- By definition, `vev[dd[x]]` and `vevwick[dd[x]]` are 0.
- `dd` is linear: it distributes over sums and takes numbers and constants out. Nested `dd` are flattened, and `dd[]` is 1.
- In output, `dd[x]` is shown with green double dots, ${:}x{:}$.

## Examples
### Basic Examples
`wick` writes products in terms of normal-ordered products and contractions:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
wick[nc[c[AN, k1, UP], c[CR, k2, UP]]]
```
The expectation value of a normal-ordered product vanishes:
```wl
vev[dd[nc[c[CR, k1, UP], c[AN, k2, UP]]]]
```

### Properties & Relations
`dd` is linear:
```wl
snegfermionoperators[c];
snegrealconstants[t];
dd[t nc[c[CR, 1, UP], c[AN, 2, UP]] + 2 c[CR, 1, UP]]
```
---
Dropping the normal-ordered terms of the expansion by `wick` gives the expectation value:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
wick[nc[c[AN, k1, UP], c[CR, k2, UP]]] /. dd[_] -> 0
```
```wl
vevwick[nc[c[AN, k1, UP], c[CR, k2, UP]]]
```

---
keywords: normal ordering, Wick's theorem, vacuum expectation value, Fermi sea
see also: normalorder, vevwick, wick, vev, dd, ordering
---
# normalorderwick

## Usage
- `normalorderwick[expr]` gives `expr - vevwick[expr]`, the operator *expr* with its vacuum expectation value, computed by Wick's theorem, subtracted.

## Details
- `normalorderwick` is `normalorder` with `vevwick` instead of `vev`. It is needed for the Fermi sea (`ordering[c] = SEA`), where `vev` does not evaluate products of more than two operators with symbolic indexes.
- For a product of two operators, the result is the normal-ordered product ${:}A B{:} = A B - \langle A B \rangle$. For longer products, $x - \langle x \rangle$ differs from ${:}x{:}$ by the terms with partial contractions; `wick` gives the full expansion.
- `normalorderwick` is `Listable`.

## Examples
### Basic Examples
A density-density product in the Fermi sea with its expectation value subtracted:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
x = nc[c[CR, k1, UP], c[AN, k1, UP], c[CR, k2, DO], c[AN, k2, DO]];
normalorderwick[x]
```
```wl
vevwick[%]
```

### Properties & Relations
`normalorder` uses `vev`, which leaves the expectation value of the four-operator product unevaluated:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k1, k2];
x = nc[c[CR, k1, UP], c[AN, k1, UP], c[CR, k2, DO], c[AN, k2, DO]];
normalorder[x]
```
---
For bilinears, `normalorderwick` and `normalorder` agree:
```wl
snegfermionoperators[c];
ordering[c] = SEA;
snegfreeindexes[k];
normalorderwick[nc[c[AN, k, UP], c[CR, k, UP]]] == normalorder[nc[c[AN, k, UP], c[CR, k, UP]]]
```

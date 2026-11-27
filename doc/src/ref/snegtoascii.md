---
keywords: ASCII, string, text representation, export, import, operator string
see also: asciitosneg, snegtex, nc, ket, bra
---
# snegtoascii

## Usage
- `snegtoascii[expr]` converts the operators, bras, kets, and operator products in *expr* to a compact ASCII notation.

## Details
- An operator `c[CR, i, j, ...]` is written as `"c+(i,j,...)"` and `c[AN, i, j, ...]` as `"c(i,j,...)"`. The indexes are converted with `ToString`, so the spin indexes `UP` and `DO` appear as 1 and 0. An operator without indexes is written as `"c+()"` or `"c()"`.
- `ket[i, j]` is written as `"|i,j>"` and `bra[i, j]` as `"<i,j|"`. Products `nc[...]` of converted factors are concatenated into one string.
- Sums and coefficients are not converted: the result is an expression that contains one string for every operator product.
- Factors that cannot be converted, such as undeclared symbols, are kept, and the product remains an `nc` of strings and expressions.
- `asciitosneg` performs the inverse conversion.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegtoascii[nc[c[CR, 1, UP], c[AN, 2, DO]]]
```
Back to the operator expression:
```wl
asciitosneg[%]
```

### Scope
Operators acting on kets:
```wl
snegfermionoperators[c];
snegtoascii[nc[c[CR, 1, UP], c[CR, 2, DO], ket[0]]]
```
---
Expressions with coefficients and several terms keep their structure:
```wl
snegfermionoperators[c];
snegrealconstants[t];
snegtoascii[t nc[c[CR, 1, UP], c[AN, 2, UP]] - c[CR, 3, UP]]
```
---
Boson operators without indexes:
```wl
snegbosonoperators[a];
snegtoascii[nc[a[CR], a[AN]]]
```

### Properties & Relations
The conversion can be inverted:
```wl
snegfermionoperators[c];
x = 2 nc[c[CR, 1, UP], c[CR, 2, DO], c[AN, 3, UP]] - c[CR, 3, UP];
asciitosneg[snegtoascii[x]] == x
```

### Possible Issues
Undeclared symbols are not converted:
```wl
snegfermionoperators[c];
snegtoascii[nc[c[CR, 1, UP], u]]
```

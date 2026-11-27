---
keywords: ASCII, string, parsing, text input, operator string
see also: snegtoascii, snegtex, nc, ket, bra
---
# asciitosneg

## Usage
- `asciitosneg[string]` parses a string in the compact ASCII operator notation and returns the corresponding operator product.
- `asciitosneg[expr]` converts all strings in the expression *expr*.

## Details
- In the string, `"c+(i,j,...)"` denotes the creation operator `c[CR, i, j, ...]` and `"c(i,j,...)"` the annihilation operator `c[AN, i, j, ...]`; *c* must be a declared operator. The indexes are separated by commas and converted with `ToExpression`, so the spin can be given as `UP`/`DO` or 1/0, and symbolic indexes are allowed. `"c+()"` and `"c()"` are operators without indexes.
- `"|i,j>"` denotes `ket[i, j]` and `"<i,j|"` denotes `bra[i, j]`.
- Juxtaposed factors are multiplied with `nc`, so the result is brought to the canonical order.
- Coefficients and sums are not parsed inside the string; give them outside, e.g. `2 "c+(1,1)" + "c+(2,1)"`.
- `snegtoascii` performs the inverse conversion.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
asciitosneg["c+(1,UP)c(2,DO)"]
```
---
A product that is not in normal order is reordered:
```wl
snegfermionoperators[c];
asciitosneg["c(1,1)c+(1,1)"]
```

### Scope
States with kets:
```wl
snegfermionoperators[c];
asciitosneg["c+(1,1)c+(1,0)|0>"]
```
---
Boson operators without indexes:
```wl
snegbosonoperators[a];
asciitosneg["a()a()a+()a+()"]
```
---
Strings inside expressions:
```wl
snegfermionoperators[c];
asciitosneg[{2 "c+(1,1)" + "c+(2,1)", "c+(k,1)"}]
```
---
Bras and kets are multiplied with `braketrule`, by default orthonormal:
```wl
asciitosneg[{"<1||1>", "<1||2>"}]
```

### Properties & Relations
`snegtoascii` is the inverse conversion:
```wl
snegfermionoperators[c];
snegtoascii[asciitosneg["c+(1,1)c+(2,0)c(3,1)"]]
```

### Possible Issues
Coefficients inside the string are not parsed:
```wl
snegfermionoperators[c];
asciitosneg["2 c+(1,1)"]
```
---
Operators that are not declared are not converted:
```wl
asciitosneg["x+(1)"]
```

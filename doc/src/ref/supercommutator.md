---
keywords: nested commutator, iterated commutator
see also: superkomutator, komutator, superanticommutator
---
# supercommutator

## Usage
- `supercommutator[a, b, n]` gives the *n*-fold nested commutator $[a, [a, \ldots [a, b] \ldots]]$; it is a synonym of `superkomutator`.

## Details
- `supercommutator[args]` evaluates `superkomutator[args]`. See `superkomutator` for details.

## Examples
### Basic Examples
Nested commutators with the number operator, $[n, c] = -c$:
```wl
snegfermionoperators[c];
Table[supercommutator[number[c[]], c[AN, UP], k], {k, 0, 3}]
```

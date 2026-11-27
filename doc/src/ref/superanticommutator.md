---
keywords: nested anticommutator, iterated anticommutator
see also: superantikomutator, antikomutator, supercommutator
---
# superanticommutator

## Usage
- `superanticommutator[a, b, n]` gives the *n*-fold nested anticommutator $\{a, \{a, \ldots \{a, b\} \ldots\}\}$; it is a synonym of `superantikomutator`.

## Details
- `superanticommutator[args]` evaluates `superantikomutator[args]`. See `superantikomutator` for details.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
Table[superanticommutator[c[AN, UP], c[CR, UP], k], {k, 0, 3}]
```

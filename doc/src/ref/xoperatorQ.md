---
keywords: Hubbard X-operator test, type predicate, declared operator
see also: snegxoperators, operatorQ, listxoperators
---
# xoperatorQ

## Usage
- `xoperatorQ[X]` gives `True` if *X* is a symbol declared with `snegxoperators` or an X-operator `X[...]`, and `False` otherwise.

## Details
- Unlike the other type predicates (`fermionQ`, `spinQ`, ...), `xoperatorQ` gives `False` for all other expressions.

## Examples
### Basic Examples
```wl
snegxoperators[X, "HubbardAtom"];
{xoperatorQ[X], xoperatorQ[X[1, "0", "UP"]], xoperatorQ[u]}
```

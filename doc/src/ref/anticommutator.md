---
keywords: anticommutator, canonical anticommutation relations
see also: antikomutator, commutator, superanticommutator, nc
---
# anticommutator

## Usage
- `anticommutator[a, b]` gives the anticommutator $\{a, b\} = a b + b a$; it is a synonym of `antikomutator`.

## Details
- `anticommutator[args]` evaluates `antikomutator[args]`. See `antikomutator` for details.

## Examples
### Basic Examples
Canonical anticommutation relations:
```wl
snegfermionoperators[c];
{anticommutator[c[AN, 1, UP], c[CR, 1, UP]], anticommutator[c[AN, 1, UP], c[CR, 1, DO]]}
```

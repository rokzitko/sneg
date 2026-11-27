---
keywords: commutator, Lie bracket
see also: komutator, anticommutator, supercommutator, nc
---
# commutator

## Usage
- `commutator[a, b]` gives the commutator $[a, b] = a b - b a$; it is a synonym of `komutator`.

## Details
- `commutator[args]` evaluates `komutator[args]`. See `komutator` for details.

## Examples
### Basic Examples
Commutator of the number operator with a creation operator, $[n, c^\dagger] = c^\dagger$:
```wl
snegfermionoperators[c];
Expand[commutator[number[c[1]], c[CR, 1, UP]]]
```
---
Canonical commutation relation of bosons:
```wl
snegbosonoperators[a];
commutator[a[AN], a[CR]]
```

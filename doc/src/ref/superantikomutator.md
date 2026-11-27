---
keywords: nested anticommutator, iterated anticommutator
see also: superkomutator, antikomutator, superanticommutator
---
# superantikomutator

## Usage
- `superantikomutator[a, b, n]` gives the *n*-fold nested anticommutator $\{a, \{a, \ldots \{a, b\} \ldots\}\}$.

## Details
- `superantikomutator[a, b, 0]` is *b*, and `superantikomutator[a, b, n]` is `antikomutator[a, superantikomutator[a, b, n - 1]]`. `superanticommutator` is a synonym.
- `Simplify` is applied to the result of every step.
- *n* must be a non-negative integer; otherwise the expression is returned unevaluated.
- See `superkomutator` for nested commutators.

## Examples
### Basic Examples
Nested anticommutators of $c_\uparrow$ with $c^\dagger_\uparrow$, $\{c, c^\dagger\} = 1$ and $\{c, 1\} = 2c$:
```wl
snegfermionoperators[c];
Table[superantikomutator[c[AN, UP], c[CR, UP], n], {n, 0, 3}]
```

### Scope
Anticommuting Majorana operators:
```wl
snegmajoranaoperators[γ];
Table[superantikomutator[γ[1], γ[2], n], {n, 0, 2}]
```
---
Spin-1/2 operators, $\{S^x, S^x\} = 1/2$ and $\{S^x, 1/2\} = S^x$:
```wl
snegspinoperators[{S, 1/2}];
Table[superantikomutator[S[1, 1], S[1, 1], n], {n, 0, 3}]
```

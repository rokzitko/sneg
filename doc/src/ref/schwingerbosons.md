---
keywords: Schwinger bosons, spin representation, bosonic spinons, large-N, SU(2)
see also: holsteinprimakoff, abrikosov, jordanwigner, snegbosonoperators
---
# schwingerbosons

## Usage
- `schwingerbosons[expr, S -> {a, b}]` replaces the spin operators `S` in *expr* by the Schwinger-boson representation with the bosons `a` and `b`.

## Details
- The representation is
  $$S^+_j = a^\dagger_j b_j, \qquad S^-_j = b^\dagger_j a_j, \qquad S^z_j = \frac12 (a^\dagger_j a_j - b^\dagger_j b_j),$$
  with $S^x = (S^+ + S^-)/2$, $S^y = (S^+ - S^-)/(2i)$. The bosons of the site `S[j...]` are `a[j...]` and `b[j...]`.
- The representation reproduces the su(2) algebra for any boson state; the spin size is fixed by the constraint $a^\dagger_j a_j + b^\dagger_j b_j = 2s$, which is not imposed by `schwingerbosons`. Then $\mathbf S_j^2 = s(s+1)$.
- The spin can be declared with or without a size. `a` and `b` must be boson operators, otherwise the message `schwingerbosons::op` is issued and `$Failed` is returned.
- The result is expanded and normal ordered.

## Examples
### Basic Examples
```wl
snegspinoperators[S];
snegbosonoperators[a, b];
schwingerbosons[{S[1, 3], S[1, 4], S[1, 5]}, S -> {a, b}]
```

### Applications
The Heisenberg exchange in terms of Schwinger bosons:
```wl
snegspinoperators[S];
snegbosonoperators[a, b];
schwingerbosons[spinspin[S[1], S[2]], S -> {a, b}]
```

### Properties & Relations
The algebra is preserved:
```wl
snegspinoperators[S];
snegbosonoperators[a, b];
sb = schwingerbosons[#, S -> {a, b}] &;
Expand[{komutator[sb[S[1, 4]], sb[S[1, 5]]] - 2 sb[S[1, 3]], komutator[sb[S[1, 1]], sb[S[1, 2]]] - I sb[S[1, 3]]}]
```
---
$\mathbf S^2 = \frac{N}{2} (\frac{N}{2} + 1)$ with the total boson number $N = n_a + n_b$:
```wl
snegspinoperators[S];
snegbosonoperators[a, b];
nn = number[a[1]] + number[b[1]];
Expand[schwingerbosons[nc[S[1, 3], S[1, 3]] + (nc[S[1, 4], S[1, 5]] + nc[S[1, 5], S[1, 4]])/2, S -> {a, b}] - nc[nn/2, nn/2 + 1]]
```

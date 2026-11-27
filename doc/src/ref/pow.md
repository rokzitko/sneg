---
keywords: power, operator power, noncommutative power, matrix power
see also: nc, snegSeries, superkomutator, MatrixPower
---
# pow

## Usage
- `pow[a, n]` gives the *n*-th power $a^n = a \cdot a \cdots a$ of the operator expression *a*, computed with the noncommutative product `nc`.

## Details
- `pow[a, 0]` is 1, `pow[a, 1]` is *a*, and `pow[a, n]` is `nc[pow[a, n - 1], a]` for integers $n > 1$.
- `Power` (`a^n`) is the commutative power and gives wrong results for operators; use `pow`.
- For negative *n*, the message `pow::negativepower` is issued and `Null` is returned. For non-integer *n*, `pow` is returned unevaluated.
- The result is not expanded; use `Expand` to collect the terms.
- `pow` is `Listable`: for a list *a*, the powers of the elements are returned.

## Examples
### Basic Examples
$(c + c^\dagger)^2 = 1$ for a fermion:
```wl
snegfermionoperators[c];
pow[c[AN, UP] + c[CR, UP], 2]
```
---
$(a + a^\dagger)^2$ for a boson:
```wl
snegbosonoperators[a];
pow[a[AN] + a[CR], 2]
```

### Scope
Occupation numbers of fermions are idempotent, $n_\sigma^k = n_\sigma$:
```wl
snegfermionoperators[c];
Table[pow[number[c[], UP], k], {k, 0, 3}]
```
---
Squares of spin operators. For a spin-1/2 operator, $(S^x)^2 = 1/4$; for spin 1, $\mathbf S^2 = 2$:
```wl
snegspinoperators[{S, 1/2}, {T, 1}];
{pow[S[1, 1], 2], Expand[pow[T[1, 1], 2] + pow[T[1, 2], 2] + pow[T[1, 3], 2]]}
```
---
Constants are taken out:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
pow[λ a[CR], 3]
```

### Applications
The square of the charge fluctuation $(n - 1)^2$ of a fermion level is the particle-hole symmetric Hubbard interaction `isozsq`:
```wl
snegfermionoperators[d];
Expand[pow[number[d[]] - 1, 2] - isozsq[d[]]]
```
---
Local moment: $(n_\uparrow - n_\downarrow)^2 = n - 2 n_\uparrow n_\downarrow$, which is 1 for single occupancy and 0 otherwise:
```wl
snegfermionoperators[d];
Expand[pow[number[d[], UP] - number[d[], DO], 2] - (number[d[]] - 2 hubbard[d[]])]
```
---
Harmonic oscillator: $\langle 0| x^4 |0\rangle = 3/4$ for $x = (a + a^\dagger)/\sqrt{2}$:
```wl
snegbosonoperators[a];
vevwick[pow[(a[AN] + a[CR])/Sqrt[2], 4]]
```

### Properties & Relations
`pow[a, n]` is the product of *n* factors:
```wl
snegfermionoperators[c];
x = c[CR, 1, UP] + c[AN, 2, UP];
pow[x, 3] == nc[x, x, x]
```
---
`snegSeries` expands functions of operators in powers computed by `pow`:
```wl
snegbosonoperators[a];
snegSeries[Exp, a[CR], 3]
```

### Possible Issues
`Power` is commutative and treats operators as numbers:
```wl
snegfermionoperators[c];
{(c[CR, UP] + c[AN, UP])^2, pow[c[CR, UP] + c[AN, UP], 2]}
```
---
Negative powers are not defined:
```wl
snegbosonoperators[a];
pow[a[CR], -1]
```
---
For a list, `pow` gives the powers of the elements, not the power of a matrix:
```wl
snegbosonoperators[a];
pow[{a[CR], a[AN]}, 2]
```

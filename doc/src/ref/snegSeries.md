---
keywords: power series, Taylor series, operator function, exponential of an operator, unitary transformation, displacement operator
see also: pow, superkomutator, Series, nc
---
# snegSeries

## Usage
- `snegSeries[f, x, n]` gives the Taylor series of $f(x)$ around $x = 0$ to order *n*, with the powers of the operator expression *x* computed with `nc`.
- `snegSeries[f[x], n]` is `snegSeries[f, x, n]`.
- `snegSeries[f, x]` and `snegSeries[f[x]]` use the order 5.

## Details
- The result is $\sum_{m=0}^{n} \frac{f^{(m)}(0)}{m!} x^m$, where $x^m$ is the noncommutative power. *f* is a function such as `Exp` or `Sin`, or a pure function such as `Sqrt[1 + #] &`.
- The coefficients are computed with `Series`. The powers are computed by repeated `nc` multiplication, with `Simplify` applied at every step.
- The series is truncated at order *n*; products of truncated series contain terms of higher order that are incomplete.
- `snegSeries[Exp[x], n]` and `snegSeries[E^x, n]` give the series of the exponential.

## Examples
### Basic Examples
The exponential of a boson creation operator:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
snegSeries[Exp, λ a[CR], 3]
```

### Scope
Functions given by their head, and pure functions:
```wl
snegbosonoperators[a];
{snegSeries[Sin[a[CR]], 3], snegSeries[Sqrt[1 + #] &, nc[a[CR], a[AN]], 2]}
```
---
For $x^2 = 1$, the exponential series gives $e^{i\theta x} = \cos\theta + i x \sin\theta$; here $x = c + c^\dagger$:
```wl
snegfermionoperators[c];
snegrealconstants[θ];
Expand[snegSeries[Exp, I θ (c[AN, UP] + c[CR, UP]), 4]]
```
---
For an idempotent operator, $n^2 = n$, the series gives $f(n) = f(0) + (f(1) - f(0))\, n$ in the limit $n \to \infty$; to third order for $\ln(1 + n)$:
```wl
snegfermionoperators[c];
snegSeries[Log[1 + #] &, number[c[], UP], 3]
```

### Applications
Displacement of a harmonic oscillator, $e^{S} a e^{-S} = a - \lambda$ for $S = \lambda (a^\dagger - a)$. The product of the truncated series is correct up to the order of the truncation:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
S = λ (a[CR] - a[AN]);
r = Expand[nc[snegSeries[Exp, S, 3], a[AN], snegSeries[Exp, -S, 3]]];
r /. λ^n_ /; n > 3 -> 0
```
---
Overlap of the unnormalized coherent state $e^{\lambda a^\dagger}|0\rangle$ with the one-boson state, $\langle 0| a\, e^{\lambda a^\dagger} |0\rangle = \lambda$:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
vevwick[nc[a[AN], snegSeries[Exp, λ a[CR], 4]]]
```

### Properties & Relations
The terms are the powers computed by `pow`:
```wl
snegbosonoperators[a];
snegSeries[Exp, a[CR] + a[AN], 2] == 1 + (a[CR] + a[AN]) + pow[a[CR] + a[AN], 2]/2
```
---
The Hadamard expansion with `superkomutator` is an alternative for $e^{S} B e^{-S}$:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
Sum[superkomutator[λ (a[CR] - a[AN]), a[AN], n]/n!, {n, 0, 3}]
```

### Possible Issues
Products of truncated series contain incomplete terms of higher order:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
S = λ (a[CR] - a[AN]);
Collect[Expand[nc[snegSeries[Exp, S, 1], a[AN], snegSeries[Exp, -S, 1]]], λ]
```

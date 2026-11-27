---
keywords: nested commutator, iterated commutator, Baker-Campbell-Hausdorff, Hadamard lemma, unitary transformation, Heisenberg equation of motion
see also: komutator, superantikomutator, supercommutator, liouvillian, snegSeries, pow
---
# superkomutator

## Usage
- `superkomutator[a, b, n]` gives the *n*-fold nested commutator $[a, [a, \ldots [a, b] \ldots]]$.

## Details
- `superkomutator[a, b, 0]` is *b*, and `superkomutator[a, b, n]` is `komutator[a, superkomutator[a, b, n - 1]]`. `supercommutator` is a synonym.
- `Simplify` is applied to the result of every step.
- *n* must be a non-negative integer; otherwise the expression is returned unevaluated.
- The nested commutators are the terms of the Hadamard (Baker-Campbell-Hausdorff) expansion of a unitary transformation,
$$e^{A} B e^{-A} = \sum_{n=0}^{\infty} \frac{1}{n!} [A, [A, \ldots [A, B]]]_n .$$
With $A = i H t$, they give the Taylor expansion of the Heisenberg operator $B(t) = e^{iHt} B e^{-iHt}$. `liouvillian` gives the list of commutators $[B, H], [[B, H], H], \ldots$ with the opposite order of the arguments.

## Examples
### Basic Examples
Nested commutators of the spin projection $S^z$ with $c^\dagger_\uparrow$, $[S^z, c^\dagger_\uparrow] = c^\dagger_\uparrow/2$:
```wl
snegfermionoperators[c];
Table[superkomutator[spinz[c[]], c[CR, UP], n], {n, 0, 3}]
```

### Scope
The spin component $S^x$ flips the spin of the operator:
```wl
snegfermionoperators[c];
Table[superkomutator[spinx[c[]], c[CR, UP], n], {n, 0, 4}]
```
---
Spin operators, $[S^z, S^x] = i S^y$, $[S^z, i S^y] = S^x$:
```wl
snegspinoperators[{S, 1/2}];
Table[superkomutator[S[1, 3], S[1, 1], n], {n, 0, 3}]
```

### Applications
Displacement of a harmonic oscillator. For $A = \lambda (a^\dagger - a)$ the expansion terminates, and $e^{A} a e^{-A} = a - \lambda$:
```wl
snegbosonoperators[a];
snegrealconstants[λ];
A = λ (a[CR] - a[AN]);
Table[superkomutator[A, a[AN], n], {n, 0, 3}]
```
---
Rotation of the spin quantization axis: $e^{i\theta S^z} c^\dagger_\uparrow e^{-i\theta S^z} = e^{i\theta/2} c^\dagger_\uparrow$. The series to fifth order:
```wl
snegfermionoperators[c];
snegrealconstants[θ];
Sum[(I θ)^n/n! superkomutator[spinz[c[]], c[CR, UP], n], {n, 0, 5}]
```
```wl
Series[Exp[I θ/2], {θ, 0, 5}]
```
---
Time evolution of the annihilation operator of a harmonic oscillator, $H = \omega a^\dagger a$: $[iHt, a] = -i \omega t\, a$, so that $a(t) = e^{-i\omega t} a$:
```wl
snegbosonoperators[a];
snegrealconstants[ω, t];
Sum[superkomutator[I ω t number[a[]], a[AN], n]/n!, {n, 0, 4}]
```

### Properties & Relations
The first nested commutator is the commutator:
```wl
snegfermionoperators[c];
superkomutator[number[c[]], c[AN, UP], 1] == Expand[komutator[number[c[]], c[AN, UP]]]
```
---
`superkomutator` and `liouvillian` iterate in opposite orders; for $[H, B]$ versus $[B, H]$ the terms differ by $(-1)^n$:
```wl
snegbosonoperators[a];
snegrealconstants[ω];
{Table[superkomutator[ω number[a[]], a[AN], n], {n, 0, 3}], liouvillian[a[AN], ω number[a[]], 3]}
```

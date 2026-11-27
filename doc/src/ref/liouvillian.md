---
keywords: Liouvillian, nested commutator, Heisenberg equation, moments, operator dynamics
see also: komutator, eomclosure, spectralmoments, greensfunction, eigenoperators
---
# liouvillian

## Usage
- `liouvillian[A, H, n]` returns the list starting with *A* and ending with the *n*th nested commutator with *H* on the right.

## Details
- With $\mathcal L A=[A,H]$, the list is $\{A,\mathcal L A,\ldots,\mathcal L^n A\}$ after algebraic simplification. The order *n* is a nonnegative integer. For order zero the result is a one-element list.
- The sign is appropriate to $i\,dA/dt=[A,H]$ for a time-independent Schrödinger-picture operator and $\hbar=1$. The Liouvillian is an ordinary commutator even for fermions. The parity-dependent bracket enters only when taking a Green's function or moment.
- Unlike `eomclosure`, this operation can retain symbolic momentum sums. It computes only the requested orders and does not require an exactly closed hierarchy.

## Examples
### Basic Examples
A free annihilator has Liouvillian eigenvalue $e$ and its adjoint has eigenvalue $-e$:
```wl
snegfermionoperators[d];
snegrealconstants[e];
h = e number[d[]];
{liouvillian[d[AN, UP], h, 2], liouvillian[d[CR, UP], h, 1]}
```

### Applications
The first commutator of an impurity operator separates its local energy from the hybridization with a bath:
```wl
snegfermionoperators[c, d];
snegfreeindexes[k];
snegrealconstants[ed];
snegrealfunctions[eps, v];
h = ed number[d[]] + sum[eps[k] number[c[k]] + v[k] hop[c[k], d[]], {k}];
Last[liouvillian[d[AN, UP], h, 1]]
```

### Properties & Relations
Total particle number is conserved by hopping even though the site occupations separately are not:
```wl
snegfermionoperators[c];
liouvillian[number[c[1]] + number[c[2]], hop[c[1], c[2]], 2]
```

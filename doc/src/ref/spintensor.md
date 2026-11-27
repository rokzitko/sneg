---
keywords: spherical tensor operator, irreducible tensor operator, Wigner-Eckart theorem, multipole, quadrupole operator, spin tensor
see also: stevens, spincomponents, snegspinoperators, spinclustermatrices, ClebschGordan
---
# spintensor

## Usage
- `spintensor[k, q, S[j]]` gives the spherical tensor operator $T^k_q$ of rank *k* and component *q* built from the spin `S[j]`.
- `spintensor[k, q, S -> j]` is the same; this form also allows the site `0`.

## Details
- The operators are defined by
  $$T^k_k = (-S^+/\sqrt{2})^k, \qquad T^k_{q-1} = \frac{[S^-, T^k_q]}{\sqrt{(k+q)(k-q+1)}},$$
  so that they satisfy the defining commutation relations of a spherical tensor, $[S^z, T^k_q] = q\, T^k_q$ and $[S^\pm, T^k_q] = \sqrt{k(k+1) - q(q \pm 1)}\, T^k_{q \pm 1}$.
- In particular $T^1_0 = S^z$, $T^1_{\pm1} = \mp S^\pm/\sqrt2$, and $T^2_0 = (3 (S^z)^2 - \mathbf S^2)/\sqrt6$.
- The results are expanded polynomials in $S^z$ and $S^\pm$. For spins with a declared size, products are reduced to the normal form, and operators with $k > 2s$ vanish. For spins without a declared size, $S^+ S^-$ products remain.
- *k* and *q* must be integers with $k \ge 0$ and $|q| \le k$; otherwise the message `spintensor::range` is issued and `$Failed` is returned. The spin must be an operator declared with `snegspinoperators`.
- `stevens` gives the Stevens operators, the Hermitian combinations of $T^k_{\pm q}$ used for crystal fields.
- Results are cached for each spin symbol and site.

## Examples
### Basic Examples
The rank-1 tensor:
```wl
snegspinoperators[S];
Table[spintensor[1, q, S[1]], {q, -1, 1}]
```
The quadrupole operator $T^2_0$ for a spin without declared size and for spin 1:
```wl
snegspinoperators[{T, 1}];
{spintensor[2, 0, S[1]], spintensor[2, 0, T[1]]}
```

### Scope
Site given as a rule, including site 0:
```wl
snegspinoperators[S];
spintensor[1, 0, S -> 0]
```
---
Operators with $k > 2s$ vanish:
```wl
snegspinoperators[{S, 1/2}];
{spintensor[1, 1, S[1]], spintensor[2, 1, S[1]]}
```

### Applications
Matrix of the quadrupole operator $T^2_0$ of a spin 3/2 in the basis $m = 3/2, \ldots, -3/2$. It is proportional to $3m^2 - 15/4$, in agreement with the Wigner-Eckart theorem:
```wl
snegspinoperators[{Q, 3/2}];
spinclustermatrices[spintensor[2, 0, Q[1]], {Q[1]}, "Blocks" -> False] // MatrixForm
```

### Properties & Relations
The tensor commutation relations for rank 3:
```wl
snegspinoperators[{Q, 3/2}];
Expand[Table[{komutator[Q[1, 3], spintensor[3, q, Q[1]]] - q spintensor[3, q, Q[1]], If[q < 3, komutator[Q[1, 4], spintensor[3, q, Q[1]]] - Sqrt[12 - q (q + 1)] spintensor[3, q + 1, Q[1]], 0]}, {q, -3, 3}]]
```
---
Hermitian conjugation, $(T^k_q)^\dagger = (-1)^q T^k_{-q}$:
```wl
snegspinoperators[{T, 1}];
Expand[conj[spintensor[2, 1, T[1]]] + spintensor[2, -1, T[1]]]
```

### Possible Issues
The rank and component must satisfy $|q| \le k$:
```wl
snegspinoperators[S];
spintensor[1, 2, S[1]]
```

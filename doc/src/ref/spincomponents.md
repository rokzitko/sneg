---
keywords: spin components, ladder operators, Cartesian components, S plus, S minus, spin algebra, normal form, su(2)
see also: snegspinoperators, snegspincomponentindexes, levicivita, spinspin, rotatespin, komutator
---
# spincomponents

## Usage
- `spincomponents[expr]` rewrites all spin operators in *expr* in terms of $S^z$ and $S^\pm$.
- `spincomponents[expr, "XYZ"]` rewrites them in terms of $S^x$, $S^y$, $S^z$.

## Details
- A spin operator `S[j..., t]` declared with `snegspinoperators` carries the component *t* as its last index: 1, 2, 3, 4, 5 for $S^x, S^y, S^z, S^+, S^-$ (the constants `SPININDEXx`, ..., `SPININDEXm`).
- The default form `"ZPM"` replaces $S^x = (S^+ + S^-)/2$ and $S^y = (S^+ - S^-)/(2i)$; the form `"XYZ"` replaces $S^\pm = S^x \pm i S^y$. The result is expanded, and products are brought to the canonical order by `nc`.
- Products of spin operators are ordered automatically, but Cartesian and ladder components are not converted into each other. Expressions that mix them, such as commutators of $\mathbf S_1 \cdot \mathbf S_2$ (written with $S^x, S^y, S^z$) with $S^+$, therefore need `spincomponents` to reach a unique form in which vanishing expressions become 0.
- For spins with a declared numeric size $s > 1/2$, same-site products are reduced to the normal form in $S^z$ and $S^\pm$, so the `"XYZ"` form is stable only for $s = 1/2$ and for spins without a declared size.
- Sums `sum[..., {a, ...}]` over spin component indexes declared with `snegspincomponentindexes` are expanded into the three terms $a = x, y, z$; other summation indexes are kept.
- Any other value of the second argument gives the message `spincomponents::basis` and returns `$Failed`.

## Examples
### Basic Examples
```wl
snegspinoperators[S];
spincomponents[S[1, 1] + S[1, 2]]
```
```wl
spincomponents[S[1, 4], "XYZ"]
```

### Scope
The Heisenberg exchange in terms of $S^z$ and $S^\pm$, $\mathbf S_1 \cdot \mathbf S_2 = S^z_1 S^z_2 + \frac12 (S^+_1 S^-_2 + S^-_1 S^+_2)$:
```wl
snegspinoperators[S];
spincomponents[spinspin[S[1], S[2]]]
```
---
Same-site products for a spin without declared size and for spin 1/2:
```wl
snegspinoperators[G, {S, 1/2}];
{spincomponents[nc[G[1, 1], G[1, 2]]], spincomponents[nc[S[1, 1], S[1, 2]]]}
```
---
Sums over symbolic Cartesian components are expanded:
```wl
snegspinoperators[S];
snegspincomponentindexes[a];
spincomponents[sum[nc[S[1, a], S[2, a]], {a}]]
```

### Applications
The total spin is conserved by the Heisenberg exchange. The commutator with $S^+_\mathrm{tot}$ vanishes only after rewriting in a single set of components:
```wl
snegspinoperators[G];
x = Expand[komutator[spinspin[G[1], G[2]], G[1, 4] + G[2, 4]]]
```
```wl
spincomponents[x]
```
---
Dzyaloshinskii-Moriya interaction $D (\mathbf S_1 \times \mathbf S_2)_z = D (S^x_1 S^y_2 - S^y_1 S^x_2)$ written with the Levi-Civita symbol:
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
spincomponents[sum[levicivita[a, b, 3] nc[S[1, a], S[2, b]], {a, b}]]
```

### Properties & Relations
The two forms are equivalent:
```wl
snegspinoperators[S];
x = spinspin[S[1], S[2]];
spincomponents[spincomponents[x], "XYZ"] - Expand[x]
```

### Possible Issues
For spins with a declared size above 1/2, Cartesian products on the same site are reduced to $S^z$ and $S^\pm$, also in the `"XYZ"` form:
```wl
snegspinoperators[{T, 1}];
spincomponents[nc[T[1, 1], T[1, 1]], "XYZ"]
```
---
Only `"ZPM"` and `"XYZ"` are allowed:
```wl
snegspinoperators[S];
spincomponents[S[1, 1], "PM"]
```

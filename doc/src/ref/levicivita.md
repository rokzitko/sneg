---
keywords: Levi-Civita symbol, antisymmetric tensor, epsilon tensor, cross product, structure constants of su(2)
see also: snegspincomponentindexes, spincomponents, LeviCivitaTensor, Signature
---
# levicivita

## Usage
- `levicivita[a, b, c]` is the totally antisymmetric Levi-Civita symbol $\varepsilon_{abc}$ with indexes 1, 2, 3 and $\varepsilon_{123} = 1$.

## Details
- For integer arguments, `levicivita[a, b, c]` is `Signature[{a, b, c}]` if all arguments are in $\{1, 2, 3\}$, and 0 otherwise.
- For symbolic arguments, the symbol vanishes if two arguments are equal, and the arguments are sorted into canonical order with the sign of the permutation.
- `levicivita` is real (`Conjugate[levicivita[a, b, c]]` is `levicivita[a, b, c]`) and has the attribute `NumericFunction`, so it is treated as a scalar in operator products.
- The su(2) algebra is $[S^a, S^b] = i \sum_c \varepsilon_{abc} S^c$; with symbolic spin component indexes (`snegspincomponentindexes`), commutators of spin operators are returned in terms of `levicivita`.

## Examples
### Basic Examples
```wl
{levicivita[1, 2, 3], levicivita[2, 1, 3], levicivita[1, 1, 3], levicivita[1, 2, 4]}
```
Symbolic indexes:
```wl
{levicivita[a, b, c], levicivita[b, a, c], levicivita[a, b, a]}
```

### Applications
The commutator of spin operators with symbolic components:
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
komutator[S[1, a], S[1, b]]
```
---
Cross product of two spins, $(\mathbf S_1 \times \mathbf S_2)_z = \sum_{ab} \varepsilon_{abz} S^a_1 S^b_2$:
```wl
snegspinoperators[S];
snegspincomponentindexes[a, b];
spincomponents[sum[levicivita[a, b, 3] nc[S[1, a], S[2, b]], {a, b}], "XYZ"]
```

### Properties & Relations
Agreement with `LeviCivitaTensor`:
```wl
Table[levicivita[i, j, k], {i, 3}, {j, 3}, {k, 3}] == Normal[LeviCivitaTensor[3]]
```

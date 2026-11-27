---
keywords: spinless fermion, declaration, spin-polarized fermions, Kitaev chain, Jordan-Wigner, canonical anticommutation relations
see also: snegfermionoperators, snegbosonoperators, snegmajoranaoperators, fermionQ, spinof, number, hop, jordanwigner, quadraticform
---
# snegspinlessfermionoperators

## Usage
- `snegspinlessfermionoperators[f, g, ...]` declares *f*, *g*, ... as fermion operators with spin 0.

## Details
- `snegspinlessfermionoperators[f]` is equivalent to `snegfermionoperators[{f, 0}]`: the operators are fermions with canonical anticommutation relations and `spinof[f]` = 0.
- For spin 0, the spin index takes the single value 0. The functions that construct operators from a level, such as `number`, `hop`, `basis`, `qszbasisvc`, `quadraticform`, and `jordanwigner`, therefore write the operators as `f[CR, j, ..., 0]` and `f[AN, j, ..., 0]`, with the spin index 0 as the last index.
- In output, the spin index 0 is not shown: `f[CR, 1, 0]` is displayed as $f^\dagger_1$.
- Operators can also be written without the spin index, e.g. `f[CR, 1]`; they obey the same algebra, but `f[CR, 1]` and `f[CR, 1, 0]` are different operators. Use one convention consistently; with the functions above, use the trailing 0.
- See `snegfermionoperators` for the general properties of fermion operators.

## Examples
### Basic Examples
```wl
snegspinlessfermionoperators[f];
{spinof[f], number[f[1]], hop[f[1], f[2]]}
```
Anticommutation relations:
```wl
{antikomutator[f[AN, 1, 0], f[CR, 1, 0]], nc[f[CR, 1, 0], f[CR, 1, 0]]}
```

### Scope
The basis of a spinless level has two states:
```wl
snegspinlessfermionoperators[f];
basis[f[]]
```

### Applications
The Kitaev chain with three sites, $H = \sum_j [-t(f_j^\dagger f_{j+1} + \mathrm{h.c.}) + \Delta(f^\dagger_j f^\dagger_{j+1} + f_{j+1} f_j)] - \mu \sum_j n_j$; at $t = \Delta$, $\mu = 0$ the spectrum of Bogoliubov quasiparticles has a zero mode:
```wl
snegspinlessfermionoperators[f];
snegrealconstants[t, mu, Dl];
H = Sum[-t hop[f[j], f[j + 1]] + Dl (nc[f[CR, j, 0], f[CR, j + 1, 0]] + nc[f[AN, j + 1, 0], f[AN, j, 0]]), {j, 2}] - mu Sum[number[f[j]], {j, 3}];
First[bogoliubov[H /. {t -> 1, Dl -> 1, mu -> 0}, {f[1], f[2], f[3]}]]
```

### Possible Issues
An operator without the spin index differs from the operator with the spin index 0, although both are displayed in the same way:
```wl
snegspinlessfermionoperators[f];
{f[CR, 1], f[CR, 1, 0], antikomutator[f[AN, 1], f[CR, 1, 0]]}
```

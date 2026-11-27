---
keywords: LaTeX, TeX, export, typesetting, formula, publication
see also: TeXForm, snegtoascii, nc, sum
---
# snegtex

## Usage
- `snegtex[expr]` gives a string with the LaTeX form of the operator expression *expr*.

## Details
- Fermion operators are written as $c^\dagger_{i\uparrow}$ (`c^\dagger_{i\uparrow}`) and $c_{i\downarrow}$: the indexes after `CR`/`AN` are separated by commas, and the spin index of spin-1/2 operators is appended as `\uparrow` or `\downarrow`. Other spin values are converted with `TeXForm`; spinless fermions have no spin index.
- Boson operators: $a^\dagger_{q}$. Majorana operators: $\gamma_{i}$. Spin operators `S[j, t]`: $S^{x}_{j}$, $S^{y}_{j}$, $S^{z}_{j}$, $S^{+}_{j}$, $S^{-}_{j}$ for $t = 1, \ldots, 5$. X-operators `X[j, a, b]`: $X^{ab}_{j}$.
- Operator names of one letter are set as math italics; longer names are set with `\mathrm`, e.g. `\mathrm{ph}^\dagger`.
- Products `nc[a, b, ...]` are written as juxtaposed factors separated by spaces; factors that are sums are enclosed in `\left(...\right)`. Symbolic sums `sum[a, {k, ...}]` are written as `\sum_{k,...}`.
- The rest of the expression (coefficients, fractions, functions) is converted with `TeXForm`, in which the operator products are atomic. Symbols with names of several letters appear as `\text{...}`, and Greek letters as LaTeX commands such as `\Delta` or `\xi`.
- The result is a `String`; use it in a LaTeX math environment.

## Developer Notes
- `snegtex` replaces every operator and operator product by a placeholder symbol, converts the expression with `TeXForm`, and substitutes the LaTeX strings of the operators afterwards. New operator types can be supported by definitions for the internal function `snegtexop`.

## Examples
### Basic Examples
The Hubbard dimer:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
snegtex[-t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]])]
```
---
Single operators:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
{snegtex[c[CR, 1, UP]], snegtex[c[AN, k, 2, DO]], snegtex[c[CR, σ]]}
```

### Scope
Boson operators, the Holstein coupling $g (a^\dagger + a) n$:
```wl
snegbosonoperators[a];
snegfermionoperators[d];
snegrealconstants[ω, g];
snegtex[ω number[a[]] + g nc[a[CR] + a[AN], number[d[]]]]
```
---
Spin operators, the Heisenberg exchange $J \mathbf S_1 \cdot \mathbf S_2$:
```wl
snegspinoperators[{S, 1/2}];
snegrealconstants[J];
snegtex[J (nc[S[1, 3], S[2, 3]] + (nc[S[1, 4], S[2, 5]] + nc[S[1, 5], S[2, 4]])/2)]
```
---
Symbolic sums:
```wl
snegfermionoperators[c];
snegfreeindexes[k, σ];
snegrealfunctions[ε];
snegtex[sum[ε[k] number[c[k], σ], {k, σ}]]
```
---
Spinless fermions, Majorana operators, and X-operators:
```wl
snegspinlessfermionoperators[f];
snegmajoranaoperators[gam];
snegxoperators[X, "HubbardAtom"];
{snegtex[nc[f[CR, 1], f[AN, 2]]], snegtex[nc[gam[1], gam[2]]], snegtex[X[1, "UP", "DO"]]}
```
---
Operators with multi-letter names:
```wl
snegbosonoperators[ph];
snegtex[ph[CR, 1]]
```

### Applications
The BCS mean-field Hamiltonian:
```wl
snegfermionoperators[c];
snegfreeindexes[k];
snegrealconstants[Δ];
snegrealfunctions[ξ];
H = sum[ξ[k] number[c[k]] - Δ (nc[c[CR, k, UP], c[CR, -k, DO]] + nc[c[AN, -k, DO], c[AN, k, UP]]), {k}];
snegtex[H]
```

### Possible Issues
Coefficients are placed according to the canonical order of `TeXForm`; they may appear after the operators, and signs can be attached to the operator products:
```wl
snegfermionoperators[c];
snegrealconstants[U, Δ];
{snegtex[U hubbard[c[]]], snegtex[Δ nc[c[CR, UP], c[CR, DO]]]}
```
---
Symbols with multi-letter names are set as text:
```wl
snegfermionoperators[c];
snegrealconstants[eps];
snegtex[eps number[c[]]]
```

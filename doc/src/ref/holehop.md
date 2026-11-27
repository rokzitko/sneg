---
keywords: hole operators, particle-hole transformation, hopping, hybridization with holes
see also: hop, anomaloushop, anhop, conj
---
# holehop

## Usage
- `holehop[h[i], p[j]]` gives the hopping term $\sum_\sigma (c^\dagger_{i\sigma} p_{j\sigma} + \mathrm{h.c.})$ written with the hole operators $h_{i\bar\sigma} = c^\dagger_{i\sigma}$ of the level `h[i]`: $\sum_\sigma (h_{i\bar\sigma}\, p_{j\sigma} + p^\dagger_{j\sigma} h^\dagger_{i\bar\sigma})$.
- `holehop[h[i], p[j], σ]` gives the term for the particle spin *σ* only.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes. The first argument is the level described by hole operators, the second the level described by particle operators.
- The particle-hole transformation used is $c^\dagger_{i\sigma} \to h_{i\bar\sigma}$, $c_{i\sigma} \to h^\dagger_{i\bar\sigma}$, without sign factors: creating an electron with spin $\sigma$ annihilates a hole with spin $\bar\sigma$. With this transformation, `hop[c[i], p[j]]` becomes `holehop[h[i], p[j]]`.
- Since the transformation has no spin-dependent sign, the hole operators do not transform as a spinor under spin rotations, and `holehop` does not commute with the conventional total spin operators $S^x$, $S^y$ built from `h` and `p`; it commutes with $S^z$. (The transformation $c^\dagger_{\uparrow} \to h_\downarrow$, $c^\dagger_\downarrow \to -h_\uparrow$ would preserve the SU(2) structure.)
- `holehop[h[i], p[j]]` equals `anomaloushop[p[j], h[i]]`.
- `holehop` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[h, f];
holehop[h[], f[]]
```
For the particle spin up, $h_\downarrow f_\uparrow + f^\dagger_\uparrow h^\dagger_\downarrow$:
```wl
holehop[h[], f[], UP]
```

### Properties & Relations
The hopping `hop[c[], f[]]` after the particle-hole transformation $c^\dagger_\sigma \to h_{\bar\sigma}$:
```wl
snegfermionoperators[c, h, f];
Expand[(hop[c[], f[]] /. {c[CR, s_] :> h[AN, 1 - s], c[AN, s_] :> h[CR, 1 - s]}) - holehop[h[], f[]]]
```
---
Relation with `anomaloushop`:
```wl
holehop[h[], f[]] == anomaloushop[f[], h[]]
```
---
`holehop` conserves the total $S^z$ but not $S^x$ and $S^y$ of the conventionally defined spin operators:
```wl
{Sx, Sy, Sz, S2} = manyspin[{h[], f[]}];
Expand[{komutator[holehop[h[], f[]], Sx], komutator[holehop[h[], f[]], Sz]}]
```

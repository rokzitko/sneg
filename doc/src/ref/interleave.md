---
keywords: interleave lists, riffle, spin up and down, occupation number vector
see also: Riffle, Transpose, qszbasisvc, makebasis
---
# interleave

## Usage
- `interleave[{a1, a2, ...}, {b1, b2, ...}, ...]` gives `{a1, b1, ..., a2, b2, ..., ...}`.

## Details
- `interleave[l1, l2, ...]` is `Flatten[Transpose[{l1, l2, ...}], 1]`. The lists must have equal lengths.
- In the occupation number vectors `vc[...]`, the spin-up and spin-down states of each level are adjacent: `vc[n1↑, n1↓, n2↑, n2↓, ...]`. `qszbasisvc` uses `interleave` to combine the spin-up and spin-down occupations.

## Examples
### Basic Examples
```wl
interleave[{a1, a2, a3}, {b1, b2, b3}]
```
```wl
interleave[{a1, a2}, {b1, b2}, {c1, c2}]
```

### Applications
Occupation number vector of a Néel state on four sites, from the occupations of the spin-up and spin-down orbitals:
```wl
snegfermionoperators[c];
makebasis[{c[1], c[2], c[3], c[4]}];
neel = vc @@ interleave[{1, 0, 1, 0}, {0, 1, 0, 1}]
```
```wl
expvvc[spinz /@ {c[1], c[2], c[3], c[4]}, neel]
```

### Properties & Relations
For two lists, `interleave` is equivalent to `Riffle`:
```wl
interleave[{a1, a2, a3}, {b1, b2, b3}] === Riffle[{a1, a2, a3}, {b1, b2, b3}]
```

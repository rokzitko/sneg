---
keywords: direct product, product basis, composite system, quantum numbers
see also: basistensorproduct, quickDBL, quickDBLSZ, mergebasis, direct
---
# directproductbasis

## Usage
- `directproductbasis[bz1, bz2, f]` gives the product basis of *bz1* and *bz2*: for every pair of subspaces, the products `nc[s1, s2]` of their states, with the quantum numbers `f[qn1, qn2]`.
- `directproductbasis[bz1, bz2]` uses `f = Join`.

## Details
- Unlike `basistensorproduct`, the subspaces are not merged: the result has one element for each pair of subspaces of *bz1* and *bz2*, in the order of `Outer`. Use `mergebasis` to merge subspaces with equal quantum numbers.
- The default `Join` concatenates the quantum number lists: `{Q1, Sz1}` and `{Q2, Sz2}` give `{Q1, Sz1, Q2, Sz2}`.
- `quickDBL` and `quickDBLSZ` are built on `directproductbasis`.
- `directproductbasis` does not call `makebasis`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
directproductbasis[qbasis[{c[1]}], qbasis[{c[2]}]]
```

### Scope
With additive quantum numbers, subspaces with equal quantum numbers occur several times:
```wl
snegfermionoperators[c];
First /@ directproductbasis[qbasis[{c[1]}], qbasis[{c[2]}], Plus]
```
`mergebasis` combines them:
```wl
First /@ mergebasis[directproductbasis[qbasis[{c[1]}], qbasis[{c[2]}], Plus]]
```

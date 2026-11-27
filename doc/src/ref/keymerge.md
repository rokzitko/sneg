---
keywords: merge subspaces, quantum numbers, key, basis
see also: mergebasis, keyselect, qsbasisvc, basistensorproduct
---
# keymerge

## Usage
- `keymerge[list]` merges the elements `{key, values}` of *list* with equal keys into a single element `{key, allvalues}`.

## Details
- The keys are sorted (`Union`), and for each key the values of all elements with this key are joined and flattened with `Flatten`.
- Applied to a list of subspaces `{{qn, states}, ...}`, `keymerge` combines the states of subspaces with equal quantum numbers. It is used in the construction of the $(Q, S)$ bases. `mergebasis` performs the same operation.
- The values are flattened completely, so they must not be lists of lists.

## Examples
### Basic Examples
```wl
keymerge[{{1, {a, b}}, {2, {c}}, {1, {d}}}]
```

### Applications
Product of the two single-level $(Q, S_z)$ bases, with the quantum numbers added; subspaces with equal total quantum numbers are merged:
```wl
snegfermionoperators[c];
b1 = qszbasis[{c[1]}]; b2 = qszbasis[{c[2]}];
makebasis[{c[1], c[2]}];
pairs = Flatten[Outer[{First[#1] + First[#2], Flatten[outer[Last[#1], Last[#2]]]} &, b1, b2, 1], 1];
{First[#], Length[Last[#]]} & /@ keymerge[pairs]
```

### Properties & Relations
`mergebasis` gives the same result:
```wl
keymerge[{{1, {a, b}}, {2, {c}}, {1, {d}}}] === mergebasis[{{1, {a, b}}, {2, {c}}, {1, {d}}}]
```

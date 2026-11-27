---
keywords: annihilation operator, operator type index
see also: CR, UP, DO, isannihilation, conj, snegfermionoperators, snegbosonoperators
---
# AN

## Usage
- `AN` is the first index of an annihilation operator: `c[AN, j, ...]` is $c_{j\ldots}$.

## Details
- `AN` has the value 1, and `CR` the value 0. The values are fixed (`AN` is `Protected`); see `CR` for the consequences.
- In the formatted output, annihilation operators are shown without a dagger.
- An operator `c[AN, ...]` acting on `VACUUM` gives zero for the default (empty) vacuum. For the Fermi-sea ordering (`ordering[c] = SEA`), `c[AN, k, ...]` with $k < 0$ creates a hole; see `isannihilation`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{c[AN, 1, UP], AN, nc[c[AN, 1, UP], VACUUM]}
```

### Properties & Relations
`AN` and `CR` are integers, `conj` maps the type index $t$ to $1 - t$:
```wl
snegbosonoperators[a];
{CR + AN, conj[a[AN, 1]], InputForm[conj[a[AN, 1]]]}
```

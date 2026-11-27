---
keywords: creation operator, operator type index, dagger
see also: AN, UP, DO, iscreation, conj, snegfermionoperators, snegbosonoperators
---
# CR

## Usage
- `CR` is the first index of a creation operator: `c[CR, j, ...]` is $c^\dagger_{j\ldots}$.

## Details
- `CR` has the value 0, and `AN` the value 1. The values are fixed (`CR` is `Protected`), and several functions depend on them: the normal order with creation operators on the left follows from `OrderedQ` because 0 < 1, and `conj` maps the first index $t$ to $1 - t$.
- Since `CR` evaluates to 0, `FullForm` and `InputForm` show the operators with the index 0; in the formatted output, creation operators are shown with a dagger, $c^\dagger$.
- The first index of fermion and boson operators is the type index. Spin operators, Majorana operators, and X-operators have no type index.
- For the Fermi-sea ordering (`ordering[c] = SEA`), whether `c[CR, k, ...]` creates or annihilates a quasiparticle depends on the sign of $k$; see `iscreation`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{c[CR, 1, UP], CR}
```
```wl
InputForm[c[CR, 1, UP]]
```

### Properties & Relations
Conjugation exchanges `CR` and `AN`:
```wl
snegfermionoperators[c];
{conj[c[CR, 1, UP]], conj[c[AN, 1, UP]]}
```
---
Creation operators are ordered to the left because `CR` < `AN`:
```wl
snegfermionoperators[c];
{OrderedQ[{CR, AN}], nc[c[AN, 2, UP], c[CR, 1, UP]]}
```

### Possible Issues
`CR` is protected and cannot be used as a variable:
```wl
CR = 5
```

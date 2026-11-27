---
keywords: creation operator, quasiparticle, normal order, Fermi sea, particle-hole
see also: isannihilation, ordering, snegOrderedQ, CR, vev, VACUUM
---
# iscreation

## Usage
- `iscreation[op]` gives `True` if the fermion or boson operator *op* creates a (quasi)particle when acting on the reference state of its family, i.e. if it is a creation operator in the sense of the normal order.

## Details
- The reference state is set by `ordering`:

| `ordering[c]` | `iscreation[c[t, k, ...]]` |
|---|---|
| EMPTY (default) | `t == CR` |
| SEA | `t == CR` and $k > 0$, or `t == AN` and $k < 0$ |
| NONE | `t == CR` |
| bosons | `t == CR` |

- For SEA, $k$ is the second index (the momentum or energy measured from the Fermi level). For a symbolic $k$ without a declared sign, the result is a symbolic condition such as `k > 0`; for $k = 0$ it is `False`, as is `isannihilation`. The role is then undetermined.
- `iscreation` and `isannihilation` are used by `snegOrderedQ` (normal order), by `vev` and `vevwick` (an operator that creates a quasiparticle at the left end of a product gives zero in the expectation value), and by the rules for `VACUUM`.
- For other operators, e.g. spin operators or undeclared symbols, `iscreation` remains unevaluated.

## Developer Notes
- `iscreation` and `isannihilation` are the extension points for custom reference states: with `ordering[c] = new` (any new symbol), define `c /: iscreation[c[...]] := ...` and `c /: isannihilation[c[...]] := ...` together with `snegOrderedQ` for the family; see `ordering` for a complete example. The SEA rules can be overridden in the same way, e.g. to use another index as the energy label.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
{iscreation[c[CR, 1, UP]], iscreation[c[AN, 1, UP]]}
```
For the Fermi sea, $c_k$ with $k < 0$ creates a hole:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{iscreation[d[CR, 2, UP]], iscreation[d[AN, -2, UP]], iscreation[d[CR, -2, UP]]}
```

### Scope
A symbolic momentum gives a condition, unless its sign is declared:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
kp /: kp > 0 = True;
{iscreation[d[CR, k, UP]], iscreation[d[CR, kp, UP]]}
```
---
Bosons:
```wl
snegbosonoperators[a];
{iscreation[a[CR]], iscreation[a[AN, 1]]}
```

### Properties & Relations
For EMPTY and SEA with known sign, exactly one of `iscreation` and `isannihilation` is `True`:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
ops = {d[CR, 1, UP], d[AN, 1, UP], d[CR, -1, UP], d[AN, -1, UP]};
Transpose[{iscreation /@ ops, isannihilation /@ ops}]
```

### Possible Issues
For the label $k = 0$, the role is undetermined in the SEA ordering:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{iscreation[d[CR, 0, UP]], isannihilation[d[CR, 0, UP]]}
```

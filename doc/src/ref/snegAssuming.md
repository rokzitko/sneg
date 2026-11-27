---
keywords: assumption, sign of index, Fermi sea, upvalue, TagSet, temporary definition
see also: ordering, vev, zeroonvac, isannihilation, iscreation, Assuming, TagSet
---
# snegAssuming

## Usage
- `snegAssuming[s /: lhs = rhs, expr]` evaluates *expr* with the temporary upvalue `s /: lhs = rhs`.
- `snegAssuming[{s1 /: lhs1 = rhs1, ...}, expr]` evaluates *expr* with several temporary upvalues.

## Details
- The assumptions are given as `TagSet` definitions, typically for the sign of a symbolic index: `k /: k > 0 = True`. They are defined before *expr* is evaluated, and all upvalues and downvalues of the tagged symbols are restored afterwards, also those defined during the evaluation of *expr*.
- `snegAssuming` has the attribute `HoldAllComplete`: *expr* is only evaluated after the assumptions are set.
- The assumption only affects tests that match the defined left-hand side literally. `k /: k > 0 = True` makes `k > 0` evaluate to `True`, but not `k < 0`, `k >= 0`, `Positive[k]`, or `UnitStep[k]`.
- In SNEG, the main use is to decide whether an operator with `ordering[c] = SEA` creates or annihilates an excitation of the Fermi sea: `iscreation` and `isannihilation` test `k > 0` and `k < 0` for the second index *k*.
- `Assuming` and `$Assumptions` do not influence these tests, since they are not used by the evaluation of `k > 0`.

## Examples
### Basic Examples
```wl
snegfreeindexes[k];
{k > 0, snegAssuming[k /: k > 0 = True, k > 0], k > 0}
```

### Scope
Several assumptions:
```wl
snegfreeindexes[k, q];
snegAssuming[{k /: k > 0 = True, q /: q < 0 = True}, {k > 0, q < 0}]
```

### Applications
For the Fermi sea, the operator $c_k$ with $k > 0$ annihilates the vacuum:
```wl
snegfermionoperators[f];
ordering[f] = SEA;
snegfreeindexes[k];
{isannihilation[f[AN, k, UP]], snegAssuming[k /: k > 0 = True, isannihilation[f[AN, k, UP]]]}
```
```wl
{zeroonvac[nc[f[CR, 2, UP], f[AN, k, UP]]], snegAssuming[k /: k > 0 = True, zeroonvac[nc[f[CR, 2, UP], f[AN, k, UP]]]]}
```

### Possible Issues
Only the literal left-hand side is assumed:
```wl
snegfreeindexes[k];
snegAssuming[k /: k > 0 = True, {k > 0, k < 0, k >= 0, UnitStep[k]}]
```

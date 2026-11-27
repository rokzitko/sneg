---
keywords: fermion test, type predicate, declared operator
see also: operatorQ, bosonQ, majoranaQ, spinQ, snegfermionoperators, snegspinlessfermionoperators, spinof
---
# fermionQ

## Usage
- `fermionQ[c]` gives `True` if *c* is a fermion operator symbol or a fermion operator `c[...]`.

## Details
- `fermionQ` is `True` for symbols declared with `snegfermionoperators` or `snegspinlessfermionoperators` and for their applications `c[t, j, ...]`.
- Majorana operators are not fermions in this sense (`majoranaQ`), nor are Grassmann numbers (`grassmanQ`).
- For other expressions, `fermionQ` remains unevaluated; use `TrueQ[fermionQ[x]]` in conditions.
- Many SNEG functions select their rules with patterns such as `op_?fermionQ[j___]`. The spin of a fermion family is `spinof[c]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegspinlessfermionoperators[f];
{fermionQ[c], fermionQ[c[CR, 1, UP]], fermionQ[f[AN, 1, 0]]}
```

### Scope
Use in patterns:
```wl
snegfermionoperators[c];
snegbosonoperators[a];
Cases[{c[CR, 1, UP], a[AN], c[AN, 2, DO]}, _?fermionQ]
```

### Possible Issues
For bosons and undeclared symbols, `fermionQ` remains unevaluated, not `False`:
```wl
snegbosonoperators[a];
{fermionQ[a], fermionQ[u], TrueQ[fermionQ[a]]}
```

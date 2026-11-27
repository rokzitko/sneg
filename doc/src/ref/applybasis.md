---
keywords: basis, map function over states, transform basis states
see also: mergebasis, mapbasis, transformbasis, bzvc2bzop, bzQ, dropemptysubspaces
---
# applybasis

## Usage
- `applybasis[bz, f]` applies the function *f* to every state of the basis *bz* and keeps the quantum numbers.

## Details
- *bz* must be a basis of the form `{{{qn...}, {states}}, ...}`, as tested by `bzQ`; otherwise `applybasis` returns unevaluated.
- The result is `{{qn, f /@ states}, ...}`. The states are neither orthogonalized nor normalized, and states that become 0 are kept; use `transformbasis` to apply an operator followed by orthogonalization.

## Examples
### Basic Examples
```wl
applybasis[{{{0}, {a, b}}, {{1}, {c}}}, f]
```

### Scope
Applying the number operator to a basis in the occupation number representation:
```wl
snegfermionoperators[d];
bz = qszbasisvc[{d[]}];
applybasis[bz, ap[number[d[]], #] &] // InputForm
```
---
Conversion of the states to the creation operator representation:
```wl
applybasis[bz, vc2ops]
```

### Applications
Adding a phonon ket $|0\rangle$ to every state:
```wl
snegfermionoperators[d];
applybasis[qszbasis[{d[]}], nc[#, ket[0]] &]
```

### Possible Issues
The quantum numbers must be lists:
```wl
applybasis[{{0, {a, b}}}, f]
```

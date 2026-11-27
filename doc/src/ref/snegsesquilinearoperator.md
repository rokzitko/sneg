---
keywords: sesquilinear form, inner product, scalar product, bra-ket, antilinear, extending SNEG
see also: sneglinearoperator, sneglinearoperatorFirst, isnumericQ, conj, scalarproductvc, scalarproductop
---
# snegsesquilinearoperator

## Usage
- `snegsesquilinearoperator[f]` makes the function *f* of two arguments antilinear in the first and linear in the second argument.

## Details
- After `snegsesquilinearoperator[f]`, the following rules hold for scalars $z$ (see `isnumericQ`):
  - $f(z\, a, b) = z^*\, f(a, b)$ and $f(a, z\, b) = z\, f(a, b)$;
  - $f(a_1 + a_2, b) = f(a_1, b) + f(a_2, b)$ and $f(a, b_1 + b_2) = f(a, b_1) + f(a, b_2)$;
  - $f(0, b) = f(a, 0) = 0$.
- This is the convention of Dirac's notation, $\langle a|b\rangle$, used in physics; in the mathematical literature, inner products are often linear in the first argument.
- The complex conjugate is computed with `conj`, so that declared real constants are invariant.
- In SNEG, `scalarproduct` and `scalarproductvc` are declared in this way.
- The rules are added as definitions of *f*; the values on basis vectors are given by further definitions. `snegsesquilinearoperator` returns `{Null}`.

## Examples
### Basic Examples
```wl
snegrealconstants[t];
snegcomplexconstants[z];
snegsesquilinearoperator[sp];
sp[z a + t b, I a]
```

### Applications
The overlap of one-electron states $|\psi\rangle = \sum_{i\sigma} \psi_{i\sigma} c^\dagger_{i\sigma}|0\rangle$, written as creation-operator expressions, with orthonormal basis states:
```wl
snegfermionoperators[c];
snegrealconstants[t];
snegcomplexconstants[z];
snegsesquilinearoperator[overlap];
overlap[c[CR, i_, s_], c[CR, j_, r_]] := KroneckerDelta[i, j] KroneckerDelta[s, r];
psi = z c[CR, 1, UP] + t c[CR, 2, UP];
{overlap[psi, psi], overlap[psi, c[CR, 1, UP]], overlap[c[CR, 1, UP], psi]}
```

### Properties & Relations
The same overlap from the vacuum expectation value, $\langle 0|\psi_1^\dagger \psi_2|0\rangle$:
```wl
snegfermionoperators[c];
snegrealconstants[t];
snegcomplexconstants[z];
psi = z c[CR, 1, UP] + t c[CR, 2, UP];
vev[nc[conj[psi], psi]]
```

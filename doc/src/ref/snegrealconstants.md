---
keywords: real constant, parameter, declaration, hopping, interaction strength, c-number, scalar
see also: snegcomplexconstants, snegintegerconstants, snegrealfunctions, snegcomplexfunctions, snegfreeindexes, isnumericQ, conj, listrealconstants
---
# snegrealconstants

## Usage
- `snegrealconstants[x, y, ...]` declares *x*, *y*, ... as real constants (parameters).

## Details
- Declared constants are scalars (see `isnumericQ`): they are factored out of operator products `nc[...]` and of all linear functions, and they commute with all operators.
- Real constants are invariant under complex conjugation: `Conjugate[x]` is *x*, and hence `conj[x a]` is `x conj[a]`.
- Typical uses are the parameters of a Hamiltonian, such as hopping $t$, interaction $U$, level energy $\epsilon$, exchange $J$, magnetic field $B$.
- Symbols that are not declared are treated as operators of unknown type: they are not factored out, and the products that contain them are not normal ordered. Every parameter in an operator expression should therefore be declared.
- The declaration sets the upvalues `snegnonopQ[x] = True` and `Conjugate[x] = x`, and adds *x* to `listrealconstants`. The arguments must be symbols; existing values are not cleared.
- Assumptions such as $x > 0$ are not implied; use `Assumptions` in `Simplify` or `$Assumptions` when needed.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
nc[c[AN, 1, UP], t c[CR, 2, UP] + U c[CR, 1, UP]]
```
Real constants are not conjugated:
```wl
conj[t nc[c[CR, 1, UP], c[AN, 2, UP]]]
```

### Scope
Expressions built from real constants are scalars and real:
```wl
snegrealconstants[t, U];
{isnumericQ[Sqrt[U^2 + 16 t^2]], Conjugate[t + I U], MemberQ[listrealconstants, t]}
```

### Applications
A Hermitian Hamiltonian: the Hubbard dimer with real parameters is invariant under conjugation:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
Expand[conj[H] - H]
```

### Properties & Relations
A complex constant is conjugated:
```wl
snegrealconstants[t];
snegcomplexconstants[z];
{Conjugate[t], Conjugate[z]}
```

### Possible Issues
An undeclared parameter is treated as an operator, and the product is not normal ordered:
```wl
snegfermionoperators[c];
nc[c[AN, 1, UP], V c[CR, 1, UP]]
```
```wl
snegrealconstants[V];
nc[c[AN, 1, UP], V c[CR, 1, UP]]
```
---
Real constants are not assumed positive:
```wl
snegrealconstants[U];
{Sqrt[U^2], Simplify[Sqrt[U^2], U > 0]}
```

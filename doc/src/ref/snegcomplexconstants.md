---
keywords: complex constant, parameter, declaration, complex hopping, Peierls phase, c-number
see also: snegrealconstants, snegcomplexfunctions, isnumericQ, conj, listcomplexconstants
---
# snegcomplexconstants

## Usage
- `snegcomplexconstants[z, w, ...]` declares *z*, *w*, ... as complex constants.

## Details
- Declared constants are scalars (see `isnumericQ`): they are factored out of operator products and linear functions.
- Complex constants are conjugated by `conj`: `conj[z a]` is `Conjugate[z] conj[a]`.
- The declaration sets the upvalue `snegnonopQ[z] = True` and adds *z* to `listcomplexconstants`.
- Use complex constants for complex hopping amplitudes, pairing amplitudes, hybridization with phases, and complex coefficients of states.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
conj[z nc[c[CR, 1, UP], c[AN, 2, UP]]]
```

### Applications
A hopping term with a complex amplitude, $z\, c^\dagger_1 c_2 + z^*\, c^\dagger_2 c_1$, is Hermitian:
```wl
snegfermionoperators[c];
snegcomplexconstants[z];
H = z nc[c[CR, 1, UP], c[AN, 2, UP]] + conj[z nc[c[CR, 1, UP], c[AN, 2, UP]]];
Expand[conj[H] - H]
```
---
The norm of a superposition $(z\, c^\dagger_{1\uparrow} + w\, c^\dagger_{2\uparrow})|0\rangle$:
```wl
snegfermionoperators[c];
snegcomplexconstants[z, w];
psi = nc[z c[CR, 1, UP] + w c[CR, 2, UP], VACUUM];
nc[conj[psi], psi]
```

### Properties & Relations
Complex constants are conjugated with `Conjugate`; real constants (`snegrealconstants`) are not:
```wl
snegcomplexconstants[z];
snegrealconstants[t];
{Conjugate[2 I z t], conj[z t]}
```

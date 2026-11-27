---
keywords: coherent state, fermionic coherent state, Grassmann numbers, Berezin integral, bosonic coherent state, path integral
see also: sneggrassmanconstants, grassmanQ, conj, VACUUM, snegspinlessfermionoperators, snegbosonoperators
---
# coh

## Usage
- `coh[z, c[j]]` gives the normalized coherent state $|z\rangle = e^{-\bar z z/2}\, e^{c^\dagger_j z} |0\rangle$ of the mode `c[j]` with the eigenvalue *z*.

## Details
- For a fermion mode, *z* is a Grassmann number declared with `sneggrassmanconstants`. Since $z^2 = 0$, the exponentials terminate: $|z\rangle = (1 + z\bar z/2)|0\rangle - z\, c^\dagger |0\rangle$, with $\bar z$ = `conj[z]`.
- $|z\rangle$ is an eigenstate of the annihilation operator, $c\,|z\rangle = z\,|z\rangle$, and it is normalized, $\langle z|z\rangle = 1$. The overlap of two coherent states is $\langle w|z\rangle = e^{-\bar w w/2 - \bar z z/2 + \bar w z}$.
- The state is written with the placeholder `VACUUM` for $|0\rangle$; `conj[coh[z, c[j]]]` gives the bra $\langle z|$.
- The second argument is the mode without the creation/annihilation index: `c[]` for a spinless fermion without indexes, `c[UP]` for the spin-up component of a spin-1/2 fermion `c`, `c[i, UP]` for an orbital index *i*.
- For a boson mode and a complex number *z* (declared with `snegcomplexconstants`), `coh` gives the bosonic coherent state $e^{-|z|^2/2} e^{z a^\dagger}|0\rangle$; the exponential of the operator is kept unexpanded.
- Berezin integrals over Grassmann variables are written as `nc[int[z], ...]`, with $\int dz\, z = 1$ and $\int dz\, 1 = 0$.

## Examples
### Basic Examples
Fermionic coherent state of a spinless fermion:
```wl
sneggrassmanconstants[z];
snegspinlessfermionoperators[g];
coh[z, g[]]
```
It is an eigenstate of the annihilation operator, $g|z\rangle = z|z\rangle$:
```wl
Expand[nc[g[AN], coh[z, g[]]] - nc[z, coh[z, g[]]]]
```

### Scope
The bra $\langle z|$:
```wl
sneggrassmanconstants[z];
snegspinlessfermionoperators[g];
conj[coh[z, g[]]]
```
---
Normalization and overlap of coherent states, $\langle w|z\rangle = 1 + w\bar w/2 + z\bar z/2 - z\bar w - w z\bar w\bar z/4$:
```wl
sneggrassmanconstants[z, w];
snegspinlessfermionoperators[g];
{Expand[nc[conj[coh[z, g[]]], coh[z, g[]]]], Expand[nc[conj[coh[w, g[]]], coh[z, g[]]]]}
```
---
A spin-1/2 fermion mode, here the spin-up component:
```wl
sneggrassmanconstants[z];
snegfermionoperators[c];
Expand[nc[c[AN, UP], coh[z, c[UP]]] - nc[z, coh[z, c[UP]]]]
```
---
Bosonic coherent state, $a|z\rangle = z|z\rangle$:
```wl
snegbosonoperators[a];
snegcomplexconstants[x];
st = coh[x, a[]]
```
```wl
Simplify[nc[a[AN], st] - x st]
```

### Properties & Relations
The Gaussian Berezin integral $\int d\bar z\, dz\, e^{-a \bar z z} = a$, with the measure `int[conj[z], z]`:
```wl
sneggrassmanconstants[z];
snegrealconstants[α];
nc[int[conj[z], z], 1 - α nc[conj[z], z]]
```
---
Grassmann numbers anticommute with each other and with fermion operators:
```wl
sneggrassmanconstants[z];
snegspinlessfermionoperators[g];
{antikomutator[z, conj[z]], antikomutator[z, g[CR]], nc[z, z]}
```

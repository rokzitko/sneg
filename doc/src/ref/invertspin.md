---
keywords: spin inversion, spin flip, spin reflection, symmetry, Zeeman, spin-flip symmetry
see also: spinxyz, spinz, number, hop, transformtoLR
---
# invertspin

## Usage
- `invertspin[expr]` exchanges the spin indexes `UP` and `DO` of all fermion operators in *expr*.

## Details
- Every spin-1/2 fermion operator $c_{i\sigma}$ is replaced by $c_{i\bar\sigma}$, without a sign: this is the spin-flip transformation $\sigma \to -\sigma$, under which $S^z \to -S^z$ and $S^\pm \to S^\mp$. (Time reversal, $c_{i\uparrow} \to c_{i\downarrow}$, $c_{i\downarrow} \to -c_{i\uparrow}$, has an additional sign.)
- The spin index must be the last index of the operator. A symbolic spin index σ becomes $1 - \sigma$ (`UP` and `DO` are 1 and 0).
- `invertspin` is linear and acts on all factors of products; it is `Listable`.
- Only spin-1/2 fermion operators are supported. Other operators are returned inside `invertspin[...]`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
invertspin[nc[c[CR, 1, UP], c[AN, 2, DO]]]
```
---
The spin projection changes sign:
```wl
snegfermionoperators[c];
Expand[invertspin[spinz[c[1]]] + spinz[c[1]]]
```

### Scope
The spin-raising operator becomes the spin-lowering operator:
```wl
snegfermionoperators[c];
Expand[invertspin[spinplus[c[1]]] - spinminus[c[1]]]
```
---
Symbolic spin indexes:
```wl
snegfermionoperators[c];
snegfreeindexes[σ];
invertspin[c[CR, 1, σ]]
```

### Applications
The Hubbard Hamiltonian is invariant under spin inversion, while a Zeeman term changes sign:
```wl
snegfermionoperators[c];
snegrealconstants[t, U, B];
H = -t hop[c[1], c[2]] + U (hubbard[c[1]] + hubbard[c[2]]);
Z = B (spinz[c[1]] + spinz[c[2]]);
{Expand[invertspin[H] - H], Expand[invertspin[Z] + Z]}
```

### Properties & Relations
`invertspin` is an involution:
```wl
snegfermionoperators[c];
x = nc[c[CR, 1, UP], c[CR, 2, DO], c[AN, 3, UP]];
invertspin[invertspin[x]] == x
```

### Possible Issues
Only spin-1/2 is supported. For other spins, the index $\sigma$ is replaced by $1 - \sigma$, which is not $-\sigma$:
```wl
snegfermionoperators[{f, 3/2}];
invertspin[f[CR, 1, 3/2]]
```
---
Boson operators are not changed:
```wl
snegbosonoperators[a];
invertspin[a[CR]]
```

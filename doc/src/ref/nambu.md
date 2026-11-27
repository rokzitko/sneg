---
keywords: Nambu spinor, particle-hole spinor, isospin, charge SU(2), bipartite lattice, superconductivity
see also: isospin, isospinxyz, manyisospin, bcs, PauliX
---
# nambu

## Usage
- `nambu[c[i]]` gives the Nambu spinor $\{c^\dagger_{i\uparrow}, c_{i\downarrow}\}$ of the fermion level `c[i]`.
- `nambu[c[i], n]` gives $\{c^\dagger_{i\uparrow}, (-1)^n c_{i\downarrow}\}$.
- `nambu[c]` is `nambu[c[]]`.

## Details
- The level is a spin-1/2 fermion level given without the creation/annihilation and spin indexes. The default phase index is $n = 0$.
- Both components raise the spin projection by $1/2$; with respect to the isospin they form a doublet, with $I^z = +1/2$ for $c^\dagger_\uparrow$ and $I^z = -1/2$ for $c_\downarrow$.
- The isospin operators are $I^a = \frac{1}{2} \sum_{\alpha\beta} \eta_\alpha \sigma^a_{\alpha\beta} \eta^\dagger_\beta$ with $\eta$ = `nambu[c[i], n]` and the Pauli matrices $\sigma^a$; see `isospin`.
- The phase $(-1)^n$ is chosen differently on the two sublattices of a bipartite lattice (e.g. $n = i$ for site $i$ of a chain), so that the total isospin commutes with the hopping (charge SU(2) symmetry of the half-filled Hubbard model).
- Note that this spinor is formed from $c^\dagger_\uparrow$ and $c_\downarrow$, the Hermitian conjugate of the Nambu spinor $\Psi = (c_\uparrow, c^\dagger_\downarrow)^T$ used in the Bogoliubov-de Gennes formalism.
- `nambu` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
nambu[c[]]
```
With the phase index $n = 1$:
```wl
nambu[c[], 1]
```

### Scope
`nambu` threads over lists of levels:
```wl
snegfermionoperators[c];
nambu[{c[1], c[2]}]
```

### Properties & Relations
Both components have $S^z = +1/2$; the isospin projections are $\pm1/2$:
```wl
snegfermionoperators[c];
eta = nambu[c[]];
{Expand[komutator[spinz[c[]], #] & /@ eta], Expand[komutator[isospinz[c[]], #] & /@ eta]}
```
---
Construction of the isospin operator from the Nambu spinor:
```wl
Expand[1/2 VMV[eta, PauliX, conj /@ eta] - isospinx[c[]]]
```

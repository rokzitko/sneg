---
keywords: Hubbard interaction, on-site Coulomb repulsion, double occupancy, Hubbard model, Anderson impurity model, Hubbard atom
see also: number, isozsq, chargecharge, hop, hamiltonian, projector2, snegfermionoperators
related links: [J. Hubbard, Proc. R. Soc. London A 276, 238 (1963)](https://doi.org/10.1098/rspa.1963.0204)
---
# hubbard

## Usage
- `hubbard[c[i]]` gives the on-site Hubbard interaction $n_{i\uparrow} n_{i\downarrow}$ of the fermion level `c[i]`.
- `hubbard[c]` is `hubbard[c[]]`.

## Details
- The level is specified without the creation/annihilation and spin indexes: `c[i]` stands for the operators `c[CR, i, σ]` and `c[AN, i, σ]`; `c[]` is a level without further indexes.
- The result is normal ordered: $n_\uparrow n_\downarrow = -c^\dagger_\downarrow c^\dagger_\uparrow c_\downarrow c_\uparrow$.
- For fermions with spin $s \ne 1/2$, declared with `snegfermionoperators[{c, s}]`, `hubbard` gives $\sum_{\sigma > \sigma'} n_\sigma n_{\sigma'}$, the number of pairs of electrons in the level.
- The Hubbard model is $H = -t \sum_{\langle ij\rangle\sigma} (c^\dagger_{i\sigma} c_{j\sigma} + \mathrm{h.c.}) + U \sum_i n_{i\uparrow} n_{i\downarrow}$. In the Anderson impurity model, $U n_\uparrow n_\downarrow$ is the interaction on the impurity level.
- Related forms of the interaction are $\frac{U}{2}(n-1)^2 = U n_\uparrow n_\downarrow - \frac{U}{2} n + \frac{U}{2}$, see `isozsq`, and $\frac{U}{2} n(n-1)$, see `chargecharge`.
- `hubbard` is `Listable`.

## Examples
### Basic Examples
The Hubbard interaction of a level `d[]`:
```wl
snegfermionoperators[d];
hubbard[d[]]
```
It equals $n_\uparrow n_\downarrow$:
```wl
hubbard[d[]] == nc[number[d[], UP], number[d[], DO]]
```

### Scope
Levels with indexes:
```wl
snegfermionoperators[c];
hubbard[c[1]]
```
---
`hubbard` threads over lists of levels:
```wl
Total[hubbard[{c[1], c[2], c[3]}]]
```
---
For a spin-1 fermion (three flavors), `hubbard` counts the pairs of occupied flavors:
```wl
snegfermionoperators[{f, 1}];
hubbard[f[]]
```

### Applications
Spectrum of the Hubbard atom $H = \epsilon\, n + U n_\uparrow n_\downarrow$ in the subspaces of charge $Q = n - 1$ and spin $S_z$:
```wl
snegfermionoperators[d];
snegrealconstants[eps, U];
H = eps number[d[]] + U hubbard[d[]];
makematricesbzvc[H, qszbasisvc[{d[]}]]
```
---
Spectrum of the Hubbard dimer. The ground state in the half-filled subspace ($Q = 0$, $S_z = 0$) has the energy $(U - \sqrt{U^2 + 16 t^2})/2$:
```wl
snegfermionoperators[c];
snegrealconstants[t, U];
H = -t hop[c[1], c[2]] + U Total[hubbard[{c[1], c[2]}]];
Map[{First[#], Eigenvalues[Last[#]]} &, makematricesbzvc[H, qszbasisvc[{c[1], c[2]}]]]
```
At large $U$, the singlet-triplet splitting is the antiferromagnetic exchange $J = 4t^2/U$:
```wl
Series[(U - Sqrt[U^2 + 16 t^2])/2, {t, 0, 4}, Assumptions -> U > 0]
```

### Properties & Relations
Relation with the charge-charge interaction $n^2 = n + 2 n_\uparrow n_\downarrow$ and with the square of the isospin, $(n-1)^2 = 1 - n + 2 n_\uparrow n_\downarrow$:
```wl
snegfermionoperators[d];
{chargecharge[d[], d[]] == number[d[]] + 2 hubbard[d[]], isozsq[d[]] == 1 - number[d[]] + 2 hubbard[d[]]}
```
---
`hubbard` is the projector onto the doubly occupied state:
```wl
hubbard[d[]] == projector2[d[]]
```
---
The interaction commutes with the number and spin operators of the level:
```wl
Expand[{komutator[hubbard[d[]], number[d[]]], komutator[hubbard[d[]], spinz[d[]]], komutator[hubbard[d[]], spinplus[d[]]]}]
```

### Possible Issues
The argument is the level, not an operator: `hubbard[d[CR, UP]]` treats `CR` and `UP` as additional level indexes:
```wl
snegfermionoperators[d];
hubbard[d[CR, UP]]
```

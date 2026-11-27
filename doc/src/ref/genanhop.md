---
keywords: anomalous hopping, singlet pairing, complex pairing amplitude, superconducting phase, nonlocal pairing, Andreev coupling
see also: anhop, anomaloushop, genhop, bcs
related links: [T. Iličin and R. Žitko, SciPost Phys. 19, 006 (2025) (superconducting Anderson impurity model)](https://doi.org/10.21468/SciPostPhys.19.1.006)
---
# genanhop

## Usage
- `genanhop[t, a[i], b[j]]` gives the singlet anomalous hopping operator with the complex amplitude *t*, $t\,(a^\dagger_{i\uparrow} b^\dagger_{j\downarrow} - a^\dagger_{i\downarrow} b^\dagger_{j\uparrow}) + \mathrm{h.c.}$
- `genanhop[t, a[i], b[j], σ]` gives $t\, a^\dagger_{i\sigma} b^\dagger_{j\bar\sigma} + t^*\, b_{j\bar\sigma} a_{i\sigma}$.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes; $\bar\sigma$ is the spin opposite to $\sigma$.
- `genanhop[t, a, b]` is `genanhop[t, a, b, UP] - genanhop[t, a, b, DO]`. The Hermitian conjugate is formed with `Conjugate[t]`.
- `genanhop[1, a, b]` is `anhop[a, b]`, and `genanhop[t, a, a]` is `2 bcs[t, a]`.
- `genanhop` is the pairing counterpart of `genhop`: a general spin-singlet quadratic coupling between two levels is `genhop[v, a, b] + genanhop[w, a, b]`, as in the hybridization of an impurity with a superconductor in the basis of NRG Ljubljana with spin-SU(2) symmetry but without charge conservation.
- `genanhop` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[a, b];
snegcomplexconstants[z];
genanhop[z, a[], b[]]
```

### Scope
A general spin-singlet coupling of an impurity level `d` with the first site `f[0]` of a superconducting Wilson chain:
```wl
snegfermionoperators[d, f];
snegcomplexconstants[v, w];
genhop[v, f[0], d[]] + genanhop[w, f[0], d[]]
```

### Properties & Relations
`genanhop` is Hermitian and conserves the total spin:
```wl
snegfermionoperators[a, b];
snegcomplexconstants[z];
H = genanhop[z, a[], b[]];
{Sx, Sy, Sz, S2} = manyspin[{a[], b[]}];
Expand[{conj[H] - H, komutator[H, Sx], komutator[H, Sy], komutator[H, Sz]}]
```
---
Relation with `anhop` and `bcs`:
```wl
{genanhop[1, a[], b[]] == anhop[a[], b[]], Expand[genanhop[z, a[], a[]] - 2 bcs[z, a[]]]}
```

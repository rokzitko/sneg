---
keywords: spin-flip hopping, Peierls phase, spin-orbit coupling, superconducting phase
see also: spinfliphop, hopphi, matrixhop, currentphi
related links: [T. Iličin and R. Žitko, arXiv:2610.08557 (Andreev spin qubits)](https://arxiv.org/abs/2610.08557)
---
# spinfliphopphi

## Usage
- `spinfliphopphi[a[i], b[j], φ]` gives the spin-flip hopping operator with the phase *φ*, $\sum_\sigma (e^{i\phi} a^\dagger_{i\sigma} b_{j\bar\sigma} + e^{-i\phi} b^\dagger_{j\sigma} a_{i\bar\sigma})$.

## Details
- The levels are spin-1/2 fermion levels given without the creation/annihilation and spin indexes; $\bar\sigma$ is the spin opposite to $\sigma$.
- It equals `matrixhop[Exp[I φ] PauliX, a[i], b[j]]`, and `spinfliphopphi[a, b, 0]` is `spinfliphop[a, b]`. The phase has the same meaning as in `hopphi`.
- `spinfliphopphi[a, b, Pi/2]` is the imaginary spin-flip hopping $i \sum_\sigma (a^\dagger_\sigma b_{\bar\sigma} - b^\dagger_\sigma a_{\bar\sigma})$.
- `spinfliphopphi` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c, d];
snegrealconstants[phi];
spinfliphopphi[c[], d[], phi]
```

### Properties & Relations
Relation with `spinfliphop` and `matrixhop`:
```wl
snegfermionoperators[c, d];
snegrealconstants[phi];
{spinfliphopphi[c[], d[], 0] == spinfliphop[c[], d[]], Expand[ExpToTrig[spinfliphopphi[c[], d[], phi] - matrixhop[Exp[I phi] PauliX, c[], d[]]]]}
```

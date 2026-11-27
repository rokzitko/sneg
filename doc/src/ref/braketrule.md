---
keywords: scalar product, braket, overlap, Kronecker delta, Dirac notation, orthonormal basis
see also: bra, ket, nc, scalarproductvc, ap
---
# braketrule

## Usage
- `braketrule[bra[i], ket[j]]` gives the scalar product $\langle i | j \rangle$.

## Details
- `braketrule` is called when a bra and a ket are juxtaposed in an `nc` product, when a bra is applied to a `vc` state with a ket (`ap`), and by `scalarproductvc` for `vc` states with kets.
- The default rule gives the product of `KroneckerDelta[i, j]` over all pairs of entries that are not `Null`: the kets form an orthonormal basis. For example, `braketrule[bra[a, Null, b], ket[c, Null, d]]` is $\delta_{ac}\delta_{bd}$.
- The default rule applies only if the bra and the ket have the same pattern of `Null` entries; otherwise `braketrule` remains unevaluated.
- Other scalar products can be implemented by additional definitions of `braketrule`, for example for non-orthogonal states. Since the default rule has a general pattern with a condition, definitions added later are stored after it and are not used for bras and kets of the default form. New rules must be placed before the default rule, e.g. with `DownValues[braketrule] = Prepend[DownValues[braketrule], rule]` (see Possible Issues).

## Examples
### Basic Examples
```wl
{braketrule[bra[1], ket[1]], braketrule[bra[1], ket[2]], braketrule[bra[x], ket[y]]}
```
Subsystem entries `Null` are ignored:
```wl
braketrule[bra[a, Null, b], ket[c, Null, d]]
```

### Scope
`braketrule` is used by `nc`:
```wl
nc[bra[x, y], ket[1, 2]]
```
---
and by `scalarproductvc`:
```wl
snegfermionoperators[d];
makebasis[{d[]}];
scalarproductvc[vc[1, 0, ket[1/2]], vc[1, 0, ket[-1/2]]]
```

### Applications
Coherent states $|\alpha\rangle$ of a harmonic oscillator are not orthogonal, $\langle\alpha|\beta\rangle = \exp(-|\alpha|^2/2 - |\beta|^2/2 + \alpha^* \beta)$. The rule is placed before the default rule:
```wl
DownValues[braketrule] = Prepend[DownValues[braketrule],
  HoldPattern[braketrule[bra[coherent[a_]], ket[coherent[b_]]]] :>
    Exp[-Abs[a]^2/2 - Abs[b]^2/2 + Conjugate[a] b]];
nc[bra[coherent[1]], ket[coherent[2]]]
```
Other kets still obey the default rule:
```wl
nc[bra[1], ket[2]]
```

### Possible Issues
A definition made with `:=` is stored after the general default rule and has no effect:
```wl
braketrule[bra[gaussian[a_]], ket[gaussian[b_]]] := Exp[-(a - b)^2/4];
nc[bra[gaussian[0]], ket[gaussian[1]]]
```

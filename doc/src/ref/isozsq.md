---
keywords: particle-hole symmetric interaction, Hubbard interaction, (n-1)^2, isospin, charging energy, Anderson impurity model
see also: hubbard, number, isospinz, chargecharge, projector02
---
# isozsq

## Usage
- `isozsq[c[i]]` gives $(n_i - 1)^2$ for the fermion level `c[i]`, four times the square of the $z$ component of the isospin, the particle-hole symmetric form of the on-site interaction.

## Details
- With $n = n_\uparrow + n_\downarrow$, $(n - 1)^2 = 1 - n + 2 n_\uparrow n_\downarrow$ for spin-1/2 fermions; it is 1 for the empty and the doubly occupied state and 0 for single occupancy. It equals $4 (I^z)^2$ with $I^z = (n - 1)/2$.
- The interaction term $\frac{U}{2}(n - 1)^2 = U n_\uparrow n_\downarrow - \frac{U}{2} n + \frac{U}{2}$ is the Hubbard interaction with the chemical potential at the particle-hole symmetric point and a constant shift. A level $\epsilon\, n + U n_\uparrow n_\downarrow$ can be written as $\frac{U}{2}(n-1)^2 + (\epsilon + U/2)(n - 1) + \mathrm{const}$, so that $\delta = \epsilon + U/2$ measures the deviation from particle-hole symmetry.
- For fermions with spin $s$, `isozsq` is $(n - 1)^2$ with the corresponding number operator.
- `isozsq` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[d];
isozsq[d[]]
```

### Scope
`isozsq` threads over lists of levels:
```wl
snegfermionoperators[c];
Total[isozsq[{c[1], c[2]}]]
```

### Applications
Spectrum of the particle-hole symmetric Anderson impurity $\frac{U}{2}(n-1)^2$: the empty and the doubly occupied states are degenerate at $U/2$, the local moment at 0:
```wl
snegfermionoperators[d];
snegrealconstants[U];
makematricesbzvc[U/2 isozsq[d[]], qszbasisvc[{d[]}]]
```
---
The particle-hole transformation $d_\uparrow \to d^\dagger_\uparrow$, $d_\downarrow \to -d^\dagger_\downarrow$ leaves $(n-1)^2$ invariant, but not $n_\uparrow n_\downarrow$:
```wl
ph = {d[CR, UP] -> d[AN, UP], d[AN, UP] -> d[CR, UP], d[CR, DO] -> -d[AN, DO], d[AN, DO] -> -d[CR, DO]};
{Expand[(isozsq[d[]] /. ph) - isozsq[d[]]], Expand[(hubbard[d[]] /. ph) - hubbard[d[]]]}
```

### Properties & Relations
Relation with `hubbard` and with the isospin:
```wl
snegfermionoperators[d];
Expand[{isozsq[d[]] - (1 - number[d[]] + 2 hubbard[d[]]), isozsq[d[]] - 4 pow[isospinz[d[]], 2]}]
```
---
$(n - 1)^2$ is the projector onto the empty and doubly occupied states:
```wl
Expand[isozsq[d[]] - projector02[d[]]]
```

---
keywords: spin lowering operator, ladder operator, spin flip, S minus
see also: spinplus, spinxyz, spinx, spiny, spinz, spinspinmp, spindown
---
# spinminus

## Usage
- `spinminus[c[i]]` gives the spin-lowering operator $S^-_i = S^x_i - i S^y_i = c^\dagger_{i\downarrow} c_{i\uparrow}$ of the fermion level `c[i]`.
- `spinminus[c]` is `spinminus[c[]]`.

## Details
- See `spinxyz` for the definition of the spin operators. For fermions with spin $s \neq 1/2$, $S^- = \sum_m \sqrt{s(s+1) - m(m-1)}\, c^\dagger_{m-1} c_m$.
- $(S^-)^\dagger = S^+$, given by `spinplus`.
- `spindown` applies the total $S^-$ of all levels in an expression to a state and normalizes the result.
- `spinminus` is `Listable`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
spinminus[c[1]]
```
$S^-$ lowers the spin projection of a singly occupied level:
```wl
nc[spinminus[c[1]], c[CR, 1, UP], VACUUM]
```

### Applications
Applying the total $S^- = S^-_1 + S^-_2$ to the triplet state $|{\uparrow\uparrow}\rangle$ gives $\sqrt2$ times the $S_z = 0$ triplet state:
```wl
snegfermionoperators[c];
nc[spinminus[c[1]] + spinminus[c[2]], c[CR, 1, UP], c[CR, 2, UP], VACUUM]
```

### Properties & Relations
$S^+ S^- = \frac12 n + S^z - n_\uparrow n_\downarrow$ for spin 1/2; in the singly occupied subspace this is $\frac12 + S^z$:
```wl
snegfermionoperators[c];
Expand[nc[spinplus[c[]], spinminus[c[]]] - (number[c[]]/2 + spinz[c[]] - hubbard[c[]])]
```

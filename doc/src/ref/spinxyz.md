---
keywords: spin operator, spin density, Pauli matrices, electron spin, spin of a fermion level, local moment, su(2)
see also: spinx, spiny, spinz, spinplus, spinminus, spinss, spinall, spinspin, manyspin, spinxyzgen, isospinxyz, snegspinoperators
---
# spinxyz

## Usage
- `spinxyz[c[i]]` gives the list $\{S^x_i, S^y_i, S^z_i\}$ of the spin operators of the fermion level `c[i]`.
- `spinxyz[c]` is `spinxyz[c[]]`.
- `spinxyz[f]` for a pure function *f* with `f[t, σ]` the operator of type *t* (`CR` or `AN`) and spin σ gives the spin operators of the generalized spin-1/2 level described by *f*.

## Details
- For a spin-1/2 fermion `c`, the spin operators are the bilinears
  $$S^a_i = \frac{1}{2} \sum_{\sigma\sigma'} c^\dagger_{i\sigma}\, \sigma^a_{\sigma\sigma'}\, c_{i\sigma'}, \qquad a = x, y, z,$$
  with the Pauli matrices $\sigma^a$ (`PauliX`, `PauliY`, `PauliZ`) in the basis $(\uparrow, \downarrow)$. Explicitly, $S^z = \frac12 (n_\uparrow - n_\downarrow)$, $S^x = \frac12 (c^\dagger_\uparrow c_\downarrow + c^\dagger_\downarrow c_\uparrow)$, $S^y = \frac{1}{2i} (c^\dagger_\uparrow c_\downarrow - c^\dagger_\downarrow c_\uparrow)$.
- The level is specified without the creation/annihilation and spin indexes: `c[i]` stands for the operators `c[CR, i, σ]` and `c[AN, i, σ]`.
- For fermions with spin $s \neq 1/2$, declared with `snegfermionoperators[{c, s}]`, $S^a = \sum_{m m'} c^\dagger_m (S^a)_{m m'} c_{m'}$ with the spin-$s$ matrices `spinmatrixX[s]`, ... and $m = s, s-1, \ldots, -s$.
- The operators obey $[S^a, S^b] = i \varepsilon_{abc} S^c$ and commute with the number operator. They act in the singly occupied subspace as spin-1/2 operators, and annihilate the empty and the doubly occupied states.
- The results are normal ordered and not further simplified.
- `spinxyz` is `Listable`: `spinxyz[{c[1], c[2]}]` gives a list of two triples.
- The individual components are given by `spinx`, `spiny`, `spinz`; `spinplus` and `spinminus` give $S^\pm = S^x \pm i S^y$, `spinss` gives $\mathbf S^2$, and `spinall` gives $\{S^x, S^y, S^z, \mathbf S^2\}$. The total spin of several levels is given by `manyspin`.
- With a function argument, `spinxyz[f]` is `spinxyzgen[f[CR, #] &]`; the function receives the operator type and the spin index, as in `c[#1, 1, #2] &`. This describes, for instance, bonding orbitals $(c_{1\sigma} + c_{2\sigma})/\sqrt2$.
- Abstract spin operators that are not composed of fermions, e.g. impurity spins in the Kondo model, are declared with `snegspinoperators`.

## Examples
### Basic Examples
Spin operators of the level `c[]`:
```wl
snegfermionoperators[c];
spinxyz[c[]]
```
They satisfy the su(2) algebra $[S^x, S^y] = i S^z$:
```wl
{sx, sy, sz} = spinxyz[c[]];
Expand[komutator[sx, sy] - I sz]
```

### Scope
Levels with indexes:
```wl
snegfermionoperators[c];
spinxyz[c[1, k]]
```
---
`spinxyz` threads over lists of levels:
```wl
snegfermionoperators[c];
spinz /@ {c[1], c[2]} == spinxyz[{c[1], c[2]}][[All, 3]]
```
---
Spin-1 fermions (three flavors $m = 1, 0, -1$):
```wl
snegfermionoperators[{f, 1}];
spinxyz[f[]]
```
---
Generalized levels are given as functions of the operator type and the spin index. The bonding and antibonding orbitals $b_\sigma, a_\sigma = (c_{1\sigma} \pm c_{2\sigma})/\sqrt2$:
```wl
snegfermionoperators[c];
b = Function[{t, σ}, (c[t, 1, σ] + c[t, 2, σ])/Sqrt[2]];
a = Function[{t, σ}, (c[t, 1, σ] - c[t, 2, σ])/Sqrt[2]];
Expand[spinxyz[b][[3]]]
```
The spins of the two orbitals add up to the total spin of the two sites:
```wl
Expand[spinxyz[a] + spinxyz[b] - spinxyz[c[1]] - spinxyz[c[2]]]
```

### Applications
The spin operators act on the singly occupied states as spin-1/2 operators and annihilate the empty and doubly occupied states:
```wl
snegfermionoperators[d];
{sx, sy, sz} = spinxyz[d[]];
{nc[sz, d[CR, UP], VACUUM], nc[sx, d[CR, UP], VACUUM], nc[sz, d[CR, UP], d[CR, DO], VACUUM]}
```
---
Matrices of $\mathbf S^2$ in the subspaces of charge $Q = n - 1$ and spin projection $S_z$: the local moment $s(s+1) = 3/4$ exists only for single occupancy ($Q = 0$):
```wl
snegfermionoperators[c];
makematricesbzvc[spinss[c[]], qszbasisvc[{c[]}]]
```
---
The pair of creation operators $(c^\dagger_\uparrow, c^\dagger_\downarrow)$ is a spin doublet (a spherical tensor of rank 1/2): $[S^z, c^\dagger_\sigma] = \frac{\sigma}{2} c^\dagger_\sigma$, $[S^-, c^\dagger_\uparrow] = c^\dagger_\downarrow$, $[S^+, c^\dagger_\uparrow] = 0$:
```wl
snegfermionoperators[c];
Expand[{komutator[spinz[c[]], c[CR, UP]], komutator[spinz[c[]], c[CR, DO]], komutator[spinminus[c[]], c[CR, UP]], komutator[spinplus[c[]], c[CR, UP]]}]
```

### Properties & Relations
$\mathbf S^2 = \frac34 (n - 2 n_\uparrow n_\downarrow)$, which is $s(s+1) = 3/4$ in the singly occupied subspace:
```wl
snegfermionoperators[c];
{sx, sy, sz} = spinxyz[c[]];
Expand[nc[sx, sx] + nc[sy, sy] + nc[sz, sz] - 3/4 (number[c[]] - 2 hubbard[c[]])]
```
---
The spin commutes with the charge and with the isospin:
```wl
snegfermionoperators[c];
{Expand[komutator[#, number[c[]]] & /@ spinxyz[c[]]], Expand[Outer[komutator, spinxyz[c[]], isospinxyz[c[]]]]}
```
---
`spinxyz[c[]]` is `spinxyzgen` applied to the creation operators:
```wl
snegfermionoperators[c];
spinxyz[c[]] == spinxyzgen[c[CR, #] &]
```

### Possible Issues
The argument is the level, not an operator. Here `CR` and `UP` are treated as additional level indexes:
```wl
snegfermionoperators[c];
spinz[c[CR, UP]]
```
---
A function argument must take two arguments, the operator type and the spin. A function of the spin index alone gives wrong operators; use `spinxyzgen` for that form:
```wl
snegfermionoperators[c];
{spinxyz[c[CR, 1, #] &], spinxyzgen[c[CR, 1, #] &]}
```

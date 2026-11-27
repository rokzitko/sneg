---
keywords: anticommutator, canonical anticommutation relations, fermions, Grassmann numbers, Majorana fermions, Bogoliubov transformation
see also: komutator, anticommutator, superantikomutator, nc, conj, acmt, snegfermionoperators
---
# antikomutator

## Usage
- `antikomutator[a, b]` gives the anticommutator $\{a, b\} = a b + b a$.

## Details
- `antikomutator[a, b]` is `nc[a, b] + nc[b, a]`, with both products brought to the canonical order. `anticommutator` is a synonym.
- Fermion operators obey the canonical anticommutation relations $\{c_\alpha, c^\dagger_\beta\} = \delta_{\alpha\beta}$, $\{c_\alpha, c_\beta\} = 0$; Majorana operators $\{\gamma_i, \gamma_j\} = \delta_{ij}$ (SNEG normalization $\gamma^2 = 1/2$); Grassmann numbers anticommute with each other and with fermion operators.
- The result is not expanded. Use `Expand` to collect the terms; an anticommutator that is a c-number may only be reduced to it after `Expand`.
- `antikomutator` is `Listable`: `antikomutator[{a1, a2}, b]` gives `{antikomutator[a1, b], antikomutator[a2, b]}`.
- Symbols that are not declared as operators or constants are treated as operators of unknown type, and the result stays in the form `nc[a, b] + nc[b, a]`.

## Examples
### Basic Examples
Canonical anticommutation relations:
```wl
snegfermionoperators[c];
{antikomutator[c[AN, 1, UP], c[CR, 1, UP]], antikomutator[c[AN, 1, UP], c[CR, 2, UP]], antikomutator[c[CR, 1, UP], c[CR, 1, DO]]}
```

### Scope
Majorana fermions:
```wl
snegmajoranaoperators[γ];
{antikomutator[γ[1], γ[1]], antikomutator[γ[1], γ[2]]}
```
---
Grassmann numbers anticommute with each other and with fermion operators:
```wl
sneggrassmanconstants[z];
snegfermionoperators[c];
{antikomutator[z, conj[z]], antikomutator[z, c[AN, 1, UP]]}
```
---
Spin-1/2 operators obey $\{S^a, S^b\} = \delta_{ab}/2$:
```wl
snegspinoperators[{S, 1/2}];
Table[antikomutator[S[1, a], S[1, b]], {a, 3}, {b, 3}]
```
---
Operators composed of several fermion operators:
```wl
snegfermionoperators[c];
antikomutator[number[c[1]], c[CR, 1, UP]]
```
---
`antikomutator` threads over lists:
```wl
snegfermionoperators[c];
antikomutator[{c[AN, 1, UP], c[AN, 1, DO]}, c[CR, 1, UP]]
```

### Applications
Bogoliubov quasiparticles of a superconductor, $\gamma_{\uparrow} = u c_{\uparrow} - v c^\dagger_{\downarrow}$, $\gamma_{\downarrow} = u c_{\downarrow} + v c^\dagger_{\uparrow}$, obey canonical anticommutation relations if $u^2 + v^2 = 1$:
```wl
snegfermionoperators[c];
snegrealconstants[u, v];
g1 = u c[AN, UP] - v c[CR, DO];
g2 = u c[AN, DO] + v c[CR, UP];
Expand[{antikomutator[g1, conj[g1]], antikomutator[g1, conj[g2]], antikomutator[g1, g2]}]
```
---
A complex fermion is the sum of two Majorana fermions, $c = (\gamma_A + i \gamma_B)/2$, with $\gamma_A = c + c^\dagger$ and $\gamma_B = -i(c - c^\dagger)$; $\{\gamma_a, \gamma_b\} = 2\delta_{ab}$:
```wl
snegfermionoperators[c];
gA = c[AN, UP] + c[CR, UP];
gB = -I (c[AN, UP] - c[CR, UP]);
Expand[{antikomutator[gA, gA], antikomutator[gA, gB], antikomutator[gB, gB]}]
```

### Properties & Relations
For boson operators, the commutator is the canonical one:
```wl
snegbosonoperators[a];
{komutator[a[AN], a[CR]], antikomutator[a[AN], a[CR]]}
```
---
$\{a, b\} = [a, b] + 2 b a$:
```wl
snegfermionoperators[c];
x = nc[c[CR, 1, UP], c[AN, 2, UP]];
y = c[CR, 2, UP];
Expand[antikomutator[x, y] - komutator[x, y] - 2 nc[y, x]]
```

### Possible Issues
The spin operators of a fermion level obey the spin-1/2 relation $\{S^x, S^x\} = 1/2$ only for single occupancy; in general, $2 (S^x)^2 = (n - 2 n_\uparrow n_\downarrow)/2$:
```wl
snegfermionoperators[c];
{sx, sy, sz} = spinxyz[c[]];
Expand[{antikomutator[sx, sx], antikomutator[sx, sy]}]
```
---
Without `Expand`, the result may not be simplified:
```wl
snegfermionoperators[c];
snegrealconstants[u, v];
antikomutator[u c[AN, UP] - v c[CR, DO], u c[CR, UP] - v c[AN, DO]]
```
```wl
Expand[%]
```

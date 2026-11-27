---
keywords: mirror symmetry, reflection, left-right symmetry, parity, substitution rules, two-channel model
see also: transformtoLR, transformtoLRvc, makebasis
---
# lrmap

## Usage
- `lrmap[{a, b, ..., z}]` gives the substitution rules for the creation operators of the mirror reflection that maps the list of levels onto its reverse, $a \leftrightarrow z$, $b \leftrightarrow y$, ....

## Details
- The levels are given without the creation and spin indexes, e.g. `c[1]` or `d[]`. Each level is expanded into its creation operators, `c[CR, 1, UP]` and `c[CR, 1, DO]` for spin 1/2 and all spin components for other spins (as in `makebasis`). The rules map the creation operators of the k-th element to those of the k-th element from the end, with the same spin.
- The middle element of a list of odd length is mapped onto itself.
- Elements can be lists of levels. A sublist is mapped onto the sublist at the mirror position, element by element; the levels inside a sublist are not reversed. This describes, e.g., several orbitals or a chain of sites on each side.
- Only creation operators appear in the rules, which is sufficient for basis states in the creation operator representation. `transformtoLR` and `transformtoLRvc` apply the rules to the basis states.
- The reflection maps the vacuum onto itself and acts on a state $f(c^\dagger)|0\rangle$ by substitution, $P f(c^\dagger_i) |0\rangle = f(c^\dagger_{\bar i}) |0\rangle$, where $\bar i$ is the mirror image of $i$; this fixes the sign convention of the parity eigenvalues.

## Examples
### Basic Examples
Two sites exchanged by the reflection:
```wl
snegfermionoperators[c];
lrmap[{c[1], c[2]}]
```

### Scope
For an odd number of levels, the middle level is invariant; here an impurity `d` between two leads `a` and `b`:
```wl
snegfermionoperators[a, b, d];
lrmap[{a[], d[], b[]}]
```
---
Sublists are mapped as a whole; here two orbitals on each side, $a_1 \leftrightarrow b_1$ and $a_2 \leftrightarrow b_2$:
```wl
snegfermionoperators[a, b];
lrmap[{{a[1], a[2]}, {b[1], b[2]}}]
```

### Applications
The reflection applied to a two-electron state, $c^\dagger_{1\uparrow} c^\dagger_{2\uparrow} |0\rangle \to c^\dagger_{2\uparrow} c^\dagger_{1\uparrow} |0\rangle = -c^\dagger_{1\uparrow} c^\dagger_{2\uparrow} |0\rangle$: the spin-polarized triplet has odd parity:
```wl
snegfermionoperators[c];
nc[c[CR, 1, UP], c[CR, 2, UP]] /. lrmap[{c[1], c[2]}]
```

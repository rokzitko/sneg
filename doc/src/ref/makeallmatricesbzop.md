---
keywords: matrix elements, transition matrix elements, invariant subspaces, creation operator representation
see also: makeallmatricesbzvc, makematricesbzop, matrixrepresentationop, qsbasis, qszbasis
---
# makeallmatricesbzop

## Usage
- `makeallmatricesbzop[op, bz]` gives the list `{{{qn1, qn2}, mat}, ...}` of the nonzero blocks of the operator *op* between all pairs of invariant subspaces of the basis *bz*, with the basis states in the creation operator representation.

## Details
- `makeallmatricesbzop` is the variant of `makeallmatricesbzvc` for bases whose states are operator expressions (`qsbasis`, `qszbasis`, `transformtoLR`, `bzvc2bzop`). The block for the pair `{qn1, qn2}` is $M_{ij} = \langle i | \hat O | j \rangle$ with $|i\rangle$ in `qn1` and $|j\rangle$ in `qn2`, computed with `matrixrepresentationop`.
- Vanishing blocks are omitted. See `makeallmatricesbzvc` for details and caveats, in particular for $(Q, S)$ bases.
- The creation operator representation is slow; for larger bases use `bzop2bzvc` and `makeallmatricesbzvc`.

## Examples
### Basic Examples
Matrix elements of $c^\dagger_{1\downarrow}$ between the $(Q, S)$ subspaces of the Hubbard dimer:
```wl
snegfermionoperators[c];
makeallmatricesbzop[c[CR, 1, DO], qsbasis[{c[1], c[2]}]]
```

### Properties & Relations
The results agree with `makeallmatricesbzvc` in the occupation number representation:
```wl
snegfermionoperators[c];
bz = qszbasis[{c[1], c[2]}];
makeallmatricesbzop[c[CR, 1, UP], bz] == makeallmatricesbzvc[c[CR, 1, UP], bzop2bzvc[bz]]
```

# SNEG - Mathematica package for calculations with noncommuting operators of the second-quantization algebra

Copyright (C) 2002-2026 Rok Zitko

SNEG is a Wolfram Language package for symbolic calculations with
operators of second quantization, with particular emphasis on
anticommuting fermionic operators. It defines operator algebras through
transformation rules and provides utilities for constructing operators,
states, bases, and matrix representations.

At its core is `nc`, a noncommutative multiplication operation that
automatically rewrites operators into a canonical form according to
selected commutation and anticommutation rules. Conventional normal
ordering places creation operators before annihilation operators;
particle-hole and no-reordering conventions are also available.
Canonical ordering simplifies expressions and supports efficient
matrix-element evaluation in a chosen basis.

SNEG automates sign-sensitive manipulations that are otherwise tedious
and error-prone.

## Features

* Operator declarations and canonical algebras for fermionic, spinless
  fermionic, bosonic, Majorana, and spin operators, with `EMPTY`, `SEA`,
  and `NONE` ordering conventions.

* High-level constructors for particle number, spin, isospin, Nambu
  operators, Hubbard interactions, ordinary and anomalous hopping,
  projection operators, and spin-spin and charge-charge couplings.

* Canonical conjugation, commutators and anticommutators, spin inversion,
  normal ordering, vacuum expectation values, and Wick expansion.

* Occupation-number representations of states, efficient application of
  operator strings to states, and conversion between state and
  product-of-operators representations.

* Generation of symmetry-adapted bases with well-defined `Q` and `S_z`,
  `Q` and `S`, or `I` and `S`, with optional reflection parity.

* Basis manipulation, transformation, merging, decomposition, and
  orthogonalization utilities.

* Dense and sparse matrix representations of operators in a chosen
  basis.

* Symbolic sums over free and dummy indexes, including automatic index
  renaming and simplification of Kronecker deltas.

* Momentum space and periodic lattices: Fourier transformations,
  momentum conservation, lattice sums, Bloch Hamiltonians, and finite
  periodic or twisted clusters with total-momentum bases.

* Spin algebras: spin operators of given size with automatic reduction,
  Jordan-Wigner, Holstein-Primakoff, Schwinger-boson and Abrikosov
  representations, spin-cluster matrices, Stevens operators, and Hubbard
  X-operators.

* Quadratic Hamiltonians: BdG matrices in real and momentum space,
  fermionic and bosonic Bogoliubov transformations, thermal averages with
  Wick's theorem, mean-field decoupling, and self-consistent solutions.

* Schrieffer-Wolff transformation to arbitrary order for effective
  Hamiltonians (Kondo, Heisenberg, t-J models, higher-order exchange).

* Equations of motion, closed Green's functions, spectral moments (sum
  rules), and Lehmann representations of finite systems.

* Dirac bra-ket notation that can be combined with second-quantization
  operator expressions.

* Particle-hole-aware ordering and Wick contractions for filled Fermi
  seas.

* Anticommuting Grassmann variables and fermionic coherent states.

* Finite single- and multi-mode phonon bases and operators, including
  fermion-phonon tensor-product bases.

* Selected Baker-Hausdorff simplifications and noncommutative operator
  power-series expansions.

## Applications

SNEG provides the symbolic foundation of
[NRG Ljubljana](https://github.com/rokzitko/nrgljubljana), a framework
for numerical renormalization group calculations for quantum impurity
problems such as Kondo and Anderson impurity models. It has also been
used for exact diagonalization of Hubbard clusters, high-order
perturbation theory, and commutators of complex operator expressions.

The package is also suitable for teaching second-quantization methods:
it exposes the algebra while automating lengthy sign-sensitive steps.
The [`examples`](examples/) directory contains notebooks that can be
adapted to more involved calculations.

## Installation

SNEG is a Wolfram Language package and requires no compilation. With
Mathematica 12.1 or newer, install or update the latest release directly
from Mathematica:

```wl
PacletInstall[
  "https://github.com/rokzitko/sneg/releases/latest/download/SNEG.paclet"
]
```

Load the installed package and check its version with:

```wl
Needs["sneg`"]
$SnegVersion
```

The canonical capitalization `Needs["Sneg`"]` is also supported.

Remove the installed paclet with `PacletUninstall["SNEG"]`.

For Mathematica versions 7 through 12.0, download the
[main branch archive](https://github.com/rokzitko/sneg/archive/refs/heads/main.zip)
or clone the repository. Place it in the per-user application directory
returned by:

```wl
FileNameJoin[{$UserBaseDirectory, "Applications", "sneg"}]
```

The directory must be named `sneg`; rename an extracted `sneg-main`
directory if necessary. Load a legacy installation with `<< sneg``.
A checkout at any location can also be loaded directly:

```wl
Get["/absolute/path/to/sneg/sneg.m"]
```

## Documentation

The paclet installs English help pages for Mathematica 14.0 and later in the Wolfram Documentation Center: search
for a SNEG function or open the guide page `paclet:SNEG/guide/SNEG`, which is
also linked from the startup message in notebooks. The help pages are
generated from the sources in [`doc/src`](doc/src); see
[`doc/README.md`](doc/README.md) for the source format and for building the
help pages from a checkout.

The [stand-alone tutorial and reference manual](doc/manual/sneg-manual.tex)
includes worked solid-state models, exercises, and solutions. Build the PDF
with `wolframscript -file doc/manual/build-manual.wl` or open one of its
[chapter notebooks](doc/notebooks/) in Mathematica.

Older function-reference notebooks are available in
[`docs/Documentation/English`](docs/Documentation/English/), including a
legacy [`quickstart.nb`](docs/Documentation/English/quickstart.nb). They
were created for Mathematica's former help browser and should be opened
directly; the old "Rebuild Help Index" integration is not supported by
modern Mathematica versions.

Additional material includes:

* Example notebooks and scripts in [`examples`](examples/).
* [Momentum space and lattices](README_momentum.md),
  [spin algebras](README_spin.md),
  [quadratic forms and mean-field theory](README_quadratic.md),
  [Schrieffer-Wolff transformation](README_schriefferwolff.md),
  [equations of motion and Green's functions](README_greens.md), and
  [matrix-valued hopping](README_matrixhop.md).
* The early minimalist [SNEG manual](docs/manual/manual.pdf).
* The bundled preprint of the
  [Computer Physics Communications article](docs/CPC_paper/paper.pdf).

When publishing work that uses SNEG, please cite Rok Zitko, "SNEG -
Mathematica package for symbolic calculations with
second-quantization-operator expressions," *Computer Physics
Communications* **182** (2011), 2259-2264,
[doi:10.1016/j.cpc.2011.05.013](https://doi.org/10.1016/j.cpc.2011.05.013).

## License

SNEG is free software distributed under the GNU General Public License,
version 2 or, at your option, any later version. See
[`LICENSE`](LICENSE) for the license text.

## Contributing to SNEG

Bug reports, fixes, new regression tests, and documentation improvements
are welcome through
[GitHub issues](https://github.com/rokzitko/sneg/issues) and
[pull requests](https://github.com/rokzitko/sneg/pulls).

Run the core regression suite from the repository root with:

```sh
wolframscript -file test-sneg.m
```

The exact-diagonalization example has a separate regression suite:

```sh
wolframscript -file examples/test-diagonalize.m
```

Build the release paclet with:

```sh
wolframscript -file build-paclet.m
```

## Compatibility

The current SNEG code requires Mathematica 7 or newer. It has been
tested with Mathematica versions through 15.

## Contact information

GitHub repository: <https://github.com/rokzitko/sneg>

* Rok Zitko
* "Jozef Stefan" Institute, F1 - Theoretical physics
* Jamova 39, SI-1000 Ljubljana, Slovenia
* Email: [rok.zitko@ijs.si](mailto:rok.zitko@ijs.si)

# SNEG - Mathematica package for calculations with non-commuting operators of the second quantization algebra

Copyright (C) 2006-2023 Rok Zitko


The SNEG library is a package for Mathematica computer algebra
system. It provides a framework for performing calculations using the
operators of the second quantisation with an emphasis on the
anti-commuting fermionic operators. It consists of a collection of
transformation rules that define the algebra of operators and a number
of utility functions.

The foundation is a definition of non-commutative multiplication with
automatic reordering of operators in a standard form (usually the
conventional normal ordering with creation operators preceding the
annihilation operators), which takes into account selected
(anti)commutation rules. Standard form reordering allows
simplifications of expressions and the choice of normal ordering
permits efficient evaluation of matrix elements in a given basis.

The library makes otherwise tedious calculations a routine
operation. Especially, it prevents inauspicious sign errors when
commuting fermionic operators.


## Features

* Collection of utility functions that generate various operator
expressions, such as electron number, electron spin and isospin,
1-electron and 2-electron hopping, projection operators, spin-spin and
charge-charge inter-site coupling, etc. These functions can be applied
to construct the Hamiltonian and operators for observables.

* Manipulation of operator expressions: canonical conjugation, spin
inversion.

* Calculation of vacuum expectation values of operator expressions.

* Occupation-number representation of states and evaluation of
operator-vector expressions. Occupation-number representations allows
great speed-up in applying a string of operators on a basis state.

* Transformations from product-of-operators to occupation-number
representations of states and vice-versa.

* Generation of basis states with well-defined particle number Q and
spin projection Sz, well-defined number Q and spin S, or well-defined
isospin I and spin S. For models with reflection symmetry, a parity
quantum number can also be introduced. 

* Utility functions for manipulating sets of basis states: conversions
between various representations, mapping a function to each state,
transformations of basis, merging several sets of basis states,
orthogonalization, etc.

* Generation of matrix representations of operators in a given basis

* Support for free (dummy) indexes and summed-over indexes: it is easy
to write multiple sums over wave-numbers k_i and spins
sigma_i. Automatic simplifications can be performed in such sums,
which take into account that multiple summed-over indexes can be
interchanged, etc.

* Support for Dirac's bra-ket notation. Bra-ket notation can be
intermixed with the second-quantization operators notation.

* Distinction between particle and hole operators. This distinction is
used in the standard normal ordering (creation operators are those
that create a particle or a hole) and in the applications of the Wick
theorem (see next entry).

* Simplifications using Wick's theorem, in particular calculation of
the ground state (vacuum) expectation values.

* Support for commuting bosonic operators.

* Support for anti-commuting Grassman variables and fermionic coherent states.

* Support for real (Majorana) fermions.

* Support for spin operators.

* Automatic simplification of expressions with exponential functions
of operators using the Baker-Campbell-Hausdorff formula.

* Built-in support for pretty printing of operator expressions,
obviating the need to use the Notation package. Colors are used to
further improve readability.

* Code for rewritting an operator expression in terms of higher-level
functions, such as number, hopping, electron-electron repulsion, spin,
etc. operators.

* Support for converting compact ASCII operator-string expressions to
the SNEG internal representation and vice-versa. This functionality is
currently in the testing stage. See the examples in the file
snegtoascii_asciitosneg.nb.


## Applications

SNEG forms the basis of "NRG Ljubljana", a framework for performing
numerical renormalization group calculations for quantum impurity
problems, such as Kondo and Anderson impurity models
(http://nrgljubljana.ijs.si/). In the past, it has also been applied
to perform exact diagonalizations on Hubbard clusters, perturbation
theory to higher orders and calculation of commutators of complex
operator expressions. It should also be suitable for educational
purposes, since it simplifies tedious calculations with
second-quantization operators, much like Mathematica simplified
learning calculus. A number of examples is included in the SNEG
library distribution; they can easily be extended to non-trivial
calculations.

## Installation

Download the latest version from the main branch on github or [here](https://github.com/rokzitko/sneg/archive/refs/heads/master.zip). The package can be installed by extracting to your  '$InstallationDirectory\AddOns\Applications\' directory. The installation directoy can be found by running `$InstallationDirectory` in a Wolfram language kernel. Alternatively, the package can be installed through the Mathematica provided GUI by going to `File > Install... > type: Application > Source: From directory` and selecting the downloaded `sneg` folder. (Note: rename the directory to `sneg` before doing the File > Install, i.e., remove any `-main` or `-2.0.0` parts of the directory name. Mathematica does not seem to like minus signs in the paths.)

The package is correctly installed if one can run 
```
<< sneg`
```

## Documentation

Documentation for SNEG library is located in Documentation/English
directory. This directory should be copied to your local Applications
directory, then merged in your Mathematica help system using Rebuild
Help Index in Help menu. The relevant pages are then located in
"Add-ons & Links" tab, in the section "sneg documentation".

In the directory docs/ there is an older version of a SNEG manual
and a long version of an article describing the SNEG package.

A number of example Mathematica notebooks can be found in the
directory examples/.


## License

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

The full text of the GPL General Public License can be found
in file LICENSE.


## Contributing to SNEG

If you make improvements to SNEG, you are encouraged to share them
with other users. Bug reports (and fixes) are very welcome as well.
The contact information is in the next section.

Interesting directions for possible further extensions are improved
support for bosonic operators (basis construction, simplifications in
the case of bosons); performance improvements; code simplifications;
and improved documentation.


## Compatibility

SNEG was mostly developed and tested using Mathematica 5.2. It was
also tested to work under Mathematica 5.0 and 5.1, as well as under
new versions 6, 7, and 8. The author tries his best to make SNEG
compatible across different versions of Mathematica.


## Contact information:

SNEG library home-page: http://nrgljubljana.ijs.si/sneg

Github repository: https://github.com/rokzitko/sneg

Rok Zitko
"Jozef Stefan" Institute
F1 - Theoretical physics
Jamova 39
SI-1000 Ljubljana
Slovenia


  rok.zitko@ijs.si (preferred contact address)


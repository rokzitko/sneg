(*
   SNEG - Mathematica package for calculations with non-commuting
   operators of the second quantization algebra

   Copyright (C) 2002-2026 Rok Zitko

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

   Contact information:
   Rok Zitko
   F1 - Theoretical physics
   "Jozef Stefan" Institute
   Jamova 39
   SI-1000 Ljubljana
   Slovenia

   rok.zitko@ijs.si
*)

BeginPackage["Sneg`"];

snegidstring = "sneg.m 2.2.0 Nov 2026";
snegcopyright = "Copyright (C) 2002-2026 Rok Zitko";

$SnegVersion = StringSplit[snegidstring][[2]];
$SnegVersion::usage = "$SnegVersion is the version of sneg package used.";

(* In notebooks, the startup message links to the help pages if they are
installed (paclet installation or a checkout with built documentation). *)
If[TrueQ[$Notebooks] &&
   StringQ[Quiet[Documentation`ResolveLink["paclet:SNEG/guide/SNEG"]]],
  Print[Row[{"sneg " <> $SnegVersion <> " " <> snegcopyright <> "  ",
    Hyperlink["Documentation \[RightGuillemet]", "paclet:SNEG/guide/SNEG"]}]],
  Print["sneg " <> $SnegVersion <> " " <> snegcopyright]
];

(*
A few NAMING CONVENTIONS (that I try to follow most of the time)
for function arguments and patterns:

  z - general complex constant/variable or general Grassman constant/variable
  x - general real constant/variable
  op - operators
  a,b,c - general expressions
  i - first index == type, AN=1 for annihilation, CR=0 for creation
  j - other indexes, site, spin etc.
  k - operator indexes, including the first (type) index
    - an exception are SEA ordered operators, where k is used to
      denote the second (wavenumber) index [for convenience]
  sp, sigma - spin, UP=1 or DO=0
  v - state in number representation

  l - list
  i - integer

Every Fermion operator has at least one index, which specifies whether it
is a creation or annihilation operator. Therefore a generic pattern
is op_[i_, j___]. Moreover, if the operator contains a spin index,
it is by convention always the last index!
*)


(**** Usage messages ****)
(* Set a usage message. The help pages are linked from the usage messages by
the front end. The third argument (formerly the target of the "More..."
button of the old help browser) is ignored. The pattern names are kept in the
private context. *)
UsageWithMore;
Begin["`Private`"];
SetAttributes[UsageWithMore, HoldFirst];
UsageWithMore[symbol_, string_, tag_:Null] := (symbol::usage = string);
End[];

UsageWithMore[snegfermionoperators,
"snegfermionoperators[a, b, ...] declares a, b, ... as spin-1/2 fermion
operators with canonical anticommutation relations.
snegfermionoperators[{a, s}] declares a fermion operator with spin s."];
UsageWithMore[snegbosonoperators,
"snegbosonoperators[a, b, c] declares a, b, and c to be
bosonic operators. They obey canonical commutation relations."];
UsageWithMore[snegmajoranaoperators,
"snegmajoranaoperators[a, b, c] declares a, b, and c to be
real (Majorana) fermionic operators."];
UsageWithMore[snegrealconstants,
"snegrealconstants[x, y] declares x and y to be real constants.
Constants are factored out from operator strings within nc[] blocks.
They are invariant under the conjugation."];
UsageWithMore[snegcomplexconstants,
"snegcomplexconstants[z, w] declares z and w to be complex constants.
Constants are factored out from operator strings within nc[] blocks."];
UsageWithMore[sneggrassmanconstants,
"sneggrassmanconstants[z, w] declares z and w to be anti-commuting
Grassman quantities."];
UsageWithMore[snegfreeindexes,
"snegfreeindexes[k, sigma] declares k and sigma to be indexes. This is
taken into account when simplifying sum[] blocks."];
UsageWithMore[nc,
"nc[a, b, ...] is the noncommutative product of the operators a, b, ...."];
UsageWithMore[fermionQ,
"fermionQ[c] returns True if c is a fermionic operator."];
UsageWithMore[bosonQ,
"bosonQ[c] returns True if c is a bosonic operator."];
UsageWithMore[operatorQ,
"operatorQ[c] returns True if c is an operator."];
UsageWithMore[ordering,
"ordering[c] = EMPTY | SEA | NONE determined how the fermionic operators
are reordered by defining the vacuum state that corresponds to
the operator c. EMPTY corresponds to an empty state vacuum, while
SEA denotes a Fermi sea filled up to a Fermi level. NONE turns off
automatic reodering for operator c."];
UsageWithMore[snegOrderedQ,
"snegOrderedQ[op1[i1], op2[i2]] returns True if operators op1[i1] and
op2[i2] are canonically ordered."];
UsageWithMore[acmt,
"acmt[op1[i1], op2[i2]] defines the value of the anti-commutator between
op1[i1] and op2[i2]. By default, the value is given by the canonical
anti-commutator for fermionic operators."];
UsageWithMore[cmt,
"cmt[op1[i1], op2[i2]] defines the value of the commutator between
op1[i1] and op2[i2]. By default, the value is given by the canonical
commutator for bosonic operators."];
UsageWithMore[grassmanQ,
"grassmanQ[z] returns True if z is a Grassman constant or variable."];
UsageWithMore[komutator,
"komutator[a, b] gives the commutator nc[a, b] - nc[b, a]."];
UsageWithMore[commutator,
"commutator[a, b] gives the commutator nc[a, b] - nc[b, a]."];
UsageWithMore[antikomutator,
"antikomutator[a, b] gives the anticommutator nc[a, b] + nc[b, a]."];
UsageWithMore[anticommutator,
"anticommutator[a, b] gives the anticommutator nc[a, b] + nc[b, a]."];
UsageWithMore[superkomutator,
"superkomutator[a, b, n] = [a, [a, ... [a, b]]], this is order-n nested
commutator."];
UsageWithMore[supercommutator,
"supercommutator[a, b, n] = [a, [a, ... [a, b]]], this is order-n nested
commutator.", "superkomutator"];
UsageWithMore[superantikomutator,
"superantikomutator[a, b, n] = {a, {a, ... {a, b}}}, this is order-n nested
anti-commutator.", "superkomutator"];
UsageWithMore[superanticommutator,
"superanticommutator[a, b, n] = {a, {a, ... {a, b}}}, this is order-n nested
anti-commutator.", "superkomutator"];
UsageWithMore[dd,
"dd[expr] denotes the double dots surrounding an expresions, i.e.
a normal ordered string"];
UsageWithMore[normalorder,
"normalorder[expr] normal orders an expression by subtracting its vacuum
expectation value (vev)."];
UsageWithMore[normalorderwick,
"normalorderwick[expr] normal orders an expression by subtracting its
vacuum expectation value (vev). Wick's theorem is used to compute the vev.",
"normalorder"];
UsageWithMore[conj,
"conj[expr] conjugates an expression."];
UsageWithMore[sum,
"sum[expr, {indexes}] denotes a symbolic sum of an expression
over indexes."];
UsageWithMore[sumThread,
"sumThread threads sum[{expr1, expr2}, {indexes}] over the list in the first
argument."];
UsageWithMore[sumExpand,
"sumExpand[expr] expands sums over the addends."];
UsageWithMore[sumCollect,
"sumCollect[expr] collects sums over the addends."];
UsageWithMore[sumSimplifyKD,
"sumSimplifyKD[expr] simplifies sums. In particular, it correctly
handles KroneckerDelta functions."];
UsageWithMore[sumFullSimplify,
"sumFullSimplify[expr] attempts to simplify an expression involving
symbolic sums."];
UsageWithMore[vev,
"vev[expr] returns the vacuum expectation value of an operator expression."];
UsageWithMore[inner,
"inner[a, b] returns the inner product between a and b using
nc[] multiplication."];
UsageWithMore[outer,
"outer[a, b] returns the outer (direct) product between a and b
using nc[] multiplication."];
UsageWithMore[VMV,
"VMV[v1, m, v2] computes the vector-matrix-vector product using nc[] inner
product multiplication."];
UsageWithMore[number,
"number[c[i]] returns the number operator corresponding to the operator
c[i]. number[c[i], sigma] with sigma=UP | DO returns the number operator
for a given spin projection only."];
UsageWithMore[hubbard,
"hubbard[c[i]] gives the on-site Hubbard interaction n_up n_down of the
fermion level c[i]."];
UsageWithMore[hamiltonian,
"hamiltonian[type, ...] is a stub: it returns the Hamiltonian 'type'."];
UsageWithMore[chargecharge,
"chargecharge[c[i], d[j]] returns the charge-charge repulsion operator
corresponding to operators (sites) c[i] and d[j]."];
UsageWithMore[PauliX, "Pauli's X matrix"];
UsageWithMore[PauliY, "Pauli's Y matrix", "PauliX"];
UsageWithMore[PauliZ, "Pauli's Z matrix", "PauliX"];
UsageWithMore[PauliPlus, "Pauli's + (spin raising) matrix", "PauliX"];
UsageWithMore[PauliMinus, "Pauli's - (spin lowering) matrix", "PauliX"];
UsageWithMore[spinxyz,
"spinxyz[op[j]] returns the x, y, and z components of the S=1/2 spin
operator corresponding to a fermionic S=1/2 operator op[j]."];
UsageWithMore[spinall,
"spinall[op[j]] returns the x, y, and z components of a S=1/2 spin
operator, as well as the S^2 operator, corresponding to a fermionic
operator op[j].", "spinxyz"];
UsageWithMore[spinss,
"spinss[op[j]] returns the S^2 total S=1/2 spin operator squared that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spinx,
"spinx[op[j]] returns the x component of a S=1/2 spin operator that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spiny,
"spiny[op[j]] returns the y component of a S=1/2 spin operator that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spinz,
"spinz[op[j]] returns the z component of a S=1/2 spin operator that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spinplus,
"spinplus[op[j]] returns the S=1/2 spin raising operator that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spinminus,
"spinplus[op[j]] returns the S=1/2 spin lowering operator that
corresponds to a fermionic operator op[j].", "spinxyz"];
UsageWithMore[spinspin,
"spinspin[a[i], b[j]] returns the spin-spin scalar product between
two S=1/2 spin operators corresponding to fermionic operators a[i]
and b[j]."];
UsageWithMore[spinspinpm,
"spinspinpm[a[i], b[j]] returns the +- part of the scalar product
between two S=1/2 spin operators corresponding to fermionic operators
a[i] and b[j].", "spinspin"];
UsageWithMore[spinspinmp,
"spinspinmp[a[i], b[j]] returns the -+ part of the scalar product
between two S=1/2 spin operators corresponding to fermionic operators
a[i] and b[j].", "spinspin"];
UsageWithMore[spinspinxy,
"spinspinxy[a[i], b[j]] returns the transverse part of the scalar product
between two S=1/2 spin operators corresponding to fermionic operators
a[i] and b[j].", "spinspin"];
UsageWithMore[spinspinz,
"spinspinxy[a[i], b[j]] returns the longitudinal part of the scalar product
between two S=1/2 spin operators corresponding to fermionic operators
a[i] and b[j].", "spinspin"];
UsageWithMore[manyspin,
"manyspin[{a[i], b[j],...}] returns the x, y, and z components of the
total spin, as well as the total spin operator squared, corresponding
to a list of operators given as the argument."];
UsageWithMore[halfintegerQ,
"halfintegerQ[z] returns True if z is an integer or a half-integer."];
UsageWithMore[spinmatrixX,
"spinmatrixX[S] returns the matrix of the X component of the spin
operator for arbitrary spin S."];
UsageWithMore[spinmatrixY,
"spinmatrixY[S] returns the matrix of the Y component of the spin
operator for arbitrary spin S.", "spinmatrixX"];
UsageWithMore[spinmatrixZ,
"spinmatrixZ[S] returns the matrix of the Z component of the spin
operator for arbitrary spin S.", "spinmatrixX"];
UsageWithMore[spinmatrixP,
"spinmatrixP[S] returns the matrix of the spin-raising operator
for arbitrary spin S.", "spinmatrixX"];
UsageWithMore[spinmatrixM,
"spinmatrixM[S] returns the matrix of the spin-lowering operator
for arbitrary spin S.", "spinmatrixX"];
UsageWithMore[direct,
"direct[b1, b2] returns a direct product of two sets of operator
expressions (nc[] multiplication is used)."];
UsageWithMore[nambu,
"nambu[c[i], nr] returns the Nambu-spinor components corresponding
to a fermionic operator c[i]. The phase is (-1)^nr. The default
value of nr is 0."];
UsageWithMore[isospinxyz,
"isospinxyz[c[i], nr] returns the isospin operator corresponding
to the fermionic operator c[i]. The phase is (-1)^nr. The default
value of nr is 0."];
UsageWithMore[isospin,
"isospin[c[i], nr] returns the isospin operator corresponding
to the fermionic operator c[i]. The phase is (-1)^nr. The default
value of nr is 0.", "isospinxyz"];
UsageWithMore[nnop,
"nnop[op] is used to define the lattice index that determines
the bipartite lattice used for the isospin operators.", "isospinxyz"];
UsageWithMore[isospinx,
"isospinx[c[i], nr] returns the x component of the isospin operator
corresponding to the fermionic operator c[i]. The phase is (-1)^nr.
The default value of nr is 0.", "isospinxyz"];
UsageWithMore[isospiny,
"isospiny[c[i], nr] returns the y component of the isospin operator
corresponding to the fermionic operator c[i]. The phase is (-1)^nr.
The default value of nr is 0.", "isospinxyz"];
UsageWithMore[isospinz,
"isospinz[c[i], nr] returns the z component of the isospin operator
corresponding to the fermionic operator c[i]. The phase is (-1)^nr.
The default value of nr is 0.", "isospinxyz"];
UsageWithMore[isospinplus,
"isospinplus[c[i], nr] returns the isospin-raising component of the
isospin operator corresponding to the fermionic operator c[i].
The phase is (-1)^nr. The default value of nr is 0.", "isospinxyz"];
UsageWithMore[isospinminus,
"isospinminus[c[i], nr] returns the isospin-lowering component of the
isospin operator corresponding to the fermionic operator c[i].
The phase is (-1)^nr. The default value of nr is 0.", "isospinxyz"];
UsageWithMore[manyisospin,
"manyisospin[{ops}, {phases}] returns the X, Y, and Z components
of the total isospin operator for a list of fermionic operators.
The phases of operators must be given as the second argument.
The total isospin squared operator is also returned."];
UsageWithMore[basis,
"basis[op[i]] returns the basis states (creation operators) for
a single site, i.e. for operator op[i]."];
UsageWithMore[pow,
"pow[expr, i] raises an expression to the i-th power using the non-commutative
multiplication nc."];
UsageWithMore[hop,
"hop[a[i], b[j]] returns the electron hopping operator between
sites a[i] and b[j]. hop[a[i], b[j], sigma] with sigma=UP | DO
does the same for a single spin projection sigma."];
UsageWithMore[matrixhop,
"matrixhop[T, a[i], b[j]] returns a^dagger . T . b plus its Hermitian
conjugate, summed over both spin indices. T is a dense or
sparse 2 by 2 matrix in the spin basis {UP, DO}. The operators must be
spin-1/2 fermions or creation-defined abstract-orbital Functions.
Pauli decomposition is used to simplify symbolic matrices, including
results of MatrixExp, under the current $Assumptions.", "hop"];
UsageWithMore[anomaloushop,
"anomaloushop[a[i], b[j]] returns the anomalous hopping operator between
sites a[i] and b[j]."];
UsageWithMore[holehop,
"holehop[h[i], p[j]] returns the electron hopping operator for
the case of a hole operator h[i] and particle operator p[j]."];
UsageWithMore[spinfliphop,
"spinfliphop[a[i], b[j]] returns the electron hopping with spin flip
operator between sites a[i] and b[j]."];
UsageWithMore[twohop,
"twohop[a[i], b[j]] returns the two-particle hopping operator
between sites a[i] and b[j]."];
UsageWithMore[invertspin,
"invertspin[expr] inverts the spin of the operator expression expr.
The spin index must be the last index of each operator appearing
in the expression."];
UsageWithMore[makebasis,
"makebasis[ops] constructs a basis of single-particle creation operators
for enlisted operators ops."];
UsageWithMore[goodopQ,
"goodopQ[c[i]] returns True if the operator c[i] appears in the basis
declared by makebasis."];
UsageWithMore[op2ndx,
"op2ndx[c[i]] returns the index of operator c[i] in the basis declared
by makebasis."];
UsageWithMore[ndx2op,
"ndx2op[ndx] returns the creation operator corresponding to
the index ndx in the basis declared by makebasis."];
UsageWithMore[vacuum,
"vacuum[] returns the vacuum vector in the occupation number (vc)
representation for a basis that has been previously defined using makebasis.",
"vacuumlc"];
UsageWithMore[ap,
"ap[op, vc] applies an operator op to a vector vc in the occupation
number representation."];
UsageWithMore[vc,
"vc[...] is a wrapper (vector) that holds the information about the occupancy
of the individual sites in the occupation number representation."];
UsageWithMore[vc2ops,
"vc2ops[expr] transforms vectors in the occupation-number representation
in expression 'expr' to the corresponding operator strings."];
UsageWithMore[scalarproductvc,
"scalarproductvc[a, b] computes the scalar product between two expressions
consisting of vectors in the occupation number representation."];
UsageWithMore[scalarproductop,
"scalarproductop[a, b] computes the vacuum expectation value of the
product between operator expressions a and b. The expression on the
left, a, is automatically conjugated."];
UsageWithMore[braketvc,
"braketvc[a, op, b] computes the braket <a|op|b>, where op is an operator
expression, while a and b are some states (in the occupation number
representation)."];
UsageWithMore[braketop,
"braketop[a, op, b] computes the braket <a|op|b>, where op is an operator
expression, while a and b are some states (in the creation operator
representation).", "braketvc"];
UsageWithMore[expvop,
"expvop[op, a] computes the braket <a|op|a>, where op is an
operator expression and a is some state (in the creation operator
representation).", "braketvc"];
UsageWithMore[expvvc,
"expvvc[op, a] computes the braket <a|op|a>, where op is an operator
expression and a is some state (in the occupation number representation).",
"braketvc"];
UsageWithMore[matrixrepresentationvc,
"matrixrepresentationvc[op, basis] computes the matrix representation
of the operator op in a given basis (occupation number representation).
matrixrepresentationvc[op, basisL, basisR] returns the matrix
representation of an operator in the case of different basis for
left-multiplication and right-multiplication."];
UsageWithMore[matrixrepresentationvcfast,
"matrixrepresentationvcfast[op, basis] computes the matrix representation
of the operator op in a given basis (occupation number representation).",
"matrixrepresentationvc"];
UsageWithMore[matrixrepresentationvcsparse,
"matrixrepresentationvcsparse[op,  basis] computes the matrix representation
of the operator op in a given basis (occupation number representation).
A sparse matrix is returned.",
"matrixrepresentationvc"];
UsageWithMore[matrixrepresentationop,
"matrixrepresentationop[op, basis] computes the matrix representation
of the operator op in a given basis (creation operator representation).",
"matrixrepresentationvc"];
UsageWithMore[makematricesbzop,
"makematricesbzop[op, bz] produces a table of operator matrices in all
invariant subspaces of the basis bz (creation operator representation)."];
UsageWithMore[makematricesbzvc,
"makematricesbzvc[op, bz] produces a table of operator matrices in all
invariant subspaces of the basis bz (occupation number representation).",
"makematricesbzop"];
UsageWithMore[bzvc2bzop,
"Transform a basis in occupation number representation
to a basis in creation operator representation."];
UsageWithMore[bzop2bzvc,
"Transform a basis in creation operator representation
to a basis in occupation number representation.", "bzvc2vzop"];
UsageWithMore[qszbasis,
"qszbasis[{ops}] returns the basis with well defined charge and
spin projection quantum numbers (Q,S_z) in creation operator representation."];
UsageWithMore[qszbasisvc,
"qszbasisvc[{ops}] returns the basis with well defined charge and
spin projection quantum numbers (Q,S_z) in occupation number representation.",
"qszbasis"];
UsageWithMore[qsbasis,
"qsbasis[{ops}] returns the basis with well defined charge and
total spin quantum numbers (Q,S) in creation operator representation.
Operators ops must be spin-1/2 fermion operators."];
UsageWithMore[qsbasisvc,
"qsbasisvc[{ops}] returns the basis with well defined charge and
total spin quantum numbers (Q,S) in occupation number representation.
Operators ops must be spin-1/2 fermion operators."];
UsageWithMore[zeroonvac,
"zeroonvac[expr] drops vacuum-annihilating parts of expression expr."];
UsageWithMore[VACUUM,
"VACUUM is a placeholder for vacuum in operator expressions. If an
annihilation operator stands just before VACUUM, the expression is zero.
Likewise, if a creation operators stands just after conj[VACUUM], the
expression drops. VACUUM is normalized to 1, i.e.
vev[nc[conj[VACUUM], VACUUM]] = 1."];
UsageWithMore[projector,
"projector[op[j], PROJ] returns the projection operator corresponding
to the fermionic operator op[j]. PROJ = PROJ0 | PROJUP | PROJDO
| PROJ2 | PROJ1 | PROJ02."];
UsageWithMore[projector0,
"projector0[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with zero
occupancy.", "projector"];
UsageWithMore[projectorUP,
"projectorUP[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with a single
spin up particle.", "projector"];
UsageWithMore[projectorDO,
"projectorDO[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with a single
spin down particle.", "projector"];
UsageWithMore[projector2,
"projector2[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with double
occupancy.", "projector"];
UsageWithMore[projector1,
"projector1[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with single
occupancy.", "projector"];
UsageWithMore[projector02,
"projector02[op[j]] returns the projection operator corresponding to the
fermionic operator op[j]. It projects to the subspace with either zero
or double occupancy.", "projector"];
UsageWithMore[decomposevc,
"decomposevc[vec, basis] decomposes a vector into components with
respect to the given basis in the occupation number representation."];
UsageWithMore[decomposeop,
"decomposeop[vec, basis] decomposes a vector into components with
respect to the given basis in the creation operator representation.",
"decomposevc"];
UsageWithMore[orthogvc,
"orthogvc[vecs, basis] orthogonalizes a list of vectors vecs in the
occupation number representation basis 'basis'."];
UsageWithMore[orthogop,
"orthogop[vecs, basis] orthogonalizes a list of vectors vecs in the
creation operator representation basis 'basis'.", "orthogvc"];
UsageWithMore[transformQStoIS,
"transformQStoIS[basis] transforms from a basis with well defined (Q,S)
quantum numbers to a basis with well defined (I,S) quantum numbers."];
UsageWithMore[bra,
"bra[i] denotes the Dirac bra <i|."];
UsageWithMore[ket,
"ket[i] denotes the Dirac ket |i>.", "bra"];
UsageWithMore[braketrule,
"braketrule[bra[i], ket[j]] defines the value of the braket <i|j>."];
UsageWithMore[spinbra,
"spinbra[S] returns the spin basis bras for spin S."];
UsageWithMore[spinket,
"spinket[S] returns the spin basis kets for spin S.", "spinbra"];
UsageWithMore[spinketbraX,
"spinketbraX[S] returns the X-component of the spin operator for spin S
in the bra-ket representation."];
UsageWithMore[spinketbraY,
"spinketbraY[S] returns the Y-component of the spin operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinketbraZ,
"spinketbraZ[S] returns the Z-component of the spin operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinketbraP,
"spinketbraP[S] returns the spin raising operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinketbraM,
"spinketbraM[S] returns the spin lowering operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinbasis,
"spinbasis[S] returns the basis for spin S in the ket representation.", "spinbra"];
UsageWithMore[transformfunc,
"transformfunc[states, op] applies op to the occupation-vector states, projects back onto their span, and orthonormalizes."];
UsageWithMore[transformbasis,
"transformbasis[basis, op] applies transformfunc to each sector of an occupation-vector basis and drops empty sectors."];
UsageWithMore[mergebasis,
"mergebasis[{basis1, basis2,...}] merges several sets of basis states."];
UsageWithMore[applybasis,
"applybasis[b, fn] applies function fn to each basis state in the
basis b."];
UsageWithMore[transformtoPH,
"transformtoPH[b, {Nph,...}] generates a direct product of a basis b for
fermions with a basis with up-to Nph excited local phonons, i.e. Nph
is the phonon number cutoff."];
UsageWithMore[phononnumber,
"phononnumber[Nph] returns the phonon number operator in the bra-ket
representation. Nph is the phonon number cutoff."];
UsageWithMore[phononplus,
"phononplus[Nph] returns the phonon raising operator in the bra-ket
representation. Nph is the phonon number cutoff."];
UsageWithMore[phononminus,
"phononminus[Nph] returns the phonon lowering operator in the bra-ket
representation. Nph is the phonon number cutoff.", "phononplus"];
UsageWithMore[phononx,
"phononx[Nph] returns the phonon displacement operator (a^dag+a)
in the bra-ket representation. Nph is the phonon number cutoff.",
"phononplus"];
UsageWithMore[transformtoLR,
"transformtoLR[b, map] transforms the basis b to a basis with well
defined parity. map is the list of the operators so that the sites
are symmetric with respect to the middle of this list."];
UsageWithMore[snegold2newrules,
"Given a set of old operators o, a set of new operators n and
a set of operator expressions for the new basis, snegold2newrules[o, n, r]
produces the transformation rules from the old basis to the new basis."];
UsageWithMore[wick,
"wick[expr] returns an operator string expressed using normal ordered
strings and contractions. This is an application of Wick's theorem."];
UsageWithMore[contraction,
"contraction[op1, op2] contracts op1 and op2 according to the
standard definition."];
UsageWithMore[vevwick,
"vevwick[expr] calculates the vacuum expectation value of an operator
expression using Wick's theorem.", "vev"];
UsageWithMore[isozsq,
"isozsq[c[i]] gives (n - 1)^2, four times the square of the z-component of
the isospin of the level c[i], the particle-hole symmetric form of the
on-site interaction."];
UsageWithMore[spinspinsymmetric,
"spinspinsymmetric[{a,b,...}] returns the symmetrized spin-spin
scalar product 1/N Sum(S_a . S_b) between pairs of S=1/2 spin
operators corresponding to the levels described by the opreators
a,b,... Here N is the number of pairs."];
UsageWithMore[maskOp,
"maskOp[op, margs] calls function op while all constants declared to
sneg are masked. This prevents some interferences between sneg and
certain Mathematica functions."];
UsageWithMore[quickISObasis,
"quickISObasis[basisops] generates an (I,S) basis."];
UsageWithMore[majoranaQ,
"majoranaQ[z] returns True if z is a Majorana fermionic operator."];
UsageWithMore[isnumericQ,
"isnumericQ[x] returns True if x is a numeric quantity which may
be factored out in front of a nc multiplication."];
UsageWithMore[mcross,
"mcross[a, b] returns a cross product a x b. nc multiplication is used."];
UsageWithMore[normvc,
"normvc[v] returns the norm of the state v in the occupation number
representation."];
UsageWithMore[normop,
"normop[v] returns the norm of the state v in the creation operator
representation.", "normvc"];
UsageWithMore[SimplifyKD,
"SimplifyKD[expr] simplifies KroneckerDelta and UnitStep expressions
in expr."];
UsageWithMore[snegAssuming,
"snegAssuming[assumption, expr] evaluates expr assuming that
'assumption' holds."];
UsageWithMore[snegSeries,
"snegSeries[f, arg, n] expands function f into a series of order n
around 0. The argument of the expansion is arg and its powers are
computed using pow."];
UsageWithMore[SnegSimplify,
"SnegSimplify[expr] attempts to rewrite an expression in terms
of higher level sneg functions."];
UsageWithMore[isannihilation,
"isannihilation[op] returns True if op is an annihilation operator."];
UsageWithMore[iscreation,
"iscreation[op] returns True if op is a creation operator.",
"isannihilation"];
UsageWithMore[dropemptysubspaces,
"dropemptysubspaces[bz] removes subspaces that contain no states
from the basis bz."];
UsageWithMore[lrmap,
"lrmap[map] returns the substitution rules with perform a left-right
reflection transformation.",
"transformtoLR"];
UsageWithMore[makeallmatricesbzvc,
"makeallmatricesbzvc[H, basis] computes the matrix representation
of operator H in the given basis (in the occupation number representation).
All pairs of invariant subspaces are considered."];
UsageWithMore[makeallmatricesbzop,
"makeallmatricesbzop[H, basis] computes the matrix representation
of operator H in the given basis (in the creation operator representation).
All pairs of invariant subspaces are considered.",
"makeallmatricesbzvc"];
UsageWithMore[asciitosneg,
"asciitosneg[string] parses a string containing a condensed operator-expression
representation and converts it in a SNEG operator-expression representation.
Operators are entered as charactes, followed by a + sign for creation
operators, the indexes are surrounded by parenthesis. The non-commutative
multiplication is implied. Bras and kets are also supported as <m|
and |n> strings. The inverse transformation can be perofmed using snegtoascii[]."];
UsageWithMore[snegtoascii,
"snegtoascii[expr] converts a SNEG expression with non-commutative multiplication
nc[], operators, bras and kets into an ASCII string representation. If the
full conversion is not possible, the function will return a SNEG expression
featuring String objects for those parts of the expression where the conversion
was possible. The inverse transformation can be performed using asciitosneg[]."];

(* Momentum space and lattices; see README_momentum.md. *)
sneglatticeindexes::usage =
"sneglatticeindexes[{R1, R2, ...}, Ns] declares R1, R2, ... as real
summation indexes that label the unit cells of a periodic lattice with Ns
cells. Ns is a positive integer or a symbol, which is declared a real
constant.";
snegmomentumindexes::usage =
"snegmomentumindexes[{k1, k2, ...}, Ns] declares k1, k2, ... as real
summation indexes that label the crystal momenta of a periodic lattice with
Ns cells. Momentum labels are defined modulo reciprocal-lattice vectors.";
sneglatticevectors::usage =
"sneglatticevectors[a1, a2, ...] declares a1, a2, ... as real lattice
vectors that can be used as displacements, e.g. c[R + a1].";
latticeindexQ::usage =
"latticeindexQ[R] returns True if R is a declared lattice index.";
momentumindexQ::usage =
"momentumindexQ[k] returns True if k is a declared momentum index.";
latticesize::usage =
"latticesize[k] returns the number of unit cells Ns associated with
a declared lattice or momentum index k.";
blochphase::usage =
"blochphase[k, R] represents the Bloch phase exp(i k.R). Phases with equal
lattice arguments are combined, sums in the lattice argument are split, and
blochphase[k, R]^* = blochphase[-k, R]. sumSimplifyKD[] performs the
lattice sums Sum_R exp(i K.R) = Ns delta(K, 0) and
Sum_k exp(i k.X) = Ns delta(X, 0).";
blochphaseexp::usage =
"blochphaseexp[expr, rules] applies the substitution rules (e.g.
{k -> {kx, ky}, a1 -> {1, 0}}) and converts Bloch phases to exponentials.";
fourier::usage =
"fourier[expr, c -> ck, R -> k] transforms the site operators c to the
momentum operators ck using c_R = Ns^(-1/2) Sum_k exp(i k.R) c_k. The site
or momentum index is the first index after CR/AN (option IndexPosition).
With \"Inverse\" -> True, the momentum operators ck are transformed back to
site operators c. Lattice sums and momentum conservation are applied unless
\"Simplify\" -> False.";
sumShift::usage =
"sumShift[expr, k -> +-k + q] shifts the summation variable k in all sums
over k. This is exact for declared lattice and momentum indexes. Sums with
SEA-ordered operators are not shifted.";
sumMomentumCanonical::usage =
"sumMomentumCanonical[expr] shifts the summed lattice and momentum indexes so
that the first operator argument depending on each index becomes the bare
index, and renames automatically renamed dummy indexes back to their root
names. This allows sumCollect[] to combine equivalent sums.";
blochhamiltonian::usage =
"blochhamiltonian[H, {ck[a], ck[b], ...}, k] returns the Bloch Hamiltonian
matrix h(k) of a translation-invariant bilinear Hamiltonian H, with
H = Sum_k Sum_ab ck^dagger_a h_ab(k) ck_b. The orbitals are given as
templates; the momentum is inserted as the first index after CR/AN and the
spin index (UP, DO for spin-1/2, 0 for spinless fermions) varies fastest.
The option \"OrbitalPositions\" -> {tau_a, ...} returns h in the convention
c_Ra = Ns^(-1/2) Sum_k exp(i k.(R + tau_a)) c_ka.";
fourierrules::usage =
"fourierrules[c -> ck, {L1, L2, ...}] returns replacement rules for the
Fourier transformation on a finite periodic cluster with sites given in
lattice coordinates n_a = 0, ..., L_a - 1 (integers in 1D) and momentum
labels m with k_a = (2 Pi m_a + theta_a)/L_a. The option
\"Twist\" -> theta sets twisted boundary conditions
c_(n + L_a e_a) = exp(i theta_a) c_n.";
clusterhop::usage =
"clusterhop[T, c[a], d[b], delta, {L1, ...}] returns
Sum_n c^dagger_(n,a) T d_(n+delta,b) + h.c. on a finite periodic cluster.
T is a number or a 2 by 2 spin matrix. The option \"Twist\" -> theta sets
twisted boundary conditions.";
sumCluster::usage =
"sumCluster[expr, {L1, ...}, rules] converts a symbolic expression with sums
over lattice and momentum indexes into the explicit expression on a finite
periodic cluster with integer site and momentum labels. Labels are folded
modulo L, Kronecker deltas are evaluated modulo L, and rules (e.g.
{a1 -> {1, 0}}) give the lattice vectors in lattice coordinates.
Twisted boundary conditions are not supported.";
momentumbasis::usage =
"momentumbasis[bz, {L1, ...}] splits the subspaces of a basis of momentum
operators (e.g. from qszbasis) according to the total crystal momentum
K = Sum m (mod L), which is appended to the quantum numbers.";

(* Spin algebras and Hubbard X-operators; see README_spin.md. *)
snegspinoperators::usage =
"snegspinoperators[S, ...] declares S as spin operators S[j..., t] with the
component t = 1, 2, 3, 4, 5 (x, y, z, +, -) as the last index; they obey the
su(2) algebra in an unspecified representation. snegspinoperators[{S, s}]
declares the spin size s (a positive half-integer or a symbol); products of
operators on the same site are then reduced automatically to the unique
normal form Sum p(S^z) (S^+-)^a (Pauli algebra in x, y, z for s = 1/2).
Spin operators commute with all other operators and are moved to the left.";
spincomponents::usage =
"spincomponents[expr] rewrites all spin operators in terms of S^z and
S^+-; spincomponents[expr, \"XYZ\"] uses S^x, S^y, S^z. Sums over spin
component indexes are expanded.";
snegspincomponentindexes::usage =
"snegspincomponentindexes[a, b, ...] declares a, b, ... as summation
indexes for Cartesian spin components (values 1, 2, 3), so that
[S[j, a], S[j, b]] = I Sum_c levicivita[a, b, c] S[j, c].";
levicivita::usage =
"levicivita[a, b, c] is the totally antisymmetric Levi-Civita symbol with
indexes 1, 2, 3.";
spinketbrarules::usage =
"spinketbrarules[{S[1], S[2], ...}] or spinketbrarules[{S -> {0, 1, ...}}]
returns rules that replace the spin operators of the listed sites by their
bra-ket representation; site k corresponds to the k-th slot of the kets of
spinbasis[{s1, s2, ...}] and QSZaddspinket[bvc, {s1, s2, ...}].";
spinclustermatrices::usage =
"spinclustermatrices[H, {S[1], ...}] returns the matrices {{Sz}, h} of the
spin Hamiltonian H in the subspaces of total S^z, in the product basis
|m1, ..., mN> (each m from +s to -s, first site varying slowest). Options:
\"Blocks\" -> False returns the full matrix, \"Sparse\" -> True returns
SparseArray objects.";
jordanwigner::usage =
"jordanwigner[expr, S -> f, {j1, j2, ...}] applies the Jordan-Wigner
transformation S^+_k = f^dagger_k Prod_(l<k) (1 - 2 n_l), S^z_k = n_k - 1/2
to spin-1/2 operators of the sites j1, j2, ... (in this order); f is a
spinless fermion operator.";
holsteinprimakoff::usage =
"holsteinprimakoff[expr, S -> b, n] applies the Holstein-Primakoff
transformation S^z = s - b^dagger b, S^+ = Sqrt[2s - b^dagger b] b and keeps
the terms with at most n boson operators. Same-site products are expanded
exactly in normal-ordered boson operators.";
schwingerbosons::usage =
"schwingerbosons[expr, S -> {a, b}] applies the Schwinger-boson
representation S^+ = a^dagger b, S^- = b^dagger a,
S^z = (a^dagger a - b^dagger b)/2 (constraint a^dagger a + b^dagger b = 2s).";
abrikosov::usage =
"abrikosov[expr, S -> f] applies the Abrikosov pseudofermion representation
S = f^dagger S f, with fermions f of the same spin as S (faithful for one
fermion per site).";
spintensor::usage =
"spintensor[k, q, S[j]] returns the spherical tensor operator T_k^q built
from the spin S[j], with T_k^k = (-S^+/Sqrt[2])^k and lowering by S^-, so
that T_1^0 = S^z and T_2^0 = (3 (S^z)^2 - S.S)/Sqrt[6]. S -> j can be used
instead of S[j].";
stevens::usage =
"stevens[k, q, S[j]] returns the Stevens operator O_k^q (e.g.
O_2^0 = 3 (S^z)^2 - s(s+1), O_2^2 = (S^x)^2 - (S^y)^2) for a spin with a
declared size.";
rotatespin::usage =
"rotatespin[expr, S, R] rotates the spin vectors, S_j -> R.S_j, with R a
3 by 3 matrix or a function returning the matrix for the site indexes j.";
snegxoperators::usage =
"snegxoperators[X, {a1, a2, ...}, {p1, p2, ...}] declares Hubbard
X-operators X[j..., a, b] = |a><b| for the local states a1, a2, ... with
fermion parities p1, p2, ... (0 or 1). snegxoperators[X, \"HubbardAtom\"]
uses the states \"0\", \"UP\", \"DO\", \"2\" of a spin-1/2 orbital.";
xoperator::usage =
"xoperator[c[j], a, b] returns the X-operator |a><b| of the Hubbard atom
(states \"0\", \"UP\", \"DO\", \"2\" = c^dagger_UP c^dagger_DO |0>) in terms
of the fermion operators c[j].";
xtofermions::usage =
"xtofermions[expr, X -> c] replaces the X-operators of the Hubbard atom by
fermion operators, X[j, a, b] -> xoperator[c[j], a, b].";

(* Quadratic forms, Bogoliubov transformations, and mean-field theory; see
README_quadratic.md. *)
quadraticform::usage =
"quadraticform[H, {a1, a2, ...}] returns {M, E0} with
H = 1/2 Psi^dagger M Psi + E0 for a quadratic Hamiltonian H of the modes
Psi = (a_1, ..., a_n, a_1^dagger, ..., a_n^dagger). Modes are templates c or
c[j...] (fermions get the spin index as the last index, varying fastest) or
functions t -> operator of t = CR, AN. M = {{h, Delta}, {Delta^dagger, -h^T}}
for fermions and {{A, B}, {B^*, A^*}} for bosons. Options: \"BdG\" -> False
returns {h, E0'} with H = a^dagger h a + E0' for number-conserving H;
\"Linear\" -> True returns {M, f, E0} with the linear boson terms
Sum_i (f_i b_i^dagger + f_i^* b_i).";
quadraticoperator::usage =
"quadraticoperator[M, {a1, ...}, E0] returns 1/2 Psi^dagger M Psi + E0
for a 2n by 2n BdG matrix M, or a^dagger M a + E0 for an n by n matrix.";
bdghamiltonian::usage =
"bdghamiltonian[H, {ck[a], ...}, k] returns {H(k), E0} with
H = 1/2 Sum_k Psi_k^dagger H(k) Psi_k + E0 for a translation-invariant
quadratic Hamiltonian with anomalous terms, Psi_k = (c_k,a, ...,
c_-k,a^dagger, ...). The orbital templates and the option \"IndexPosition\"
are as in blochhamiltonian.";
bogoliubov::usage =
"bogoliubov[M, \"Fermion\"] and bogoliubov[M, \"Boson\"] diagonalize a BdG
matrix: they return {eps, T} with Psi = T Phi,
Phi = (gamma_1, ..., gamma_n, gamma_1^dagger, ...), the quasiparticle
energies eps_m and T^dagger M T = diag(eps, -eps) (fermions; T unitary) or
diag(eps, eps) (bosons; T^dagger eta T = eta, positive-definite M). For
symbolic matrices, blocks of at most two modes are supported.
bogoliubov[H, {a1, ...}] returns {eps, T, Egs} with the ground-state energy
Egs. With \"Momentum\" -> True, M is a Bloch BdG matrix H(k) from
bdghamiltonian; eps then lists all 2n diagonal elements of T^dagger H(k) T.";
bogoliubovrules::usage =
"bogoliubovrules[T, {a1, ...}, g] returns the rules
a_i -> Sum_m (T_im g_m + T_i,n+m g_m^dagger) and their conjugates for the
quasiparticle operators g (a spinless fermion or boson operator symbol, with
g_m = g[m], or a list of mode templates).";
gaussianaverage::usage =
"gaussianaverage[expr, M, {a1, ...}, T] returns the expectation value of
expr in the thermal state at temperature T (T = 0: ground state) of the
quadratic Hamiltonian with the BdG matrix M (or the Hamiltonian itself),
using Wick's theorem. expr may be a list. With \"Momentum\" -> k, M is a
Bloch BdG matrix H(k) of the orbitals and expr contains operators with
momenta k and -k.";
avg::usage =
"avg[x] denotes the expectation value of the operator expression x, as
created by meanfield. It is linear, avg[x]^* = avg[conj[x]], and it is
treated as a number in operator products.";
meanfield::usage =
"meanfield[H, channels] decouples products of four or more fermion or boson
operators into the mean-field Hamiltonian with the averages avg[...] of
pairs of operators. channels is \"All\" (default; Hartree-Fock-Bogoliubov),
\"Hartree\" (<a^dagger a> of the same mode), \"Fock\" (<a^dagger b> of
different modes), \"Pairing\" (<a b>, <a^dagger b^dagger>), \"Normal\"
(Hartree and Fock), a list of these, or a function f[o1, o2] that selects the
allowed contractions <o1 o2>.";
meanfieldsolve::usage =
"meanfieldsolve[Hmf, {a1, ...}, {avg[...] -> x0, ...}] solves the
mean-field equations for the averages in Hmf self-consistently and returns
an Association with the averages, the energy <Hmf>, the quasiparticle
energies, the chemical potential, and convergence information. Options:
\"Temperature\", \"ParticleNumber\" (fermions), \"Mixing\", \"Tolerance\",
\"MaxIterations\".";

(* Schrieffer-Wolff transformation; see README_schriefferwolff.md. *)
schriefferwolff::usage =
"schriefferwolff[H0, V, P, n] returns {P H0 P, P V P, P H_eff^(2) P, ...,
P H_eff^(n) P}, the contributions of order 0, ..., n in V to the effective
Hamiltonian H_eff = e^S (H0 + V) e^-S, where the anti-Hermitian generator S
is block-off-diagonal with respect to the projector P (canonical
Schrieffer-Wolff transformation). H0 must be diagonal in the occupation
numbers of the local modes (fermion number operators or diagonal
X-operators, e.g. U n_up n_dn); band energies, linear bosons and Zeeman
terms are allowed for the other operators. P is a projector written with
number operators, e.g. projector1[d[]]. For lattice sums over a declared
lattice index, P can be a rule {c, f} meaning the local projector f[c[R]] on
every site (up to second order). Options: \"Generator\" -> True returns
{list, {S_1, ..., S_n}}; \"Simplify\" sets the function applied to the
coefficients.";
eigenoperators::usage =
"eigenoperators[O, H0] returns {{omega, O_omega}, ...} with
Sum O_omega = O and [H0, O_omega] = omega O_omega, for H0 diagonal in the
occupation numbers.";
schriefferwolffmatrix::usage =
"schriefferwolffmatrix[H0, V, pind, n] performs the canonical
Schrieffer-Wolff transformation for matrices, with P spanned by the basis
states pind, and returns the P blocks of H_eff^(m), m = 0, ..., n. H0 must
not couple P and Q; numerical non-diagonal H0 are diagonalized within the
blocks. \"Generator\" -> True also returns the generators S_m.";
fermionstox::usage =
"fermionstox[expr, c -> X] replaces the spin-1/2 fermion operators c[j, s]
by the X-operators of the Hubbard atom (inverse of xtofermions).";
xtospin::usage =
"xtospin[expr, X -> S] replaces the X-operators X^(s s') of singly occupied
sites by spin-1/2 operators, X^(UP,UP) = 1/2 + S^z, X^(UP,DO) = S^+, ...;
terms with the states \"0\" or \"2\" are dropped with a message.";

(* Equations of motion, Green's functions, and sum rules; see README_greens.md. *)
liouvillian::usage =
"liouvillian[A, H, n] returns {A, [A, H], [[A, H], H], ...} with n
commutators (the Liouvillian L A = [A, H] applied repeatedly).";
spectralmoments::usage =
"spectralmoments[A, B, H, n] returns the moments
<[L^k A, B]_eta>, k = 0, ..., n, of the spectral function of the retarded
Green's function <<A; B>>, with the anticommutator (eta = +1) for A odd in
the fermion operators and the commutator otherwise. The averages are avg[...]
or given by \"Average\" -> f.";
eomclosure::usage =
"eomclosure[A, H] returns {{O_1, ...}, M, a}: the operator monomials
generated from A = a . O by repeated commutation with H, and the matrix M with
[O_i, H] = Sum_j M_ij O_j. The option \"MaxOperators\" limits the number of
operators.";
greensfunction::usage =
"greensfunction[A, B, H, z] returns the retarded Green's function
<<A; B>>_z = a . (z - M)^-1 . N from the closed equations of motion, with
N_i = <[O_i, B]_eta>. The averages are avg[...] or given by \"Average\" -> f,
e.g. thermalaverage[#, H, modes, T] &.";
thermalaverage::usage =
"thermalaverage[X, H, {c[1], ...}, T] returns the thermal average of X in
the Fock space of the fermion modes (T = 0: ground state, degenerate ground
states with equal weights), by exact diagonalization.";
lehmann::usage =
"lehmann[A, B, H, {c[1], ...}, T] returns the poles and weights
{{omega_j, w_j}, ...} of the Lehmann representation
<<A; B>>_z = Sum_j w_j/(z - omega_j), by exact diagonalization in the Fock
space of the fermion modes.";

(* Public symbols without usage messages: the developer interface, constants,
and functions used in existing notebooks and in NRG Ljubljana. All other
symbols defined below are in the private context Sneg`Private`. *)
{
  ADD, addto, allcombinations, AN, anhop, antisym, appendquantumnumber,
  applytocr, BASIS, basistensorproduct, bcs, block, braket, bzQ, coh,
  contract, CR, current, currentphi, der, directproductbasis, DO, dropzeros,
  EMPTY, FactorSeparableSumCustom, fastpow, fixspin, fixspinket, free,
  FreeOfIndexQ, FullSimplifyKD, genanhop, genhop, hopphi, Hubbard, indecesAN,
  indecesCR, indexvalue2list, int, interleave, isospinx1, isospinx2,
  ketbratensorproduct, keymerge, keyselect, KS, latticeindextype,
  listbosonoperators, listcomplexconstants, listfermionoperators,
  listfreeindexes, listgrassmanconstants, listintegerconstants,
  listlatticeindexes, listlatticevectors, listmajoranaoperators,
  listmomentumindexes, listofpairs, listrealconstants,
  listspincomponentindexes, listspinoperators, listxoperators, mambpair,
  mapbasis, maxcran, NONE, nonebasis, nonebasisvc, ops2vc, orthogbasisop,
  orthogbasisvc, outofdiag02, outofdiag20, pairpattern, phononbasis,
  phononid, PrettyInput, PrettyOutput, Problem, PROJ0, PROJ02, PROJ1, PROJ2,
  PROJDO, projectorE, projectorEphi, projectorO, projectorOphi, PROJUP,
  qbasis, qbasisvc, qscombinefnc, qsspincombinefnc, QSZaddspinket, quickDBL,
  quickDBLSZ, quickISOSZbasis, quickSU2basis, remainder, remove1state,
  removestates, ruleAbs, ruleExpandUnderSum, ruleFullSimplifyUnderSum,
  rulesimplifyscalar, rulestripscalar, rulesumDistribute, rulesumFactor,
  sbasis, sbasisvc, scalar, scalarproduct, SEA, showbzmat, snegaddop,
  snegcomplexfunctions, SnegFullSimplify, snegintegerconstants,
  sneglinearoperator, sneglinearoperatorFirst, snegnonopQ, snegorthog,
  snegpositiveconstants, snegrealfunctions, snegsesquilinearoperator,
  SnegSimplifySpin, snegspinlessfermionoperators, snegsumJoinWithRenaming,
  SnegTeXForm, snegxparity, spincomponentindexQ, spindown, spindownvc,
  spinflip, spinfliphopphi, SPININDEXm, SPININDEXp, SPININDEXx, SPININDEXy,
  SPININDEXz, spinketbraI, spinketbraOPS, spinketbraspinspin, spinketbraXYZ,
  spinlessbasis, spinof, spinQ, spinspinx, spinspiny, spinxyzgen,
  ssJWRnewname, SU2basistensorproduct, SU2combine, SU2merge, su3g, su3t,
  sum2list, sumAbstractIndex, sumAutoRename, sumCollectSimplifyKD,
  sumNameIndex, sumSimplify, szbasis, szbasisvc, TeXString, Tminus,
  transform1, transformtoLRvc, UP, vccolor, vccolorDefault, vccolorSimple,
  vevwick2, vevwicknew, wickorder, xoperatorQ, xparities, xstates,
  ZeroMatrix, Zeroonvac
};

snegtex::usage =
"snegtex[expr] returns a LaTeX string for an expression with operators,
using the notation c^\\dagger_{i\\uparrow}, S^+_j, ... .";

Begin["`Private`"];

(** Load required packages **)

(* We need KSubsets from Combinatorica by Sriram V. Pemmaraju
and Steven S. Skiena. *)

KS = Compile[{{n, _Integer}, {k, _Integer}},
             Module[{h, ss = Range[k], x},
                    Table[(h = Length[ss]; x = n;
                           While[x === ss[[h]], h--; x--];
                           ss = Join[Take[ss, h - 1],
                                     Range[ss[[h]]+1, ss[[h]]+Length[ss]-h+1]
                                ]),
                          {Binomial[n, k]-1}
                    ]
             ]
     ]

KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,2] := Module[{i, j},
  Flatten[Table[{l[[i]], l[[j]]},
                {i, Length[l]-1},
                {j, i+1, Length[l]}
          ],
          1
  ]
]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])
KSubsets[s_List, k_Integer] := Prepend[Map[s[[#]] &, KS[Length[s], k]], s[[Range[k] ]] ]

snegzeroarrayQ[m_] := And @@ (TrueQ[Simplify[# == 0]] & /@ Flatten[m]);
snegorthog[m_] := Orthogonalize[m, Simplify[Dot[Conjugate[#1], #2]] &];
snegorthog[m_] /; snegzeroarrayQ[m] := {};

(* When PrettyOutput is set to True, nc[] is formated in infix notation with
CenterDot and operators are formated in familiar notation with daggers,
arrows for spin projection, etc. *)
(* When PrettyInput is set to True, some input expressions are parsed,
providing a counterpart to PrettyOutput. *)

If[!ValueQ[PrettyOutput], PrettyOutput = True];
If[!ValueQ[PrettyInput], PrettyInput = True];

PrettyOutput::Usage =
"Setting PrettyOutput to False prior to loading the package sneg disables
the pretty printing of sneg expressions.";
PrettyInput::Usage = 
"Setting PrettyInput to False prior to loading the package sneg disables
the translation of certain markup into sneg functions.";


(* Standard shorthands for operator indexes; some functions depend
explicitly upon these particular values. This holds notably for ap[].
For this reasion, these values have Protected attribute. *)

ClearAttributes[{CR, AN, DO, UP}, Protected];

CR::usage =
"c[CR] is a creation operator.";
AN::usage =
"c[AN] is an annihilation operator.";
DO::usage =
"c[CR, DO] is a spin-down (creation) operator.";
UP::usage =
"c[CR, UP] is a spin-up (creation) operator.";

CR = 0;
AN = 1;
DO = 0;
UP = 1;

SetAttributes[{CR, AN, DO, UP}, Protected];

(* list?? store symbols that have been promoted to operators,
constants or indexes. *)
If[!ValueQ[listfermionoperators],  listfermionoperators={} ];
If[!ValueQ[listbosonoperators],    listbosonoperators={} ];
If[!ValueQ[listmajoranaoperators], listmajoranaoperators={} ];
If[!ValueQ[listspinoperators],     listspinoperators={} ];

If[!ValueQ[listrealconstants],     listrealconstants={} ];
If[!ValueQ[listintegerconstants],  listintegerconstants={} ];
If[!ValueQ[listcomplexconstants],  listcomplexconstants={} ];
If[!ValueQ[listgrassmanconstants], listgrassmanconstants={} ];

If[!ValueQ[listfreeindexes],       listfreeindexes={} ];

(* addto[list, a] adds a to list and removes duplicates *)
addto[list_, element_] := (list = Union[Append[list, element]]);
SetAttributes[addto, HoldFirst];

(* Define formatting rules for an operator (pretty printing). *)
SnegPPoperator[x_, spin_:1/2] := Block[{},
     (* With spin index *)
     Format[x[i_, j___, sigma_], StandardForm] :=
      Module[{dag, sgm},
       dag = Which[i === CR, "\[Dagger]",
                   i === AN, "",
                   True, Null];
       sgm = Which[
           spin == 1/2 && sigma === UP,
              StyleForm["\[UpArrow]", FontColor -> Red],
           spin == 1/2 && sigma === DO,
              StyleForm["\[DownArrow]", FontColor -> Blue],
           spin == 0 && sigma === 0,
              "",
           True,
              sigma ];

       DisplayForm @ If[dag === Null,
          RowBox[{x, "[", RowBox[{i,j,sigma}], "]"}],
          StyleBox[ Subsuperscript[x, RowBox @ Append[{j}, sgm], dag ],
            ScriptSizeMultipliers -> 1,
            ScriptBaselineShifts -> {1, 1}]
        ]
      ];

     (* Without any indexes (apart from creation/annihilation type index) *)
     Format[x[i_], StandardForm] :=
      Module[{dag},
       dag = Which[i === CR, "\[Dagger]",
                   i === AN, "",
                   True, Null];

       DisplayForm @ If[dag === Null,
          RowBox[{x, "[", RowBox[{i}], "]"}],
          StyleBox[ Superscript[x, dag ],
            ScriptSizeMultipliers -> 1,
            ScriptBaselineShifts -> {1, 1}]
        ]
      ];
   ];

(* Convert x to a string in the TeX format *)
TeXString[x_] := ToString[x, TeXForm];
SetAttributes[TeXString, Listable];

(* TeX conversion rules for a (fermionic) operator *)
(* XXX: broken as of 24.8.2010 due to a bug in Mathematica which reapplies TeXForm[]
   to the output from our definition of TeXForm formatting. Weird! *)
SnegTeXForm[x_, spin_] := Module[{},
   Format[x[i_, j___, sigma_], TeXForm] := SnegTeXForm1[x[i, j, sigma]];

   SnegTeXForm1[op_[i_, j___, sigma_]] :=
    Module[{dag, sgm},
      dag = Which[i === CR, "\\dag",
                  i === AN, "{}",
                  True, "{}"];
      sgm = Which[
           spin == 1/2 && sigma === UP,
              "\\uparrow",
           spin == 1/2 && sigma === DO,
              "\\downarrow",
           True,
              sigma ];
      SequenceForm[ ToString[op], "^", dag, "_{", Append[TeXString[{j}], sgm], "}" ]
   ];

   Format[x[i_], TeXForm] := SnegTeXForm2[x[i]];

   SnegTeXForm2[op_[i_]] :=
    Module[{dag},
      dag = Which[i === CR, "\\dag",
                  i === AN, "{}",
                  True, "{}"];
      SequenceForm[ ToString[op], HoldForm["^"], dag ]
   ];
];

(* Default spin is 1/2 *)
(* Note that the spin of a given operator needs to be redefined
   by changing the appropriate upvalue, e.g. spinup[c] ^= 3/2. *)
spinof[x_] := 1/2;

(* Add one fermion operator with given spin. *)
snegaddop[{x_, spin_}] := Block[{},
  operatorQ[x] ^= True;
  fermionQ[x] ^= True;
  spinof[x] ^= spin;
  SetAttributes[x, NHoldAll];

(*  SnegTeXForm[x, spin]; *)
  If[PrettyOutput, SnegPPoperator[x, spin] ];

  addto[listfermionoperators, x];
];

(* By default, all fermionic operators are assumed to
be spin-1/2 operators. *)
snegaddop[x_] := snegaddop[{x, 1/2}];

(* Define which symbols denote fermionic operators *)
snegfermionoperators[l__] := Scan[snegaddop, {l}];

(* Spinless operators don't carry a spin index. This affects
   their pretty printing. *)
snegspinlessfermionoperators[l__] := Scan[snegaddop[{#, 0}]&, {l}];

(* Define formatting rules for a bosonic operator. *)
SnegPPbosonoperator[x_] := Block[{},
     Format[x[i_, j__], StandardForm] :=
      Module[{dag},
       dag = Which[i === CR, "\[Dagger]",
                   i === AN, "",
                   True, Null];

       DisplayForm @ If[dag === Null,
          RowBox[{x, "[", RowBox[{i,j}], "]"}],
          StyleBox[ Subsuperscript[x, RowBox @ {j}, dag ],
            ScriptSizeMultipliers -> 1,
            ScriptBaselineShifts -> {1, 1}]
        ]
      ];

     Format[x[i_], StandardForm] :=
      Module[{dag},
       dag = Which[i === CR, "\[Dagger]",
                   i === AN, "",
                   True, Null];

       DisplayForm @ If[dag === Null,
          RowBox[{x, "[", RowBox[{i}], "]"}],
          StyleBox[ Superscript[x, dag],
            ScriptSizeMultipliers -> 1,
            ScriptBaselineShifts -> {1, 1}]
        ]
      ];
   ];


(* Add one bosonic operator *)
snegaddbosonop[x_] := Block[{},
  operatorQ[x] ^= True;
  bosonQ[x] ^= True;
  SetAttributes[x, NHoldAll];

  If[PrettyOutput, SnegPPbosonoperator[x] ];

  addto[listbosonoperators, x];
];

(* Define which symbols denote bosonic operators *)
snegbosonoperators[l__] := Scan[snegaddbosonop, {l}];

(* Define which symbols are numerical constants *)
snegrealconstants[l__] := Scan[
{  (* We must define the symbol to be numeric. *)
   snegnonopQ[#] ^= True;
   Conjugate[#] ^= #;
   addto[listrealconstants, #];
}&, {l}];

flagrealfunction[x_] := (
  snegnonopQ[x] ^= True;
  Conjugate[x[args___]] ^= x[args];
  isnumericQ[x[___]] := True;
);
snegrealfunctions[l__] := Scan[flagrealfunction, {l}];

snegpositiveconstants[l__] := Scan[
{  snegnonopQ[#] ^= True;
   Conjugate[#] ^= #;
   addto[listrealconstants, #];

   Unprotect[Power];
   Power[Power[#, n_], 1/2] /; (EvenQ[n]) := Power[#,n/2];
   Power[-Power[#, n_], 1/2] /; (EvenQ[n]) := I Power[#,n/2];
   Protect[Power];
}&, {l}];

sneggrassmanconstants[l__] := Scan[
{
  grassmanQ[#] ^= True;
  grassmanQ[conj[#]] ^= True;
  If[PrettyOutput,
    Format[conj[#]] := OverBar[#];
  ];
  addto[listgrassmanconstants, #];
}&, {l}];

snegmajoranaoperators[l__] := Scan[
{
  operatorQ[#] ^= True;
  majoranaQ[#] ^= True;
  conj[#] ^= #;
  SetAttributes[#, NHoldAll];
  addto[listmajoranaoperators, #];
}&, {l}];

(* CONVENTION: the last index is {x=1,y=2,z=3,p=4,m=5}. *)
SPININDEXx=1;
SPININDEXy=2;
SPININDEXz=3;
SPININDEXp=4;
SPININDEXm=5;

(* Define formatting rules for a spin operator. *)
SnegPPspinoperator[x_] := Block[{},
     Format[x[i___, j_], StandardForm] :=
      Module[{ndx},
       ndx = Which[j === SPININDEXx, StyleForm["x", FontColor -> Blue],
                   j === SPININDEXy, StyleForm["y", FontColor -> Blue],
                   j === SPININDEXz, StyleForm["z", FontColor -> Blue],
                   j === SPININDEXp, StyleForm["+", FontColor -> Blue],
                   j === SPININDEXm, StyleForm["-", FontColor -> Blue],
                   True, j];

       DisplayForm @ StyleBox[
              If[ {i} === {}, Superscript[x, ndx],
                              Subsuperscript[x, RowBox @ {i}, ndx ] ],
            ScriptSizeMultipliers -> 1,
            ScriptBaselineShifts -> {1, 1}]
      ];
   ];

snegaddspinop[x_Symbol] := (
  operatorQ[x] ^= True;
  spinQ[x] ^= True;
  SetAttributes[x, NHoldAll];

  (* This allows for some tricks later on... *)
  x[i___, -SPININDEXx] = -x[i, SPININDEXx];
  x[i___, -SPININDEXy] = -x[i, SPININDEXy];
  x[i___, -SPININDEXz] = -x[i, SPININDEXz];
  x[i___, 0] = 0;

  If[PrettyOutput, SnegPPspinoperator[x] ];

  addto[listspinoperators, x];
);

(* {x, s} declares a spin operator with spin size s, which enables the
reduction of products of operators on the same site (see "Spin algebras"). *)
snegaddspinop[{x_Symbol, s_}] /; snegdeclarespinsize[s] := (
  snegaddspinop[x];
  spinof[x] ^= s;
  snegspinsizeQ[x] ^= True;
);

snegspinoperators[l__] := Scan[snegaddspinop, {l}];


snegintegerconstants[l__] := Scan[
{
   snegnonopQ[#] ^= True;
   Conjugate[#] ^= #;
   addto[listintegerconstants, #];}&,
{l}];

snegcomplexconstants[l__] := Scan[
{
   snegnonopQ[#] ^= True;
   addto[listcomplexconstants, #];}&,
{l}];

snegcomplexfunctions[l__] := Scan[
{
  snegnonopQ[#] ^= True;
  isnumericQ[#[___]] := True;
  isnumericQ[Conjugate[#[___]]] := True;
}&, {l}];

(* Define which symbols are free indexes that appear in sum[]s *)
snegfreeindexes[l__] := Scan[
{  snegnonopQ[#] ^= True;
   freeindex[#] ^= True;
   addto[listfreeindexes, #];}&,
{l}];

(* Make commonly used functions Numeric *)
SetAttributes[KroneckerDelta, NumericFunction];
SetAttributes[UnitStep, NumericFunction];

Unprotect[KroneckerDelta];
KroneckerDelta[a_List, b_List] /; Length[a] == Length[b] :=
  Inner[KroneckerDelta, a, b, Times];

KroneckerDelta /: Conjugate[KroneckerDelta[a__]] :=
  KroneckerDelta[a] /; Conjugate[{a}] === {a};

Protect[KroneckerDelta];

Unprotect[UnitStep];
UnitStep /: Conjugate[UnitStep[a_]] :=
  UnitStep[a] /; Conjugate[a] === a;
Protect[UnitStep];

If[PrettyOutput,
  Unprotect[KroneckerDelta];
  Format[KroneckerDelta[x_, y_], StandardForm] :=
   DisplayForm @
    StyleBox[SubscriptBox[StyleForm["\[Delta]", FontColor -> Green],
        RowBox[{x,y}]],
        ScriptSizeMultipliers -> 1,
        ScriptBaselineShifts -> {1, 1}];
  Protect[KroneckerDelta];

  Unprotect[UnitStep];
  Format[UnitStep[x_], StandardForm] := DisplayForm @
    RowBox[{StyleForm["\[Theta]", FontColor -> Green],
      "(", x, ")"}];
  Protect[UnitStep];
];

(* Checking for operator quantities with indexes *)
operatorQ[op_?operatorQ[k___]] := True;
fermionQ[op_?fermionQ [k___]] := True;
bosonQ[op_?bosonQ [k___]] := True;
majoranaQ[op_?majoranaQ[k___]] := True;
spinQ[op_?spinQ[k___]] := True;

(* Fall back: a fermionic operator is clearly an operator. *)
operatorQ[op_?fermionQ] := True;
operatorQ[op_?bosonQ] := True;
operatorQ[op_?majoranaQ] := True;
operatorQ[op_?spinQ] := True;

(* isnumericQ[] return True if the expression does not involve operators.
Expectation values avg[...] are numbers even though their arguments are
operator expressions. Applications f[...] of declared real or complex
functions (snegnonopQ[f]) are numbers. *)

isnumericQ[z_] := And @@ (If[NumericQ@#, True,
  If[MemberQ[Quiet[Attributes@Evaluate@Head@#], NumericFunction] ||
     snegnonopQ[#] === True || snegnonopQ[Head[#]] === True, True, False]] & /@
  Level[If[FreeQ[z, avg], z, z /. _avg -> 1], {0, Infinity}]);

(* Make "op" be a linear operator in all of its arguments. *)
(* Such operators are, for example, nc[], dd[], ap[], invertspin[]... *)

sneglinearoperator[op_] := {
  op[a___, z_?isnumericQ, b___] := z op[a, b];
  op[a___, z_?isnumericQ b_, c___] := z op[a, b, c];
  op[a___, b1_ + b2_, c___] := op[a, b1, c] + op[a, b2, c];
};

(* Similar to sneglinearoperator[op], but the operator op is
   made linear in the first argument only. Such operators are,
   for example, vev[], zeroonvac[].*)

sneglinearoperatorFirst[op_] := {
  op[z_?grassmanQ] := z;
  op[HoldPattern[nc[z_?grassmanQ, rest__]]] := nc[z, op[nc[rest]]];
  op[z_?isnumericQ] := z;
  op[z_?isnumericQ, b__] := z op[b];
  op[z_?isnumericQ b_, c___] := z op[b, c]; (* Important: b_, not b__ ! *)
  op[b1_ + b2_, c___] := op[b1, c] + op[b2, c];
};

(* Note: note the pattern b__ in the first line *)
sneglinearoperatorWithLast[op_] := {
  op[a___, z_?isnumericQ, b__] := z op[a, b];
  op[a___, z_?isnumericQ b_, c___] := z op[a, b, c];
  op[a___, b1_ + b2_, c___] := op[a, b1, c] + op[a, b2, c];
};

(* Definition for an operator with at most two arguments; the operator
is conjugate-linear with respect to the first argument and linear
with respect to the second argument. This is the usual convention
in the Dirac's bra-ket notation, but differs from the usual convention
in the mathematical literature. *)

snegsesquilinearoperator[op_] := {
  op[z_?isnumericQ a_, b_] := conj[z] op[a, b];
  op[a_, z_?isnumericQ b_] := z op[a, b];
  op[a1_ + a2_, b_] := op[a1, b] + op[a2, b];
  op[a_, b1_ + b2_] := op[a, b1] + op[a, b2];
  (* Special case *)
  op[a_, 0] := 0;
  op[0, b_] := 0;
};

(* nc is a reserved word and it is obviously never an operator.
This definition speeds up some tests. *)
operatorQ[nc] = False;

(**** Define operations for non-commuting operators ****)

nc[a___, HoldPattern[nc[b__]], c___] := nc[a, b, c]; (* Associativity *)

(* An alternative approach to make nc[] associative is to set the
attributes Flat and OneIdentity. In this case, some tweakings of the
definitions of nc[] are required in order to avoid infinite recursion
loops. I've managed to implement such a version (4.7.2006) and it
works -- much slower! (A working version of that approach is stored as
sneg-Flat-OneIdentity.m in directory special/ for experimental
purposes and further testing. It even passes all the regression
tests!). It is slower by an order of magnitude, apparently because the
pattern matching process is more involved. *)

sneglinearoperator[nc];

(* nc[] is Listable *)
SetAttributes[nc, Listable];

(* Product of zero operators equals one, see Times[]. *)
nc[] := 1;

(* Rule for single terms, see Times[]. *)
(* This rule implies idempotency of multiplication. *)
(* NOTE: When this rule interferes with some other desired behavior,
one can use a workaround such as nc[c,]/.Null->1 *)
nc[c_] := c;

(* Pauli rule *)
nc[a___, op_[k___], op_[k___], b___] /; fermionQ[op] := 0;

ordering[op_?fermionQ] = EMPTY; (* Default ordering is "Empty band" *)
(* Override with ordering[op] = SEA or some other custom definition. *)

(* In the case of empty band, we always reorder so that the creation
operators go to the left of the operator product. This is achieved by
standard ordering test OrderedQ, since the creation operators corresponds to
CR=0, while annihilation operators correspond to AN=1. As a side-effect, the
operators are also ordered in increasing order with respect to the following
operator indexes. *)

(*
Canonical sorting:
=================
- creation operators to the left, annihilation operators to the right
- sort by index
- sort by spin index
=> for trivial vacuum (no particles), this reduces to simple sorting
   by all indexes!
*)

snegOrderedQ[x1:op_[___], x2:op_[___]] /;
  (fermionQ[op] && ordering[op] === EMPTY) := OrderedQ[{x1, x2}];


(* ordering=NONE rules, added 4. 3. 2010. *)
(* This disables reordering of the operators which is useful when
   working with very long operator expressions. Note that certain
   assumptions about the vacuum state and the normal ordering
   are implied. *)

snegOrderedQ[x1 : op_[___], x2 : op_[___]] /;
  (fermionQ[op] && ordering[op] === NONE) := True;

isannihilation[op_[t_, j___]] /; (ordering[op] == NONE) := (t == AN);
iscreation[op_[t_, j___]] /; (ordering[op] == NONE) := (t == CR);
contraction[x1:op_[AN, j1___], x2:op_[CR, j2___]] /;
  (ordering[op] == NONE) :=
    Which[
      TrueQ[bosonQ[op]], cmt[x1, x2],
      TrueQ[fermionQ[op]], acmt[x1, x2],
      True, If[{j1} === {j2}, 1, 0, 0]
    ];
contraction[op_[___], op_[___]] /; (ordering[op] == NONE) := 0;

(* Independent operator families still have zero cross contractions when
   automatic reordering is disabled. Respect a custom fermionic acmt. *)
contraction[x1:op1_[t1_, j1___], x2:op2_[t2_, j2___]] /;
  (op1 =!= op2 && ordering[op1] === NONE && ordering[op2] === NONE &&
   (TrueQ[fermionQ[op1]] || TrueQ[bosonQ[op1]]) &&
   (TrueQ[fermionQ[op2]] || TrueQ[bosonQ[op2]])) :=
    If[t1 === AN && t2 === CR && TrueQ[fermionQ[op1]] && TrueQ[fermionQ[op2]],
      acmt[x1, x2], 0];

(* NEW RULES for ordering=SEA, 25. 3. 2007 *)

snegOrderedQ[x1:op_[i1_, k1___], x2:op_[i2_, k2___]] /;
  (fermionQ[op] && ordering[op] === SEA &&
  iscreation[x2] && isannihilation[x1]) := False;

snegOrderedQ[x1:op_[i1_, k1___], x2:op_[i2_, k2___]] /;
  (fermionQ[op] && ordering[op] === SEA &&
  iscreation[x1] && isannihilation[x2]) := True;

(* If no other pattern was matched, use standard reordering rules. The
idea is that in the absence of information about the
creation/annihilation character of the operators, we may as well
reorder the expression in a form that is convenient for further
simplifications. *)

snegOrderedQ[x1:op_[___], x2:op_[___]] /;
  (fermionQ[op] && ordering[op] === SEA) := OrderedQ[{x1, x2}];

(* Ordering of fermionic operators with different heads. Determine the
creation/annihilation role of each operator independently. If a role is not
known, retain the raw CR/AN index as the fallback ordering key. *)
snegOrderedQ[
  x1:op1_?fermionQ[i1_,___],
  x2:op2_?fermionQ[i2_,___]
] /; (op1 =!= op2) := Module[
  {creation1, annihilation1, creation2, annihilation2, type1, type2},
  If[ordering[op1] === NONE || ordering[op2] === NONE, Return[True]];

  creation1 = TrueQ[iscreation[x1]];
  annihilation1 = TrueQ[isannihilation[x1]];
  creation2 = TrueQ[iscreation[x2]];
  annihilation2 = TrueQ[isannihilation[x2]];

  type1 = Which[
    creation1 && !annihilation1, CR,
    annihilation1 && !creation1, AN,
    True, i1
  ];
  type2 = Which[
    creation2 && !annihilation2, CR,
    annihilation2 && !creation2, AN,
    True, i2
  ];

  Which[
    MemberQ[{CR, AN}, type1] && MemberQ[{CR, AN}, type2],
      OrderedQ[{{type1, x1}, {type2, x2}}],
    type1 === type2,
      OrderedQ[{x1, x2}],
    True,
      True
  ]
];

(* Ordering of bosonic operators *)
snegOrderedQ[x1 : op_[___], x2 : op_[___]] /;
  (bosonQ[op] && ordering[op] === NONE) := True;
snegOrderedQ[x1:op_[___], x2:op_[___]] /; bosonQ[op] :=
  OrderedQ[{x1, x2}];

(* Ordering of spin operators *)
snegOrderedQ[x1:op_[___], x2:op_[___]] /; spinQ[op] :=
  OrderedQ[{x1, x2}];

(* Perform reordering for operators with equal Head. *)

nc[a___, x1:op_[k1__], x2:op_[k2__], b___] /;
  (fermionQ[op] && !snegOrderedQ[x1, x2]) :=
    -nc[a, x2, x1, b] + nc[a, acmt[x1, x2], b];

nc[a___, x1:op_[k1__], x2:op_[k2__], b___] /;
  (bosonQ[op] && !snegOrderedQ[x1, x2]) :=
    nc[a, x2, x1, b] + nc[a, cmt[x1, x2], b];

(* Only reorder spin operator when the spin index is determined (x,y,z,p,m). *)
nc[a___, x1:op_[k1___, t1_], x2:op_[k2___,t2_], b___] /;
  (spinQ[op] && 1 <= t1 <= 5 && 1 <= t2 <= 5 && !snegOrderedQ[x1, x2]) :=
    nc[a, x2, x1, b] + nc[a, cmt[x1, x2], b];


(* IMPORTANT NOTE: fermionic operators with different Head are assumed
to anti-commute; bosonic operators with different Head are assumed to
commute; bosonic and fermionic operators are assumed to commute. *)

(* Creation operators always moved to the left *)

nc[a___, x1:op1_?fermionQ[i1_,___], x2:op2_?fermionQ[i2_,___], b___] /;
  ( (op1 =!= op2) && ordering[op1] =!= NONE && ordering[op2] =!= NONE &&
    snegOrderedQ[x1, x2] === False ) := -nc[a, x2, x1, b];

nc[a___, x1:op1_?bosonQ[i1_,___], x2:op2_?bosonQ[i2_,___], b___] /;
  ( (op1 =!= op2) && ordering[op1] =!= NONE && ordering[op2] =!= NONE &&
    (i1 == AN && i2 == CR) ) := nc[a, x2, x1, b];

(* In the case of the same *type* of bosonic operators, sort according to
Head (symbol), i.e. in alphabetical order. Fermionic operators are handled
by snegOrderedQ[] above. *)

nc[a___, x1:op1_?bosonQ[i1_,___], x2:op2_?bosonQ[i2_,___], b___] /;
  ( (op1 =!= op2) && ordering[op1] =!= NONE && ordering[op2] =!= NONE &&
    (i1 === i2) && !OrderedQ[{x1,x2}]) :=
    nc[a, x2, x1, b];

(* Support for expansion blocking. *)
block::usage = "block[expr] is a wrapper for expressions to prevent
automatic expansion. By definition, nc[block[expr]]=expr. Note also
that the expression withing the block will be evaluated in the usual
way. Use block, for example, to prevent automatic distribution over
Plus.";
nc[block[expr_]] := expr;

sneginfixlist[l_List, elem_] :=
  Most @ Flatten[Map[{#, elem}&, l], 1];

If[PrettyOutput,
  (* Infix notation with green center dots. *)
  MakeBoxes[HoldPattern[nc[a__]], fmt:(StandardForm|TraditionalForm)] :=
    RowBox[sneginfixlist[
      Map[MakeBoxes[#, fmt]&, {a}],
      StyleBox["\[CenterDot]", FontColor -> Green] ]];
];

If[PrettyInput,
  (* Interpret center dots as nc[] multiplication. *)
  (* NOTE: The center dot should not be mistaken for the scalar product. *)
  CenterDot[a___] := nc[a];
];


(* Bosonic operators are commuted to the left of fermionic operators. *)
nc[a___, op1_, op2_, b___] /; (fermionQ[op1] && bosonQ[op2]) := nc[a, op2, op1, b];


(* Support for anti-commuting Grassman numbers *)
nc[a___, z1_?grassmanQ, z2_?grassmanQ, b___] /; (!OrderedQ[{z1,z2}]) :=
  -nc[a, z2, z1, b];

nc[a___, z_, b___, z_, c___] /; grassmanQ[z] := 0;

nc[a___, b_?bosonQ, z_?grassmanQ, c___] := nc[a, z, b, c];

nc[a___, b_, z_, c___] /; ((fermionQ[b] || majoranaQ[b]) && grassmanQ[z]) :=
  -nc[a, z, b, c];



(* Support for Majorana fermions *)

nc[a___, op_, op_, c___] /; majoranaQ[op] := 1/2 nc[a, c];

(* bug fixed, 24.9.2013 *)
nc[a___, x1:op1_?majoranaQ, x2:op2_?majoranaQ, c___] /;
  (op1 =!= op2 && !OrderedQ[{x1, x2}]) := -nc[a, op2, op1, c] + nc[a, acmt[x1, x2], c];

snegOrderedQ[x1:op_[__], x2:op_[__]] /; majoranaQ[op] := OrderedQ[{x1, x2}];

acmt[x1:op_[j1__], x2:op_[j2__]] /; majoranaQ[op] :=
  If[Length[{j1}] == Length[{j2}],
    Inner[KroneckerDelta, {j1}, {j2}, Times], 0];

acmt[x1:op1_[___], x2:op2_[___]] /; majoranaQ[op1] && majoranaQ[op2] && op1 =!= op2 := 0;

(* Consistent with nc[op, op] == 1/2 for Majorana operators, so {op, op} == 1. *)
acmt[op_Symbol, op_Symbol] /; majoranaQ[op] := 1;

acmt[op1_Symbol, op2_Symbol] /; majoranaQ[op1] && majoranaQ[op2] && op1 =!= op2 := 0;


(* Majorana operators are always anti-commuted to the left of the (Dirac)
fermionic operators. This allows for mixed Majorana-Dirac expressions. *)
nc[a___, x1:op1_?fermionQ, x2:op2_?majoranaQ, b___] := -nc[a, x2, x1, b];

(* Coherent states *)
coh[z_, c_[k___]] := nc[Exp[-nc[conj[z], z]/2], Exp[nc[c[CR, k], z]], VACUUM];

(* Count the number of operators with anticommuting properties *)

acmtcount[l_List] := Count[l, _?((fermionQ[#] || majoranaQ[#] || grassmanQ[#]) &), 1];


(* Berezin integral *)

nc[a___, int[z_], b___] /; (grassmanQ[z] && FreeQ[{b}, z, 1]) := 0;
nc[a___, int[z_], z_, b___] /; (grassmanQ[z] && FreeQ[{b}, z, 1]) :=
    nc[a, b];
nc[a___, int[z_], b__, z_, c___] /;
  (grassmanQ[z] && FreeQ[{b}, z, 1] && FreeQ[{c}, z, 1]) :=
    nc[a, b, c] (-1)^acmtcount[{b}];

nc[int[z__]] := 0;
nc[a___, int[z__], b___] := nc[a, Sequence @@ Map[int, {z}], b];


(* NOTE: 0.->0 rules are useful to eliminate numerically zero terms in
the expressions *)
nonuls = {0. -> 0, 0. I -> 0, Complex[0.,0.] -> 0, Complex[x_, 0.] :> x,
          Complex[0.,y_] :> I y};



(* Baker-Hausdorff relations for simple cases *)

(* ++ Exp Exp ++ *)

nc[a___, Exp[b1_], Exp[b2_], c___] /;
  (Simplify[ komutator[b1, b2] ] === 0) := nc[a, Exp[b1 + b2], c];

nc[a___, Exp[b1_], Exp[b2_], c___] /;
    (Simplify[ superkomutator[b1, b2, 2] ] === 0 &&
     Simplify[ superkomutator[b2, b1, 2] ] === 0) :=
  nc[a, Exp[b1 + b2], Exp[komutator[b1, b2]/2], c];

(* ++ Exp op Exp ++ *)

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /;
  (b1 == -b3 && Simplify[ komutator[b1, nc[b2]] ] === 0) :=
  nc[a, nc[b2], c];

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /;
  (b1 == -b3 && Simplify[ superkomutator[b1, nc[b2], 2] ] === 0) :=
  nc[a, nc[b2], c] + nc[a, komutator[b1, nc[b2]], c];

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /; (b1 == -b3) :=
  Module[{kmt},
    kmt = Simplify[
      (Simplify @ komutator[b1, nc[b2]] /. nonuls) / nc[b2] ];
    Exp[kmt] nc[a, nc[b2], c] /; isnumericQ[kmt]
  ];

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /; (b1 == -b3) :=
  Module[{kmt},
    kmt = Simplify[
      (Simplify @ superkomutator[b1, nc[b2], 2] /. nonuls) / nc[b2] ];
    nc[a, Simplify[ Cos[I Sqrt[kmt]] nc[b2] +
                    Sin[I Sqrt[kmt]]/(I Sqrt[kmt]) *
                    komutator[b1, nc[b2]]] /. nonuls, c] /;
                      isnumericQ[kmt]
  ];

(* ++ op Exp ++ *)

nc[a___, b2__, Exp[b3_], c___] /;
  Simplify[ komutator[b3, nc[b2]] ] === 0 :=
  nc[a, Exp[b3], nc[b2], c];

nc[a___, b2__, Exp[b3_], c___] /;
  Simplify[ superkomutator[b3, nc[b2], 2] ] === 0 :=
  nc[a, Exp[b3], nc[b2], c] - nc[a, Exp[b3], komutator[b3, nc[b2]], c];

(* Mendas-Milutinovic relations, see J. Phys. A: Math. Gen. 22, L687
(1989). *)

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /;
  (b1 == -b3 && antikomutator[b1, nc[b2]] === 0) :=
  nc[a, nc[b2], Exp[-2 b1], c];

nc[a___, Exp[b1_], b2__, Exp[b3_], c___] /;
  (b1 == -b3 && superantikomutator[b1, nc[b2], 2] === 0) :=
  nc[a, nc[b2], Exp[-2 b1], c] + nc[a, antikomutator[b1, nc[b2]], Exp[-2 b1], c];

(* Special simplification rule. It is applicable, for example,
to S=1/2 spin operators. *)

nc[a___, Exp[b_], c___] /; (isnumericQ @ Simplify @ pow[b, 2]) :=
  Module[{t},
    t = Simplify @ pow[b, 2];
    If[t === 0,
      nc[a, 1 + b, c],
      nc[a, Cosh[Sqrt[t]] + b/Sqrt[t] Sinh[Sqrt[t]], c]
    ]
  ];

(* acmt[] returns 1 if one operator is creation operator, and the other
operator is the annihilation operators, while all other indexes are equal. *)

(* Fermionic operators with different heads obey canonical
anti-commutation relations. *)
acmt[x1:op1_?fermionQ[___], x2:op2_?fermionQ[___]] /;
  (op1 =!= op2) := 0;

acmt[op_[i_, ___], op_[i_, ___]] /; fermionQ[op] := 0;

acmt[op_[i1_], op_[i2_]] /; fermionQ[op] := If[i1 =!= i2, 1, 0];

acmt[op_[i1_, j1___], op_[i2_, j2___]] /; fermionQ[op] :=
  If[i1 =!= i2 && Length[{j1}] == Length[{j2}],
    Inner[KroneckerDelta, {j1}, {j2}, Times], 0];


(* cmt[op1, op2] returns the commutator [op1,op2] for bosonic operators. *)
cmt[op_[i_, ___], op_[i_, ___]] /; bosonQ[op] = 0;

cmt[op_[CR], op_[AN]] /; bosonQ[op] = -1;
cmt[op_[AN], op_[CR]] /; bosonQ[op] = +1;

cmt[op_[CR, j1__], op_[AN, j2__]] /; bosonQ[op] :=
  If[Length[{j1}] == Length[{j2}],
    -Inner[KroneckerDelta, {j1}, {j2}, Times], 0];

cmt[op_[AN, j1__], op_[CR, j2__]] /; bosonQ[op] :=
  If[Length[{j1}] == Length[{j2}],
    +Inner[KroneckerDelta, {j1}, {j2}, Times], 0];



(* cmt[op1, op2] returns the commutator [op1,op2] for spin operators. *)

antisym[1,2] = 3;
antisym[2,3] = 1;
antisym[3,1] = 2;
antisym[2,1] = -3;
antisym[3,2] = -1;
antisym[1,3] = -2;
antisym[i_, i_] = 0;

cmt[op_[i___, t1_], op_[i___, t2_]] /; spinQ[op] && 1 <= t1 <= 3 && 1 <= t2 <= 3 =
   I op[i, antisym[t1, t2]];

cmt[op_[i___, SPININDEXz], op_[i___, SPININDEXp]] /; spinQ[op] = +op[i, SPININDEXp];
cmt[op_[i___, SPININDEXz], op_[i___, SPININDEXm]] /; spinQ[op] = -op[i, SPININDEXm];

cmt[op_[i___, SPININDEXp], op_[i___, SPININDEXz]] /; spinQ[op] = -op[i, SPININDEXp];
cmt[op_[i___, SPININDEXm], op_[i___, SPININDEXz]] /; spinQ[op] = +op[i, SPININDEXm];

cmt[op_[i___, SPININDEXp], op_[i___, SPININDEXp]] /; spinQ[op] = 0;
cmt[op_[i___, SPININDEXm], op_[i___, SPININDEXm]] /; spinQ[op] = 0;

cmt[op_[i___, SPININDEXp], op_[i___, SPININDEXm]] /; spinQ[op] = +2 op[i, SPININDEXz];
cmt[op_[i___, SPININDEXm], op_[i___, SPININDEXp]] /; spinQ[op] = -2 op[i, SPININDEXz];

cmt[op_[i___, SPININDEXp], op_[i___, SPININDEXx]] /; spinQ[op] =  1 op[i, SPININDEXz];
cmt[op_[i___, SPININDEXp], op_[i___, SPININDEXy]] /; spinQ[op] =  I op[i, SPININDEXz];
cmt[op_[i___, SPININDEXm], op_[i___, SPININDEXx]] /; spinQ[op] = -1 op[i, SPININDEXz];
cmt[op_[i___, SPININDEXm], op_[i___, SPININDEXy]] /; spinQ[op] =  I op[i, SPININDEXz];

cmt[op_[i___, SPININDEXx], op_[i___, SPININDEXp]] /; spinQ[op] = -1 op[i, SPININDEXz];
cmt[op_[i___, SPININDEXy], op_[i___, SPININDEXp]] /; spinQ[op] = -I op[i, SPININDEXz];
cmt[op_[i___, SPININDEXx], op_[i___, SPININDEXm]] /; spinQ[op] =  1 op[i, SPININDEXz];
cmt[op_[i___, SPININDEXy], op_[i___, SPININDEXm]] /; spinQ[op] = -I op[i, SPININDEXz];

cmt[op_[i1__, j1_], op_[i2__, j2_]] /; spinQ[op] && {i1} =!= {i2} :=
  If[Length[{i1}] == Length[{i2}],
    +Inner[KroneckerDelta, {i1}, {i2}, Times], 0] cmt[op[i1, j1], op[i1, j2]];

cmt[op1_?spinQ[i1___], op2_?spinQ[i2___]] /; (op1 =!= op2) = 0;

(* (Anti-)commutators *)
SetAttributes[komutator, Listable];
komutator[x1:op1_?spinQ[___], x2:op2_?spinQ[___]] /;
  op1 =!= op2 := cmt[x1, x2];
komutator[a_, b_] := nc[a, b] - nc[b, a];

SetAttributes[antikomutator, Listable];
antikomutator[a_, b_] := nc[a, b] + nc[b, a];

(* Aliases with correct spelling *)
commutator[a___] := komutator[a];
anticommutator[a___] := antikomutator[a];

(* Nested (anti-)commutators *)
superkomutator[a_, b_, 0] := b;
superkomutator[a_, b_, 1] := Simplify @ komutator[a, b];
superkomutator[a_, b_, i_Integer] /; i > 1 :=
    Simplify @ komutator[a, Simplify @ superkomutator[a, b, i - 1] ];

superantikomutator[a_, b_, 0] := b;
superantikomutator[a_, b_, 1] := Simplify @ antikomutator[a, b];
superantikomutator[a_, b_, i_Integer] /; i > 1 :=
    Simplify @ antikomutator[a, Simplify @ superantikomutator[a, b, i - 1]];

(* Aliases with correct spelling *)
supercommutator[a___] := superkomutator[a];
superanticommutator[a___] := superantikomutator[a];

(* Other helper functions *)
extracteps[a_, b_, H_] := anticommutator[a, commutator[H, conj[b]]] // Expand;

(* (Manual) normal ordering *)
SetAttributes[normalorder, Listable];
normalorder[a_] := a - vev[a];

SetAttributes[normalorderwick, Listable];
normalorderwick[a_] := a - vevwick[a];

(* Fix parenthesis output in pretty printing. Used for
dd[], vc[], bra[], ket[]. *)
fixparens[head_] := Module[{},
  MakeBoxes[-head[i___], fmt:(StandardForm|TraditionalForm)] :=
    InterpretationBox[#1, #2] & @@ {
      RowBox[{MakeBoxes["-", fmt], MakeBoxes[head[i], fmt]}],
      -head[i] };
  MakeBoxes[a_Complex * head[i___], fmt:(StandardForm|TraditionalForm)] :=
    InterpretationBox[#1, #2] & @@ {
      RowBox[{"(", MakeBoxes[a, fmt], ")", MakeBoxes[head[i], fmt]}],
      a head[i] };
  MakeBoxes[a_?AtomQ * head[i___], fmt:(StandardForm|TraditionalForm)] :=
    InterpretationBox[#1, #2] & @@ {
      RowBox[{MakeBoxes[a, fmt], MakeBoxes[head[i], fmt]}],
      a head[i] };
];

(* dd[] denotes a double dots surrounding an expression, i.e. a normal
ordered operator string. This is merely a placeholder. To explicitly
evaluate the normal ordered expression, use normalorder[] function. *)

If[PrettyOutput,
  (* Enclose the expression between two dark green double dots. *)
  Format[dd[i__], StandardForm] := DisplayForm @
    RowBox[{StyleForm["\[Colon]", FontColor -> Green],
    i,
    StyleForm["\[Colon]", FontColor -> Green]}];

  fixparens[dd];
];

sneglinearoperator[dd];

(* Associativity *)
dd[a___, HoldPattern[dd[b__]], c___] := dd[a, b, c];

dd[] := 1;

(* Conjugation *)
SetAttributes[conj, Listable];
conj[z_?isnumericQ] := Conjugate[z];
conj[z_?isnumericQ a_] := Conjugate[z] conj[a];
conj[a_ + b_] := conj[a] + conj[b];
conj[nc[a_, b__]] := conj[nc[b]] ~ nc ~ conj[a];

(* Complex (Dirac) fermion *)
conj[op_?fermionQ[i_, j___]] := op[CR + AN - i, j];

(* Boson *)
conj[op_?bosonQ[i_, j___]] := op[CR + AN - i, j];

(* Real (Majorana) fermion *)
conj[x:op_?majoranaQ[___]] := x;

(* Conjugation of sums *)
conj[sum[z_, it_List]] := sum[conj[z], it];

(* Conjugation of exponential terms *)
conj[Exp[expr_]]:=Exp[conj[expr]];

(* conj threads over Rules *)
conj[Rule[a_, b_]] := Rule[conj[a], conj[b]];

(* NOTE: Repeated conjugation is automatically eliminated! *) (* A
continuous linear operator defined on all of a Hilbert space has a unique
adjoint operator, so this rule is usually valid. However this does not hold
for operators that are defined only on a subset of the Hilbert space and for
unbounded operators. In the case of finite-dimensional Hilbert spaces that
are typically used in sneg, there is no difference between adjoint operators
and Hermitian conjugate operators, thus there are no reasons for concern and
the repeated conjugation may be automatically eliminated. *)

(* See also Francois Gieres, "Mathematical surprises and Dirac's formalism
in quantum mechanics", quant-ph/990069. *)

conj[conj[a_]] := a;

(* Simplify into absolute values *)
ruleAbs = {
  z_ Conjugate[z_] :> Abs[z]^2,
  z_ conj[z_] :> Abs[z]^2
};

(**** Symbolic sums ****)

(* No sum at all *)
sum[z_, {}] := z;

(* Pull isolated sums in front of non-commutative multiplications. Avoid
index capture and leave nested/product sums to the auto-renaming rules. *)
nc[a___, sum[f_, {q__}], c___] /;
  FreeQ[{a, c}, sum[_, _List]] && FreeOfIndexQ[{a, c}, {q}] :=
    sum[nc[a, f, c], {q}];

(* The indexes must be sorted for the simplifications to work correctly *)
sum[z_, it_List] := sum[z, Sort[it]] /; Not[OrderedQ[it]];

(* Returns True if expr contains none of symbols in list it *)
FreeOfIndexQ[expr_, it_List] := And @@ Map[FreeQ[expr, #]&, it];

(* Return True if expr can be factored out in front of the sum[] *)
sumFactorizableQ[expr_, it_List] := FreeOfIndexQ[expr, it];

(* It's always safe to extract a number *)
sum[z_ a_, it_List] := z sum[a, it] /; NumberQ[z];

sum[z_ a_, it_List] := z sum[a, it] /; sumFactorizableQ[z, it];

sum[z_, it_List] := z sum[1, it] /; (sumFactorizableQ[z, it] && z =!= 1);


(* Join two Lists. Send a message if the indexes overlap. *)
snegsumJoin::overlap = "Overlapping indexes detected when joining
`` and ``.";
snegsumJoin[l1_List, l2_List] := Module[{l},
  l = Sort @ Join[l1, l2];
  If[l =!= Union[l],
    Message[snegsumJoin::overlap, l1, l2];
  ];
  l
];

(* Automatically rename indexes in sums when name collisions occur.
The name is formed by taking the symbol name that occurs twice and
appending a number. The number is chosen such that the new name does
not yet appear in listfreeindexes. If such behavior is undesirable, it
can be disabled by setting sumAutoRename to False. *)

sumAutoRename = True;

ssJWRnewname[n_, allnames_List, expr_:1] := Module[{ns, i, nn},
  ns = ToString[n];
  If[$VersionNumber >= 5.1,
    (* For the following line, Mathematica 5.1 is required *)
    ns = StringReplace[ns,
      ShortestMatch[x : (_ ~~ ___)] ~~ DigitCharacter .. ~~ EndOfString :> x];
  ];
  For[i = 1, True, i++,
    nn = ToExpression[ ns <> ToString[i] ];
    If[!MemberQ[allnames, nn] && FreeQ[expr, nn], Break[]];
  ];
  (* Automatically declare the new index quantity *)
  snegfreeindexes[nn];
  snegcopyindexproperties[n, nn];
  nn
];

snegsumJoinWithRenaming[a1_, a0___, a2_, it1_List, it2_List, reverse_:False] :=
Module[{l, rule, allnames, captureContext, freshContext, nn},
  l = it1;
  rule = {};
  allnames = Union[it1, it2];
  captureContext = {a1, a0};
  freshContext = {a1, a0, a2};
  Scan[ If[FreeQ[l, #] && FreeQ[captureContext, #],
    AppendTo[l, #],
    (* else *)
    nn = ssJWRnewname[#, allnames, freshContext];
    AppendTo[l, nn];
    AppendTo[allnames, nn];
    AppendTo[rule, # -> nn] ]&, it2];
  If[reverse == False,
    sum[nc[a1, a0, a2 /. rule], Sort @ l],
    sum[nc[a2 /. rule, a0, a1], Sort @ l]
  ]
];

(* Product of sums *)
nc[x1___, sum[a1_, it1_List], a0___, sum[a2__, it2_List], x2___] /; (!sumAutoRename) :=
  nc[x1, sum[nc[a1, a0, a2], snegsumJoin[it1,it2]], x2];

nc[x1___, sum[a1_, it1_List], a0___, sum[a2_, it2_List], x2___] /; sumAutoRename :=
  nc[x1, snegsumJoinWithRenaming[a1, a0, a2, it1, it2], x2];

(* Special case for the commutators [a,b]=ab-ba where a and b are both sums.
Renaming is performed for the second argument (b) in both cases. Typically,
the resulting expressions can be simplified to a greater extent in this
approach. *)

komutator[x1:sum[a1_, it1_List], x2:sum[a2_, it2_List]] /; sumAutoRename :=
  snegsumJoinWithRenaming[a1, a2, it1, it2, False] -
  snegsumJoinWithRenaming[a1, a2, it1, it2, True];

(* Sum of sum *)
sum[b_. sum[a_, it1_List], it2_List] :=
  sum[b a, snegsumJoin[it1, it2]];
(* Keep nested sums intact while auto-renaming is enabled, so product-of-sums
rules still see the full expression and can rename dummy-index collisions. *)
sum[b_. sum[a_, it1_List] + c_, it2_List] /; (!sumAutoRename) :=
  sum[b a, snegsumJoin[it1, it2]] + sum[c, it2];
sum[b_ (sum[a_, it1_List] + c_), it2_List] /; (!sumAutoRename) :=
  sum[b a, snegsumJoin[it1, it2]] + sum[b c, it2];

(* Factorization sum -> sum*sum. *)
FactorSeparableSumCustom[sum[expr_, idx_List]] :=
 Module[{facs, facIdx, vertsF, vertsI, edges, comps, termFromComp, k, s},
  facs = If[Head[expr] === Times, List @@ expr, {expr}];
  facIdx[fac_] := Select[idx, Not @ FreeQ[fac, #] &];
  vertsF = Table[{"F", k}, {k, Length[facs]}];
  vertsI = Table[{"I", s}, {s, idx}];
  edges = Flatten @ Table[
    UndirectedEdge[{"F", k}, {"I", s}], {k, Length[facs]}, {s, facIdx[facs[[k]]]} ];

  (* include isolated indices as singleton components by explicitly adding them as vertices *)
  comps = ConnectedComponents @ Graph[Join[vertsF, vertsI], edges];

  termFromComp[c_] := Module[{fk, ii, subexpr, kkk, sss},
    fk = facs[[ Cases[c, {"F", kkk_} :> kkk] ]];
    ii = Cases[c, {"I", sss_} :> sss];
    subexpr = Times @@ fk /. Times[] -> 1;
    Which[
     ii === {},                          subexpr,                 (* factor-only: pull out *)
     fk === {},                          sum[1, ii],              (* index-only: empty factor -> 1 *)
     True,                               sum[subexpr, ii]         (* mixed: usual *)
    ]
  ];

   Times @@ (termFromComp /@ comps)
];

rulesumFactor = {
  s:sum[_, _List] :> FactorSeparableSumCustom[s]
};

(* Thread the first argument *)
rulesumThread = {
sum[z_List, it_List] :> Map[sum[#, it]&, z]
}

sumThread[expr_] := expr //. rulesumThread;

(* NOTE: Mathematica Thread[] function by default threads over all
arguments! Behavior similar to that of sumThread[] would be achieved
by using Thread[expr, List, 1]. *)

(* Simplification rules and functions for sum expressions. *)

rulesumExpand = {
sum[a_ + b_, it_List] :> sum[a, it] + sum[b, it],

sum[a_, it_List] :> sum[ExpandAll[a], it]
};

sumExpand[expr_] := expr //. rulesumExpand;

(* Expand[] here, not ExpandAll[], we don't want to blow up the denominators *)
ruleExpandUnderSum = {
  sum[a_, it_List] :> sum[Expand[a], it]
};

(* Does not expand under sums as rulesumExpand does. *)
rulesumDistribute = {
  sum[a_ + b_, it_List] :> sum[a, it] + sum[b, it]
};

rulesumCollect = {
Plus[z1_. sum[a1_, it_List], z2_. sum[a2_, it_List]] /;
  sumFactorizableQ[z1, it] && sumFactorizableQ[z2, it] :>
    sum[z1 a1+z2 a2, it],

Plus[z1_. sum[a1_, it1_List], z2_. sum[a2_, it2_List]] /;
  Intersection[it1, it2] =!= {} &&
  sumFactorizableQ[z1, it1] && sumFactorizableQ[z2, it2] :>
    Module[{int},
      int = Intersection[it1, it2];
      sum[sum[z1 a1, Complement[it1, int]]
         +sum[z2 a2, Complement[it2, int]], int]
    ]
};

sumCollect[expr_] := expr //. rulesumCollect;

(* scalar[] marker for explicitly declaring numerical (non-operator) expressions.
   Speed optimization. Particularly useful for expressions involving sum[]. *)

(* Always pulled out of noncommutative multiply blocks *)
nc[a___, b_. scalar[x_], c___] := scalar[x] nc[a, b, c];

(* Drop zeros *)
scalar[0] := 0;
scalar[0.] := 0;
scalar[0.+I 0.] := 0;

(* Sums of numeric/scalar terms are numeric quantities. This includes
   symbolic cardinalities such as sum[1, {i}] generated by sum factorization. *)
isnumericQ[sum[scalar[_], {__}]] := True;
isnumericQ[sum[z_?isnumericQ, {__}]] := True;

(* A scalar is a scalar... *)
scalar[a_. scalar[b_]] := scalar[a b];

(* scalar is greedy: it absorbs all non-operator quantities *)
a_  scalar[b_] /; isnumericQ[a] ^:= scalar[a b];
scalar[a_] + scalar[b_] ^:= scalar[a + b];
scalar[a_] scalar[b_] ^:= scalar[a b];

(* Conjugation handling. Note that we use conj[] for scalar, too, instead of Conjugate[]. *)
conj[scalar[x_]] := scalar[conj[x]];
conj[scalar[x_] a_] := scalar[conj[x]] conj[a];

(* Speed optimization for vev[] *)
vev[scalar[x_] a_] := scalar[x] vev[a];
vev[scalar[x_]] := x;

(* Pull sums out of scalar[] placeholders! *)
scalar[a_. sum[b_, {q__}]] := sum[scalar[a  b], {q}];

(* Transformation rules involving scalar[] blocks *)
rulestripscalar = {
  scalar[x_] :> x
};

rulesimplifyscalar = {
  scalar[x_] :> scalar[Simplify[x]]
};

rulecollectscalar = {
  sum[a_ scalar[z1_] + a_ scalar[z2_] + b_., {q_}] :> sum[a scalar[z1 + z2] + b, {q}]
};

(* Simplification rules for KroneckerDelta in sum[] expressions. *)
rulesumSimplifyKD = {
  sum[KroneckerDelta[n1_, n2_] a_. + b_., it_List] /; (MemberQ[it, n1] && FreeQ[n2, n1]) :>
    sum[a /. {n1 :> n2}, Complement[it, {n1}]] + sum[b, it],

  sum[KroneckerDelta[n1_, n2_] a_. + b_., it_List] /; (MemberQ[it, n2] && FreeQ[n1, n2]) :>
    sum[a /. {n2 :> n1}, Complement[it, {n2}]] + sum[b, it],

  sum[scalar[KroneckerDelta[n1_, n2_] z_ + c_.] a_. + b_., it_List] /; (MemberQ[it, n1] && FreeQ[n2, n1]) :>
    sum[(scalar[z] a) /. {n1 :> n2}, Complement[it, {n1}]] + sum[scalar[c] a, it] + sum[b, it],

  sum[scalar[KroneckerDelta[n1_, n2_] z_ + c_.] a_. + b_., it_List] /; (MemberQ[it, n2] && FreeQ[n1, n2]) :>
    sum[(scalar[z] a) /. {n2 :> n1}, Complement[it, {n2}]] + sum[scalar[c] a, it] + sum[b, it],

  sum[scalar[(KroneckerDelta[n1_, n2_] z_ + c_.)/d_] a_. + b_., it_List] /; (MemberQ[it, n1] && FreeQ[n2, n1]) :>
    sum[(scalar[z/d] a) /. {n1 :> n2}, Complement[it, {n1}]] + sum[scalar[c/d] a, it] + sum[b, it],

  sum[scalar[(KroneckerDelta[n1_, n2_] z_ + c_.)/d_] a_. + b_., it_List] /; (MemberQ[it, n2] && FreeQ[n1, n2]) :>
    sum[(scalar[z/d] a) /. {n2 :> n1}, Complement[it, {n2}]] + sum[scalar[c/d] a, it] + sum[b, it],

  HoldPattern[sum[a1_ + a2_, x:{i___, n1_, n2_, j___}]] /;
    (FreeQ[a1, n2] && FreeQ[a2, n1]) :>
      sum[1, {n2}] sum[a1, {i, n1, j}] + sum[1, {n1}] sum[a2, {i, n2, j}],

  HoldPattern[sum[a_ b_., {i___, n1_, j___}]] /;
    (FreeQ[a, n1] && FreeOfIndexQ[b, {i, j}]) :>
      sum[a, {i, j}] sum[b, {n1}],

  HoldPattern[sum[a_ (b_ + c_), x:{i___, n1_, j___}]] /;
    (FreeQ[a, n1] && FreeOfIndexQ[b, {i, j}]) :>
      sum[a c, x] + sum[a, {i, j}] sum[b, {n1}]
};

(* The lattice step (see "Momentum space and lattices") eliminates
Kronecker deltas of linear combinations of lattice or momentum indexes and
performs lattice sums over Bloch phases. *)
snegsumKDstep[expr_] :=
  snegsumlatticestep[(expr //. rulesumSimplifyKD) /. rulelatticeKDreduce];

sumSimplifyKD[expr_] := FixedPoint[snegsumKDstep, expr, 50];

(* Optimization *)
sumSimplifyKD[expr_] /; FreeQ[expr, KroneckerDelta[__]] &&
  FreeQ[expr, blochphase] := expr;

(* Collect KroneckerDelta[] in sums, prior to simplification. *)
rulesumCollectKD = {
  sum[a_, {q__}] :> sum[Collect[a, KroneckerDelta[__]], {q}]
};
sumCollectSimplifyKD[expr_] := sumSimplifyKD[expr //. rulesumCollectKD];

(* Replace indexes with abstract indexes ndxfunc[i] with i=1,...,nrindexes *)
(* Be careful: this is only useful for fine-tuned applications to address
very specific problems! *)

sumAbstractIndex::usage =
"sumAbstractIndex[expr] replaces sum indexes with abstract indexes.
This is useful for simplifications in the case of several equivalent
summation indexes.";

sumAbstractIndex[expr_, ndxfunc_] := Module[{ r },
  SetAttributes[ndxfunc, NumericFunction];
  r = sum[a_, it_List] :> Module[{rule, len, i},
        len = Length[it];
        (* Evaluate each replacement while its iterator is in scope. *)
        rule = Table[it[[i]] -> ndxfunc[i], {i, len}];
        sum[a //. rule, it //. rule]
      ];
  expr /. r
];

sumNameIndex::usage =
"sumNameIndex[expr] is to be used in conjunction with
sumAbstractIndex[expr].";

sumNameIndex[expr_, ndxfunc_, li_List] := Module[ {len, i},
  len = Length[li];
  expr //. Table[ndxfunc[i] -> li[[i]], {i, len}]
];

rulesumStripSums = {
  sum[expr_, {k___}] :> expr
};

(* sumFullSimplifyOLD tries to simplify an expression involving symbolic
summations. The heuristic approach used turns out to work very well in
practice. *)

sumFullSimplifyOLD[expr_] := Module[{ex},
  ex = Expand[expr];
  ex = sumCollect[ex];
  ex = Simplify[ex];
  ex = sumSimplifyKD[ex];
  ex = SimplifyKD[ex];
  ex = Simplify[ex];

  (* We must also expand within sum[] *)
  ex = sumExpand[ex];
  ex = Simplify[ex];
  ex = sumSimplify[ex];
  ex = Simplify[ex];

  (* and then collect again *)
  ex = sumCollect[ex];
  ex = Simplify[ex]
];

(* Expand, simplify, collect *)
sumSimplifyFnc1[expr_] := Module[{ex},
  ex = Expand[expr];
  ex = sumExpand[ex];
  ex = Simplify[ex];
  ex = sumCollect[ex]
];

(* Expand within, simplify, collect *)
sumSimplifyFnc2[expr_] := Module[{ex},
  ex = sumExpand[expr];
  ex = Simplify[ex];
  ex = sumCollect[ex]
];

(* Simplify KD *)
sumSimplifyFnc3[expr_] := Module[{ex},
  ex = sumSimplifyKD[expr];
  ex = SimplifyKD[ex]
];

(* Collect, simplify, simplifyKD *)
sumSimplifyFnc4[expr_] := Module[{ex},
  ex = sumCollect[expr];
  ex = Simplify[ex];
  ex = sumSimplifyKD[ex]
];

(* Transform a list of rules to a list of functions that apply these rules.
Such functions can be used as transformation functions in Simplify. *)

rulestofuncs[rules_List] := Map[Function[expr, expr /. #] &, rules];

(* It is better to apply individual simplification rules separately, since
the rules in each block can have conflicting effects as regards the
complexity of the expression that is being simplified. *)

sumsimplfuncs = Join[ { Automatic, SimplifyKD, sumSimplifyFnc1,
  sumSimplifyFnc2, sumSimplifyFnc3, sumSimplifyFnc4 },
  Sequence @@ Map[rulestofuncs,
    {rulesumExpand, rulesumCollect, rulesumSimplifyKD} ]
];


sumSimplify[expr_] := Simplify[expr,
  TransformationFunctions -> sumsimplfuncs];

(* sumFullSimplify first calls sumFullSimplifyOLD, then applies additional
  rules to furher simplify the resulting expression. *)

sumFullSimplify[expr_] := FullSimplify[expr,
  TransformationFunctions -> Append[Prepend[sumsimplfuncs, sumFullSimplifyOLD],
    sumCollect[sumMomentumCanonical[sumExpand[#]]] &] ];


(* Annihilation vs. creation operators! *)

isannihilation[op_[t_, j___]] /; (ordering[op] == EMPTY) := (t == AN);
iscreation[op_[t_, j___]] /; (ordering[op] == EMPTY) := (t == CR);

(* Override the following two rules if necessary! *)
isannihilation[op_[t_, k_, j___]] /; (ordering[op] == SEA) :=
  (t == AN && k > 0) || (t == CR && k < 0);
iscreation[op_[t_, k_, j___]] /; (ordering[op] == SEA) :=
  (t == CR && k > 0) || (t == AN && k < 0);

(* For bosonic operators *)
isannihilation[op_[t_, j___]] /; bosonQ[op] := (t == AN);
iscreation[op_[t_, j___]] /; bosonQ[op] := (t == CR);


(* ### Vacuum expectation value ### *)

sneglinearoperatorFirst /@ {vev, vevwick};

(* Single operator (not enclosed in nc[]) *)
SetAttributes[{vev, vevwick}, Listable];
vev[_?fermionQ[__]] := 0;
vevwick[_?fermionQ[__]] := 0;
vevwick2[_?fermionQ[__]] := 0;

vev[_?bosonQ[__]] := 0;
vevwick[_?bosonQ[__]] := 0;
vevwick2[_?bosonQ[__]] := 0;

(* Odd number of operators *)
vev[x:HoldPattern[nc[_?fermionQ[__]..]]] /; OddQ[Length[x]] := 0;
vevwick[x:HoldPattern[nc[_?fermionQ[__]..]]] /; OddQ[Length[x]] := 0;

vev[x:HoldPattern[nc[_?bosonQ[__]..]]] /; OddQ[Length[x]] := 0;

(* The following rule is useful in some situations, for example when
normalorder[] is used to obtain a normal ordered operator string (which may
feature a vev term), to which we then apply vev anew: this combined
operation should yield 0. *)

vev[vev[x__]] := vev[x];
vev[vevwick[x__]] := vevwick[x];
vevwick[vev[x__]] := vevwick[x];
vevwick[vevwick[x__]] := vevwick[x];

(* vev is applied to addends in symbolic sums *)

vev[sum[ops_, it_List]] := sum[ vev[ops], it ];
vevwick[sum[ops_, it_List]] := sum[ vevwick[ops], it ];

(* By definition, the vacuum expectation value of a normal ordered
expression is zero! *)

vev[dd[_]] := 0;
vevwick[dd[_]] := 0;

(* Annihilates vacuum to the right *)

vev[ nc[___, x:op_?fermionQ[__]] ] /; isannihilation[x] := 0;
vevwick[ nc[___, x:op_?fermionQ[__]] ] /; isannihilation[x] := 0;

vev[ nc[___, x:op_?bosonQ[AN,___]] ] := 0;


(* Annihilates vacuum to the left *)

vev[ nc[x:op_?fermionQ[__], ___] ] /; iscreation[x] := 0;
vevwick[ nc[x:op_?fermionQ[__], ___] ] /; iscreation[x] := 0;

vev[ nc[x:op_?bosonQ[CR,___]] ] := 0;


(* Evaluate residual bosonic annihilation-creation inversions without
changing the ordering of the original nc expression. *)
vev[HoldPattern[nc[
    a___,
    x1_,
    x2_,
    b___
  ]]] /; (TrueQ[bosonQ[x1]] && TrueQ[bosonQ[x2]] &&
    TrueQ[isannihilation[x1]] && TrueQ[iscreation[x2]] &&
    (ordering[Head[x1]] === NONE || ordering[Head[x2]] === NONE)) :=
  vev[nc[a, x2, x1, b]] +
    If[Head[x1] === Head[x2],
      cmt[x1, x2] vev[nc[a, b]],
      0
    ];


(* Special rules for vev in the case of ordering=SEA *)

(* (1-n_k) |0> *)
vev[nc[a___, op_[AN, k_, j___], op_[CR, k_, j___]]] /;
  (fermionQ[op] && ordering[op] === SEA) := UnitStep[k] vev[nc[a]];

(* <0| (1-n_k) *)
vev[nc[op_[AN, k_, j___], op_[CR, k_, j___], b___]] /;
  (fermionQ[op] && ordering[op] === SEA) := UnitStep[k] vev[nc[b]];

(* n_k |0> *)
vev[nc[a___, op_[CR, k_, j___], op_[AN, k_, j___]]] /;
  (fermionQ[op] && ordering[op] === SEA) := UnitStep[-k] vev[nc[a]];

(* <0| n_k *)
vev[nc[op_[CR, k_, j___], op_[AN, k_, j___], b___]] /;
  (fermionQ[op] && ordering[op] === SEA) := UnitStep[-k] vev[nc[b]];

(* VEV of two operators with two different sets of ultimate indexes is zero.
Here we need to be extremely careful about the free indexes which can take
an arbitrary value. Note the four-argument version of the If[] statement! *)

vev[nc[op_[i1_, k1_, j1___], op_[i2_, k2_, j2___]]] := 0 /;
  fermionQ[op] &&
  ordering[op] === SEA && If[{j1} != {j2}, True, False, False];

(* Special rule. The last index is spin and we assume that the vacuum
state has well defined z-component of spin. The first index is operator
type, which can also be interpreted as the z-component of the isospin,
and we assume that the vacuum has well define particle number. The
total isospin and spin of the operator string must be equal to zero. *)

vev[x:HoldPattern[nc[_?((fermionQ[#] && ordering[#] === SEA)&)
  [_?((# === CR || # === AN)&), _, ___, _?((# === UP || # === DO)&)]..]] ] :=
  Module[{tmp, isospin, spin},
  tmp = Map[(# /. opp_[ii_,kk_,jj___, ss_] :> {ii, ss}) &, List @@ x];
  isospin = tmp[[All,1]];
  spin = tmp[[All,2]];
  isospin = Map[If[# == CR, 1/2, -1/2]&, isospin];
  spin = Map[If[# == UP, 1/2, -1/2]&, spin];
  (* Note: change of spin depends on the isospin of the operator! *)
  spin = spin (2isospin); (* elementwise multiplication *)

  isospin = Plus @@ isospin;
  spin = Plus @@ spin;
  
  0 /; (isospin != 0 || spin != 0)
];


(* Finally, an explicit procedure for bilinear forms in the case of SEA
ordering. This is then used for more general expression, for example in
conjunction with Wick's theorem. *)

vev[nc[op1_[i1_, k1_, j1___], op2_[i2_, k2_, j2___]]] :=
  If[op1 === op2,
    KroneckerDelta[{j1}, {j2}] KroneckerDelta[k1, k2] *
    (If[i1 == CR && i2 == AN, UnitStep[-k1], 0]
    + If[i1 == AN && i2 == CR, UnitStep[k1], 0]),
    0
  ] /;
  fermionQ[op1] && fermionQ[op2] &&
  ordering[op1] === SEA && ordering[op2] === SEA;


(*** Inner and outer products using nc ***)

inner[a_, b_, c___] := Inner[nc, a, b, c];
outer[a_, b_, c___] := Outer[nc, a, b, c];

(* Vector-matrix-vector product using inner multiplication *)
VMV[v1_, m_, v2_] := (v1 ~ inner ~ m) ~ inner ~ v2;

(* Direct product of two lists. May be used in in basis construction. *)
direct[l1_List, l2_List] := Flatten @ outer[l1, l2];

(*** Auxiliary functions for operator construction ***)

(* Abstract-orbital Function arguments are creation-defined: fn[CR, sigma]
   is authoritative. The corresponding annihilation expression is always
   derived as conj[fn[CR, sigma]]; fn[AN, sigma] is never evaluated. *)

(* Number operator *)
SetAttributes[number, Listable];

number[op_?fermionQ[j___], sigma_] := op[CR, j, sigma] ~ nc ~ op[AN, j, sigma];
number[op_?fermionQ[j___]] /; spinof[op] == 1/2 :=
  number[op[j], UP] + number[op[j], DO];
number[op_?fermionQ[j___]] /; spinof[op] != 1/2 := Module[{s},
  Sum[number[op[j], s], {s, -spinof[op], +spinof[op]}]
];

(* Number operator for abstract function argument *)
number[fn_Function, sigma_] := With[{cr = fn[CR, sigma]},
  cr ~ nc ~ conj[cr]
];
number[fn_Function] := number[fn, UP] + number[fn, DO];

(* Number operator for bosonic operators *)
number[op_?bosonQ[j___]] := op[CR, j] ~ nc ~ op[AN, j];

number[op_?AtomQ, a___] := number[op[], a];

(* Number-1 squared operator, i.e. z-component of isospin squared *)
SetAttributes[isozsq, Listable];
isozsq[op_?fermionQ[j___]] := pow[number[op[j]]-1, 2];

(* Hubbard's local e-e repulsion operator *)
SetAttributes[hubbard, Listable];
hubbard[op_?fermionQ[j___]] /; spinof[op] == 1/2 :=
  number[op[j], UP] ~ nc ~ number[op[j], DO];
hubbard[op_?fermionQ[j___]] /; spinof[op] != 1/2 := Module[{smin, smax, s1, s2},
  smin = -spinof[op];
  smax = +spinof[op];
  Sum[Sum[number[op[j], s1] ~ nc ~ number[op[j], s2],
    {s1, s2+1, smax}], {s2, smin, smax}]
];

hubbard[op_?fermionQ, a___] /; AtomQ[op] := hubbard[op[], a];

(* The Hubbard model for operator op[], nn sites and parameters
   U, t and eps *)
hamiltonian[Hubbard, op_[j___], nn_, U_, t_, eps_] := Module[{i},
  Sum[eps number[op[i, j]] + U hubbard[op[i, j]], {i, 1, nn}] +
  t Sum[hop[op[i, j], op[i+1, j]], {i, 1, nn-1}]
];

(* Inter-site charge-charge interaction *)
chargecharge[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  number[op1[j1]] ~ nc ~ number[op2[j2]];

(* Spin operators (Pauli matrices) for S=1/2 fermions. Note that
they must be multiplied by 1/2 to obtain true spin operators. *)
PauliX = {{0, 1}, {1, 0}};
PauliY = {{0, -I}, {I, 0}};
PauliZ = {{1, 0}, {0, -1}};
PauliPlus = PauliX + I PauliY;
PauliMinus = PauliX - I PauliY;

(* Spin X, Y and Z operators for an arbitrary generalized creation operator
g, i.e. some interesting combination of one-site creation operators. g must
be a pure function of spin index. *)
spinxyzgen::usage =
"spinxyzgen[g] generates x, y, and z components of a S=1/2 spin
operator corresponding to an (operator) expression g. g must be a
pure function of the spin index. For example, the conventional spin
operators are obtained by spinxyzgen[op[CR, #]&].";

spinxyzgen[g_] := Module[{cr, an},
  cr = {g[UP], g[DO]};
  an = conj /@ cr;
  1/2 VMV[cr, #, an]& /@ {PauliX, PauliY, PauliZ}
];

spinxyzgen[g_, spin_?halfintegerQ] := Module[{cr, an, s},
  cr = Table[g[s], {s, +spin, -spin, -1}];
  an = conj /@ cr;
  VMV[cr, #, an]& /@
    {spinmatrixX[spin], spinmatrixY[spin], spinmatrixZ[spin]}
];

SetAttributes[{spinxyz, spinall, spinss, spinx, spiny, spinz,
  spinplus, spinminus}, Listable];

(* Returns the x, y and z components of the spin operator. *)
spinxyz[op_?fermionQ[j___]] /; spinof[op] == 1/2 :=
  spinxyzgen[op[CR, j, #]&];
spinxyz[op_?fermionQ[j___]] /; spinof[op] != 1/2 :=
  spinxyzgen[op[CR, j, #]&, spinof[op]];

spinxyz[op_] /; AtomQ[op] := spinxyz[op[]];

(* Spin-1/2 operator for abstract function argument. *)
spinxyz[fn_Function] := spinxyzgen[fn[CR, #]&];

(* Returns the x, y, and z components of the spin operator, as well as
the S^2 operator *)
spinall[op_?fermionQ[j___]] := Module[{sxyz, ss},
  sxyz = spinxyz[op[j]];
  ss = sxyz ~ inner ~ sxyz;
  Append[sxyz, ss]
];


(* Shorthand calls for generation of individual spin operators. *)
spinss[op_?fermionQ[j___]] := spinall[op[j]] [[4]];
spinx[op_?fermionQ[j___]] := spinall[op[j]] [[1]];
spiny[op_?fermionQ[j___]] := spinall[op[j]] [[2]];
spinz[op_?fermionQ[j___]] := spinall[op[j]] [[3]];

spinplus[op_?fermionQ[j___]] :=  Simplify[ spinx[op[j]] + I spiny[op[j]] ];
spinminus[op_?fermionQ[j___]] := Simplify[ spinx[op[j]] - I spiny[op[j]] ];

spinss[op_] /; AtomQ[op] := spinss[op[]];
spinx[op_] /; AtomQ[op] := spinx[op[]];
spiny[op_] /; AtomQ[op] := spiny[op[]];
spinz[op_] /; AtomQ[op] := spinz[op[]];
spinplus[op_] /; AtomQ[op] := spinplus[op[]];
spinminus[op_] /; AtomQ[op] := spinminus[op[]];

(* Scalar product of spin operators for op1 and op2 *)

spinspin[fn1_Function, fn2_Function] := Module[{sp1, sp2},
  sp1 = spinxyz[fn1];
  sp2 = spinxyz[fn2];
  Simplify[ sp1 ~ inner ~ sp2 ]
];

spinspin[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  Simplify[ spinxyz[op1[j1]] ~ inner ~ spinxyz[op2[j2]] ];

spinspin[op1_, op2_] /; AtomQ[op1] && AtomQ[op2] := spinspin[op1[], op2[]];

spinspin[op1_?fermionQ[j1___], fn2_Function] :=
  spinspin[op1[#1, j1, #2]&, fn2];

spinspin[fn1_Function, op2_?fermionQ[j2___]] :=
  spinspin[fn1, op2[#1, j2, #2]&];

spinspin[op1_?spinQ[j1___], op2_?spinQ[j2___]] := Module[{i},
  Sum[nc[op1[j1, i], op2[j2, i]], {i, 3}]
];

(* Recall: S1 . S2 = S1^z S2^z + 1/2 (S1^+ S2^- + S1^- S2^+) *)
(* Note: spinspin[a,b] = spinspinz[a,b] + spinspinxy[a,b] -> no factor 1/2
   in this definition! *)

(* Transverse part of the spin-spin interaction *)
spinspinpm[op1_?fermionQ[j1___],op2_?fermionQ[j2___]] :=
  spinplus[op1[j1]] ~ nc ~ spinminus[op2[j2]];

spinspinmp[op1_?fermionQ[j1___],op2_?fermionQ[j2___]] :=
  spinminus[op1[j1]] ~ nc ~ spinplus[op2[j2]];

spinspinx[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  Expand[ spinx[op1[j1]]~nc~spinx[op2[j2]] ];

spinspiny[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  Expand[ spiny[op1[j1]]~nc~spiny[op2[j2]] ];

spinspinxy[op1_?fermionQ[j1___],op2_?fermionQ[j2___]] := Module[{sp1, sp2},
  sp1 = spinspinpm[op1[j1], op2[j2]];
  sp2 = spinspinmp[op1[j1], op2[j2]];
  (sp1+sp2)/2
];

(* Longitudinal part of the spin-spin interaction *)
spinspinz[op1_?fermionQ[j1___],op2_?fermionQ[j2___]] := Module[{},
  spinz[op1[j1]] ~ nc ~ spinz[op2[j2]]
];

(* Given a list of operators 'ops', manyspin[] constructs the
components of the total spin, as well as the total spin squared operator. *)
manyspin[ops_List] := Module[{sxyz, ss},
  sxyz = Expand @ (Plus @@ Map[spinxyz, ops]);
  ss = Expand @ (sxyz ~ inner ~ sxyz);
  Append[sxyz, ss]
];

(* Returns True if z is half-spin or entire spin *)
halfintegerQ[z_] := IntegerQ[2z];

(* Spin matrices for arbitrary spin *)
spinmatrixZ[S_?halfintegerQ] :=
    Table[If[sz1 == sz2, sz1, 0], {sz1, S, -S, -1}, {sz2, S, -S, -1}];
spinmatrixP[S_?halfintegerQ] :=
    Table[If[sz1 == sz2 + 1, Sqrt[S(S + 1) - sz2(sz2 + 1)], 0], {sz1,
          S, -S, -1}, {sz2, S, -S, -1}];
spinmatrixM[S_?halfintegerQ] :=
    Table[If[sz1 == sz2 - 1, Sqrt[S(S + 1) - sz2(sz2 - 1)], 0], {sz1,
          S, -S, -1}, {sz2, S, -S, -1}];
spinmatrixX[S_?halfintegerQ] := 1/2(spinmatrixP[S] + spinmatrixM[S]);
spinmatrixY[S_?halfintegerQ] := 1/(2I)(spinmatrixP[S] - spinmatrixM[S]);


(* Nambu spinor *)
SetAttributes[nambu, Listable];
nambu[op_?fermionQ[j___], n_:0] := {op[CR, j, UP], (-1)^n op[AN, j, DO]};
nambu[op_?AtomQ, n___] := nambu[op[], n];

(* BCS pairing operator *)
bcs[op_?fermionQ[j1___], Phi_:0] :=
  Exp[ I Phi] nc[op[CR, j1, UP], op[CR, j1, DO]] +
  Exp[-I Phi] nc[op[AN, j1, DO], op[AN, j1, UP]]

bcs[Delta_, op_?fermionQ[j1___]] :=
  Delta nc[op[CR, j1, UP], op[CR, j1, DO]] +
  Conjugate[Delta] nc[op[AN, j1, DO], op[AN, j1, UP]]

(* X, Y and Z componentes of the isospin operator. *)

SetAttributes[{isospinxyz, isospin, isospinx, isospiny, isospinz,
  isospinplus, isospinminus}, Listable];

isospinxyz[op_?fermionQ[j___], n_:0] := Module[{nam, nbar},
  nam = nambu[op[j], n];
  nbar = conj /@ nam;
  1/2 VMV[nam, #, nbar]& /@ {PauliX, PauliY, PauliZ}
];

isospin[x__] := isospinxyz[x];

(* Shorthand calls for generating individual isospin operators. *)
isospinx[op_?fermionQ[j___], n_:0] := isospin[op[j], n] [[1]];
isospiny[op_?fermionQ[j___], n_:0] := isospin[op[j], n] [[2]];
isospinz[op_?fermionQ[j___], n_:0] := isospin[op[j], n] [[3]];

isospinx1[op_?fermionQ[j___], 0] := 1/2 nc[op[CR, j, UP], op[CR, j, DO]];
isospinx2[op_?fermionQ[j___], 0] := 1/2 nc[op[AN, j, DO], op[AN, j, UP]];

isospinplus[a__]  := Expand[isospinx[a] + I isospiny[a]];
isospinminus[a__] := Expand[isospinx[a] - I isospiny[a]];

isospinx[op_, n___] /; AtomQ[op] := isospinx[op[], n];
isospiny[op_, n___] /; AtomQ[op] := isospiny[op[], n];
isospinz[op_, n___] /; AtomQ[op] := isospinz[op[], n];
isospinplus[op_, n___] /; AtomQ[op] := isospinplus[op[], n];
isospinminus[op_, n___] /; AtomQ[op] := isospinminus[op[], n];

(* Sum of isospin generators for operators enlisted in ops. *)
manyisospin::unequallength = "Different lengths: ops=`` ns=``.";
manyisospin[ops_List, ns1_:Null] := Module[{len, ns, ixyz, ii, i},
  len = Length[ops];
  If[ns1 === Null, ns = Table[0, {len}], ns = ns1];
  If[Length[ns] != len,
    Message[manyisospin::unequallength, ops, ns];
    Return[];
  ];
  ixyz = Expand @ (Plus @@ Table[isospin[ ops[[i]], ns[[i]] ], {i, len}]);
  ii = (ixyz ~ inner ~ ixyz) // Expand;
  Append[ixyz, ii]
];

(* Basis states for a single site *)
SetAttributes[basis, Listable];
basis[op_?fermionQ[j___]] := Module[{ops, n},
  ops = mbfunc[op[j]];
  Join[{1}, Flatten[
    Table[(nc @@ Reverse[#])& /@ KSubsets[ops, n], {n, Length[ops]}],
    1]]
];

(* Integer powers of operators *)
SetAttributes[pow, Listable];
pow[op_, 0] := 1;
pow[op_, 1] := op;
pow[op_, i_Integer] /; i>1 := nc[pow[op, i-1], op];

pow::negativepower = "Negative power detected: (``)^(``)";
pow[op_, i_] /; i<0 := (Message[pow::negativepower, op, i]; Null);

SetAttributes[fastpow, Listable];
fastpow[op_, 0] := 1;
fastpow[op_, 1] := Simplify[op];
fastpow[op_, i_Integer] /; i>1 := Simplify @ nc[fastpow[op, i-1], Simplify[op]];

fastpow::negativepower = "Negative power detected: (``)^(``)";
fastpow[op_, i_] /; i<0 := (Message[fastpow::negativepower, op, i]; Null);

(* Additional optimizations for low powers. They make use of Mathematica's
   result-caching capabilities. *)
fastpow[op_, 4] := Simplify @ nc[fastpow[op, 2], fastpow[op, 2]];
fastpow[op_, 6] := Simplify @ nc[fastpow[op, 4], fastpow[op, 2]];
fastpow[op_, 8] := Simplify @ nc[fastpow[op, 4], fastpow[op, 4]];

(* DANGEROUS: avoid using Power directly! *)
(* The modified Power operators is can be used only when defined explicitly
   by calling the function definencPower[]. *)
definencPower[] := Module[{},
  Unprotect[Power];
  Power[expr_, i_Integer] := pow[expr, i] /;
   (!isnumericQ[expr] && 
    (Count[expr, nc[__]] != 0 || Count[expr, _?operatorQ] != 0) &&
    i >= 0);
  Protect[Power];
];

(* Hopping operator *)
SetAttributes[hop, Listable];
hop[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  op1[CR, j1, sigma] ~ nc ~ op2[AN, j2, sigma] +
  op2[CR, j2, sigma] ~ nc ~ op1[AN, j1, sigma];

hop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  hop[op1[j1], op2[j2], UP] + hop[op1[j1], op2[j2], DO];

hop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /;
  (spinof[op1] == spinof[op2] != 1/2) := Module[{s},
  Sum[ hop[op1[j1], op2[j2], s], {s, -spinof[op1], spinof[op1]} ]
];

hop[fn1_Function, fn2_Function, sigma_] := With[{
  seed = fn1[CR, sigma] ~ nc ~ conj[fn2[CR, sigma]]},
  seed + conj[seed]
];

hop[fn1_Function, fn2_Function] :=
  hop[fn1, fn2, UP] + hop[fn1, fn2, DO];

hop[fn1_Function, op2_?fermionQ[j2___]] := hop[fn1, op2[#1, j2, #2]&];
hop[op1_?fermionQ[j1___], fn2_Function] := hop[op1[#1, j1, #2]&, fn2];

(* Matrix-valued hopping. Rows and columns are ordered as {UP, DO}.
   This constructor is not Listable: a matrix is a single argument. *)
matrixhop::matrix = "Expected a 2 by 2 hopping matrix, got ``.";
matrixhop::op = "Expected a spin-1/2 fermion operator or an abstract-orbital Function, got ``.";

matrixhopCreation[op_?fermionQ[j___]] /; TrueQ[spinof[op] == 1/2] :=
  {op[CR, j, UP], op[CR, j, DO]};
matrixhopCreation[fn_Function] := {fn[CR, UP], fn[CR, DO]};
matrixhopCreation[op_] := (Message[matrixhop::op, op]; $Failed);

matrixhop[T_, op1_, op2_] := Module[
  {mat, cr1, cr2, terms, paulis, coefficients, conjugates},
  If[!MatrixQ[T] || Dimensions[T] =!= {2, 2},
    Message[matrixhop::matrix, T];
    Return[$Failed]
  ];
  mat = If[Head[T] === SparseArray, Normal[T], T];
  cr1 = matrixhopCreation[op1];
  If[cr1 === $Failed, Return[$Failed]];
  cr2 = matrixhopCreation[op2];
  If[cr2 === $Failed, Return[$Failed]];
  terms = Outer[nc, cr1, conj[cr2]];

  (* T = Sum[t_mu sigma_mu], with t_mu = Tr[T.sigma_mu]/2.
     Simplify the four scalars, rather than the operator expression.
     Work with evaluated matrices so that precomputed MatrixExp results
     benefit as well. Numeric matrices and matrices containing inexact
     numbers use the direct contraction below to avoid roundoff from
     decomposing and reconstructing their entries. *)
  If[!MatrixQ[mat, NumericQ] && FreeQ[mat, _?InexactNumberQ],
    paulis = {IdentityMatrix[2], PauliX, PauliY, PauliZ};
    coefficients = Simplify[ExpToTrig[Tr[mat.#]/2 & /@ paulis]];
    If[LeafCount[coefficients] < LeafCount[Flatten[mat]],
      terms = Total[Flatten[# terms]] & /@ paulis;
      conjugates = Simplify[Conjugate[coefficients]];
      Return[Total[MapThread[
        Which[
          #1 === #2, #1 (#3 + conj[#3]),
          #1 === -#2, #1 (#3 - conj[#3]),
          True, #1 #3 + #2 conj[#3]
        ] &, {coefficients, conjugates, terms}]]]
    ]
  ];

  (* Matrix entries are scalars even if they have not been declared as
     SNEG constants. Conjugate them separately from the operator terms. *)
  Total[Flatten[mat terms + Conjugate[mat] conj[terms]]]
];

(* Generic hopping with a complex-valued parameter t *)
genhop[t_, op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  t op1[CR, j1, sigma]~nc~op2[AN, j2, sigma] +
  Conjugate[t] op2[CR, j2, sigma]~nc~op1[AN, j1, sigma];

genhop[t_, op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /; (spinof[op1] == spinof[op2] == 1/2) :=
  genhop[t, op1[j1], op2[j2], UP] + genhop[t, op1[j1], op2[j2], DO];

(* Anomalous hopping, a a + a^+ a^+ *)
(* This version is not SU(2) invariant. See below for spin-rotation-invariant version (anhop). *)
SetAttributes[anomaloushop, Listable];
anomaloushop[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  op1[CR, j1, sigma]   ~ nc ~ op2[CR, j2, 1-sigma] +
  op2[AN, j2, 1-sigma] ~ nc ~ op1[AN, j1, sigma];

anomaloushop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  anomaloushop[op1[j1], op2[j2], UP] + anomaloushop[op1[j1], op2[j2], DO];

anomaloushop[fn1_Function, fn2_Function, sigma_] := With[{
  seed = fn1[CR, sigma] ~ nc ~ fn2[CR, 1-sigma]},
  seed + conj[seed]
];

anomaloushop[fn1_Function, fn2_Function] :=
  anomaloushop[fn1, fn2, UP] + anomaloushop[fn1, fn2, DO];

anomaloushop[fn1_Function, op2_?fermionQ[j2___]] := anomaloushop[fn1, op2[#1, j2, #2]&];
anomaloushop[op1_?fermionQ[j1___], fn2_Function] := anomaloushop[op1[#1, j1, #2]&, fn2];

(* anhop has a different sign convention as anomaloushop and is a singlet tensor
operator with respect to spin SU(2) symmetry. *)
SetAttributes[anhop, Listable];
anhop[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  op1[CR, j1, sigma]   ~ nc ~ op2[CR, j2, 1-sigma] +
  op2[AN, j2, 1-sigma] ~ nc ~ op1[AN, j1, sigma];

anhop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  anhop[op1[j1], op2[j2], UP] - anhop[op1[j1], op2[j2], DO]; (* sign! *)

anhop[fn1_Function, fn2_Function, sigma_] := With[{
  seed = fn1[CR, sigma] ~ nc ~ fn2[CR, 1-sigma]},
  seed + conj[seed]
];

anhop[fn1_Function, fn2_Function] :=
  anhop[fn1, fn2, UP] - anhop[fn1, fn2, DO]; (* sign! *)

anhop[fn1_Function, op2_?fermionQ[j2___]] := anhop[fn1, op2[#1, j2, #2]&];
anhop[op1_?fermionQ[j1___], fn2_Function] := anhop[op1[#1, j1, #2]&, fn2];

(* Anomalous hopping with a complex-valued parameter t *)
SetAttributes[genanhop, Listable];
genanhop[t_, op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  t op1[CR, j1, sigma]   ~ nc ~ op2[CR, j2, 1-sigma] +
  Conjugate[t] op2[AN, j2, 1-sigma] ~ nc ~ op1[AN, j1, sigma];

genanhop[t_, op1_?fermionQ[j1___], op2_?fermionQ[j2___]] /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  genanhop[t, op1[j1], op2[j2], UP] - genanhop[t, op1[j1], op2[j2], DO]; (* sign! *)

(* Hopping with spin-flip *)
SetAttributes[spinfliphop, Listable];
spinfliphop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] := Module[{sigma},
  Sum[
    op1[CR, j1, sigma] ~ nc ~ op2[AN, j2, 1-sigma] +
    op2[CR, j2, sigma] ~ nc ~ op1[AN, j1, 1-sigma],
  {sigma, DO, UP} ]
];

(* Hopping with hole operators:
c^\dag_{k,\sigma} f_\sigma} + f^\dag_\sigma c_{k\sigma}
\to
h_{k,-\sigma} f_\sigma + f^\dag_\sigma h^\dag_{k,-\sigma}
*)

SetAttributes[holehop, Listable];
holehop[oph_?fermionQ[jh___], opf_?fermionQ[jf___], sigma_] :=
  oph[AN, jh, 1-sigma] ~ nc ~ opf[AN, jf, sigma] +
  opf[CR, jf, sigma] ~ nc ~ oph[CR, jh, 1-sigma];

holehop[oph_?fermionQ[jh___], opf_?fermionQ[jf___]] := Module[{sigma},
  Sum[holehop[oph[jh], opf[jf], sigma], {sigma, DO, UP}]
];

(* 2-electron hopping operator *)
SetAttributes[twohop, Listable];
twohop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  nc[ op1[CR, j1, DO], op1[CR, j1, UP], op2[AN, j2, DO], op2[AN, j2, UP] ] +
  nc[ op2[CR, j2, DO], op2[CR, j2, UP], op1[AN, j1, DO], op1[AN, j1, UP] ];

(* Hopping with a phase change *)
hopphi[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_, phi_] :=
  Exp[I phi]  op1[CR, j1, sigma] ~ nc ~ op2[AN, j2, sigma] +
  Exp[-I phi] op2[CR, j2, sigma] ~ nc ~ op1[AN, j1, sigma];

hopphi[op1_?fermionQ[j1___], op2_?fermionQ[j2___], phi_] /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  hopphi[op1[j1], op2[j2], UP, phi] +
  hopphi[op1[j1], op2[j2], DO, phi];

(* Hopping with spin-flip and phase change *)
SetAttributes[spinfliphopphi, Listable];
spinfliphopphi[op1_?fermionQ[j1___], op2_?fermionQ[j2___], phi_] :=
  Module[{sigma},
    Sum[Exp[I phi] op1[CR, j1, sigma]~nc~op2[AN, j2, 1 - sigma] +
    Exp[-I phi] op2[CR, j2, sigma]~nc~op1[AN, j1, 1 - sigma],
          {sigma, DO, UP}]
  ];

(* Current operator *)
(*
 H = hopphi[d[1], d[2], phi]
 j = d/dt N1  (i.e. current from orbital 2 to orbital 1!!)
 j = i[H,N1] = current[d[1], d[2], phi]
*)

currentphi[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_, phi_] :=
  hopphi[op1[j1], op2[j2], sigma, phi-Pi/2];

currentphi[op1_?fermionQ[j1___], op2_?fermionQ[j2___], phi_]  /;
  (spinof[op1] == spinof[op2] == 1/2) :=
  currentphi[op1[j1], op2[j2], UP, phi] +
  currentphi[op1[j1], op2[j2], DO, phi];

current[op1_?fermionQ[j1___], op2_?fermionQ[j2___], sigma_] :=
  currentphi[op1[j1], op2[j2], sigma, 0];

current[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] :=
  currentphi[op1[j1], op2[j2], 0];

(****************** MISC ********************)

sneglinearoperator[invertspin];
SetAttributes[invertspin, Listable];

invertspin[] := 1;
invertspinindex[j_] := (1-j); (* This works for S=1/2 only! *)

(* Inverts the spin of all operators (spin reflection) *)
invertspin[op_?fermionQ[i_, k___, j_]] :=
  op[i, k, invertspinindex[j]];

invertspin[HoldPattern[ nc[a__] ]] := nc @@ Map[invertspin, {a}];



(************** AP **************)

(* FINITE-BASIS CONVENTION:
   mbfunc[], BASIS, vacuum[], ap[], the basis generators, vc/operator
   conversions, and lrmap[] use bare-particle occupations relative to the
   zero-particle vacuum. They represent modes using op[CR,...] and do not
   follow ordering[op].

   Consequently, this subsystem does not construct a Fermi-sea
   quasiparticle/hole basis for SEA operators. Operator-form finite bases
   are supported only with EMPTY-vacuum semantics. *)

(* mbfunc[] expands each generic operator a[j] into a pair of creation
operators for both spin orientations. This function is used in basis
construction in makebasis[] and in lrmap[] for constructing a mapping
between left and right sites under a mirror symmetry. It can be
generalized for other symmetries (i.e. orbital indexes) and other
representations of the SU(2) group (i.e. S=1 representation instead of
S=1/2). *)

SetAttributes[mbfunc, Listable];
mbfunc[op_[j___]] /; spinof[op] == 1/2 :=
  { op[CR, j, UP], op[CR, j, DO] };
mbfunc[op_[j___]] /; spinof[op] != 1/2 := Module[{s},
  Table[ op[CR, j, s], {s, +spinof[op], -spinof[op], -1} ]
];


(* BASIS holds a (global) list of creation operators for all possible
   single-particle states. It is constructed using a call to makebasis[],
   see below. Creation operators for all many-particle states in the (Q,S)
   basis are constructed by combining these operators using suitable
   Clebsch-Gordan coefficients. *)

If[!ValueQ[BASIS], BASIS = {}];

(* Construct basis for common enlisted operators.
   Example: makebasis[{a[],f[1]}] 
   Expansion over both spin orientations is automatically done. *)

makebasis[l_List] := BASIS = Flatten @ (mbfunc @ l);
makebasis[l__] := makebasis[{l}];

(* Is the operator in the basis? Disregards CR vs. AN type. *)
(* NOTE: op_fermionQ would be inappropriate here! *)
goodopQ[op_[i_, j___]] := Count[BASIS, op[CR, j]] == 1;

(* Mapping of operators to positions in state vectors. An index
   is only returned if the operator actually exists in the basis,
   otherwise the function call is unevaluated. *)
SetAttributes[{op2ndx, ndx2op}, Listable];
op2ndx[op_?fermionQ[i_, j___]] := Position[BASIS, op[CR, j]] [[1, 1]] /;
  goodopQ[op[i, j]];

ndx2op[n_Integer] := BASIS[[n]];


(* Vector notation *)

(* The default coloring function for vector objects assumes that the
basis is formed by spin-1/2 fermions and eventually an additional
element from the ket basis. *)

vccolorDefault[i_List] := Module[{a, b},
  a = If[OddQ[Length[i]], Most[i], i];
  a = a /. {0 -> "\[EmptyRectangle]", 1 -> "\[FilledRectangle]"};
  b = Flatten[Map[{StyleForm[First[#], FontColor -> Red],
    StyleForm[Last[#], FontColor -> Blue]} &, Partition[a, 2]], 1];
  If[OddQ[Length[i]], AppendTo[b, Last[i]]];
  b
];

(* Empty or filled black rectangles. *)

vccolorSimple[i_List] :=
  i /. {0 -> "\[EmptyRectangle]", 1 -> "\[FilledRectangle]"};

vccolor := vccolorDefault;

If[PrettyOutput,
  Unprotect[vc];

  Format[vc[i__], StandardForm] :=
   DisplayForm @ RowBox[Join[
    {StyleForm["\[LeftDoubleBracketingBar]", FontColor -> Green]},
    vccolor[{i}],
    {StyleForm["\[RightAngleBracket]", FontColor -> Green]}]
  ];

  fixparens[vc];

  Protect[vc];
];

(* Prevent overwriting! *)
Protect[vc];

(* Vacuum vector: vc[0, 0, ..., 0] *)
vacuum[] := vc @@ Table[0, {Length[BASIS]}];

(* Special rule for computing scalar products *)
nc[a___, conj[v1_vc], v2_vc, b___] := scalarproductvc[v1, v2] nc[a, b];

(*** Applies an operator to a vector in occupation number representation ***)
 
sneglinearoperatorWithLast[ap];
SetAttributes[ap, Listable];

ap[a___, 0] := 0; (* special rule *)

ap[v_vc] := v;

ap[HoldPattern[x:nc[___]], v_vc] :=
  Fold[ ap[#2, #1]&, v, Reverse[ List @@ x] ];


(* NOTE: the following implementation of ap[] is valid for any spin of
the fermionic operators !!! *)

ap[op_?fermionQ[i_, j___], v_vc] := Module[{ndx, sign, ok},
  ndx = op2ndx[op[i, j]];
  sign = If[EvenQ[Length[Cases[Take[v, ndx-1], 1]]], 1, -1];
  ok = If[v[[ndx]] == i, 1, 0];
  ok sign MapAt[(1-#)&, v, ndx]
] /; goodopQ[op[i, j]];

(* Resolve unevaluated repeated applications of ap by multiplying
   the operators. This is particularly useful for applying Dirac bras.
   See below. *)
ap[a___, HoldPattern[ ap[b___, c_] ]] := ap[nc[a,b], c];

(* Special rules *)
ap[1, VACUUM] := vacuum[];
ap[a__, VACUUM] := ap[a, vacuum[]];

(* If a vc vector appears as the last element in a nc product, ap[] is
   called automatically. This allows for equivalent treatment of basis
   sets in occupation number and in creation operator representation
   as regards the application of operators expression to the basis
   states. *)
nc[a___, v_vc] := ap[nc[a], v];

(* vc2ops[] transforms a vc[] expression (occupation number
representation) for a state into an equivalent expression in terms of
the creation operators. *)

sneglinearoperator[vc2ops];
SetAttributes[vc2ops, Listable];

(* Special case: expressions with a trailing ket vector. *)
vc2ops[vc[l___, ket[k__]]] := nc[vc2ops[vc[l]], ket[k]];

vc2ops::incom = "Incompatible lenghts of vc[] and BASIS.";
vc2ops[x:vc[l___]] := Module[{a},
  If[Length[{l}] != Length[BASIS],
    Message[vc2ops::incom]; Infinity,
    nc @@ Pick[BASIS, x, 1]
  ]
];

(* Inverse operation *)
ops2vc[op_, vak_] := ap[op, vak];
ops2vc[op_] := ap[op, vacuum[]];

bzvc2bzop[bvc_] := Map[{First[#], vc2ops[#[[2]]]}&, bvc];
(* If the vacuum state vac is not provided, vacuum[] is used. *)
bzop2bzvc[bz_, vak_] := Map[{First[#], ap[#[[2]], vak]}&, bz];
bzop2bzvc[bz_] := bzop2bzvc[bz, vacuum[] ];


(* It is very important that the integers in vc[] are not converted to real
values by application of N[] to some state. *)
SetAttributes[vc, NHoldAll];

(* Scalar product of an expressions with number representation vectors *)
(* IMPORTANT: by definition, an inner product is linear in the first argument
and conjugate-linear in the second argument, i.e., it is a sesquilinear form!
In physics, on the other hand, the convention is that the first argument is
conjugate-linear (bra), while the second argument is linear (ket).
As of sneg.m v202 (17.11.2009), scalarproductvc[] follows the second convention,
in order to be in conformance with the definition of scalarproductop[]. *)

snegsesquilinearoperator[scalarproduct];
snegsesquilinearoperator[scalarproductvc];
SetAttributes[{scalarproduct, scalarproductvc, scalarproductop}, Listable];

scalarproductvc[
  vc[a___, ket[k1___]], vc[b___, ket[k2___]]
] /; pairpattern[{k1}, {k2}] :=
  scalarproductvc[vc[a], vc[b]] braketrule[bra[k1], ket[k2]];

scalarproductvc[a_vc, b_vc] := If[a === b, 1, 0];

scalarproductvc[scalar[z_] a_, b_] := scalar[conj[z]] scalarproductvc[a, b];
scalarproductvc[a_, scalar[z_] b_] := scalar[z] scalarproductvc[a, b];

scalarproductop[scalar[z_] a_, b_] := scalar[conj[z]] scalarproductop[a, b];
scalarproductop[a_, scalar[z_] b_] := scalar[z] scalarproductop[a, b];

scalarproductvc[a_, 0.] := 0;
scalarproductvc[0., b_] := 0;
scalarproductvc[a_, 0.+I 0.] := 0;
scalarproductvc[0.+I 0., b_] := 0;

scalarproduct[a___] = scalarproductvc[a]; (* an alias *)

(* This is essentially a bracket of two operator expressions *)
scalarproductop[a_, b_] := vevwick[conj[a] ~ nc ~ zeroonvac[b]];

SetAttributes[{expvop, expvvc, braketop, braketvc}, Listable];

(* Helper function: this is a braket <a|op|b> (creation operator rep) *)
braketop[a_, op_, b_] := Module[{ levo, desno },
  levo = conj[a] // Expand;
  desno = op ~ nc ~ b // Expand;
(* desno = onvac @ desno; *)
  vev[levo ~ nc ~ desno // Expand]
  ];

(* Helper function: this is a braket <a|op|a> (creation operator rep) *)
expvop[op_, a_] := braketop[a, op, a];
  
(* Helper function: this is a braket <a|op|b> (occupation number rep) *)
braketvc[a_, op_, b_] := scalarproductvc[a, ap[op, b]];

(* An alias *)
braket[x___] := braketvc[x];

(* Helper function: this is a braket <a|op|a> (occupation number rep) *)
expvvc[op_, a_] := braketvc[a, op, a];

(* Construct a matrix representation of operator 'a' in basis 'l'.
   Note: 'l' must be expressed in occupation number representation. *)
matrixrepresentationvc[a_, l_List] := Module[{bl},
  bl = Map[ap[a, #]&, l];
  Outer[scalarproductvc, l, bl]
];

(* Construct a matrix representation of operator 'a' between two
   different subspaces 'l1' and 'l2'. States 'l1' are bras,
   while states 'l2' are kets. *)
matrixrepresentationvc[a_, l1_List, l2_List] := Module[{bl},
  bl = Map[ap[a, #]&, l2];
  Outer[scalarproductvc, l1, bl]
];

(* As above, but for operator expressions *)
matrixrepresentationop[a_, l_List] := Module[{bl},
  bl = Map[nc[a, #]&, l];
  Outer[scalarproductop, l, bl]
];

matrixrepresentationop[a_, l1_List, l2_List] := Module[{bl},
  bl = Map[nc[a, #]&, l2];
  Outer[scalarproductop, l1, bl]
];


(************* ASCII <-> SNEG expression *************)

(* a and b are delimiter strings, such as parenthesis. *)
snegToString[a_String, b_String] := StringJoin[a, b];

snegToString[a_String, b_String, i_] := StringJoin[a, ToString[i], b];

snegToString[a_String, b_String, l__] :=
  StringJoin[a, snegListToString[{l}], b];

snegListToString[l_List] := addcommas[ToString /@ l];

(* Concatenate a list of strings, separating them using commas *)
addcommas[l : {_String ..}] :=
  StringDrop[StringJoin[Map[# <> "," &, l]], -1];

(* Split a list of substrings separated by commas and transform them
to Mathematica expressions. *)
removecommas[s_String] := Map[ToExpression, StringSplit[s, ","]];
removecommas[""] := {};

(* Replace operators, kets, bras with string expressions. Concatenate
the expression, taking into account the noncommutative multiplication
nc[]. *)
snegtoasciirules = {
  op_[] /; operatorQ[op] :> ToString[op] <> "()",
  op_[t_, i___] /; operatorQ[op] :>
    ToString[op] <> If[t == CR, "+", ""] <> snegToString["(", ")", i],
  ket[] :> "|>",
  ket[i__] :> snegToString["|", ">", i],
  bra[] :> "<|",
  bra[i__] :> snegToString["<", "|", i],
  HoldPattern[nc[l : _String ..]] :> StringJoin[l],
  HoldPattern[nc[a___, s1_String, s2_String, b___]] :>
    nc[a, StringJoin[s1, s2], b]
};

snegtoascii[expr_] := expr //. snegtoasciirules;

(* Try to build an operator expression by parsing a string. *)

buildop[op_String, type_] := With[{x = ToExpression[op]},
  x[type] /; (x =!= $Failed && operatorQ[x])];

buildop[op_String, type_, margs_String] :=
  With[{x = ToExpression[op], arglist = removecommas[margs]},
    x @@ Prepend[arglist, type] /; (x =!= $Failed && operatorQ[x])];

buildket[n_String] := ket @@ removecommas[n];

buildbra[n_String] := bra @@ removecommas[n];

snegstringparse[s_String] := StringCases[s, {
  (op : Except[Characters["|<>()+"]] ..) ~~
  "+(" ~~ (margs : Except[Characters[")"]] ...) ~~ ")" :>
    buildop[op, CR, margs],
  (op : Except[Characters["|<>()+"]] ..) ~~
  "(" ~~ (margs : Except[Characters[")"]] ...) ~~ ")" :>
    buildop[op, AN, margs],
  "|" ~~ (ndx : Except[Characters["|<>()"]] ...) ~~ ">" :>
    buildket[ndx],
  "<" ~~ (ndx : Except[Characters["|<>()"]] ...) ~~ "|" :>
    buildbra[ndx],
   str__ :> str
}];

asciitosnegrules = {
  s_String :> nc @@ snegstringparse[s]
};

asciitosneg[expr_] := expr //. asciitosnegrules;

(************* (Q,Sz) BASIS **************)

(* Make a basis of good quantum numbers Q and S_z. This is trivially done in
the occupation number representation vc[...]. *)
(* IMPORTANT: As of version 1.147, makebasis[] is called automatically. *)

(* Spin-1/2 specific implementation! *)
qszbasisvc[l_List, 1/2] := Module[{n = Length[l], ndxs},
  makebasis[l];
  ndx2digs[ndx_] := IntegerDigits[ndx, 2, n];

  qunum[up_, do_] := Module[{iup, ido},
    iup = Plus @@ ndx2digs[up];
    ido = Plus @@ ndx2digs[do];
    {iup+ido-n , 1/2 (iup-ido)}
  ]; (* Q ans S_z *)

  ndx2vc[up_, do_] := vc @@ interleave[ndx2digs[up], ndx2digs[do]];

  ndxs = Table[ {qunum[up, do], ndx2vc[up, do]},
    {up, 0, 2^n-1}, {do, 0, 2^n-1}];
  ndxs = Flatten[ndxs, 1];
  mergebasis[ndxs]
];

(* Generic implementation *)
qszbasisvc[l_List] := Module[{b, n, a, qmask, szmask},
  b = makebasis[l];
  n = Length[b];

  a = Table[ IntegerDigits[i, 2, n], {i, 0, 2^n-1}];
  qmask = Table[1, {n}];
  szmask = b /. {
    op_?fermionQ[CR, i___, SP_] /; spinof[op] == 1/2 :>
      If[SP == UP, 1/2, -1/2],
    op_?fermionQ[CR, i___, SP_] /; spinof[op] != 1/2 :> SP
  };
  a = Map[{{qmask.# - n/2, szmask.#}, vc @@ #}&, a];
  a = mergebasis[a];
  a
];

qszbasis[l_List, 1/2] := bzvc2bzop @ qszbasisvc[l, 1/2];
qszbasis[l_List] := bzvc2bzop @ qszbasisvc[l];

(* Interleave lists *)
interleave::usage =
"interleave[{a1, a2, ...}, {b1, b2, ..}] returns {a1, b1, a2, b2,...}.";
interleave[l__] := Flatten[ Transpose[{l}], 1];

(************* BASIS with no symmetries ***************)

(* Make basis with no symmetries, i.e. no good quantum numbers. *)
nonebasisvc[l_List] := Module[{n},
  n = Length[makebasis[l]];
  {{{}, Table[ vc @@ IntegerDigits[i, 2, n], {i, 0, 2^n-1}]}}
];

nonebasis[l_List] := bzvc2bzop @ nonebasisvc[l];

(********** BASIS with U(1) charge conservation symmetry **********)

(* Make a basis with good quantum number Q *)

qbasisvc[l_List] := Module[{n, b},
  n = Length[makebasis[l]];
  b = Table[ vc @@ IntegerDigits[i, 2, n], {i, 0, 2^n-1}];
  b = Map[{{Count[#, 1, {1}] - n/2}, {#}}&, b];
  b = mergebasis[b];
  b
];

qbasis[l_List] := bzvc2bzop @ qbasisvc[l];


(************* (Q,S) BASIS ***************)

(* Make a basis of good quantum numbers Q and S *)
(* Convention: always use the highest possible Sz, i.e. Sz == S ! *)

qsbasis::spin = "qsbasis requires spin-1/2 fermion operators. Unsupported operators: ``. Use qbasis, nonebasis, or qszbasis for spinless operators.";
qsbasisvc::spin = "qsbasisvc requires spin-1/2 fermion operators. Unsupported operators: ``. Use qbasisvc, nonebasisvc, or qszbasisvc for spinless operators.";

qsbasisSpinHalfOperatorQ[op_[___]] :=
  fermionQ[op] === True && TrueQ[spinof[op] == 1/2];
qsbasisSpinHalfOperatorQ[_] := False;

qsbasisBadOps[l_List] := Select[l, !qsbasisSpinHalfOperatorQ[#]&];

qsbasisvcold[l_List] := bzop2bzvc[ qsbasis[l] ];


qsbasisvc[l_List] := Module[{bad},
  bad = qsbasisBadOps[l];
  If[bad =!= {},
    Message[qsbasisvc::spin, bad];
    Return[$Failed]
  ];
  makebasis[l];
  If[l === {},
    Return[{{{0, 0}, {vacuum[]}}}]
  ];
  qsbasisvc[{}, l]
];

qsbasisvc[m_List, {}] := m;

qsbasisvc[{}, l_List] := Module[{op1, op2},
  op1 = First[l] /. op_[j___] :> op[CR, j, UP];
  op2 = spinflip[op1];
  qsbasisvc[ 
    {
      { {-1, 0}, {ap[1, VACUUM]} },
      { {0, 1/2}, {ap[op1, VACUUM]} },
      { {1, 0}, {ap[nc[op2, op1], VACUUM]} }
    }, Drop[l, 1]
  ]
];

qsbasisvc[m_List, l_List] := Module[{mmnew},
  mmnew = Simplify @ keymerge[ ADDvc[m, First[l]] ];
  qsbasisvc[mmnew, Rest[l]]
];

ADDvc[l_List, x:op_[___]] :=
  Map[ADDQSvc[#, x]&, l] ~ Flatten ~ 1;

ADDQSvc[l_List, x:op_[___]] :=
  Map[ADDONEvc[#, x]&, Map[{First[l],#}&, l[[2]] ] ] ~ Flatten ~ 1;

ADDONEvc[{{q_, s_}, a_}, op_[j___]] := Module[{op1, op2, res},
  op1 = op[CR, j, UP];
  op2 = spinflip[op1];
  res = {
    { {q-1, s}, {a} },
    { {q+1, s}, {nc[op2, op1, a]} },
    { {q, s+1/2}, {nc[op1, a]} }
  };
  If[s >= 1/2, Append[res, {
    {q, s-1/2}, { Sqrt[2s/(2s+1)] nc[op2, a] -
                  Sqrt[1/(2s+1)] nc[op1, spindownvc[a]] }
  }], res]
];

(* Change 0<->1 in the last index of an operator. This is just a faster
version of the more general invertspin[] function. *)
spinflip[op_[i_, j___, sigma_]] := op[i, j, 1-sigma];

(* We start with an empty list of (Q,S) states and the full list
   of all single-particle operators. *)
(* makebasis is called automatically. While not necessary, such behave
   is desirable for consistency with qszbasis[]. *)
qsbasis[l_List] := Module[{bad},
  bad = qsbasisBadOps[l];
  If[bad =!= {},
    Message[qsbasis::spin, bad];
    Return[$Failed]
  ];
  makebasis[l];
  If[l === {},
    Return[{{{0, 0}, {1}}}]
  ];
  qsbasis[{}, l]
];

(* Last step: when no other operators need to be added, we're done! *)
qsbasis[m_List, {}] := m;

(* First step: construct the three possible (Q,S) states from one
   single-particle creation operator. *)
qsbasis[{}, l_List] := Module[{op1, op2},
  op1 = First[l] /. op_[j___] :> op[CR, j, UP];
  op2 = spinflip[op1];
  qsbasis[
    {
      { {-1, 0}, {1} },
      { {0, 1/2}, {op1} },
      { {1, 0}, {nc[op2, op1]} }
    }, Drop[l, 1]
  ]
];

(* Intermediate steps: add one single-particle operator from list 'l'
   and construct suitable combination of operators with a well-defined
   (Q,S) quantum numbers. This step is reiterated until completion. *)
qsbasis[m_List, l_List] := Module[{mmnew},
  mmnew = Simplify @ keymerge[ ADD[m, First[l]] ];
  qsbasis[mmnew, Rest[l]]
];


(********** BASIS with SU(2) spin invariance only **********)

(* Approach: create a basis with (Q,S) quantum numbers, then
drop Q and finally merge subspaces with equal S quantum number. *)

sbasisvc[l_List] := Module[{bvc},
  bvc = qsbasisvc[l];
  bvc = Map[{ {#[[1,2]]}, #[[2]] }&, bvc];
  bvc = mergebasis[bvc];
  bvc
];

sbasis[{}] := (makebasis[{}]; {{{0}, {1}}});
sbasis[l_List] := bzvc2bzop @ sbasisvc[l];

(********** BASIS with U(1) spin invariance only **********)

szbasisvc[l_List] := Module[{bvc},
  bvc = qszbasisvc[l];
  bvc = Map[{ {#[[1,2]]}, #[[2]] }&, bvc];
  bvc = mergebasis[bvc];
  bvc
];

szbasis[l_List] := bzvc2bzop @ szbasisvc[l];

(***** Miscelaneous functions used in basis construction *****)

(* Returns 0 for operator expressions which annihilate vacuum *)
sneglinearoperatorFirst[zeroonvac];
SetAttributes[zeroonvac, Listable];

zeroonvac[y:op_?fermionQ[___]] :=
  If[isannihilation[y], 0, y];
zeroonvac[x:nc[a___, y:op_?fermionQ[i_, j___]]] :=
  If[isannihilation[y], 0, x];

zeroonvac[y:op_?bosonQ[AN,___]] := 0;
zeroonvac[x:nc[a___, y:op_?bosonQ[AN, j___]]] := 0;

(* zeroonvac only works in the space of fermion operators *)
zeroonvac[k_ket] := k;
zeroonvac[nc[a___, k_ket]] := nc[zeroonvac[nc[a]], k];

(* If no rule was matched, we return the argument. *)
zeroonvac[a_] := a;

nc[a___, x:op_?operatorQ[j___], VACUUM, b___] /;
  (isannihilation[x]) := 0;
nc[a___, conj[VACUUM], x:op_?operatorQ[j___], b___] /;
  (iscreation[x]) := 0;

(* Vacuum state must be normalized to 1! *)
(* vev[nc[conj[VACUUM], VACUUM]] = 1; *)
nc[a___, conj[VACUUM], VACUUM, b___] := nc[a, b];

(* VACUUM placeholder corresponds to the vacuum state in the fermionic
space. Grassman numbers can therefore be moved to the left of the
conj[VACUUM] placeholder. *)
nc[a___, conj[VACUUM], z_?grassmanQ, b___] :=
  nc[a, z, conj[VACUUM], b];

If[PrettyOutput,
  Format[VACUUM, StandardForm] :=
    StyleForm["\[LeftDoubleBracketingBar]\[Diameter]\[RightAngleBracket]",
      FontColor -> Magenta];
  Format[conj[VACUUM], StandardForm] :=
    StyleForm["\[LeftAngleBracket]\[Diameter]\[RightDoubleBracketingBar]",
      FontColor -> Magenta];
];


(* Smart spindown: determines which operators appear in the expressions,
builds a corresponding spin down operator and applies it to the expression. *)
SPINDOWN[a_] := nc[Plus @@ Map[spinminus, Union[Cases[a,
op_?operatorQ[i_, j___, sigma_] :> op[j], {0, Infinity} ]]], a];

(* Calculate a 'norm' for an operator expression, i.e. sqrt(<expr|expr>). *)

SetAttributes[{normop, normvc}, Listable];

normop[expr_] := Sqrt @ vev[ scalarproductop[expr, expr] ];
normvc[expr_] := Sqrt @ vev[ scalarproductvc[expr, expr] ];

(* applytocr[] applys an operator to an expression, drops parts which
annihilate the vacuum and "normalizes" the reminder *)
applytocr[op_, expr_] := Module[{b},
  b = zeroonvac[nc[op, expr]];
  If[b =!= 0, b / normop[b], 0]
];

(* Smarter spindown: drops parts which annihilate vacuum and "normalizes"
the remainder *)
spindown::Usage = "spindown[v] effectively applies the spin-lowering
operator to the state v in the creation operator representation.";
spindown[a_] := Module[{b},
  b = zeroonvac[SPINDOWN[a]];
  If[b =!= 0, b / normop[b], 0]
];


(* spindown function for states in the occupation number representation *)
spindownvc::Usage = "spindownvc[v] effectively applies the spin-lowering
operator to the state v in the occupation number representation.";

sneglinearoperator[spindownvc1];
spindownvc1[vc[a___, k:ket[___]]] := ap[k, spindownvc1[vc[a]]];
spindownvc1[a_vc] := Module[{b, fn},
  b = List @@ Partition[a,2];
  fn[vc[1,0], {i_}] := vc @@ (Join @@ MapAt[vc[0,1]&, b, i]);
  fn[_, {_}] := 0;
  Total[MapIndexed[fn, b]]
];

spindownvc[a_] := spindownvc1[a] // (If[#=!=0, #/normvc[#], 0])&;

(* Get sublists with a given key *)
keyselect::usage =
"keyselect[list, key] selects sublists with the first element equal to key.";
keyselect[l_, key_] := Select[l, First[#] == key &];

(* Merge sublists with the same first element (key) *)
(* Below there is another implementation mergebasis[] which does the same! *)

keymerge::usage =
"keymerge[list] joins the values of entries {key, values} with the same key.";
keymerge[l_] := Module[{keys, sublists},
  keys = Union[ l[[All,1]] ];
  sublists = Map[keyselect[l, #]&, keys];
  Table[{keys[[i]], sublists[[i,All,2]] // Flatten}, {i, Length[keys]}]
];

(* Add one (doublet) operator to a (Q,S) basis. This is used
   by qsbasis[] to generate the entire list of basis states. *)
ADD[l_List, x:op_[j___]] :=
  Map[ADDQS[#, x]&, l] ~ Flatten ~ 1;

ADDQS[l_List, x:op_[j___]] :=
  Map[ADDONE[#, x]&, Map[{First[l],#}&, l[[2]] ] ] ~ Flatten ~ 1;

ADDONE[{{q_, s_}, a_}, op_[j___]] := Module[{op1, op2, res},
  op1 = op[CR, j, UP];
  op2 = spinflip[op1];
  res = {
    { {q-1, s}, {a} },
    { {q+1, s}, {nc[op2, op1, a]} },
    { {q, s+1/2}, {nc[op1, a]} }
  };
  If[s >= 1/2, Append[res, {
    {q, s-1/2}, { Sqrt[2s/(2s+1)] nc[op2, a] -
    Sqrt[1/(2s+1)] nc[op1, spindown[a] ] }
  }], res] (* cf. Krishna-Murthy et al. *)
];

(**** Projection operators ****)

(* Indexing is the same as in T. Rejec's operachain.cpp code for
   evaluation of operator expressions *)

(* Short-cuts *)
ClearAttributes[{PROJ0, PROJUP, PROJDO, PROJ2, PROJ1, PROJ02}, Protected];

PROJ0::usage =
"Argument for projector[]. Subspace with zero occupancy.";
PROJUP::usage =
"Argument for projector[]. Subspace with spin up particle.";
PROJDO::usage =
"Argument for projector[]. Subspace with spin down particle.";
PROJ2::usage =
"Argument for projector[]. Subspace with double occupancy.";
PROJ1::usage =
"Argument for projector[]. Subspace with single occupancy.";
PROJ02::usage =
"Argument for projector[]. Subspace with either zero or double occupancy.";

PROJ0 = 1;
PROJUP = 2;
PROJDO = 3;
PROJ2 = 4;
PROJ1 = 5;
PROJ02 = 6;

SetAttributes[{PROJ0, PROJUP, PROJDO, PROJ2, PROJ1, PROJ02}, Protected];

SetAttributes[{projector, projector0, projectorUP, projectorDO,
projector1, projector2, projector02}, Listable];

(* Subspace with 0 particles *)
projector[op_[j___], 1] := nc[ 1-number[op[j], UP], 1-number[op[j], DO] ];
projector0[op_[j___]] := projector[op[j], PROJ0];

(* Subspace with spin up particle *)
projector[op_[j___], 2] := nc[   number[op[j], UP], 1-number[op[j], DO] ];
projectorUP[op_[j___]] := projector[op[j], PROJUP];

(* Subspace with spin down particle *)
projector[op_[j___], 3] := nc[   number[op[j], DO], 1-number[op[j], UP] ];
projectorDO[op_[j___]] := projector[op[j], PROJDO];

(* Subspace with two particles *)
projector[op_[j___], 4] := nc[   number[op[j], UP],   number[op[j], DO] ];
projector2[op_[j___]] := projector[op[j], PROJ2];

(* Subspace with one particle of either spin *)
projector[op_[j___], 5] := projector[op[j], 2] + projector[op[j], 3];
projector1[op_[j___]] := projector[op[j], PROJ1];

(* Subspace with occupancy 0 or 2 *)
projector[op_[j___], PROJ02] :=
  projector[op[j], PROJ0] + projector[op[j], PROJ2];
projector02[op_[j___]] := projector[op[j], PROJ02];

outofdiag02[op_[j___]] := nc[op[AN, j, DO], op[AN, j, UP]];
outofdiag20[op_[j___]] := nc[op[CR, j, UP], op[CR, j, DO]];
projectorE[op_[j___]] := (projector0[op[j]] + projector2[op[j]] + outofdiag02[op[j]] + outofdiag20[op[j]])/2;
projectorO[op_[j___]] := (projector0[op[j]] + projector2[op[j]] - outofdiag02[op[j]] - outofdiag20[op[j]])/2;

projectorEphi[op_[j___], phi_] := (projector0[op[j]] + projector2[op[j]] +
  Exp[I  phi] outofdiag02[op[j]] + Exp[-I  phi] outofdiag20[op[j]])/2;
projectorOphi[op_[j___], phi_ ] := (projector0[op[j]] + projector2[op[j]] -
  Exp[I  phi] outofdiag02[op[j]] - Exp[-I  phi] outofdiag20[op[j]])/2;

projector[op_?AtomQ, n___] := projector[op[], n];
projector0[op_?AtomQ] := projector0[op[]];
projectorUP[op_?AtomQ] := projectorUP[op[]];
projectorDO[op_?AtomQ] := projectorDO[op[]];
projector1[op_?AtomQ] := projector1[op[]];
projector2[op_?AtomQ] := projector2[op[]];
projector02[op_?AtomQ] := projector02[op[]];
projectorE[op_?AtomQ] := projectorE[op[]];
projectorO[op_?AtomQ] := projectorO[op[]];
projectorEphi[op_?AtomQ, phi_] := projectorEphi[op[], phi];
projectorOphi[op_?AtomQ, phi_] := projectorOphi[op[], phi];


(**** Operations on basis sets ****)
bzvc2bzop[bvc_] := Map[{First[#], vc2ops[#[[2]]]}&, bvc];
(* If the vacuum state vac is not provided, vacuum[] is used. *)
bzop2bzvc[bz_, vak_] := Map[{First[#], ap[#[[2]], vak]}&, bz];
bzop2bzvc[bz_] := bzop2bzvc[bz, vacuum[] ];

(* Decompose a vector in a given basis *)
decomposeop[vec_, basis_] := Map[scalarproductop[#, vec]&, basis];
decomposevc[vec_, basis_] := Map[scalarproductvc[#, vec]&, basis];


(**** Orthogonalization ****)

orthogop[vecs_?VectorQ, basis_?VectorQ] := Module[{m, l},
  m = Map[ decomposeop[#, basis]&, vecs ];
  m = Simplify[m]; (* See below! *)
  l = snegorthog[m];
  l = Simplify[l];
  l = Select[l, Not[snegzeroarrayQ[#]] &]; (* Drop proven zero vectors *)
  Expand @ Map[(# . basis)&, l]
];

(* TO DO: bottleneck! Can we do it in a more efficient manner? *)
orthogvc[vecs_?VectorQ, basis_?VectorQ] := Module[{m, l},
  m = Map[ decomposevc[#, basis]&, vecs ];

  (* Occasionally, when dealing with very complex sets of basis vectors,
  decomposevc[] returns large non-simplified expressions. We need to
  Simplify them before performing the orthogonalisation. *)

  m = Simplify[m];
  l = snegorthog[m];
  l = Simplify[l];
  l = Select[l, Not[snegzeroarrayQ[#]] &]; (* Drop proven zero vectors *)
  Expand @ Map[(# . basis)&, l]
];

(*** Projecting out a state ***)

(* Project out 'state' from a space of 'vecs'. Is is assumed that
   both 'state' and 'vecs' are normalized to 1.
   The resulting vectors are reorthogonalized. *)
remove1state[vecs_, state_] := Module[{vecs2},
  If[vecs != {},
      vecs2 = Map[# - scalarproductvc[state, #] state &, vecs];
      vecs2 = orthogvc[vecs2, vecs] // Expand,
      vecs2 = {}
  ];
  vecs2
];

(* Project out 'states' from a space of 'vecs'. *)
removestates[vecs_, states_] := Module[{},
  Fold[remove1state, vecs, states]
];

(**
Transformation from basis with well defined (Q,S) to a basis with well
defined (I,S) quantum numbers.

Strategy: take states with high Q (these are maximum weight states with
respect to the isospin SU(2) group), apply Tminus to generate other states
of the multiplets, and subtract those states from the lower Q subspaces.
Repeat until no low Q states remain. At this point, the basis consists only
of maximum-weight representative states, which is the (I,S) basis that we
wanted to construct.

NOTE: this function may also be applied to a (Q,Sz) basis to generate a
(I,Sz) basis. In fact, it is applicable to all (Q,...) -> (I,...)
transformations.
**)

transformQStoIS[baza_, TM_] := transformQStoIS2[TM, Reverse[baza], {}];

transformQStoIS::Tminus = "Operator Tminus is not defined.";
transformQStoIS[baza_] := Module[{},
  If[!ValueQ[Tminus], Message[transformQStoIS::Tminus]; Return[$Failed]];
  transformQStoIS2[Tminus, Reverse[baza], {}]
];

transformQStoIS2[TM_, {}, novabaza_] := dropemptysubspaces[novabaza];
transformQStoIS2[TM_, baza_, novabaza_] := Module[{nova2, ISv, ostali},
  {ISv, ostali} = transform1[baza, TM];
  nova2 = Append[novabaza, ISv];
  transformQStoIS2[TM, ostali, nova2]
];

(* baza is a basis List in the format as returned by function qsbasis[] *)
(* Tminus is the isospin lowering operator that we apply to the vectors in
   the subspace that we're subtracting from other subspaces *)
transform1[baza_, Tminus_] :=
 Module[{prvi, ostali, subspace, states, statesplus, pos, ISvec, i,
         nrqn, subvector, isubspace},
  prvi = First[baza];  (* The subspace that we are subtracting *)
  ostali = Rest[baza]; (* The subspaces that we are subtracting from *)

  (* subspace=(Q, ...) indexes. Note that no assumptions is made about
     the indexes past the first one. *)
  subspace = First[prvi];
  nrqn = Length[subspace]; (* Total number of quantum numbers *)
  (* (1,0,...) vector, used in subtraction of quantum numbers later on *)
  subvector = Table[If[i==1, 1, 0], {i, nrqn}];

  (* Q -> I change of the first quantum number *)
  isubspace = subspace; isubspace[[1]] = (1/2) isubspace[[1]];

  states = prvi[[2]];  (* Vectors *)

  (* Vectors to which we are consecutively applying Tminus *)
  statesplus = states;

  For[i = 1, True, i++,
    statesplus = Map[ap[Tminus, #] &, statesplus];
    statesplus = Expand @ statesplus; (* WAS: Simplify *)
    statesplus = DeleteCases[statesplus, 0]; (* Drop nul vectors *)
    If[statesplus == {},
      Break[]; (* We're done *)
    ];

    (* Normalize to 1 *)
    statesplus = Map[#/normvc[#] &, statesplus];
    statesplus = Expand @ statesplus; (* WAS: Simplify *)

    (* Find relevant subspaces from which we can project out *)
    pos = Position[ostali, x_ /; First[x] == subspace - (2i)subvector, {1},
                   Heads -> False];

    If[pos == {},
      Break[],
      (* Performing the projecting *)
      ostali = MapAt[{First[#], removestates[#[[2]], statesplus]} &,
              ostali, pos];
    ];
  ];

  (* Return {(I, ...) pair, vectors} and the matrix of remaining vectors *)
  ISvec = {isubspace, states};
  {ISvec, ostali}
];

(*** BRAKET ***)

(* Dirac's bra-ket notation for arbitrary states. This is useful to represent
non-fermionic degrees of freedom, such as bosons, spins, rotators, etc. *)

If[PrettyOutput,
  (* Enclose bras and kets in suitable angle brackets and bars (orange!),
     replace Null elements by small circles. *)
  PrettyOutputBraKetRule = { Null -> "\[SmallCircle]" };

  Format[bra[i___], StandardForm] :=
   DisplayForm @ RowBox[Flatten[{
    StyleForm["\[LeftAngleBracket]", FontColor -> Orange],
    If[{i} =!= {}, RowBox[{i} /. PrettyOutputBraKetRule], {}],
    StyleForm["\[RightBracketingBar]", FontColor -> Orange]
  }, 1]];
  Format[ket[i___], StandardForm] :=
   DisplayForm @ RowBox[Flatten[{
    StyleForm["\[LeftBracketingBar]", FontColor -> Orange],
    If[{i} =!= {}, RowBox[{i} /. PrettyOutputBraKetRule], {}],
    StyleForm["\[RightAngleBracket]", FontColor -> Orange]
  }, 1]];

  fixparens[bra];
  fixparens[ket];
];

(* Convention: always commute bras and kets to the right of the
fermionic (!) operators *)

nc[a___, x1:bra[b___], x2:op_?fermionQ[i___], d___] := nc[a, x2, x1, d];
nc[a___, x1:ket[k___], x2:op_?fermionQ[i___], d___] := nc[a, x2, x1, d];

(* Returns a list with True at non-Null positions *)
nonnullpattern[l_List] := nonnullQ /@ l;
nonnullQ[expr_] := (expr =!= Null);

(* Returns True if there are no conflicting non-Null elements in two lists. This
is used in concatenating kets and bras. *)
compatiblepattern[l1_List, l2_List] := (Length[l1] == Length[l2]) && (Nor @@
  Thread[And[nonnullpattern[l1], nonnullpattern[l2]]]);

(* Returns True if non-Null elements come in pairs. This is used in calculating
scalar product, i.e. expectation value. *)
pairpattern[l1_List, l2_List] := (Length[l1] == Length[l2]) && !(Or @@
  Thread[Xor[nonnullpattern[l1], nonnullpattern[l2]]]);

(* Return position that do not contain the Null element *)
nonnullpos[l_List] := Position[l, _?nonnullQ, {1, Infinity}, Heads->False];

(* Default rule for the expectation values: Kronecker delta over all non-Null indexes *)
braketrule[bra[b___], ket[k___]]  /; pairpattern[{b}, {k}] :=
  Module[{pos, l1, l2},
    pos = nonnullpos[{b}];
    l1 = Extract[{b}, pos];
    l2 = Extract[{k}, pos];
    Inner[KroneckerDelta, l1, l2, Times]
  ];

(* When bra and ket are juxtaposed, we can evaluate the scalar
product. We only do this when the bra and ket are compatible in the
sense that both have the same pair pattern: quantum numbers at
equivalent position are defined either in both bra and ket or in
neither. *)

nc[a___, x1:bra[b___], x2:ket[k___], d___] /; pairpattern[{b}, {k}] :=
  braketrule[x1, x2] nc[a, d]

(* bkcombine[]: combine two lists in such a manner that Null elements
are treated as placeholders that are filled only if the corresponding
element in the other list is non-Null. Used in concatenation of bras
and kets.*)
(* TO DO: This could be made to work even for multi-dimensional
lists. *)

bkcombine::incompat = "Incompatible pattern of non-Null elements";
bkcombine[l1_List, l2_List] := Module[{pos},
  pos = nonnullpos[l2];
  If[pos =!= {}, ReplacePart[l1, l2, pos, pos], l1]
] /; compatiblepattern[l1, l2];

bkcombine[l1_List, l2_List] := (Message[bkcombine::incompat];
  error[bkcombine, l1,l2]) /; !compatiblepattern[l1, l2];

(* Direct product: concatenates all indexes, if possible! *)
nc[a___, bra[i1___], bra[i2___], b___] /;
  compatiblepattern[{i1}, {i2}] := nc[a, bra @@ bkcombine[{i1}, {i2}], b];
nc[a___, ket[i1___], ket[i2___], b___] /;
  compatiblepattern[{i1}, {i2}] := nc[a, ket @@ bkcombine[{i1}, {i2}], b];

(* Tensor product of rank-one operators on disjoint Hilbert factors. A bare
bra-ket pair with complementary support remains unevaluated. *)
nc[a___,
   ket[k1___], bra[r1___],
   ket[k2___], bra[r2___],
   d___] /;
    pairpattern[{k1}, {r1}] &&
    pairpattern[{k2}, {r2}] &&
    compatiblepattern[{k1}, {k2}] :=
  nc[a,
    ket @@ bkcombine[{k1}, {k2}],
    bra @@ bkcombine[{r1}, {r2}],
    d];

(* Conjugation *)
conj[ket[k___]] := bra[k];
conj[bra[b___]] := ket[b];

(* Build an object with head 'head', and Null elements at positions where
'positionarray' contains zeros, and 'value' at positions where
'positionarray' contains ones. *)
snegbuild[head_, value_, positionarray_] :=
  head @@ (positionarray /. {0 -> Null, 1 -> value});
buildket[qn_, positionarray_] := snegbuild[ket, qn, positionarray];
buildbra[qn_, positionarray_] := snegbuild[bra, qn, positionarray];

(* Spin states and spin operators *)
spinket[s_?halfintegerQ, positionarray_:{1}] :=
  buildket[#, positionarray]& /@ Range[s, -s, -1];
spinbra[s_?halfintegerQ, positionarray_:{1}] :=
  buildbra[#, positionarray]& /@ Range[s, -s, -1];

spinketbraZ[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], spinmatrixZ[s], spinbra[s, positionarray]];
spinketbraP[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], spinmatrixP[s], spinbra[s, positionarray]];
spinketbraM[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], spinmatrixM[s], spinbra[s, positionarray]];
spinketbraX[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], spinmatrixX[s], spinbra[s, positionarray]];
spinketbraY[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], spinmatrixY[s], spinbra[s, positionarray]];

(* Identity operator *)
spinketbraI[s_?halfintegerQ, positionarray_:{1}] :=
  VMV[spinket[s, positionarray], IdentityMatrix[2s+1],
      spinbra[s, positionarray]];

(* Convenience definitions *)
spinketbraOPS[s_?halfintegerQ, positionarray_: {1}] :=
  (#[s, positionarray]) & /@
  {spinketbraX, spinketbraY, spinketbraZ, spinketbraP, spinketbraM};
spinketbraXYZ[s_?halfintegerQ, positionarray_: {1}] :=
  (#[s, positionarray]) & /@
  {spinketbraX, spinketbraY, spinketbraZ};

(* Spin-spin exchange coupling *)
spinketbraspinspin[{s1_?halfintegerQ, s2_?halfintegerQ}] :=
  Module[{sx1, sy1, sz1, sx2, sy2, sz2},
    {sx1, sy1, sz1} = spinketbraXYZ[s1, {1, 0}];
    {sx2, sy2, sz2} = spinketbraXYZ[s2, {0, 1}];
    Expand[sx1~nc~sx2 + sy1~nc~sy2 + sz1~nc~sz2]
  ];

(* Generates a basis for spin operators. The key is the value of
   the Z-component of spin, S_z. *)
spinbasis[s_?halfintegerQ, positionarray_:{1}] :=
   {{#}, {buildket[#, positionarray]}}& /@ Range[s, -s, -1];

(* Generates a direct-product basis for two spin operators. The key
   is the Z-component of the TOTAL spin. *)
spinbasis[{a_?halfintegerQ, b_?halfintegerQ}] :=
  basistensorproduct[spinbasis[a, {1, 0}],
                     spinbasis[b, {0, 1}],
                     Function[{sz1, sz2}, sz1+sz2] ];

spinbasis[spins:{_?halfintegerQ, _?halfintegerQ, __?halfintegerQ}] :=
  Module[{n, pos, bases},
    n = Length[spins];
    pos[i_] := Table[If[j == i, 1, 0], {j, n}];
    bases = MapIndexed[spinbasis[#1, pos[First[#2]]] &, spins];
    Fold[basistensorproduct[#1, #2, Function[{sz1, sz2}, sz1+sz2]] &,
         First[bases], Rest[bases]]
  ];

(* Apply rule to the basis states (vektor form) in 'baza'.
Used by transformbasis[]. *)
transformfunc[baza_, rule_] := Module[{vecs},
  vecs = Map[Simplify[ ap[rule, #] ]&, baza];
  orthogvc[vecs, baza]
];

(* Apply rule to an entire basis in the form of a list of {{Q. numbers},
{basis states}}... *)
transformbasis[bvc_List, rule_] := Module[{bvcnew},
  bvcnew = Map[{First[#], transformfunc[ #[[2]], rule ]}&, bvc];
  dropemptysubspaces[bvcnew]
];

(* Apply several rules to each basis element. *)
(* XXX: Untested! *)
transformbasismulti[bvc_List, rules_List] := Module[{bvcnew},
  bvcnew = Map[transformbasis[bvc, #]&, rules];
  bvcnew = Flatten[bvcnew, 1];
  mergebasis[bvcnew]
];

(* Merge the basis: gather basis states with the same "key". *)
mergebasis[bvc_List] := Module[{qn},
  qn = Union @ bvc[[All,1]]; (* Get all different quantum numbers. *)
  Map[{#, collectvalues[#, bvc]}&, qn]
];

(* Make a spinless basis by projecting out all states with spin-down
electrons. *)
spinlessbasis[bzin_List] := Module[{bz},
  bz = bzin //. { op_?fermionQ[CR, i___, DO] -> 0 };
  bz = Map[{First[#], DeleteCases[Last[#], 0]}&, bz];
  bz = mergebasis[bz];
  bz = DeleteCases[bz, {{___},{}}]
];

(* Get all basis vectors with the same quantum numbers (key). *)
collectvalues[key_, bvc_List] := Flatten @ 
  (Select[bvc, (First[#] === key)&] [[All,2]]);

(* Drop subspaces with no states from a given basis bvc *)
dropemptysubspaces[bvc_List] := Select[bvc, #[[2]] =!= {} &];

(* Apply a function to an entire basis. *)
applybasis[bvc_?bzQ, fn_] := Map[{First[#], fn /@ Last[#]}&, bvc];

(* Note: spin basis in S representation can be conveniently
constructed using qsbasis[] which includes a pseudofermion that
corresponds to the spin, and then projecting out zero and double
occupancy states, and finally replacing the remaining single particle
pseudofermion operators with spin kets. One should be careful about
the anticommutation operators, though. The pseudospin operator must be
moved to the rightmost position of the operator string by overriding
the conventional (anti)commutation rules. Of course, this only works
for spin 1/2. *)

QSpseudospin[bvc_List, x:op_?operatorQ[]] := Module[{vecs},
  vecs = transformbasis[bvc, projector[x, PROJ1]];
  vecs = applybasis[vecs, vc2ops];
  vecs = vecs /. op[j___] :> hold[op[j]];
  vecs = vecs //. nc[a___, x1:hold[op[j1___]], x2:b_?operatorQ[j2___], c___] :>
    -nc[a, x2, x1, c];
  vecs = vecs /. hold[op[i_, sp_, j___]] :> ket[If[sp == UP, 1/2, -1/2]]
];

(* basistensorproduct combines two basis sets bz1 and bz2 by forming
tensor products. Function qnfn defines how to combine the lists of
quantum numbers. *)
basistensorproduct[bz1_List, bz2_List, qnfn_:List] := Module[{fn},
  fn[{qn1_, vec1_}, {qn2_, vec2_}] :=
    {qnfn[qn1, qn2], Flatten @ outer[vec1, vec2]};
  mergebasis[Flatten[Outer[fn, bz1, bz2, 1], 1]]
];

(* Create a new basis by forming products of basis states from each of the
   basis given as arguments. qnmergefnc specifies how the lists of quantum numbers
   are to be merged to give the quantum number corresponding to the subspaces
   of the new basis. *)
(* XXX: See also basistensorproduct[] - essentially the same! *)
directproductbasis[basis1_List, basis2_List, qnmergefnc_:Join] :=
  Flatten[Outer[directproductsubspace[##, qnmergefnc] &, basis1, basis2, 1], 1];

(* Perform the merging with the pair of basis subspaces. *)
directproductsubspace[{qn1_List, states1_List}, {qn2_List, states2_List},
      qnmergefnc_] := {qnmergefnc[qn1, qn2], Flatten[outer[states1, states2], 1]};


(* Add a spin-ket for spin 'SP' to a (Q,Sz) basis. *)
QSZaddspinket[bvc_List, {SP_}] := QSZaddspinket[bvc, SP];
QSZaddspinket[bvc_List, SP_] := Module[{spinbz},
  spinbz = spinbasis[SP];
  basistensorproduct[spinbz, bvc, Function[{qn1, qn2},
    {First[qn2], Last[qn2] + First[qn1]}]]
];

(* Application of an operator to a occupation number representation vector *)
ap[x:ket[___], vc[]] := vc[x];
ap[x:ket[___], vc[j__]] := vc[j, x] /; Head[Last[{j}]] =!= ket;
ap[ket[i___], vc[j___, ket[k___]]] := vc[j, ket @@ bkcombine[{i}, {k}]] /;
  compatiblepattern[{i}, {k}];
ap[a___, x1:bra[i___], vc[j___, x2:ket[k___]]] := ap[a, vc[j]] braketrule[x1, x2] /;
  pairpattern[{i}, {k}];

ketbratensorproduct[x_] := x;

(* Tensor product of operator expressions in terms of ket,bra terms *)
ketbratensorproduct[x1_, x2_] := Module[{w1, w2, y1, y2, width},
  width[x_] := Max[0, Sequence @@ Cases[x,
    (ket[a___] | bra[a___]) :> Length[{a}], {0, Infinity}]];
  w1 = width[x1];
  w2 = width[x2];
  y1 = x1 /. {
    ket[a___] :> ket @@ Join[{a}, ConstantArray[Null, w2]],
    bra[a___] :> bra @@ Join[{a}, ConstantArray[Null, w2]]
  };
  y2 = x2 /. {
    ket[a___] :> ket @@ Join[ConstantArray[Null, w1], {a}],
    bra[a___] :> bra @@ Join[ConstantArray[Null, w1], {a}]
  };
  nc[y1, y2]
];

ketbratensorproduct[x1_, x2_, rest__] :=
  Fold[ketbratensorproduct, x1, {x2, rest}];

(* Merge argument lists of neighboring bra/ket terms. *)
ruletensor = {
  nc[a___, bra[i__], bra[j__], b___] :> nc[a, bra[i, j], b],
  nc[a___, ket[i__], ket[j__], b___] :> nc[a, ket[i, j], b]
};

(* Direct product of a fermionic basis and a (single) phonon basis
with up to cutoffs={Nph,...} phonons. *)

transformtoPH[bvc_List, cutoffs:{_Integer ..}] := Module[{vecs, fn, phbasis},
  phbasis = phononbasis[cutoffs];
  fn[x_] = nc[x, #]&;
  vecs = Map[applybasis[bvc, fn[#]]&, phbasis];
  mergebasis @ Flatten[vecs, 1]
];

phononbasis[Nph_Integer] := Table[ket[i], {i, 0, Nph}];
phononbasis[{Nph_Integer}] := phononbasis[Nph];

(* Phonon hamiltonian construction functions *)
phononnumber[Nph_Integer] := Sum[i nc[ket[i], bra[i]], {i, 0, Nph}];
phononplus[Nph_Integer] := Sum[ Sqrt[i+1] nc[ket[i+1], bra[i]], {i, 0, Nph-1}];
phononminus[Nph_Integer] := Sum[ Sqrt[i] nc[ket[i-1], bra[i]], {i, 1, Nph}];
phononx[Nph_Integer] := phononplus[Nph] + phononminus[Nph];

(* Extension to multiple phonons *)
phononbasis[cutoffs : {_Integer ..}] :=
  Flatten[Outer[(nc[##] //. ruletensor) &, Sequence @@ Map[phononbasis, cutoffs]], Length[cutoffs]-1];

phononid[Nph_Integer] := Sum[nc[ket[i], bra[i]], {i, 0, Nph}];

phononnumber::mode = "Mode index `1` must be an integer from 1 through `2`.";
phononplus::mode = "Mode index `1` must be an integer from 1 through `2`.";
phononminus::mode = "Mode index `1` must be an integer from 1 through `2`.";
phononx::mode = "Mode index `1` must be an integer from 1 through `2`.";

phononnumber[i_, cutoffs : {_Integer ..}] := Module[{nr, ops},
  nr = Length[cutoffs];
  If[!IntegerQ[i] || i < 1 || i > nr,
    Message[phononnumber::mode, i, nr];
    Return[$Failed]
  ];
  ops = ConstantArray[phononid, nr];
  ops[[i]] = phononnumber;
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononplus[i_, cutoffs : {_Integer ..}] := Module[{nr, ops},
  nr = Length[cutoffs];
  If[!IntegerQ[i] || i < 1 || i > nr,
    Message[phononplus::mode, i, nr];
    Return[$Failed]
  ];
  ops = ConstantArray[phononid, nr];
  ops[[i]] = phononplus;
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononminus[i_, cutoffs : {_Integer ..}] := Module[{nr, ops},
  nr = Length[cutoffs];
  If[!IntegerQ[i] || i < 1 || i > nr,
    Message[phononminus::mode, i, nr];
    Return[$Failed]
  ];
  ops = ConstantArray[phononid, nr];
  ops[[i]] = phononminus;
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononx[i_, cutoffs : {_Integer ..}] := Module[{nr, ops},
  nr = Length[cutoffs];
  If[!IntegerQ[i] || i < 1 || i > nr,
    Message[phononx::mode, i, nr];
    Return[$Failed]
  ];
  ops = ConstantArray[phononid, nr];
  ops[[i]] = phononx;
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

(*** LEFT/RIGHT SYMMETRY SUPPORT ***)

(* Given l, the list of operators, lrmap[] returns the transformation rules
that perform the left/right transformation. Since mbfunc[] has attribute
Listable, l can be a nested list. In this case, each sublist denotes sites
which are equivalent under the mirror symmetry. For example: {a[], {b[],
c[]}, d[]}: in this case, a maps to d and vice versa, but c and d are
invariant. Or for {{a[],b[]},{c[],d[]}}: a maps to c, b maps to d, and vice
versa. *)

lrmap[l_List] := Module[{l1, l2},
  l1 = Flatten @ (mbfunc /@ l);
  l2 = Flatten @ (mbfunc /@ Reverse[l]);
  Thread[l1 -> l2]
];

(* Perform an operation o on each subspace vectors *)

mapbasis::compat = "Incompatible subspaces";
mapbasis[bz1_, bz2_, o_:(1/2 Plus[##]&)] := Module[
  {subs1, vecs1, subs2, vecs2, vecs},
  subs1 = bz1[[All, 1]];
  vecs1 = bz1[[All, 2]];
  subs2 = bz2[[All, 1]];
  vecs2 = bz2[[All, 2]];
  If[subs1 =!= subs2,
    Message[mapbasis::compat];
    Return[];
  ];

  vecs = Table[o[vecs1[[i]], vecs2[[i]]], {i, Length[vecs1]}];

  Transpose[{subs1, vecs}]
];

(* Orthogonalize bz1 with respect to original basis bz *)

orthogbasisop::usage =
"orthogbasisop[bz1, bz0] orthogonalizes the basis bz1 with respect
to the original basis bz. Both basis sets must be given in the creation
operator representation.";
orthogbasisop[bz1_, bz_] := mapbasis[bz1, bz, orthogop];

orthogbasisvc::usage =
"orthogbasisvc[bz1, bz0] orthogonalizes the basis bz1 with respect
to the original basis bz. Both basis sets must be given in the occupation
number representation.";
orthogbasisvc[bvc1_, bvc_] := mapbasis[bvc1, bvc, orthogvc];

(* Append a quantum number 'qn' to the good quantum numbers of each subspace
in basis bz *)
appendquantumnumber[bz_, qn_] := Map[{ Append[First[#], qn], #[[2]] }&, bz];

(* Putting it all together: transform a given basis to an equivalent basis
which takes into account the l/r symmetry, i.e. parity. l is the list of
consecutive operators that are fed to lrmap[] function. Additional
transformation rules can be specified as an optional parameter
'snextrarule'.*)
(* The input basis 'bz' must be in the operator form. *)

transformtoLR[bz_, l_, snextrarule_:{}] := Module[{bz2, bzsim, bzasim},
  bz2 = bz /. Join[lrmap[l], snextrarule];
  bzsim  = mapbasis[bz, bz2, 1/2 Plus[##]& ];
  bzasim = mapbasis[bz, bz2, 1/2 Subtract[##]& ];
  {bzsim, bzasim} = Map[dropemptysubspaces@orthogbasisop[#, bz]&, {bzsim, bzasim}];
  bzsim  = appendquantumnumber[bzsim, 1];
  bzasim = appendquantumnumber[bzasim, -1];
  Sort @ Join[bzsim, bzasim]
];

transformtoLRvc[bz_, l_, vak_, snextrarule_:{}] := Module[
  {bvc, bz2, bzsim, bzasim, bvcsim, bvcasim},
  bvc = bzop2bzvc[bz, vak];
  bz2 = bz /. Join[lrmap[l], snextrarule];
  bzsim  = mapbasis[bz, bz2, 1/2 Plus[##]& ];
  bzasim = mapbasis[bz, bz2, 1/2 Subtract[##]& ];
  bvcsim  = bzop2bzvc[bzsim, vak];
  bvcasim = bzop2bzvc[bzasim, vak];
  {bvcsim, bvcasim} =
    Map[dropemptysubspaces@orthogbasisvc[#, bvc]&, {bvcsim, bvcasim}];
  bvcsim  = appendquantumnumber[bvcsim, 1];
  bvcasim = appendquantumnumber[bvcasim, -1];
  Sort @ Join[bvcsim, bvcasim]
];


(** Transformations of basis sets **)

(* Produce a vector with 1 on i-th site in a length-n vector *)
enain[i_, n_] := Table[If[j == i, 1, 0], {j, n}];

(* Given a set of operators "oldbasis", a set of operators "newbasis" and
a set of operator expressions for newbasis, produce the transformation
rules from the oldbasis to the new basis! *)
snegold2newrules[oldbasis_, newbasis_, rules_] := Module[{n, ob, mat, x, i},
  n = Length[oldbasis];
  If[n != Length[newbasis],
    Print["Mismatching lengths."];
    Return[];
  ];
  ob = Table[oldbasis[[i]] -> enain[i, n], {i, Length[oldbasis]}];
  mat = rules /. ob;
  x = Norm[mat . Transpose[Conjugate[mat]] - IdentityMatrix[n]];
  If[x != 0,
    Print["The transformation rule is not unitary."];
    Return[];
  ];
  mat = Inverse[mat];
  Thread[Rule[oldbasis, mat . newbasis]]
];


(*** WICK'S THEOREM ***)

(* Produce all possible pairs with elements taken from two lists *)
(* OPTIMIZATION: cache the result! *)
allpairs[m_List, n_List] := allpairs[m,n] = Flatten[Outer[List, m, n], 1];

(* Return a list of positions of CR/AN opperators *)
indecesCR[HoldPattern[nc[l__]]] := Flatten @ Position[{l}, op_[CR, j___], {1}];
indecesAN[HoldPattern[nc[l__]]] := Flatten @ Position[{l}, op_[AN, j___], {1}];

(* Return the maximal number of pairs that can be contracted. *)
maxcran[HoldPattern[x : nc[l__]]] :=
  Min[Length @ indecesCR[x], Length @ indecesAN[x]];

(* Returns True if there are no overlapping elements in the list of pairs *)
disjointQ[{l__}] :=
  Length[{l}] == 1 || Length[Union[l]] == (Plus @@ Map[Length, {l}]);

(* Conflicting lists of pairs are zeroed *)
zeroconflict[z_List] := Map[If[disjointQ[#], #, 0] &, z];

(* Drop 0 elements from a List *)
dropzeros[l_List] := Delete[l, Position[l, 0, {1}]];

(* Drop lists of pairs that have overlapping indecs *)
dropconflicts[l_List] := dropzeros @ zeroconflict[l];

(* Generate lists of nr pairs to be contracted *)
(* OPTIMIZATION: cache the result! *)
contractedpairs[nr_, l_List] := contractedpairs[nr,l] =
  dropconflicts @ KSubsets[l, nr];

(* Contraction of two operators according to the standard definition:
substract the normal ordered expression. *)
contraction[x1 : op1_[t1_, i1___], x2 : op2_[t2_, i2___]] /;
    (ordering[op1] =!= NONE || ordering[op2] =!= NONE) :=
       nc[x1, x2] - normalorder[x1 ~ nc ~ x2];

(* Bosonic endpoints are even. Preserve the historical odd behavior for all
other operators, including custom operator types. *)
wickparity[x_] := If[TrueQ[bosonQ[x]], 0, 1];

(* Keep legacy integer markers for odd endpoints. remainder[] converts them
to parity-tagged markers before calculating the permutation sign. *)
wickmark[n_Integer, x_] :=
  If[wickparity[x] === 0, wickslot[n, 0], n];

wickswapsign[p1_Integer, p2_Integer] :=
  If[p1 === 1 && p2 === 1, -1, 1];

(* Contract one pair. ctr is an order index that labels the contracted
endpoints, factor is the prefactor of the string and ops is the string that
we are currently applying the contraction to. *)

contractone[{ctr_, factor_, ops_List}, {i1_, i2_}] :=
  Module[{l = ops, c, x1 = ops[[i1]], x2 = ops[[i2]]},
    (* Preserve order! *)
    If[i1 < i2,
      c = contraction[x1, x2],
      c = contraction[x2, x1]
    ];
    l[[i1]] = wickmark[ctr, x1];
    l[[i2]] = wickmark[ctr, x2];
    {ctr + 1, c*factor, l}
  ];

(* Return the remaining operator string. Endpoint markers are commuted to
the beginning of the list and ordered. A sign is introduced only when both
crossing endpoints are odd. Note: List2 is a non-Listable List. *)

remainder[{ops__}] := Module[{ops2},
  (* Bare integers are legacy odd endpoint markers. Replace only at level 1
  so integer operator indexes remain untouched. *)
  ops2 = Replace[List2[ops], nr_Integer :> wickslot[nr, 1], {1}];
  ops2 = ops2 //. {
    List2[a___,
      p1:wickslot[nr1_Integer, q1_Integer],
      p2:wickslot[nr2_Integer, q2_Integer], b___] :>
        wickswapsign[q1, q2] List2[a, p2, p1, b] /; nr2 < nr1,
    List2[a___, x:c_[i__],
      p:wickslot[nr_Integer, q_Integer], b___] :>
        wickswapsign[wickparity[x], q] List2[a, p, x, b] /; operatorQ[c]
  };
  ops2 = ops2 /. {x:List2[i__] :> DeleteCases[x, _wickslot, {1}]};
  ops2 = ops2 /. {List2[i___] :> dd[nc[i]]}
];

(* Contract "pairs" in the operator string "ops" *)

contract[ops_List, pairs_List] := Module[{ctr, factor, indexes},
  {ctr, factor, indexes} = Fold[contractone, {1, 1, ops}, pairs];
  remainder[indexes] * factor
];

(* Do the Wick! *)

wickorder[0, HoldPattern[x : nc[l__]]] := dd[x];

wickorder[nr_ /; nr > 0, HoldPattern[x : nc[l__]]] := With[
  {
    cp = contractedpairs[nr, allpairs[indecesCR[x], indecesAN[x]]]
  },
  Plus @@ Map[contract[{l}, #] &, cp]
];

sneglinearoperator[wick];
SetAttributes[wick, Listable];

(* Write an operator string using normal ordered strings and contractions *)
wick[HoldPattern[x : nc[l__]]] := Module[{i, m},
  m = maxcran[x];
  Sum[wickorder[i, x], {i, 0, m}]
];
wick[] := 1;

(* wick for formal sums *)
wick[sum[a_, b_]] := sum[wick[a], b];

(* VEV calculation using the Wick theorem *)
(* Idea: 1) the expression is rewritten in terms of contractions and
normal ordered terms. 2) vev is taken. 3) All terms but the last one
(with nr=maxcran[x]) drop out since vev[dd[...]]=0. In other words,
we do the same calculation as wick[] does, but calling wickorder[]
for a single argument. *)

vevwick[HoldPattern[x : nc[l__]]] := wickorder[maxcran[x], x] /. {dd[_]->0};
vevwick[] := 1;

(* vevwick2[] is a more efficient implementation of vevwick[]: it
immediately drops terms which are zero. Note that here we made certain
assumptions about the vacuum state and the normal ordered products,
thus this version of vevwick is not fully general! It is much faster,
though. Added 4.3.2010 *)

sneglinearoperatorFirst[vevwick2];
vevwick2[HoldPattern[x : nc[l__]]] := Module[{ic, ia, crops, anops},
  ic = indecesCR[x];
  ia = indecesAN[x];
  If[Length[ic] != Length[ia], Return[0]];
  (* Bug trap *)
  crops = Check[Extract[x, Map[{#} &, ic]], Null];
  anops = Check[Extract[x, Map[{#} &, ia]], Null];
  If[crops === Null || anops === Null, Return[Problem[{l}]] ];
  (* Symbolic indexes may still contract through KroneckerDelta. *)
  If[Sort[Head /@ crops] =!= Sort[Head /@ anops], Return[0]];
  (* The term contributes. Perform the calculation! *)
  vevwick[x]
];


(* Generate an (I,S) basis, for example: quickISObasis[{a[],b[],c[]}].
NOTE: nnop[] will be used, if defined, otherwise all signs are taken to be
equal (this is likely not what you want!). *)

quickisobasis[basisops__, fnc_] := Module[{Tminus, nnop2},
  nnop2[i_] := If[ValueQ @ nnop[i], nnop[i], 0];
  Tminus = Simplify @ Total @ Map[isospinminus[#, nnop2[#]]&, basisops];
  bzvc2bzop @ transformQStoIS[fnc @ basisops, Tminus]
];

quickISObasis[basisops__]   := quickisobasis[basisops, qsbasisvc];
quickISOSZbasis[basisops__] := quickisobasis[basisops, qszbasisvc];
quickSU2basis[basisops__]   := quickisobasis[basisops, qbasisvc];

(* Product basis with double (charge, isospin, etc.) symmetry. *)
(* The result has quantum numbers (Q1, Q2), (I1, I2), etc. *)
quickDBL[basisops1_, basisops2_, fnc_] := Module[{bz1, bz2},
  bz1 = fnc @ basisops1;
  bz2 = fnc @ basisops2;
  makebasis[Join[basisops1, basisops2]];
  (* Drop the second index *)
  bz1 = mergebasis @ Map[{{#[[1, 1]]}, #[[2]]} &, bz1];
  bz2 = mergebasis @ Map[{{#[[1, 1]]}, #[[2]]} &, bz2];
  directproductbasis[bz1, bz2, Join[#1, #2] &]
];

(* Product basis with double (charge, isospin, etc.) symmetry and a total spin SZ projection. *)
(* The result has quantum numbers (Q1, Q2, SZ), (I1, I2, SZ), etc. *)
quickDBLSZ[basisops1_, basisops2_, fnc_] := Module[{bz1, bz2},
  bz1 = fnc @ basisops1;
  bz2 = fnc @ basisops2;
  makebasis[Join[basisops1, basisops2]];
  (* NOTE: we are summing S_z. *)
  mergebasis @ directproductbasis[bz1, bz2, {#1[[1]], #2[[1]], #1[[2]]+#2[[2]]} &]
];

(* bzQ[b] returns True if the argument b is of the correct form to
   represent a basis: a list of {{quantum numbers}, {states}} pairs. *)
bzQ[{ {{___},{___}}.. }] = True;
bzQ[_] = False;

(* makematricesbzop[] and makematricesbzvc[] produce a table of operator
   matrices for all invariant subspaces. The functions are appropriate for
   operators that are diagonal with respect to the conserved quantum numbers
   that are used to enumerate the invariant subspaces, i.e. singlets with
   respect to the group of symmetry operations which are explicitly taken
   into account in the problem. For general operators, use
   makeallmatricesbzop[] and makeallmatricesbzvc[] which combine all pairs
   of invariant subspaces. *)

makematricesbzop[op_, basis_?bzQ] := Map[
  {First[#], Simplify @ matrixrepresentationop[op, #[[2]], #[[2]]]}&, basis ];

makematricesbzvc[op_, basis_?bzQ] := Map[
  {First[#], Simplify @ matrixrepresentationvc[op, #[[2]], #[[2]]]}&, basis ];

(* Helper function for makeallmatricesbzvc : process a pair of invariant
   subspaces. FNC is either matrixrepresentationvc or
   matrixrepresentationop. *)

mambpair[{qn1_, vecs1_}, {qn2_, vecs2_}, op_, FNC_] := Module[{pair, matrep, tot},
  pair = {qn1, qn2};
  matrep = FNC[op, vecs1, vecs2];
  (* TRICK: Drop combinations of subspaces that do not contribute. *)
  tot = Total[Abs[matrep], 2];
  If[!TrueQ[tot == 0], {pair, matrep}, HoldComplete[Sequence[]]]
];

listofpairs[l1_, l2_] := Flatten[Outer[List, l1, l2, 1], 1];

(* A generalization of makematricesbzvc[] and makematricesbzop[] which
   handle operators which are not diagonal in the quantum numbers that
   enumerate the invariant subspaces. *)

makeallmatricesbzvc[op_, basis_] := ReleaseHold @
  mambpair[Sequence @@ #, op, matrixrepresentationvc]& /@
  listofpairs[basis, basis];

makeallmatricesbzop[op_, basis_] := ReleaseHold @
  mambpair[Sequence @@ #, op, matrixrepresentationop]& /@
  listofpairs[basis, basis];

(* Used in maskconstants[] and other functions. *)
getallconstants[] := Union[listrealconstants, listcomplexconstants,
  listintegerconstants, listfreeindexes];

(* maskconstants[] builds a list of replacement rules for all constants that
have been defined using snegrealconstants[] and similar functions. It
returns the rules and the inverse rules.  This is useful, since
Eigensystem[] has problems when parameters in the matrix are defined to have
special properties. *)

maskconstants[] := Module[{l, rl},
  l = getallconstants[];
  l = Map[# -> ToExpression[ToString[#] <> ToString[Unique[]]] &, l];
  {l, Reverse /@ l}
];

(* Perform operation op while all known constants have been masked. *)

maskOp[op_] := Module[{map, invmap, tmp, res},
  {map, invmap} = maskconstants[];
  tmp = Hold[op] /. map;
  res = ReleaseHold[tmp];
  res /. invmap
];

(* Sometimes arguments needs to be evaluated before being passed to
a function. *)

maskOp[op_, margs__] := Module[{map, invmap, tmp, res},
  {map, invmap} = maskconstants[];
  tmp = {margs} /. map;
  res = ReleaseHold[op] @@ tmp;
  res /. invmap
];

SetAttributes[maskOp, HoldFirst];

(* Cross product *)

mcross[a_List, b_List] := {
  nc[a[[2]], b[[3]]] - nc[a[[3]], b[[2]]],
  nc[a[[3]], b[[1]]] - nc[a[[1]], b[[3]]],
  nc[a[[1]], b[[2]]] - nc[a[[2]], b[[1]]]
};


(* +++ Simplification rules +++ *)

allsnegsimplifyrules = {Automatic, SnegSimplifyNumber, SnegSimplifySpin,
      SnegSimplifyHop, SnegSimplifyHubbard, SnegSimplifyMisc};

SnegSimplifyNumber[expr_] :=
  expr //. {nc[op_[CR, j___], op_[AN, j___]] :>
              HoldForm[number[op[j]]] /; bosonQ[op],
            nc[op_[CR, j___, sigma_], op_[AN, j___, sigma_]] :>
              HoldForm[number[op[j], sigma]] /; fermionQ[op],
            Expand[z_. (HoldForm[number[i_, 1]] + HoldForm[number[i_, 0]]) ] :>
              z HoldForm[number[i]]};

ruleSnegSimplifySpin =
  Block[{op1, op2, rule1, rule2, rule3, l1, l2, ir1, ir2, i1, i2},
  (* Note: By setting upvalues, the following rules will be removed
  when op1 and op2 go out of scope. *)
    operatorQ[op1] ^= True;
    operatorQ[op2] ^= True;
    fermionQ[op1] ^= True;
    fermionQ[op2] ^= True;
    l1 = {HoldForm @ spinx[op1[i1]],
          HoldForm @ spiny[op1[i1]],
          HoldForm @ spinz[op1[i1]]};
    l2 = {HoldForm @ spinx[op2[i2]],
          HoldForm @ spiny[op2[i2]],
          HoldForm @ spinz[op2[i2]]};
    ir1 = {op1 -> a_?operatorQ, op2 -> b_?operatorQ};
    ir2 = {op1 -> a, op2 -> b};

    (* Rules for a single spin operator *)

    rule1  = Thread[(spinxyz[op1[i1___]] /. ir1) -> (l1 /. ir2)];
    rule1a = Thread[(Expand[spinxyz[op1[i1___]]] /. ir1) -> (l1 /. ir2)];
    rule1b = Thread[(Expand[2spinxyz[op1[i1___]]] /. ir1) -> (2l1 /. ir2)];
    rule1c = Thread[(Expand[I spinxyz[op1[i1___]]] /. ir1) -> (I l1 /. ir2)];
    rule1d = Thread[(Expand[2I spinxyz[op1[i1___]]] /. ir1) -> (2I l1 /. ir2)];
    rule1e = Thread[(Simplify[2I spinxyz[op1[i1___]]] /. ir1) -> (2I l1 /. ir2)];
    rule1f = Thread[(Simplify[-2I spinxyz[op1[i1___]]] /. ir1) -> (-2I l1 /. ir2)];

    (* Rules for products of spin operators *)

    rule2 = Thread[
          Flatten[Expand[
                  4 outer[spinxyz[op1[i1___]], spinxyz[op2[i2___]]]] /.
                ir1, 1] ->
            Flatten[Expand[4 outer[l1, l2]] /. ir2, 1]];

    rule2b = Thread[
          Flatten[Expand[
                  outer[spinxyz[op1[i1___]], spinxyz[op2[i2___]]]] /.
                ir1, 1] ->
            Flatten[Expand[outer[l1, l2]] /. ir2, 1]];

    (* Some symmetrized combinations of spin - spin products *)
    rule2a = {
          (* xx + yy *)
          Rule @@ (rule2[[1]] + rule2[[5]] /.
                HoldPattern[i_ -> j_] :> Expand[-1/2{z_. i, z j}]),
          (* xy + yx *)
          Rule @@ (rule2[[2]] + rule2[[4]] /.
                HoldPattern[i_ -> j_] :> Expand[I/2{z_. i, z j}]),
          (* xz + zx *)
          Rule @@ (rule2[[3]] + rule2[[7]] /.
                HoldPattern[i_ -> j_] :> Expand[{z_. i, z j}]),
          (* yz + zy *)
          Rule @@ (rule2[[6]] + rule2[[8]] /.
                HoldPattern[i_ -> j_] :> Expand[-I{z_. i, z j}])
          };

    (* Rules for combining the components of spin operator pairs *)
    rule3 = {Expand[z_. inner[l1, l2] /.
               Join[ir1, {i1 -> i1___, i2 -> i2___}]] ->
                (z HoldForm[spinspin[op1[i1], op2[i2]]] /. ir2)};

    Join[rule3,
         rule1, rule1a, rule1b, rule1c, rule1d, rule1e, rule1f,
         rule2, rule2b, rule2a]
  ];

SnegSimplifySpin[expr_] := Simplify[expr, TransformationFunctions ->
  {Automatic, Function[x, x //. ruleSnegSimplifySpin]}];

ruleSnegSimplifyHop = Block[{op1, op2, ir1, ir2, rule1, rule2},
  operatorQ[op1] ^= True;
  operatorQ[op2] ^= True;
  fermionQ[op1] ^= True;
  fermionQ[op2] ^= True;
  ir1 = {op1 -> a_?operatorQ, op2 -> b_?operatorQ};
  ir2 = {op1 -> a, op2 -> b};
  rule1 = Expand[z_. hop[op1[i___], op2[j___]] /. ir1] ->
    (z HoldForm[hop[op1[i], op2[j]]] /. ir2);
    rule2 = Expand[z_. twohop[op1[i___], op2[j___]] /. ir1] ->
    (z HoldForm[twohop[op1[i], op2[j]]] /. ir2);
  {rule1, rule2}
 ];

SnegSimplifyHop[expr_] := expr //. ruleSnegSimplifyHop;

ruleSnegSimplifyHubbard = Block[{op1},
  operatorQ[op1] ^= True;
  fermionQ[op1] ^= True;
  (-hubbard[op1[i___]] /. {op1 -> a_?operatorQ}) -> -HoldForm[
    hubbard[a[i]]]
 ];

SnegSimplifyHubbard[expr_] := expr //. ruleSnegSimplifyHubbard;

ruleSnegSimplifyMisc = Block[{op1, op2, ir1, ir2, rule1, rule2},
  operatorQ[op1] ^= True;
  operatorQ[op2] ^= True;
  fermionQ[op1] ^= True;
  fermionQ[op2] ^= True;
  ir1 = {op1 -> a_?operatorQ, op2 -> b_?operatorQ};
  ir2 = {op1 -> a, op2 -> b};
  (* Sz.Sz + n.n rule *)
  rule1 = (Expand[z_. (2spinz[op1[i___]]~nc~spinz[op2[j___]] -
                  1/2chargecharge[op1[i___], op2[j___]]) ] /. ir1) ->
           z (2nc[HoldForm[spinz[op1[i]]], HoldForm[spinz[op2[j]]]] -
                  1/2HoldForm[chargecharge[op1[i], op2[j]]] /. ir2);
  rule2 = (Expand[z_. (-2spinz[op1[i___]]~nc~spinz[op2[j___]] -
                  1/2chargecharge[op1[i___], op2[j___]]) ] /. ir1) ->
           z (-2nc[HoldForm[spinz[op1[i]]], HoldForm[spinz[op2[j]]]] -
                  1/2HoldForm[chargecharge[op1[i], op2[j]]] /. ir2);
  {rule1, rule2}
];

SnegSimplifyMisc[expr_] := expr //. ruleSnegSimplifyMisc;

(* SnegSimplify[] works well for "simple" expressions *)

SnegSimplify[expr_] := Module[{x},
  x = Collect[expr, getallconstants[] ];
  x = SnegSimplifyNumber[x];
  x = SnegSimplifyHubbard[x];
  x = SnegSimplifyHop[x];
  x = SnegSimplifySpin[x];
  x = SnegSimplifyMisc[x];
  x = Collect[x, getallconstants[] ]
];

SnegFullSimplify[expr_] := Simplify[expr,
  TransformationFunctions -> { Automatic, SnegSimplifyNumber,
  SnegSimplifyHubbard, SnegSimplifyHop, SnegSimplifySpin,
  SnegSimplifyMisc, SnegSimplify }];

(* snegSeries[f, expr, n] computes the power series of function f
around 0 to order n. The result is returned in terms of powers
of expr. *)
snegSeries[f_, expr_, n_:5] := Module[{s, x, pw},
  s = Normal @ Series[f[x], {x, 0, n}];
  s /. x^pw_. :> fastpow[expr, pw]  (* pow -> fastpow, 25.8.2010 *)
];

snegSeries[f_[expr_], n_] := snegSeries[f, expr, n];
snegSeries[f_[expr_]] := snegSeries[f, expr, 5];
snegSeries[E^expr_, n_] := snegSeries[Exp, expr, n];
snegSeries[E^expr_] := snegSeries[Exp, expr, 5];

(* Example: snegSeries[Exp[c]] *)

(* SNTR[in_] generates a test line to be used in test - sneg.m for
   regression testing. *)

SetAttributes[SNTR, HoldFirst];
SNTR[in_] := Module[{out},
      out = Release[in];
      fn[expr_] := ToString @ InputForm @ expr;
      strip[str_String] := StringDrop[StringDrop[str, -1], 9];
      "test[" <> strip @ fn @HoldForm[in] <> ", " <> fn @out <> "];"];

(* snegAssuming evaluates expr for assumed value of an argument.
   TagSet must be used to define the assumption.
   Upvalues and downvalues of the argument are saved and restored
   after the evaluation. *)
(* snegAssumingHoldForm is similar to snegAssuming, but it returns
   the result enwrapped by HoldForm[]. *)

SetAttributes[snegAssuming, HoldAllComplete];
SetAttributes[snegAssumingHoldForm, HoldAllComplete];


snegAssumingHoldForm[x:HoldPattern[{TagSet[_,__]..}], expr_] := Module[
{storeup, storedown, res, vars},
  vars = ReleaseHold[Hold[x] /. HoldPattern[TagSet[var_, defs__]] :> var];
  storeup = Map[{#, UpValues[#]}&, vars];
  storedown = Map[{#, DownValues[#]}&, vars];
  ReleaseHold[x];
  res = ReleaseHold[expr];
  res = Apply[HoldForm, {res}];
  Map[(UpValues[Evaluate @ First[#]] = Last[#])&, storeup];
  Map[(DownValues[Evaluate @ First[#]] = Last[#])&, storedown];
  res
];

snegAssumingHoldForm[HoldPattern[x:TagSet[var_, def__]], expr_] :=
 snegAssumingHoldForm[{x}, expr];

snegAssuming[x___] := snegAssumingHoldForm[x] /. HoldForm[a_] -> a;

(* Simplify KroneckerDelta and UnitStep expressions. *)
SimplifyKDfunc[expr_] := (expr //. {
  KroneckerDelta[k_, l_] UnitStep[k_] UnitStep[-l_] -> 0,
  KroneckerDelta[k_, l_] UnitStep[-k_] UnitStep[l_] -> 0,
  UnitStep[k_] UnitStep[-k_] -> 0,
  UnitStep[k_]+UnitStep[-k_] -> 1,
  1 - UnitStep[-k_] :> UnitStep[k],
  UnitStep[-k_] -1 :> -UnitStep[k],
  HoldPattern[KroneckerDelta[x__]^n_Integer?Positive] :> KroneckerDelta[x],
  HoldPattern[UnitStep[x__]^n_Integer?Positive] :> UnitStep[x]
  });
  
SimplifyKD[expr_] := Simplify[expr,
TransformationFunctions -> {Automatic, SimplifyKDfunc}];

FullSimplifyKD[expr_] := FullSimplify[expr,
TransformationFunctions -> {Automatic, SimplifyKDfunc}];

(* "Thread" spin projection values enlisted in l over all operators
that appear in expr. *)
snegSpinThread[expr_, l_List] :=
  Plus @@ Map[expr /. o_?operatorQ[i___] :> o[i, #] &, l];

sum2list[HoldPattern[Plus[i___]]] := {i};
sum2list[i_] := {i};

(* Similar to matrixrepresentationvc[], but this function
returns a sparse matrix and it is significantly faster. *)
matrixrepresentationvcsparse[a_, l_List] := Module[
  {n = Length[l], parts, states, bracoeffs},
  parts = Replace[Expand /@ l,
    HoldPattern[x_. v_vc] :> {x, v}, {1}];
  If[!And @@ Map[MatchQ[#, {_, _vc}] &, parts],
    Return[SparseArray[matrixrepresentationvc[a, l]]]
  ];
  states = parts[[All, 2]];
  (* Auxiliary kets can have symbolic or user-defined overlaps. *)
  If[!FreeQ[states, _ket] || !DuplicateFreeQ[states],
    Return[SparseArray[matrixrepresentationvc[a, l]]]
  ];
  bracoeffs = conj /@ parts[[All, 1]];
  SparseArray[Select[Flatten[
    Table[
      (sum2list @ Collect[ap[a, l[[i]]], states]) /.
        x_. v_vc :> Module[{pos = Position[states, v], row},
          If[pos === {}, 0,
            row = pos[[1, 1]];
            {row, i} -> bracoeffs[[row]] x
          ]
        ],
      {i, n}],
    1], (#=!=0)&], {n, n}]
];

(* Similar to matrixrepresentationvc[], but significantly faster. *)
(* Simple monomial vectors use the optimized path. Other bases fall back
to matrixrepresentationvc[]. *)
matrixrepresentationvcfast[a_, l_List] :=
  matrixrepresentationvcfast[a, l, l];

matrixrepresentationvcfast[a_, l1_List, l2_List] := Module[
  {n = Length[l1], parts, states, bracoeffs},
  parts = Replace[Expand /@ l1,
    HoldPattern[x_. v_vc] :> {x, v}, {1}];
  If[!And @@ Map[MatchQ[#, {_, _vc}] &, parts],
    Return[matrixrepresentationvc[a, l1, l2]]
  ];
  states = parts[[All, 2]];
  (* Auxiliary kets can have symbolic or user-defined overlaps. *)
  If[!FreeQ[states, _ket] || !DuplicateFreeQ[states],
    Return[matrixrepresentationvc[a, l1, l2]]
  ];
  bracoeffs = conj /@ parts[[All, 1]];
  Map[
    indexvalue2list[
      Sort[
        (sum2list @ Collect[ap[a, #], states]) /.
        x_. v_vc :> Module[{pos = Position[states, v], row},
          If[pos === {}, 0,
            row = pos[[1, 1]];
            {row, bracoeffs[[row]] x}
          ]
        ]
      ],
    n] &,
  l2] // Transpose
];

(* Transform {index, value} pairs to a vector. Operator application can also
   produce scalar zeros, including inexact and complex zeros, between records. *)
indexvalue2list[iv_List, n_Integer] :=
  Fold[ReplacePart[#1, Last[#2], First[#2]] &, Table[0, {n}],
  Select[iv, !(NumberQ[#] && TrueQ[# == 0])&] ];

(* Symmetrized spin-spin correlation for a list of operators. *)
spinspinsymmetric[ops_List] := Module[{l},
  l=spinspin @@@ Subsets[ops, {2}];
  Total[l]/Length[l]  // Simplify
];

(* Show Hamiltonian matrices in an easy-to-comprehend formatted form. *)
showbzmat[bz_?bzQ] :=
  MatrixForm[Map[{First[#], MatrixForm @ Last[#]}&, bz]];

(* Make a dispatch table which allows very fast computation of operator.vc operations. *)
(* vc basis must be previously declared by makebasis[], so that BASIS contains the
relevant operators which appear in the operator. *)
opdispatch[op_] := Module[{states, resultstates},
  states = vc @@@ Tuples[{0, 1}, Length[BASIS]];
  resultstates = nc[op, states];
  Dispatch[Thread[states -> resultstates]]
];

(* Routines for producing tensor product basis for SU(2) multiplets *)

qscombinefnc[s_, {q1_, _}, {q2_, _}] := {q1 + q2, s};
qsspincombinefnc[s_, {q1_, _}, {___}] := {q1, s};

SU2basistensorproduct[l1_, l2_,
                      spinfnc1_, spinfnc2_,
                      combineqnfnc_,
                      fixspin1fnc_, fixspin2fnc_] :=
  mergebasis @
   Flatten[Outer[
    SU2merge[#1, #2, spinfnc1, spinfnc2,
             combineqnfnc, fixspin1fnc, fixspin2fnc] &,
    l1, l2, 1],
   2];

SU2merge[{qn1_, l1_}, {qn2_, l2_},
         spinfnc1_, spinfnc2_,
         combineqnfn_,
         fixspin1fnc_, fixspin2fnc_] :=
 Module[{s1, s2, states},
   s1 = spinfnc1[qn1];
   s2 = spinfnc2[qn2];
   states =
    Partition[
     Flatten[
      Outer[
       SU2combine[#1, #2, {s1, s2}, fixspin1fnc, fixspin2fnc] &,
       l1, l2]
     ],
    2];
   states = mergebasis @
     Map[{combineqnfn[First[#], qn1, qn2], {Last[#]}} &, states]
 ];

SU2combine[op1_, op2_, {s1_, s2_}, fixspin1fnc_, fixspin2fnc_] := Module[
 {s, sz1, sz2},
  Table[
    {s, Sum[
     (* The projection selection rule also avoids nonphysical-argument
        messages for terms whose coefficient is known to vanish. *)
     If[sz1 + sz2 == s,
      With[{cg = ClebschGordan[{s1, sz1}, {s2, sz2}, {s, s}]},
       If[cg =!= 0,
          cg  nc[fixspin1fnc[op1, s1, sz1] , fixspin2fnc[op2, s2, sz2]],
          0]
      ], 0], {sz1, -s1, s1}, {sz2, -s2, s2}]},
   {s, Abs[s1 - s2], s1 + s2}
  ]
];

fixspin[op_, s_, s_] := op;
fixspin[op_, s_, sz_] /; sz < s := Nest[spindown, op, s - sz];

fixspinket[k_, s_, s_] := k;
fixspinket[k_, s_, sz_] /; sz < s := k /. ket[i_] :> ket[i - (s - sz)];

(* Miscellaneous useful routines *)

(* Returns a n-by-n matrix of zeros *)
ZeroMatrix[n_] := 0 IdentityMatrix[n];

(* SU(3) symmetry *)
(* The defining representation: Gell-Mann matrices. *)
su3g[1] = {{0, 1, 0}, {1, 0, 0}, {0, 0, 0}};
su3g[2] = {{0, -I, 0}, {I, 0, 0}, {0, 0, 0}};
su3g[3] = {{1, 0, 0}, {0, -1, 0}, {0, 0, 0}};
su3g[4] = {{0, 0, 1}, {0, 0, 0}, {1, 0, 0}};
su3g[5] = {{0, 0, -I}, {0, 0, 0}, {I, 0, 0}};
su3g[6] = {{0, 0, 0}, {0, 0, 1}, {0, 1, 0}};
su3g[7] = {{0, 0, 0}, {0, 0, -I}, {0, I, 0}};
su3g[8] = 1/Sqrt[3] {{1, 0, 0}, {0, 1, 0}, {0, 0, -2}};
su3t[i_] = su3g[i]/2;

(* Construct all combination from a list of alternatives *)

allcombinations[l_List] := allcombinations[l, {{}}];
allcombinations[{}, g_List] := g;
allcombinations[l_List, g_List] :=
 allcombinations[Rest[l], Flatten[Outer[Append, g, First[l], 1], 1]];


(* New much improved implementation of the VEV calculation using
Wick's theorem. *)

sneglinearoperatorFirst[vevwicknew];

vevwicknew[ nc[___, x:op_?fermionQ[__]] ] /; isannihilation[x] := 0;
vevwicknew[ nc[x:op_?fermionQ[__], ___] ] /; iscreation[x] := 0;

vevwicknew[op_?operatorQ[__]] := 0; (* single operator *)
vevwicknew[HoldPattern[l_nc]] /; OddQ[Length[l]] := 0;

vevwicknew[HoldPattern[l_nc]] /; EvenQ[Length[l]] :=
 Module[{oplist, l1, l2, crops, pos, l3, contributions},
  oplist = List @@ l;
  l1 = indecesCR[l];
  l2 = indecesAN[l];

  (* Extract creation operators *)
  crops = Map[l[[#]] &, l1];

  (* Index equality may remain symbolic, so match by operator head and let
     contract[] produce the corresponding KroneckerDelta factors. *)
  pos = Map[
    Function[crop,
      Select[l2, Head[oplist[[#]]] === Head[crop] &]
    ],
    crops
  ];

  (* Generate all allowed permutations *)
  l3 = allcombinations[pos];
  l3 = Select[l3, DuplicateFreeQ];

  (* Calculate the contractions *)
  contributions = Map[contract[oplist, Transpose[{l1, #}]] &, l3];

  Total[contributions /. {dd[_]->0}]
];

(* Derivatives under the sum *)
der[a_ + b_, c_] := der[a, c] + der[b, c];
der[a_ /; FreeQ[a, sum], c_] := D[a, c];
der[a_ b_, c_] /; FreeQ[a, sum] := D[a, c] b + a der[b, c];

(* Division *)
nc[a___, b_/c_, d___] /; isnumericQ[c] := nc[a, b, d]/c;
vev[a_/b_] /; isnumericQ[b] := vev[nc[a]]/b;
conj[1/a_] := 1/conj[a];
conj[nc[a__]/b_] := conj[nc[a]]/conj[b];
sum[a_./(n_ b_ + n_ c_), l_List] /; isnumericQ[n] && sumFactorizableQ[n, l] := (1/n) sum[a/(b + c), l];

(* Improved zeroonvac function; tries harder to drop terms. *)
Zeroonvac[a_ + b_] := Zeroonvac[a] + Zeroonvac[b];
Zeroonvac[a_ b_] /; isnumericQ[a] := a Zeroonvac[b];
Zeroonvac[a_ scalar[x_]] := scalar[x] Zeroonvac[a];
Zeroonvac[sum[a_, {q__}]] := sum[Zeroonvac[a], {q}];
Zeroonvac[HoldPattern[nc[a__]]] := zeroonvac[nc[a]];
Zeroonvac[x : op_[a__]] /; operatorQ[op] := zeroonvac[x];
Zeroonvac[a_] /; isnumericQ[a] := a;

ruleFullSimplifyUnderSum = {
  sum[a_, {q__}] :> sum[FullSimplify[a], {q}]
};


(**** Momentum space and lattices ****)

(* Lattice indexes R label the unit cells of a periodic lattice with Ns
cells, momentum indexes k label its crystal momenta. The Fourier convention
is c_R = Ns^(-1/2) Sum_k e^(i k.R) c_k. Momentum labels are defined modulo
reciprocal-lattice vectors and site labels modulo the lattice period
(periodic boundary conditions). On these index sets a shift k -> +-k + q is
a bijection; this makes the elimination of Kronecker deltas of linear
combinations and the shifts of summation variables exact. Neither property
holds for open chains, for SEA-ordered (linearized, unbounded) bands, or
for generic indexes, which therefore keep the old behavior. *)

If[!ValueQ[listlatticeindexes],  listlatticeindexes = {} ];
If[!ValueQ[listmomentumindexes], listmomentumindexes = {} ];
If[!ValueQ[listlatticevectors],  listlatticevectors = {} ];

latticeindextype[_] := None;
latticevectorQ[_] := False;
latticeindexQ[x_Symbol] := latticeindextype[x] === "Lattice";
latticeindexQ[_] := False;
momentumindexQ[x_Symbol] := latticeindextype[x] === "Momentum";
momentumindexQ[_] := False;
latticetypedQ[x_] := latticeindexQ[x] || momentumindexQ[x];
latticelikeQ[x_] := latticetypedQ[x] || TrueQ[latticevectorQ[x]];

snegsymbolsof[e_] := Union[Cases[{e}, _Symbol, Infinity]];
snegHasLatticeQ[e_] := Or @@ Map[latticelikeQ, snegsymbolsof[e]];

(* Index types ("Lattice", "Momentum") appearing in an expression. Lattice
vectors count as lattice-type quantities. *)
snegindextypes[e_] := Union[
  Map[latticeindextype, Select[snegsymbolsof[e], latticetypedQ]],
  If[Or @@ Map[TrueQ[latticevectorQ[#]] &, snegsymbolsof[e]], {"Lattice"}, {}]];

sneglatticeindexes::size = "The lattice size `1` must be a positive integer or an expression built from symbols.";
snegdeclarelatticesize[n_Integer?Positive] := True;
snegdeclarelatticesize[n_] /; !NumericQ[n] && snegsymbolsof[n] =!= {} := (
  Scan[If[snegnonopQ[#] =!= True, snegrealconstants[#]] &, snegsymbolsof[n]];
  True);
snegdeclarelatticesize[n_] := (Message[sneglatticeindexes::size, n]; False);

snegaddlatticeindex[x_Symbol, n_, type_] := (
  snegfreeindexes[x];
  Conjugate[x] ^= x;
  latticeindextype[x] ^= type;
  latticesize[x] ^= n;
  If[type === "Lattice",
    addto[listlatticeindexes, x],
    addto[listmomentumindexes, x]];
);

sneglatticeindexes[l_List, n_] /; snegdeclarelatticesize[n] :=
  Scan[snegaddlatticeindex[#, n, "Lattice"] &, l];
snegmomentumindexes[l_List, n_] /; snegdeclarelatticesize[n] :=
  Scan[snegaddlatticeindex[#, n, "Momentum"] &, l];

sneglatticevectors[l__] := Scan[
  (snegrealconstants[#];
   latticevectorQ[#] ^= True;
   addto[listlatticevectors, #]) &, {l}];

(* Indexes created by automatic renaming inherit the properties of the
original index. *)
snegcopyindexproperties[old_Symbol, new_Symbol] := (
  If[Conjugate[old] === old, Conjugate[new] ^= new];
  If[latticetypedQ[old],
    snegaddlatticeindex[new, latticesize[old], latticeindextype[old]]];
  If[spincomponentindexQ[old], snegspincomponentindexes[new]];
);
snegcopyindexproperties[___] := Null;

(* Sums over lattice or momentum indexes that do not appear in the summand
evaluate to the number of unit cells. *)
snegfreelatticeindex[z_, it_List] := Module[
  {s = Select[it, latticetypedQ[#] && FreeQ[z, #] &]},
  If[s === {}, None, First[s]]];

sum[z_, it_List] /; snegfreelatticeindex[z, it] =!= None :=
  With[{x = snegfreelatticeindex[z, it]},
    latticesize[x] sum[z, DeleteCases[it, x]]];

(* Bloch phase e^(i k.R). The first argument is a momentum, the second a
lattice vector (in 1D, integer displacements are allowed). Integer factors
are moved into the first argument, sums in the second argument are split. *)
SetAttributes[blochphase, NumericFunction];
blochphase[0, _] := 1;
blochphase[_, 0] := 1;
blochphase[k_List, x_List] /; Length[k] == Length[x] := Exp[I k . x];
blochphase[k_, x_Plus] := Times @@ Map[blochphase[k, #] &, List @@ x];
blochphase[k_, n_Integer x_] := blochphase[n k, x];
blochphase[k_, n_Integer] /; n =!= 1 := blochphase[n k, 1];
blochphase[k_, x_] /; Expand[x] =!= x := blochphase[k, Expand[x]];
blochphase[k_, x_] /; Expand[k] =!= k := blochphase[Expand[k], x];
blochphase /: blochphase[k1_, x_] blochphase[k2_, x_] := blochphase[k1 + k2, x];
blochphase /: Power[blochphase[k_, x_], n_Integer] := blochphase[n k, x];
blochphase /: Conjugate[blochphase[k_, x_]] := blochphase[-k, x];

If[PrettyOutput,
  Format[blochphase[k_, x_], StandardForm] := DisplayForm @
    SuperscriptBox["e", RowBox[{"\[ImaginaryI]", k, "\[CenterDot]", x}]];
];

blochphaseexp[expr_, rules_List:{}] := (expr /. rules) /.
  blochphase[k_, x_] :> Exp[I If[ListQ[k] && ListQ[x], k . x, k x]];

(* Canonical Kronecker delta for a linear combination d of lattice or
momentum labels: d = 0 is solved for the first variable with a unit
coefficient. *)
snegindexdelta[d0_] := Module[{d = Expand[d0], vars, c},
  vars = Select[snegsymbolsof[d], (latticetypedQ[#] &&
    MemberQ[{1, -1}, Coefficient[d, #]] &&
    FreeQ[Expand[d - Coefficient[d, #] #], #]) &];
  If[vars === {},
    KroneckerDelta[d, 0],
    c = Coefficient[d, First[vars]];
    KroneckerDelta[First[vars], Expand[First[vars] - d/c]]]
];

(* delta(k, k + q) -> delta(q, 0) for lattice or momentum labels. *)
snegKDreducibleQ[a_, b_] := Module[{d = Expand[a - b], common},
  common = Intersection[Select[snegsymbolsof[a], latticetypedQ],
                        Select[snegsymbolsof[b], latticetypedQ]];
  Or @@ Map[FreeQ[d, #] &, common]];

rulelatticeKDreduce = {
  KroneckerDelta[a_, b_] /; snegKDreducibleQ[a, b] :> snegindexdelta[a - b]
};

(* Return {m, solution} if the delta of d can be solved for a summation
index m with coefficient +-1 that has the same type as all other lattice
quantities in d. Coefficients other than +-1 have several solutions on the
torus and are left alone. The last eligible index is eliminated. *)
snegKDsolve[d0_, it_List] := Module[{d = Expand[d0], cands},
  cands = Select[Reverse[it], (latticetypedQ[#] &&
    MemberQ[{1, -1}, Coefficient[d, #]] &&
    FreeQ[Expand[d - Coefficient[d, #] #], #] &&
    snegindextypes[d] === {latticeindextype[#]}) &];
  If[cands === {}, {},
    With[{m = First[cands]}, {m, Expand[m - d/Coefficient[d, m]]}]]
];

(* Eliminate one summation index from a single term using a Kronecker
delta. Since delta is 0 or 1, f(delta) = delta f(1) whenever f(0) = 0. *)
snegKDsumterm[term_, it_List] := Module[{kds, s, kd, res = $Failed},
  kds = Union[Cases[{term}, k_KroneckerDelta /; Length[k] == 2, Infinity]];
  Do[
    s = snegKDsolve[kd[[1]] - kd[[2]], it];
    If[s =!= {} && (term /. kd -> 0) === 0,
      res = sum[(term /. kd -> 1) /. s[[1]] -> s[[2]],
                DeleteCases[it, s[[1]]]];
      Break[]
    ],
    {kd, kds}];
  res
];

(* Lattice sums Sum_R e^(i K.R) = Ns delta(K, 0) and
Sum_k e^(i k.X) = Ns delta(X, 0). The summation index may appear only in
Bloch phases that enter the term multiplicatively. *)
snegpart[e_, {}] := e;
snegpart[e_, p_List] := Extract[e, p];

snegphasesum1[term_, x_] := Module[{pos, phases, tot = 0, ok = True, c, K, X, ii},
  pos = Select[Position[term, HoldPattern[blochphase[_, _]]],
    !FreeQ[snegpart[term, #], x] &];
  If[pos === {}, Return[$Failed]];
  If[!FreeQ[term /. (ph_blochphase /; !FreeQ[ph, x]) :> 1, x],
    Return[$Failed]];
  If[!And @@ Map[Function[p, And @@ Table[
       MemberQ[{Times, scalar}, Head[snegpart[term, Take[p, ii]]]],
       {ii, 0, Length[p] - 1}]], pos],
    Return[$Failed]];
  phases = Map[snegpart[term, #] &, pos];
  Scan[Function[ph,
    {K, X} = {ph[[1]], Expand[ph[[2]]]};
    If[latticeindexQ[x],
      c = Coefficient[X, x];
      If[!IntegerQ[c] || !FreeQ[X - c x, x] || !FreeQ[K, x], ok = False];
      tot += c K,
      (* else: momentum index *)
      K = Expand[K];
      c = Coefficient[K, x];
      If[!IntegerQ[c] || !FreeQ[K - c x, x] || !FreeQ[X, x], ok = False];
      tot += c X
    ]], phases];
  If[!ok, Return[$Failed]];
  latticesize[x] snegindexdelta[tot] (term /. x -> 0)
];

snegphasesumterm[term_, it_List] := Module[{r, xx, res = $Failed},
  Do[
    If[latticetypedQ[xx] && !FreeQ[term, xx],
      r = snegphasesum1[term, xx];
      If[r =!= $Failed, res = sum[r, DeleteCases[it, xx]]; Break[]]
    ],
    {xx, Reverse[it]}];
  res
];

(* scalar[a + b] c -> scalar[a] c + scalar[b] c, so that Kronecker deltas
and phases inside scalar[] blocks can be handled term by term. *)
snegdistributescalar[t_] := Module[{pos},
  pos = Position[t, scalar[_Plus], {0, 1}, 1];
  If[pos === {} || (FreeQ[t, KroneckerDelta] && FreeQ[t, blochphase]),
    {t},
    With[{s = Extract[t, First[pos]]},
      Map[(ReplacePart[t, First[pos] -> scalar[#]]) &, List @@ Expand[s[[1]]]]]]
];

snegsumlattice1[s:sum[a_, it_List]] := Module[{terms, changed = False, out},
  If[Select[it, latticetypedQ] === {}, Return[s]];
  terms = Flatten[Map[snegdistributescalar,
    If[Head[a] === Plus, List @@ a, {a}]]];
  out = Map[Function[t, Module[{r},
    r = snegKDsumterm[t, it];
    If[r === $Failed, r = snegphasesumterm[t, it]];
    If[r === $Failed, sum[t, it], changed = True; r]]], terms];
  If[changed, Total[out], s]
];

snegsumlatticestep[expr_] := expr /. s:sum[_, _List] :> snegsumlattice1[s];

(* Sums containing SEA-ordered operators must not be shifted: for
linearized bands the shift of the summation variable is not a symmetry
of the Fermi sea. *)
snegseaQ[a_] := !FreeQ[a,
  (op_Symbol)[___] /; (TrueQ[fermionQ[op]] && ordering[op] === SEA)];

sumShift::index = "`1` is not a declared lattice or momentum index.";
sumShift::shift = "The substitution `1` -> `2` is not a shift of the form +-`1` + const.";
sumShift::sea = "Sums containing SEA-ordered operators are not shifted.";
sumShift[expr_, Rule[k_, new_]] := Module[{c, r},
  If[!latticetypedQ[k], Message[sumShift::index, k]; Return[expr]];
  c = Coefficient[Expand[new], k];
  r = Expand[new - c k];
  If[!MemberQ[{1, -1}, c] || !FreeQ[r, k] ||
     Complement[snegindextypes[r], {latticeindextype[k]}] =!= {},
    Message[sumShift::shift, k, new]; Return[expr]];
  expr /. sum[a_, it_List] /; MemberQ[it, k] :>
    If[snegseaQ[a],
      Message[sumShift::sea]; sum[a, it],
      sum[a /. k -> new, it]]
];

(* Shift each summed lattice/momentum index so that the first operator
argument depending on it becomes the bare index. *)
snegsumcanonical1[s:sum[a_, it_List]] := Module[{res = a, args, e, c, r},
  If[snegseaQ[a], Return[s]];
  Scan[Function[m,
    args = Cases[{res}, (op_Symbol)[_, j__] /; TrueQ[operatorQ[op]] :> {j},
      Infinity];
    args = Select[Flatten[args, 1], !FreeQ[#, m] &];
    If[args =!= {},
      e = Expand[First[args]];
      c = Coefficient[e, m];
      r = Expand[e - c m];
      If[e =!= m && MemberQ[{1, -1}, c] && FreeQ[r, m],
        res = res /. m -> c (m - r)]
    ]],
    Select[it, latticetypedQ]];
  sum[res, it]
];

(* Root index of an automatically renamed index, e.g. k for k12. *)
snegdummyroot[m_Symbol] := Module[{name = SymbolName[m], root},
  root = StringReplace[name,
    ShortestMatch[x : (_ ~~ ___)] ~~ DigitCharacter .. ~~ EndOfString :> x];
  If[root =!= name && NameQ[Context[m] <> root],
    With[{r = Symbol[Context[m] <> root]},
      If[MemberQ[listfreeindexes, r] && latticeindextype[r] === latticeindextype[m],
        r, None]],
    None]];
snegdummyroot[_] := None;

(* Rename bound lattice/momentum indexes to their root names where the root
does not occur in the summand, so that equivalent sums can be collected. *)
snegcanonicaldummies1[s:sum[a_, it_List]] := Module[{res = a, idx = it, r},
  Scan[Function[m,
    r = snegdummyroot[m];
    If[r =!= None && latticetypedQ[m] && !MemberQ[idx, r] && FreeQ[res, r],
      res = res /. m -> r; idx = idx /. m -> r]],
    it];
  sum[res, idx]];

sumMomentumCanonical[expr_] := (expr /. s:sum[_, _List] :> snegsumcanonical1[s]) /.
  s:sum[_, _List] :> snegcanonicaldummies1[s];

(* Fourier transformation between site and momentum operators. *)
Options[fourier] = {"IndexPosition" -> 1, "Inverse" -> False, "Simplify" -> True};
fourier::index = "`1` must be a declared `2` index.";
fourier::op = "Only fermionic and bosonic operators can be transformed: `1`.";
fourier::capture = "The expression already contains the summation index `1`.";

snegfouriersimplify[e_] := FixedPoint[
  sumCollect[sumSimplifyKD[Expand[sumExpand[#]]]] &, e, 10];

fourier[expr_, Rule[c_Symbol, ck_Symbol], Rule[x_Symbol, k_Symbol],
  opts:OptionsPattern[]] := Module[
  {p = OptionValue["IndexPosition"], inv = TrueQ[OptionValue["Inverse"]],
   from, to, idx, ns, phase, rule, res},
  If[!latticeindexQ[x], Message[fourier::index, x, "lattice"]; Return[$Failed]];
  If[!momentumindexQ[k], Message[fourier::index, k, "momentum"]; Return[$Failed]];
  {from, to, idx} = If[inv, {ck, c, x}, {c, ck, k}];
  If[!(TrueQ[fermionQ[from]] || TrueQ[bosonQ[from]]),
    Message[fourier::op, from]; Return[$Failed]];
  If[!FreeQ[expr, idx], Message[fourier::capture, idx]; Return[$Failed]];
  ns = latticesize[idx];
  (* forward: c_R = Ns^(-1/2) Sum_k e^(+i k.R) c_k
     inverse: c_k = Ns^(-1/2) Sum_R e^(-i k.R) c_R *)
  phase[t_, s_] := If[inv,
    If[t === AN, blochphase[-s, idx], blochphase[s, idx]],
    If[t === AN, blochphase[idx, s], blochphase[-idx, s]]];
  rule = from[t_, a___] /; (t === CR || t === AN) && Length[{a}] >= p :>
    With[{s = {a}[[p]], pre = Take[{a}, p - 1], post = Drop[{a}, p]},
      ns^(-1/2) sum[phase[t, s] to[t, Sequence @@ pre, idx, Sequence @@ post],
        {idx}]];
  res = expr /. rule;
  If[TrueQ[OptionValue["Simplify"]],
    res = snegrenamedummy[snegfouriersimplify[res], idx]];
  res
];

(* Cosmetic: rename an automatically renamed dummy index (k1, k2, ...)
back to its root name k where this causes no collision. *)
snegrenamedummy[e_, idx_Symbol] := e /. sum[a_, it_List] /;
  !MemberQ[it, idx] && FreeQ[a, idx] &&
  Select[it, StringMatchQ[ToString[#], ToString[idx] ~~ DigitCharacter ..] &] =!= {} :>
    With[{m = First[Select[it,
        StringMatchQ[ToString[#], ToString[idx] ~~ DigitCharacter ..] &]]},
      sum[a /. m -> idx, it /. m -> idx]];

(* Bloch Hamiltonian h_ab(k) = {c_ka, [H, c_kb^dag]} (fermions) or
[b_ka, [H, b_kb^dag]] (bosons). Orbitals are templates op[extra...]; the
momentum is inserted at "IndexPosition". Fermions get the spin index as the
last index ({UP, DO} for spin 1/2, 0 for spinless operators), varying
fastest. *)
Options[blochhamiltonian] = {"IndexPosition" -> 1, "Check" -> True,
  "OrbitalPositions" -> None};
blochhamiltonian::index = "`1` must be a declared momentum index.";
blochhamiltonian::rest = "The matrix element `1` still contains operators or unevaluated sums.";
blochhamiltonian::transl = "The Hamiltonian is not translation invariant for the given orbitals; the residual off-diagonal part is `1`.";
blochhamiltonian::herm = "The Bloch Hamiltonian is not Hermitian.";
blochhamiltonian::orb = "Invalid orbital template `1`.";
blochhamiltonian::anom = "The Hamiltonian contains terms that do not conserve the particle number (anomalous or linear terms), which are not described by h(k); use bdghamiltonian.";

(* True if every operator product contains equal numbers of creation and
annihilation operators with the given heads. *)
snegnumberconservingQ[H_, heads_List] := Module[{cnt},
  cnt[l_List] := Count[l, (h_Symbol)[CR, ___] /; MemberQ[heads, h]] -
    Count[l, (h_Symbol)[AN, ___] /; MemberQ[heads, h]];
  FreeQ[Last[Reap[H /. {HoldPattern[nc[x__]] :> (Sow[cnt[{x}]]; 1),
      o:(h_Symbol)[CR | AN, ___] /; MemberQ[heads, h] :> (Sow[cnt[{o}]]; 1)}]],
    n_Integer /; n != 0]];

snegblochmodes[op_Symbol, q_, p_] := snegblochmodes[op[], q, p];
snegblochmodes[op_Symbol[e___], q_, p_] /; TrueQ[fermionQ[op]] || TrueQ[bosonQ[op]] :=
  With[{pre = Take[{e}, Min[p - 1, Length[{e}]]],
        post = Drop[{e}, Min[p - 1, Length[{e}]]]},
    If[TrueQ[fermionQ[op]],
      Map[With[{sg = #}, Function[t, op[t, Sequence @@ pre, q, Sequence @@ post, sg]]] &,
        snegspinlabels[op]],
      {Function[t, op[t, Sequence @@ pre, q, Sequence @@ post]]}]];

(* Spin labels of a fermionic operator, as used by number[] and hop[]:
{UP, DO} for spin 1/2, S, S-1, ..., -S otherwise (0 for spinless). *)
snegspinlabels[op_] := If[spinof[op] == 1/2, {UP, DO},
  Range[spinof[op], -spinof[op], -1]];
snegblochmodes[o_, ___] := (Message[blochhamiltonian::orb, o]; $Failed);

snegsumreduce[x_] := FixedPoint[
  sumSimplifyKD[sumCollect[Expand[sumExpand[#]]]] &, Expand[x], 10];

snegblochelement[H_, m1_, m2_, boson_] := snegsumreduce[
  If[boson, komutator, antikomutator][m1[AN], komutator[H, m2[CR]]]];

blochhamiltonian[H_, orbs_List, k_Symbol, opts:OptionsPattern[]] := Module[
  {p = OptionValue["IndexPosition"], kx, px, modesk, modesp, boson, h, hp,
   off, tau, i, j, m1, m2},
  If[!momentumindexQ[k], Message[blochhamiltonian::index, k]; Return[$Failed]];
  kx = ssJWRnewname[k, Join[listfreeindexes, {k}], {H, orbs}];
  modesk = Map[snegblochmodes[#, kx, p] &, orbs];
  If[MemberQ[modesk, $Failed], Return[$Failed]];
  modesk = Flatten[modesk];
  boson = TrueQ[bosonQ[Head[modesk[[1]][CR]]]];
  If[!snegnumberconservingQ[H, Union[Map[Head[#[CR]] &, modesk]]],
    Message[blochhamiltonian::anom]; Return[$Failed]];
  h = Table[snegblochelement[H, m1, m2, boson], {m1, modesk}, {m2, modesk}];
  If[!FreeQ[h, sum] || !FreeQ[h, _?operatorQ[___]],
    Message[blochhamiltonian::rest,
      First[Select[Flatten[h], !FreeQ[#, sum] || !FreeQ[#, _?operatorQ[___]] &]]];
    Return[$Failed]];
  If[TrueQ[OptionValue["Check"]],
    px = ssJWRnewname[k, Join[listfreeindexes, {k, kx}], {H, orbs}];
    modesp = Flatten[Map[snegblochmodes[#, px, p] &, orbs]];
    hp = Table[snegblochelement[H, modesk[[i]], modesp[[j]], boson],
      {i, Length[modesk]}, {j, Length[modesp]}];
    off = Simplify[hp /. KroneckerDelta[kx, px] -> 0];
    If[!MatchQ[Flatten[{off}], {0 ..}],
      Message[blochhamiltonian::transl, off /. {kx -> k}]; Return[$Failed]];
    If[!MatchQ[Flatten[Simplify[h - ConjugateTranspose[h]]], {0 ..}],
      Message[blochhamiltonian::herm]];
  ];
  tau = OptionValue["OrbitalPositions"];
  If[ListQ[tau] && Length[tau] == Length[orbs],
    tau = Flatten[MapThread[ConstantArray[{#1}, #2] &,
      {tau, Map[Length[snegblochmodes[#, kx, p]] &, orbs]}], 1][[All, 1]];
    h = Table[blochphase[-kx, tau[[i]]] h[[i, j]] blochphase[kx, tau[[j]]],
      {i, Length[h]}, {j, Length[h]}]];
  h /. kx -> k
];


(* Finite periodic clusters. Sites are integer vectors n in lattice
coordinates, n_a = 0, ..., L_a - 1 (plain integers in 1D); momentum labels
are integer vectors m with k_a = (2 Pi m_a + theta_a)/L_a. Twisted boundary
conditions are c_(n + L_a e_a) = e^(i theta_a) c_n. *)

snegclustersites[dims_List] := If[Length[dims] == 1,
  Range[0, First[dims] - 1],
  Tuples[Map[Range[0, # - 1] &, dims]]];

snegclustertwist[tw_, d_] := Which[
  tw === Automatic, ConstantArray[0, d],
  ListQ[tw], tw,
  True, ConstantArray[tw, d]];

fourierrules::unitary = "The Fourier transformation is not unitary.";

(* Numerical unitarity check of a transformation matrix; matrices with
symbolic entries (e.g. symbolic twists) are not checked. *)
snegcheckunitary[u_?MatrixQ] := If[VectorQ[Flatten[N[u]], NumericQ] &&
    Max[Abs[N[u . ConjugateTranspose[u] - IdentityMatrix[Length[u]]]]] > 10^-10,
  Message[fourierrules::unitary]; False,
  True];
Options[fourierrules] = {"IndexPosition" -> 1, "Twist" -> Automatic};
fourierrules[Rule[c_Symbol, ck_Symbol], dims_List, opts:OptionsPattern[]] :=
Module[{p = OptionValue["IndexPosition"], d = Length[dims], twist, sites,
  ntot, ph, u},
  twist = snegclustertwist[OptionValue["Twist"], d];
  sites = snegclustersites[dims];
  ntot = Times @@ dims;
  ph[m_, n_] := Exp[I Total[(2 Pi Flatten[{m}] + twist) Flatten[{n}]/dims]];
  u = Outer[ph, sites, sites, 1]/Sqrt[ntot];
  snegcheckunitary[u];
  {c[t_, a___] /; (t === CR || t === AN) && Length[{a}] >= p &&
      MemberQ[sites, {a}[[p]]] :>
    With[{n = {a}[[p]], pre = Take[{a}, p - 1], post = Drop[{a}, p]},
      ntot^(-1/2) Total[Map[
        If[t === AN, ph[#, n], Exp[-I Total[(2 Pi Flatten[{#}] + twist) Flatten[{n}]/dims]]] *
          ck[t, Sequence @@ pre, #, Sequence @@ post] &, sites]]]}
];

clusterhop::dims = "The displacement `1` does not match the cluster dimensions `2`.";
Options[clusterhop] = {"Twist" -> Automatic};
snegclustermodes[op_Symbol[e___], n_] :=
  If[TrueQ[fermionQ[op]],
    Map[op[CR, n, e, #] &, snegspinlabels[op]],
    {op[CR, n, e]}];

clusterhop[T_, o1:(_Symbol[___]), o2:(_Symbol[___]), delta_, dims_List,
  opts:OptionsPattern[]] := Module[{d = Length[dims], twist, sites, del, mat,
  site, total = 0},
  del = Flatten[{delta}];
  If[Length[del] =!= d, Message[clusterhop::dims, delta, dims]; Return[$Failed]];
  twist = snegclustertwist[OptionValue["Twist"], d];
  sites = snegclustersites[dims];
  Do[
    Module[{nv = Flatten[{site}], target, w, fold, cr1, cr2, tm, terms},
      target = nv + del;
      w = Floor[target/dims];
      fold = Mod[target, dims];
      If[d == 1, fold = First[fold]];
      cr1 = snegclustermodes[o1, site];
      cr2 = snegclustermodes[o2, fold];
      tm = If[MatrixQ[T], T, T IdentityMatrix[Length[cr1]]] Exp[I twist . w];
      terms = Outer[nc, cr1, conj[cr2]];
      total += Total[Flatten[tm terms + Conjugate[tm] conj[terms]]]
    ],
    {site, sites}];
  total
];
clusterhop[T_, o1_Symbol, o2_, rest__] := clusterhop[T, o1[], o2, rest];
clusterhop[T_, o1_, o2_Symbol, rest__] := clusterhop[T, o1, o2[], rest];

(* Explicit finite-cluster version of a symbolic lattice expression with
periodic (untwisted) boundary conditions. Kronecker deltas, Bloch phases,
operators, and nc products are kept inert while labels are substituted, so
that labels are folded modulo L before any evaluation. *)
sumCluster::size = "The lattice size `1` does not match the cluster `2`.";
Options[sumCluster] = {"IndexPosition" -> 1, "Operators" -> Automatic};
sumCluster[expr_, dims_List, vrules_List:{}, opts:OptionsPattern[]] := Module[
  {p = OptionValue["IndexPosition"], d = Length[dims], sites, heads, sizes,
   ntot = Times @@ dims, e, fold, foldlabel},
  sites = snegclustersites[dims];
  heads = OptionValue["Operators"];
  If[heads === Automatic,
    heads = Union[Cases[{expr}, (op_Symbol)[t_, a___] /; TrueQ[operatorQ[op]] &&
      Length[{a}] >= p && snegHasLatticeQ[{a}[[p]]] :> op, Infinity]]];
  sizes = Union[Map[latticesize, Select[snegsymbolsof[expr], latticetypedQ]]];
  Scan[If[NumericQ[#] && # =!= ntot, Message[sumCluster::size, #, dims]] &, sizes];
  foldlabel[v_] := If[VectorQ[v, IntegerQ] && Length[v] == d, Mod[v, dims],
    If[IntegerQ[v] && d == 1, Mod[v, First[dims]], v]];
  e = expr /. {KroneckerDelta[a_, b_] /; snegHasLatticeQ[{a, b}] :>
                 snegKDinert[a, b],
               blochphase[a_, b_] :> snegBPinert[a, b]};
  e = e /. nc -> snegNCinert;
  e = e /. (op_Symbol)[t_, a___] /; MemberQ[heads, op] :> snegOPinert[op, t, a];
  e = e //. sum[a_, it_List] /; Select[it, latticetypedQ] =!= {} :>
    With[{li = Select[it, latticetypedQ], rest = Select[it, !latticetypedQ[#] &]},
      sum[Total[Map[(a /. Thread[li -> #]) &, Tuples[sites, Length[li]]]], rest]];
  e = e /. vrules /. Thread[Select[sizes, !NumericQ[#] &] -> ntot];
  e = e /. {snegKDinert[a_, b_] :> With[{dd = a - b},
              If[VectorQ[Flatten[{dd}], NumericQ],
                If[And @@ Thread[Mod[Flatten[{dd}], dims] == 0], 1, 0],
                KroneckerDelta[a, b]]],
            snegBPinert[k_, x_] :> If[VectorQ[Flatten[{k, x}], NumericQ],
              Exp[2 Pi I Total[Flatten[{k}] Flatten[{x}]/dims]],
              blochphase[k, x]]};
  e = e /. snegOPinert[op_, t_, a___] :> With[{l = {a}},
    op[t, Sequence @@ ReplacePart[l, p -> foldlabel[l[[p]]]]]];
  Expand[e /. snegNCinert -> nc]
];

(* Split the subspaces of a basis of momentum-space operators by the total
crystal momentum K = Sum m (mod L), which is appended to the quantum
numbers. *)
momentumbasis::mixed = "The basis state `1` does not have a definite total crystal momentum.";
Options[momentumbasis] = {"IndexPosition" -> 1};
momentumbasis[bz_List, dims_List, opts:OptionsPattern[]] := Module[
  {p = OptionValue["IndexPosition"], d = Length[dims], kof, out},
  kof[state_] := Module[{st, mons, ks},
    st = If[FreeQ[state, vc], state, vc2ops[state]];
    mons = sum2list[Expand[st]];
    ks = Union[Map[Function[mon, Module[{labels},
      labels = Cases[{mon}, (op_Symbol)[CR, a___] /; TrueQ[operatorQ[op]] &&
        Length[{a}] >= p :> {a}[[p]], Infinity];
      Which[
        d == 1, Mod[Total[labels], First[dims]],
        labels === {}, ConstantArray[0, d],
        True, Mod[Total[labels], dims]]]], mons]];
    If[Length[ks] != 1, Message[momentumbasis::mixed, state]; Throw[$Failed, snegmomentumbasis]];
    First[ks]];
  Catch[
    out = Flatten[Map[Function[sub,
      Map[{Append[First[sub], kof[First[#]]], #} &, GatherBy[Last[sub], kof]]],
      bz], 1];
    SortBy[out, First],
    snegmomentumbasis]
];

(**** Spin algebras ****)

(* Spin operators S[j..., t] carry the component t (1..5 = x, y, z, +, -) as
their last index. Without a declared size they obey only the su(2) Lie
algebra, which holds in every representation. snegspinoperators[{S, s}]
declares the size s of all spins S[j...]. Products of operators on the
same site are then reduced to the normal form Sum p(S^z) (S^+-)^a:

  - for every size, S^+ S^- = s(s+1) - (S^z)^2 + S^z, and Cartesian
    components in such products are written in terms of S^+-;
  - for numeric s, (S^z)^b (S^+-)^a with b + a > 2s is reduced as well. The
    remaining (2s+1)^2 monomials with b + a <= 2s are linearly independent,
    so the normal form is unique;
  - for s = 1/2, products of Cartesian components follow the Pauli rule
    S^a S^b = delta_ab/4 + (i/2) eps_abc S^c and stay Cartesian.

Spin operators commute with all other operators. They are moved to the left
of bosons, Majorana and Dirac fermions; different spin symbols are sorted
alphabetically. See README_spin.md. *)

If[!ValueQ[listspincomponentindexes], listspincomponentindexes = {} ];

snegspinsizeQ[_] := False;

snegspinoperators::size = "The spin size `1` must be a positive half-integer or an expression built from symbols.";
snegdeclarespinsize[s_?NumericQ] /; IntegerQ[2 s] && s > 0 := True;
snegdeclarespinsize[s_] /; !NumericQ[s] && snegsymbolsof[s] =!= {} := (
  Scan[If[snegnonopQ[#] =!= True, snegrealconstants[#]] &, snegsymbolsof[s]];
  True);
snegdeclarespinsize[s_] := (Message[snegspinoperators::size, s]; False);

snegnumspinQ[op_Symbol] := TrueQ[snegspinsizeQ[op]] && NumericQ[spinof[op]];
snegnumspinQ[_] := False;
snegsymspinQ[op_Symbol] := TrueQ[snegspinsizeQ[op]] && !NumericQ[spinof[op]];
snegsymspinQ[_] := False;

(* Conjugation *)
conj[op_?spinQ[j___, t_Integer]] /; 1 <= t <= 3 := op[j, t];
conj[op_?spinQ[j___, SPININDEXp]] := op[j, SPININDEXm];
conj[op_?spinQ[j___, SPININDEXm]] := op[j, SPININDEXp];
conj[op_?spinQ[j___, t_?spincomponentindexQ]] := op[j, t];

(* Spin operators commute with other operators and are moved to the left. *)
nc[a___, x1_, x2:op_?spinQ[___], b___] /;
  (TrueQ[fermionQ[x1]] || TrueQ[bosonQ[x1]] || TrueQ[majoranaQ[x1]]) :=
    nc[a, x2, x1, b];
nc[a___, x1:op_?spinQ[___], z_?grassmanQ, b___] := nc[a, z, x1, b];
nc[a___, x1:op1_?spinQ[___], x2:op2_?spinQ[___], b___] /;
  (op1 =!= op2 && !OrderedQ[{op1, op2}]) :=
    nc[a, x2, x1, b] + nc[a, cmt[x1, x2], b];

(* Reduction of products of spin operators with a declared numeric size. *)
snegspinmatrix[s_, 1] := spinmatrixX[s];
snegspinmatrix[s_, 2] := spinmatrixY[s];
snegspinmatrix[s_, 3] := spinmatrixZ[s];
snegspinmatrix[s_, 4] := spinmatrixP[s];
snegspinmatrix[s_, 5] := spinmatrixM[s];

snegspinnormalQ[s_, comps_List] := If[s == 1/2, Length[comps] <= 1,
  SubsetQ[{3, 4, 5}, comps] && !(MemberQ[comps, 4] && MemberQ[comps, 5]) &&
  OrderedQ[comps] && Length[comps] <= 2 s];

(* The normal form of the product of components comps as a list of
{coefficient, components} pairs. The matrix element <m + q|M|m> of the
product M determines the polynomial p in p(S^z) (S^+)^q, q >= 0, at the
2s + 1 - q points m + q (similarly for S^-), so that p follows by
interpolation. *)
snegspinreduction[s_, comps_List] := snegspinreduction[s, comps] =
 Module[{m, mm, terms = {}, pts, cl, z, a},
  m = Dot @@ Map[snegspinmatrix[s, #] &, comps];
  If[s == 1/2 && SubsetQ[{1, 2, 3}, comps],
    Return[DeleteCases[
      Prepend[Table[{2 Tr[m . snegspinmatrix[s, t]], {t}}, {t, 3}], {Tr[m]/2, {}}],
      {0, _}]]];
  mm[mf_, mi_] := m[[s - mf + 1, s - mi + 1]];
  Do[
    a = Abs[q];
    pts = If[q >= 0,
      Table[{mi + q, mm[mi + q, mi]/
        Product[Sqrt[s (s + 1) - (mi + k) (mi + k + 1)], {k, 0, a - 1}]}, {mi, -s, s - a}],
      Table[{mi - a, mm[mi - a, mi]/
        Product[Sqrt[s (s + 1) - (mi - k) (mi - k - 1)], {k, 0, a - 1}]}, {mi, -s + a, s}]];
    pts[[All, 2]] = Simplify[pts[[All, 2]]];
    If[AnyTrue[pts[[All, 2]], # =!= 0 &],
      cl = CoefficientList[Expand[InterpolatingPolynomial[pts, z]], z];
      Do[If[cl[[b + 1]] =!= 0,
        AppendTo[terms, {cl[[b + 1]],
          Join[ConstantArray[3, b], ConstantArray[If[q >= 0, 4, 5], a]]}]],
        {b, 0, Length[cl] - 1}]],
    {q, -2 s, 2 s}];
  terms];

snegspinrun[op_, {j___}, comps_List] := Total[Map[
  #[[1]] (nc @@ Map[op[j, #] &, #[[2]]]) &, snegspinreduction[spinof[op], comps]]];

nc[a___, r:Longest[(op_?snegnumspinQ[j___, _Integer]) ..], b___] /;
  Length[{r}] >= 2 && VectorQ[{r}[[All, -1]], 1 <= # <= 5 &] &&
  !snegspinnormalQ[spinof[op], {r}[[All, -1]]] :=
    nc[a, snegspinrun[op, {j}, {r}[[All, -1]]], b];

(* Symbolic size: Cartesian components in same-site products are written in
terms of S^+-, and S^+ S^- is eliminated using the Casimir operator. *)
snegcart2pm[op_[j___, 1]] := (op[j, 4] + op[j, 5])/2;
snegcart2pm[op_[j___, 2]] := (op[j, 4] - op[j, 5])/(2 I);
snegcart2pm[x_] := x;

nc[a___, x1:op_?snegsymspinQ[j___, t1_Integer], x2:op_[j___, t2_Integer], b___] /;
  (1 <= t1 <= 5 && 1 <= t2 <= 5 && (t1 <= 2 || t2 <= 2)) :=
    nc[a, snegcart2pm[x1], snegcart2pm[x2], b];
nc[a___, op_?snegsymspinQ[j___, 4], op_[j___, 5], b___] :=
  With[{s = spinof[op]},
    s (s + 1) nc[a, b] - nc[a, op[j, 3], op[j, 3], b] + nc[a, op[j, 3], b]];

(* Symbolic Cartesian components *)
spincomponentindexQ[_] := False;
snegspincomponentindexes[l__] := Scan[
  (snegfreeindexes[#];
   Conjugate[#] ^= #;
   spincomponentindexQ[#] ^= True;
   addto[listspincomponentindexes, #]) &, {l}];

sum[z_, it_List] /; AnyTrue[it, spincomponentindexQ[#] && FreeQ[z, #] &] :=
  With[{x = SelectFirst[it, spincomponentindexQ[#] && FreeQ[z, #] &]},
    3 sum[z, DeleteCases[it, x]]];

SetAttributes[levicivita, NumericFunction];
levicivita[a_Integer, b_Integer, c_Integer] :=
  If[SubsetQ[{1, 2, 3}, {a, b, c}], Signature[{a, b, c}], 0];
levicivita[a_, b_, c_] /; !DuplicateFreeQ[{a, b, c}] := 0;
levicivita[a_, b_, c_] /; !OrderedQ[{a, b, c}] :=
  Signature[{a, b, c}] levicivita @@ Sort[{a, b, c}];
levicivita /: Conjugate[levicivita[a___]] := levicivita[a];

snegspincompQ[t_] := spincomponentindexQ[t] || MemberQ[{1, 2, 3, 4, 5}, t];

cmt[op_[i___, t1_], op_[i___, t2_]] /; spinQ[op] &&
  (spincomponentindexQ[t1] || spincomponentindexQ[t2]) &&
  snegspincompQ[t1] && snegspincompQ[t2] := snegspincmt[op, {i}, t1, t2];

snegspincmt[op_, {i___}, t1_Integer, t2_Integer] := cmt[op[i, t1], op[i, t2]];
snegspincmt[op_, {i___}, 4, t2_] := snegspincmt[op, {i}, 1, t2] + I snegspincmt[op, {i}, 2, t2];
snegspincmt[op_, {i___}, 5, t2_] := snegspincmt[op, {i}, 1, t2] - I snegspincmt[op, {i}, 2, t2];
snegspincmt[op_, {i___}, t1_, 4] := snegspincmt[op, {i}, t1, 1] + I snegspincmt[op, {i}, t1, 2];
snegspincmt[op_, {i___}, t1_, 5] := snegspincmt[op, {i}, t1, 1] - I snegspincmt[op, {i}, t1, 2];
snegspincmt[op_, {i___}, t1_, t2_] :=
  With[{c = ssJWRnewname[SelectFirst[{t1, t2}, spincomponentindexQ], {t1, t2}, {i, t1, t2}]},
    I sum[levicivita[t1, t2, c] op[i, c], {c}]];

komutator[x1:op_?spinQ[___, t1_], x2:op_?spinQ[___, t2_]] /;
  (spincomponentindexQ[t1] || spincomponentindexQ[t2]) := cmt[x1, x2];

(* Rewriting in a single set of components *)
spincomponents::basis = "The component set `1` must be \"ZPM\" or \"XYZ\".";
spincomponents[expr_, basis_String:"ZPM"] := Module[{e},
  If[!MemberQ[{"ZPM", "XYZ"}, basis],
    Message[spincomponents::basis, basis]; Return[$Failed]];
  e = expr //. sum[z_, it_List] /; AnyTrue[it, spincomponentindexQ] :>
    With[{ci = Select[it, spincomponentindexQ], rest = Select[it, !spincomponentindexQ[#] &]},
      sum[Total[Map[z /. Thread[ci -> #] &, Tuples[{1, 2, 3}, Length[ci]]]], rest]];
  e = If[basis === "ZPM",
    e /. {op_?spinQ[j___, 1] :> (op[j, 4] + op[j, 5])/2,
          op_?spinQ[j___, 2] :> (op[j, 4] - op[j, 5])/(2 I)},
    e /. {op_?spinQ[j___, 4] :> op[j, 1] + I op[j, 2],
          op_?spinQ[j___, 5] :> op[j, 1] - I op[j, 2]}];
  Expand[e]];

(* Lists of spin sites are given as {S[1], S[2], ...} or, since S[0]
evaluates to zero, as {S -> {0, 1, ...}}; multi-index sites are written as
S -> {{1, "a"}, ...}. The result is a list of {op, {site indexes}}. *)
snegspinsites::spec = "`1` is not a spin site specification.";
snegspinsites::size = "The spin `1` needs a declared numeric size, e.g. snegspinoperators[{`1`, 1/2}].";
snegspinsites[spec_List] := Catch[Flatten[Map[snegspinsite1, spec], 1], snegspinsites];
snegspinsite1[(Rule | RuleDelayed)[op_Symbol?spinQ, l_List]] := Map[{op, Flatten[{#}]} &, l];
snegspinsite1[op_Symbol?spinQ[j___]] := {{op, {j}}};
snegspinsite1[x_] := (Message[snegspinsites::spec, x]; Throw[$Failed, snegspinsites]);
snegspinsitesizes[sites_List] := If[AllTrue[sites[[All, 1]], snegnumspinQ],
  spinof /@ sites[[All, 1]],
  Message[snegspinsites::size, First[Select[sites[[All, 1]], !snegnumspinQ[#] &]]]; $Failed];

(* Replacement rules to the bra-ket representation; site k corresponds to
the k-th slot of the kets, as in spinbasis[{s1, s2, ...}]. Each operator
includes the identity of the other slots, so that its matrix elements
between full kets are defined. *)
spinketbrarules[spec_List] := Module[{sites, sizes, n, pos, id},
  sites = snegspinsites[spec];
  If[sites === $Failed, Return[$Failed]];
  sizes = snegspinsitesizes[sites];
  If[sizes === $Failed, Return[$Failed]];
  n = Length[sites];
  pos[k_] := If[n == 1, {1}, UnitVector[n, k]];
  id[k_] := nc @@ Table[spinketbraI[sizes[[l]], pos[l]], {l, Complement[Range[n], {k}]}];
  Flatten[Table[
    With[{op = sites[[k, 1]], j = sites[[k, 2]], s = sizes[[k]], p = pos[k], ik = id[k]},
      MapThread[(op @@ Append[j, #1]) -> Expand[nc[#2[s, p], ik]] &,
        {Range[5], {spinketbraX, spinketbraY, spinketbraZ, spinketbraP, spinketbraM}}]],
    {k, n}]]
];

(* Matrices of a spin Hamiltonian in the product basis |m1, ..., mN>, with
each m running from +s to -s and the first site varying slowest. With
"Blocks" -> True the matrix is split into the S^z_tot subspaces. *)
spinclustermatrices::op = "`1` is not an operator of the listed spin sites.";
spinclustermatrices::sz = "The operator does not conserve the total S^z; use \"Blocks\" -> False.";
Options[spinclustermatrices] = {"Blocks" -> True, "Sparse" -> False};
spinclustermatrices[h_, spec_List, opts:OptionsPattern[]] := Catch[Module[
  {sites, sizes, n, dims, dim, idx, embed, mat, m, sz, szs, out, form},
  sites = snegspinsites[spec];
  If[sites === $Failed, Throw[$Failed, spinclustermatrices]];
  sizes = snegspinsitesizes[sites];
  If[sizes === $Failed, Throw[$Failed, spinclustermatrices]];
  n = Length[sites];
  dims = 2 sizes + 1;
  dim = Times @@ dims;
  Do[idx[sites[[k, 1]], sites[[k, 2]]] = k, {k, n}];
  embed[k_, t_] := embed[k, t] = If[n == 1, SparseArray[snegspinmatrix[sizes[[1]], t]],
    KroneckerProduct @@ Table[
      If[l == k, SparseArray[snegspinmatrix[sizes[[k]], t]], IdentityMatrix[dims[[l]], SparseArray]],
      {l, n}]];
  mat[x_Plus] := Total[mat /@ List @@ x];
  mat[HoldPattern[nc[a__]]] := Dot @@ (mat /@ {a});
  mat[x_Times] := With[{num = Select[x, isnumericQ], rest = Select[x, !isnumericQ[#] &]},
    num mat[rest]];
  mat[x:op_?spinQ[j___, t_Integer]] /; 1 <= t <= 5 := If[IntegerQ[idx[op, {j}]],
    embed[idx[op, {j}], t],
    Message[spinclustermatrices::op, x]; Throw[$Failed, spinclustermatrices]];
  mat[z_?isnumericQ] := z IdentityMatrix[dim, SparseArray];
  mat[x_] := (Message[spinclustermatrices::op, x]; Throw[$Failed, spinclustermatrices]);
  m = SparseArray[mat[Expand[h]], {dim, dim}];
  form = If[TrueQ[OptionValue["Sparse"]], Identity, Normal];
  If[!TrueQ[OptionValue["Blocks"]], Throw[form[m], spinclustermatrices]];
  sz = Flatten[Outer[Plus, Sequence @@ Map[Range[#, -#, -1] &, sizes]]];
  If[AnyTrue[m["NonzeroPositions"], sz[[#[[1]]]] != sz[[#[[2]]]] &],
    Message[spinclustermatrices::sz]; Throw[$Failed, spinclustermatrices]];
  szs = Union[sz];
  Map[With[{p = Flatten[Position[sz, #]]}, {{#}, form[m[[p, p]]]}] &, szs]
], spinclustermatrices];

(* Representations of spin operators in terms of fermions and bosons. The
substitution rules act on S[j..., t] with t = 1..5. *)
snegspinmaprules[op_, f_] := {
  op[j___, 1] :> Expand[(f[{j}, 4] + f[{j}, 5])/2],
  op[j___, 2] :> Expand[(f[{j}, 4] - f[{j}, 5])/(2 I)],
  op[j___, 3] :> f[{j}, 3],
  op[j___, 4] :> f[{j}, 4],
  op[j___, 5] :> f[{j}, 5]};

(* Jordan-Wigner: S^+_k = f^dagger_k Prod_(l<k) (1 - 2 n_l), S^z_k = n_k - 1/2
for the sites in the given order; f is a spinless fermion. *)
jordanwigner::spin = "The Jordan-Wigner transformation requires spin-1/2 operators.";
jordanwigner::op = "`1` must be a spinless fermion operator.";
jordanwigner[expr_, op_Symbol?spinQ -> f_Symbol, labels_List] := Module[
  {sites, string, rules},
  If[spinof[op] =!= 1/2, Message[jordanwigner::spin]; Return[$Failed]];
  If[!TrueQ[fermionQ[f]] || spinof[f] =!= 0, Message[jordanwigner::op, f]; Return[$Failed]];
  sites = Map[Flatten[{#}] &, labels];
  string[k_] := nc @@ Table[1 - 2 number[f @@ sites[[l]]], {l, k - 1}];
  rules = Flatten[Table[
    With[{j = sites[[k]], sp = Expand[nc[f[CR, Sequence @@ sites[[k]], 0], string[k]]],
          sm = Expand[nc[string[k], f[AN, Sequence @@ sites[[k]], 0]]]},
      {op[Sequence @@ j, 1] -> Expand[(sp + sm)/2],
       op[Sequence @@ j, 2] -> Expand[(sp - sm)/(2 I)],
       op[Sequence @@ j, 3] -> number[f @@ j] - 1/2,
       op[Sequence @@ j, 4] -> sp,
       op[Sequence @@ j, 5] -> sm}],
    {k, Length[sites]}]];
  Expand[expr /. rules]
];

(* Holstein-Primakoff: S^z = s - b^dagger b, S^+ = Sqrt[2s - b^dagger b] b.
Each normal-ordered same-site product (S^z)^n (S^+-)^a is expanded exactly
in normal-ordered boson monomials, using the Newton series
h(b^dagger b) = Sum_m Delta^m h(0)/m! (b^dagger)^m b^m; terms with more than
nmax boson operators are dropped. *)
holsteinprimakoff::size = "The spin `1` needs a declared size, e.g. snegspinoperators[{`1`, s}].";
holsteinprimakoff::op = "`1` must be a bosonic operator.";
holsteinprimakoff[expr_, op_Symbol?spinQ -> bb_Symbol, nmax_Integer?NonNegative] := Module[
  {s, e, run, hpnc, newton, deg, trunc},
  If[!TrueQ[snegspinsizeQ[op]], Message[holsteinprimakoff::size, op]; Return[$Failed]];
  If[!TrueQ[bosonQ[bb]], Message[holsteinprimakoff::op, bb]; Return[$Failed]];
  s = spinof[op];
  newton[h_, m_] := Sum[(-1)^(m - l) Binomial[m, l] h[l], {l, 0, m}]/m!;
  run[{j___}, comps_List] := Module[{nz = Count[comps, 3], a, h, cr, an},
    a = Count[comps, 4 | 5];
    cr = bb[CR, j]; an = bb[AN, j];
    If[MemberQ[comps, 5],
      h = Function[x, If[nz == 0, 1, (s - a - x)^nz] Product[Sqrt[2 s - x - k], {k, 0, a - 1}]];
      Sum[newton[h, m] nc[Sequence @@ ConstantArray[cr, m + a], Sequence @@ ConstantArray[an, m]],
        {m, 0, Floor[(nmax - a)/2]}],
      h = Function[x, If[nz == 0, 1, (s - x)^nz] Product[Sqrt[2 s - x - k], {k, 0, a - 1}]];
      Sum[newton[h, m] nc[Sequence @@ ConstantArray[cr, m], Sequence @@ ConstantArray[an, m + a]],
        {m, 0, Floor[(nmax - a)/2]}]]];
  hpnc[l_List] := nc @@ Map[
    If[MatchQ[First[#], op[___, _Integer]],
      run[Most[List @@ First[#]], Map[Last, #]], nc @@ #] &,
    Split[l, MatchQ[#1, op[___, _Integer]] && MatchQ[#2, op[___, _Integer]] &&
      Most[List @@ #1] === Most[List @@ #2] &]];
  e = spincomponents[expr];
  e = e /. {HoldPattern[nc[a__]] :> hpnc[{a}], x:op[___, _Integer] :> hpnc[{x}]};
  deg[t_] := Count[{t}, bb[_, ___], Infinity];
  trunc[x_] := Total[Map[
    Which[!FreeQ[#, sum], # /. sum[z_, it_List] :> sum[trunc[z], it],
          deg[#] <= nmax, #,
          True, 0] &,
    sum2list[Expand[x]]]];
  trunc[e]
];

(* Schwinger bosons: S^+ = a^dagger b, S^- = b^dagger a,
S^z = (a^dagger a - b^dagger b)/2, with a^dagger a + b^dagger b = 2s. *)
schwingerbosons::op = "`1` and `2` must be bosonic operators.";
schwingerbosons[expr_, op_Symbol?spinQ -> {a_Symbol, b_Symbol}] := (
  If[!TrueQ[bosonQ[a]] || !TrueQ[bosonQ[b]], Message[schwingerbosons::op, a, b]; Return[$Failed]];
  Expand[expr /. snegspinmaprules[op, Function[{jj, t}, Which[
    t == 3, (number[a @@ jj] - number[b @@ jj])/2,
    t == 4, nc[a[CR, Sequence @@ jj], b[AN, Sequence @@ jj]],
    t == 5, nc[b[CR, Sequence @@ jj], a[AN, Sequence @@ jj]]]]]]);

(* Abrikosov pseudofermions: S = f^dagger S f with 2s + 1 flavors; the
representation is faithful in the subspace with one fermion per site. *)
abrikosov::spin = "The fermion `1` must have the spin `2` of the spin operators.";
abrikosov[expr_, op_Symbol?spinQ -> f_Symbol?fermionQ] := (
  If[spinof[f] =!= spinof[op], Message[abrikosov::spin, f, spinof[op]]; Return[$Failed]];
  Expand[expr /. snegspinmaprules[op, Function[{jj, t}, With[{v = spinxyz[f @@ jj]},
    Which[t <= 3, v[[t]], t == 4, Expand[v[[1]] + I v[[2]]], t == 5, Expand[v[[1]] - I v[[2]]]]]]]]);

(* A single spin site, given as S[j...] or S -> j. *)
snegspinsite[(Rule | RuleDelayed)[op_Symbol?spinQ, j_]] := {op, Flatten[{j}]};
snegspinsite[op_Symbol?spinQ[j___]] := {op, {j}};
snegspinsite[x_] := (Message[snegspinsites::spec, x]; $Failed);

(* Spherical tensor operators: T_k^k = (-S^+/Sqrt[2])^k and
T_k^(q-1) = [S^-, T_k^q]/Sqrt[(k+q)(k-q+1)], so that T_1^0 = S^z,
T_1^(+-1) = -+S^(+-)/Sqrt[2], and T_2^0 = (3 (S^z)^2 - S.S)/Sqrt[6]. *)
spintensor::range = "The rank must satisfy k >= 0 and -k <= q <= k.";
spintensor[k_Integer, q_Integer, x_] := Module[{st = snegspinsite[x]},
  If[st === $Failed, Return[$Failed]];
  If[k < 0 || Abs[q] > k, Message[spintensor::range]; Return[$Failed]];
  snegspintensor[st[[1]], st[[2]], k, q]];
snegspintensor[op_, j_List, k_, k_] := snegspintensor[op, j, k, k] =
  Expand[(-1/Sqrt[2])^k nc @@ ConstantArray[op @@ Append[j, 4], k]];
snegspintensor[op_, j_List, k_, q_] /; q < k := snegspintensor[op, j, k, q] =
  Expand[komutator[op @@ Append[j, 5], snegspintensor[op, j, k, q + 1]]/
    Sqrt[(k + q + 1) (k - q)]];

(* Stevens operators O_k^q (Abragam and Bleaney normalization), e.g.
O_2^0 = 3 (S^z)^2 - s(s+1), O_2^2 = ((S^+)^2 + (S^-)^2)/2 = (S^x)^2 - (S^y)^2,
O_2^-2 = ((S^+)^2 - (S^-)^2)/(2i). For q > 0, O_k^(+-q) is proportional to
(-1)^q T_k^q +- T_k^-q, normalized such that the coefficient of
(S^z)^(k-q) (S^+)^q is L/2 (L/(2i) for -q), where L is the leading coefficient
of the primitive integer polynomial proportional to d^q P_k(z)/dz^q; for
q = 0 the coefficient of (S^z)^k is L. *)
snegspinoperators[{snegstevensS, snegstevenss}];
stevens::size = "The spin `1` needs a declared size, e.g. snegspinoperators[{`1`, 1/2}].";
stevens[k_Integer, q_Integer, x_] := Module[{st = snegspinsite[x]},
  If[st === $Failed, Return[$Failed]];
  If[k < 0 || Abs[q] > k, Message[spintensor::range]; Return[$Failed]];
  If[!TrueQ[snegspinsizeQ[st[[1]]]], Message[stevens::size, st[[1]]]; Return[$Failed]];
  Expand[snegstevens[k, q] /. {snegstevensS[t_] :> st[[1]] @@ Append[st[[2]], t],
    snegstevenss -> spinof[st[[1]]]}]];
snegstevensL[k_, q_] := Module[{z, cl},
  cl = CoefficientList[D[LegendreP[k, z], {z, q}], z];
  cl = cl LCM @@ Denominator[cl];
  Last[cl]/(GCD @@ cl)];
snegstevens[0, 0] = 1;
snegstevens[k_, q_] := snegstevens[k, q] = Module[{p = Abs[q], tp, tm, mono, alpha, L},
  tp = (-1)^p snegspintensor[snegstevensS, {}, k, p];
  tm = snegspintensor[snegstevensS, {}, k, -p];
  mono = nc @@ Join[ConstantArray[snegstevensS[3], k - p], ConstantArray[snegstevensS[4], p]];
  alpha = Coefficient[tp, mono];
  L = snegstevensL[k, p];
  Expand[Which[
    p == 0, L/alpha tp,
    q > 0, L/(2 alpha) (tp + tm),
    True, L/(2 I alpha) (tp - tm)]]];

(* Rotation of spin vectors, S_j -> R(j).S_j, with R a 3 by 3 matrix or a
function of the site indexes. *)
rotatespin::matrix = "The rotation `1` must be a 3 by 3 matrix.";
rotatespin[expr_, op_Symbol?spinQ, r_] := Catch[Expand[expr /.
  op[j___, t_Integer] /; 1 <= t <= 5 :> Module[{m = If[MatrixQ[r], r, r[j]], v},
    If[!(MatrixQ[m] && Dimensions[m] == {3, 3}),
      Message[rotatespin::matrix, m]; Throw[$Failed, rotatespin]];
    v = m . {op[j, 1], op[j, 2], op[j, 3]};
    Which[t <= 3, v[[t]], t == 4, v[[1]] + I v[[2]], True, v[[1]] - I v[[2]]]]],
  rotatespin];

(**** Hubbard X-operators ****)

(* X[j..., a, b] = |a><b| acts on the local states of site j. The states
carry fermion parities; X^ab is fermionic if the parities of a and b differ.
On the same site X^ab X^cd = delta_bc X^ad. On different sites (or for
different symbols) X-operators commute or, if both are fermionic,
anticommute; for symbolic sites
X_i^ab X_j^cd = +-X_j^cd X_i^ab + delta_ij (delta_bc X^ad -+ delta_da X^cb).
X-operators are independent of all other operators: they are moved to the
right of spin operators and to the left of bosons, Majorana and Dirac
fermions, anticommuting with fermions when fermionic. *)

If[!ValueQ[listxoperators], listxoperators = {} ];

xoperatorQ[_] := False;
xoperatorQ[op_?xoperatorQ[___]] := True;

snegxoperators::spec = "The states `1` and the parities `2` (0 or 1) must be lists of equal length with distinct states.";
snegxhubbardatom = {{"0", "UP", "DO", "2"}, {0, 1, 1, 0}};
snegxoperators[x_Symbol, "HubbardAtom"] := snegxoperators[x, Sequence @@ snegxhubbardatom];
snegxoperators[x_Symbol, states_List, parities_List] := If[
  Length[states] == Length[parities] && DuplicateFreeQ[states] &&
    SubsetQ[{0, 1}, parities],
  operatorQ[x] ^= True;
  xoperatorQ[x] ^= True;
  xstates[x] ^= states;
  xparities[x] ^= AssociationThread[states, parities];
  SetAttributes[x, NHoldAll];
  If[PrettyOutput,
    Format[x[j___, a_, b_], StandardForm] := DisplayForm @ StyleBox[
      If[{j} === {}, Superscript[x, RowBox[{a, b}]],
        Subsuperscript[x, RowBox[{j}], RowBox[{a, b}]]],
      ScriptSizeMultipliers -> 1, ScriptBaselineShifts -> {1, 1}]];
  addto[listxoperators, x];,
  Message[snegxoperators::spec, states, parities]; $Failed];

(* Fermion parity of X[j..., a, b], or None for labels that are not states. *)
snegxparity[op_[___, a_, b_]] := With[{p = xparities[op]},
  If[KeyExistsQ[p, a] && KeyExistsQ[p, b], Mod[p[a] + p[b], 2], None]];
snegxparity[_] := None;

conj[op_?xoperatorQ[j___, a_, b_]] := op[j, b, a];

nc[x___, op_?xoperatorQ[j___, a_, b_], op_[j___, c_, d_], y___] :=
  KroneckerDelta[b, c] nc[x, op[j, a, d], y];

nc[x___, x1:op_?xoperatorQ[j1___, a_, b_], x2:op_[j2___, c_, d_], y___] /;
  ({j1} =!= {j2} && !OrderedQ[{x1, x2}] &&
   snegxparity[x1] =!= None && snegxparity[x2] =!= None) :=
  With[{sg = (-1)^(snegxparity[x1] snegxparity[x2])},
    sg nc[x, x2, x1, y] +
    If[Length[{j1}] == Length[{j2}],
      Inner[KroneckerDelta, {j1}, {j2}, Times] (
        KroneckerDelta[b, c] nc[x, op[j1, a, d], y] -
        sg KroneckerDelta[d, a] nc[x, op[j1, c, b], y]), 0]];

nc[x___, x1:op1_?xoperatorQ[___, _, _], x2:op2_?xoperatorQ[___, _, _], y___] /;
  (op1 =!= op2 && !OrderedQ[{op1, op2}] &&
   snegxparity[x1] =!= None && snegxparity[x2] =!= None) :=
  (-1)^(snegxparity[x1] snegxparity[x2]) nc[x, x2, x1, y];

nc[x___, x1:op1_?xoperatorQ[___, _, _], x2:op2_?spinQ[___], y___] :=
  nc[x, x2, x1, y];
nc[x___, x1_, x2:op_?xoperatorQ[___, _, _], y___] /;
  (TrueQ[bosonQ[x1]] && snegxparity[x2] =!= None) := nc[x, x2, x1, y];
nc[x___, x1_, x2:op_?xoperatorQ[___, _, _], y___] /;
  ((TrueQ[fermionQ[x1]] || TrueQ[majoranaQ[x1]]) && snegxparity[x2] =!= None) :=
    (-1)^snegxparity[x2] nc[x, x2, x1, y];
nc[x___, x1:op_?xoperatorQ[___, _, _], z_?grassmanQ, y___] /;
  snegxparity[x1] =!= None := (-1)^snegxparity[x1] nc[x, z, x1, y];

(* X-operators of the Hubbard atom with the states |0>, |UP> = c^dagger_UP|0>,
|DO> = c^dagger_DO|0>, |2> = c^dagger_UP c^dagger_DO|0>, in terms of the
fermion operators of site j. *)
xoperator::state = "`1` is not one of the states \"0\", \"UP\", \"DO\", \"2\".";
xoperator[op_?fermionQ[j___], a_, b_] := Module[{cr, p0},
  If[!SubsetQ[First[snegxhubbardatom], {a, b}],
    Message[xoperator::state, First[Complement[{a, b}, First[snegxhubbardatom]]]];
    Return[$Failed]];
  cr["0"] = 1;
  cr["UP"] = op[CR, j, UP];
  cr["DO"] = op[CR, j, DO];
  cr["2"] = nc[op[CR, j, UP], op[CR, j, DO]];
  p0 = nc[1 - number[op[j], UP], 1 - number[op[j], DO]];
  Expand[nc[cr[a], p0, conj[cr[b]]]]];
xoperator[op_?AtomQ, a_, b_] := xoperator[op[], a, b];

xtofermions::states = "`1` must be declared with the states of the Hubbard atom, snegxoperators[`1`, \"HubbardAtom\"].";
xtofermions[expr_, x_Symbol?xoperatorQ -> c_Symbol?fermionQ] := (
  If[Sort[xstates[x]] =!= Sort[First[snegxhubbardatom]] ||
     Map[xparities[x], First[snegxhubbardatom]] =!= Last[snegxhubbardatom],
    Message[xtofermions::states, x]; Return[$Failed]];
  Expand[expr /. x[j___, a_, b_] :> xoperator[c[j], a, b]]);

(**** Quadratic forms, Bogoliubov transformations, and mean-field theory ****)

(* A quadratic Hamiltonian of n modes a_i is written in the BdG form
H = 1/2 Psi^dagger M Psi + E0 with Psi = (a_1, ..., a_n, a_1^dagger, ...,
a_n^dagger). For fermions M = {{h, Delta}, {Delta^dagger, -h^T}} with
Delta^T = -Delta, for bosons M = {{A, B}, {B^*, A^*}} with B^T = B. The
matrix elements follow from M_pq = eta_p eta_q [Psi_p, [H, Psi^dagger_q]]_-+
with eta = (1, ..., 1, -1, ..., -1) for bosons and eta = 1 for fermions, so
that M is always in this symmetrized form. A Bogoliubov transformation
Psi = T Phi, Phi = (gamma_1, ..., gamma_n, gamma_1^dagger, ...), with
T = {{U, V^*}, {V, U^*}} gives H = Sum_m eps_m gamma_m^dagger gamma_m + Egs,
Egs = E0 - 1/2 Sum eps (fermions) or E0 + 1/2 Sum eps (bosons). *)

(* Expectation values. avg[] is linear, applied to the canonically ordered
operator expression, and treated as a number by nc[] (see isnumericQ). *)
sneglinearoperatorFirst[avg];
avg[sum[a_, it_List]] := sum[avg[a], it];
avg[_?fermionQ[___]] := 0;
avg /: Conjugate[avg[x_]] := avg[conj[x]];
If[PrettyOutput,
  Format[avg[x_], StandardForm] := DisplayForm @
    RowBox[{"\[LeftAngleBracket]", x, "\[RightAngleBracket]"}];
];

(* Modes: templates op or op[j...] (fermions get the spin labels as the last
index, varying fastest) or functions t -> operator expression of t = CR, AN. *)
quadraticform::mode = "Invalid mode `1`.";
quadraticform::stat = "The modes must be all fermionic or all bosonic.";
snegquadmodes[specs_List] := Module[{m},
  m = Flatten[Map[snegquadmode, specs]];
  If[MemberQ[m, $Failed] || m === {}, $Failed, m]];
snegquadmode[op_Symbol] := snegquadmode[op[]];
snegquadmode[op_Symbol[e___]] /; TrueQ[fermionQ[op]] :=
  Map[With[{s = #}, Function[t, op[t, e, s]]] &, snegspinlabels[op]];
snegquadmode[op_Symbol[e___]] /; TrueQ[bosonQ[op]] := {Function[t, op[t, e]]};
snegquadmode[f_Function] := {f};
snegquadmode[x_] := (Message[quadraticform::mode, x]; $Failed);

(* "Fermion" or "Boson" *)
snegquadstat[modes_List] := Module[{heads},
  heads = Union[Cases[Map[#[AN] &, modes], (h_Symbol)[___] /; TrueQ[operatorQ[h]] :> h,
    {0, Infinity}]];
  Which[
    heads === {}, $Failed,
    And @@ Map[TrueQ[fermionQ[#]] &, heads], "Fermion",
    And @@ Map[TrueQ[bosonQ[#]] &, heads], "Boson",
    True, $Failed]];

(* All elements vanish: exactly, or within rounding errors for inexact
numbers. *)
snegzeroQ[l_List, scale_:1] := And @@ Map[
  If[NumericQ[#] && Precision[#] < Infinity, Abs[#] < 10^-10 scale, TrueQ[Simplify[#] === 0]] &,
  l];

snegquadeta[stat_, n_] := If[stat === "Boson",
  Join[ConstantArray[1, n], ConstantArray[-1, n]], ConstantArray[1, 2 n]];
snegquadbracket[stat_] := If[stat === "Boson", komutator, antikomutator];

snegquadsetup[specs_List] := Module[{modes, stat},
  modes = snegquadmodes[specs];
  If[modes === $Failed, Return[$Failed]];
  stat = snegquadstat[modes];
  If[stat === $Failed, Message[quadraticform::stat]; Return[$Failed]];
  {modes, stat}];

quadraticoperator::dim = "The matrix dimension `1` does not match `2` modes.";
quadraticoperator[M_?MatrixQ, specs_List, E0_:0] := Module[{s, modes, n, psi, psid},
  s = snegquadsetup[specs];
  If[s === $Failed, Return[$Failed]];
  modes = First[s];
  n = Length[modes];
  Which[
    Length[M] == 2 n && Length[First[M]] == 2 n,
      psi = Join[Map[#[AN] &, modes], Map[#[CR] &, modes]];
      psid = Join[Map[#[CR] &, modes], Map[#[AN] &, modes]];
      Expand[1/2 Sum[If[M[[i, j]] === 0, 0, M[[i, j]] nc[psid[[i]], psi[[j]]]],
        {i, 2 n}, {j, 2 n}] + E0],
    Length[M] == n && Length[First[M]] == n,
      Expand[Sum[If[M[[i, j]] === 0, 0, M[[i, j]] nc[modes[[i]][CR], modes[[j]][AN]]],
        {i, n}, {j, n}] + E0],
    True,
      Message[quadraticoperator::dim, Dimensions[M], n]; $Failed]
];

quadraticform::sum = "Symbolic sums are not supported; use bdghamiltonian for momentum-space Hamiltonians or sumCluster for finite clusters.";
quadraticform::order = "The Hamiltonian contains terms that are not quadratic in the modes (higher-order terms or products with other operators): `1`.";
quadraticform::rest = "The Hamiltonian contains terms that are not quadratic forms of the given modes: `1`.";
quadraticform::odd = "The Hamiltonian contains terms linear in the fermion operators.";
quadraticform::linear = "The Hamiltonian contains terms linear in the boson operators; use \"Linear\" -> True.";
quadraticform::herm = "The quadratic form is not Hermitian.";
quadraticform::anom = "The Hamiltonian contains anomalous terms; use \"BdG\" -> True.";
Options[quadraticform] = {"BdG" -> True, "Linear" -> False, "Check" -> True};
quadraticform[H_, specs_List, opts:OptionsPattern[]] := Module[
  {s, modes, stat, n, psi, psid, eta, br, hx, cols, M, bad, R, f, g, E0, h},
  s = snegquadsetup[specs];
  If[s === $Failed, Return[$Failed]];
  {modes, stat} = s;
  n = Length[modes];
  If[!FreeQ[H, sum], Message[quadraticform::sum]; Return[$Failed]];
  psi = Join[Map[#[AN] &, modes], Map[#[CR] &, modes]];
  psid = Join[Map[#[CR] &, modes], Map[#[AN] &, modes]];
  eta = snegquadeta[stat, n];
  br = snegquadbracket[stat];
  hx = Expand[H];
  cols = Map[Expand[komutator[hx, #]] &, psid];
  M = Table[Expand[eta[[i]] eta[[j]] br[psi[[i]], cols[[j]]]], {i, 2 n}, {j, 2 n}];
  bad = Select[Flatten[M], !isnumericQ[#] &];
  If[bad =!= {}, Message[quadraticform::order, First[bad]]; Return[$Failed]];
  (* Rounding errors of inexact coefficients are removed from the remainder. *)
  R = Chop[Expand[hx - quadraticoperator[M, specs]]];
  If[stat === "Fermion",
    If[!isnumericQ[R],
      If[MemberQ[Map[snegtermops, sum2list[R]], {_, {_}}],
        Message[quadraticform::odd], Message[quadraticform::rest, R]];
      Return[$Failed]];
    f = g = ConstantArray[0, n],
    (* bosons: linear terms f_i b_i^dagger + g_i b_i *)
    f = Chop[Map[Expand[komutator[#[AN], R]] &, modes]];
    g = Chop[Map[Expand[komutator[R, #[CR]]] &, modes]];
    bad = Select[Join[f, g], !isnumericQ[#] &];
    If[bad =!= {}, Message[quadraticform::rest, First[bad]]; Return[$Failed]];
    R = Chop[Expand[R - Sum[f[[i]] modes[[i]][CR] + g[[i]] modes[[i]][AN], {i, n}]]];
    If[!isnumericQ[R], Message[quadraticform::rest, R]; Return[$Failed]];
    If[!MatchQ[Flatten[{f, g}], {0 ...}] && !TrueQ[OptionValue["Linear"]],
      Message[quadraticform::linear]; Return[$Failed]]];
  E0 = R;
  If[TrueQ[OptionValue["Check"]] &&
     !snegzeroQ[Join[Flatten[M - ConjugateTranspose[M]], g - Conjugate[f]]],
    Message[quadraticform::herm]];
  If[!TrueQ[OptionValue["BdG"]],
    If[!MatchQ[Flatten[M[[1 ;; n, n + 1 ;; 2 n]]], {0 ...}],
      Message[quadraticform::anom]; Return[$Failed]];
    h = M[[1 ;; n, 1 ;; n]];
    E0 = Expand[E0 + If[stat === "Boson", 1, -1] Tr[h]/2];
    M = h];
  If[TrueQ[OptionValue["Linear"]], {M, f, E0}, {M, E0}]
];

(* BdG Hamiltonian in momentum space, Psi_k = (c_k, c_-k^dagger). *)
Options[bdghamiltonian] = {"IndexPosition" -> 1, "Check" -> True};
bdghamiltonian::index = "`1` must be a declared momentum index.";
bdghamiltonian::rest = blochhamiltonian::rest;
bdghamiltonian::transl = blochhamiltonian::transl;
bdghamiltonian::herm = "The BdG Hamiltonian is not Hermitian.";
bdghamiltonian::linear = "The Hamiltonian contains terms with an odd number of operators.";
bdghamiltonian::ordering = "The constant E0 requires creation operators to the left of annihilation operators (ordering EMPTY) for `1`.";
bdghamiltonian::stat = quadraticform::stat;

snegbdgmodes[orbs_, q_, p_] := Module[{up},
  up = Map[snegblochmodes[#, q, p] &, orbs];
  If[MemberQ[up, $Failed], Return[$Failed]];
  up = Flatten[up];
  Join[up, Map[With[{m = #}, Function[t, m[CR + AN - t] /. q -> -q]] &, up]]];

snegbdgelement[H_, m1_, m2_, eta1_, eta2_, boson_] := snegsumreduce[
  eta1 eta2 If[boson, komutator, antikomutator][m1[AN], komutator[H, m2[CR]]]];

bdghamiltonian[H_, orbs_List, k_Symbol, opts:OptionsPattern[]] := Module[
  {p = OptionValue["IndexPosition"], kx, px, modes, modesp, heads, stat, boson,
   n, eta, h, hp, off, hc, E0, i, j},
  If[!momentumindexQ[k], Message[bdghamiltonian::index, k]; Return[$Failed]];
  kx = ssJWRnewname[k, Join[listfreeindexes, {k}], {H, orbs}];
  modes = snegbdgmodes[orbs, kx, p];
  If[modes === $Failed, Return[$Failed]];
  n = Length[modes]/2;
  heads = Union[Map[Head[#[CR]] &, modes]];
  stat = snegquadstat[modes[[1 ;; n]]];
  If[stat === $Failed, Message[bdghamiltonian::stat]; Return[$Failed]];
  boson = stat === "Boson";
  If[!snegevenQ[H, heads], Message[bdghamiltonian::linear]; Return[$Failed]];
  If[!boson && !And @@ Map[ordering[#] === EMPTY &, heads],
    Message[bdghamiltonian::ordering, heads]; Return[$Failed]];
  eta = snegquadeta[stat, n];
  h = Table[snegbdgelement[H, modes[[i]], modes[[j]], eta[[i]], eta[[j]], boson],
    {i, 2 n}, {j, 2 n}];
  If[!FreeQ[h, sum] || !FreeQ[h, _?operatorQ[___]],
    Message[bdghamiltonian::rest,
      First[Select[Flatten[h], !FreeQ[#, sum] || !FreeQ[#, _?operatorQ[___]] &]]];
    Return[$Failed]];
  If[TrueQ[OptionValue["Check"]],
    px = ssJWRnewname[k, Join[listfreeindexes, {k, kx}], {H, orbs}];
    modesp = snegbdgmodes[orbs, px, p];
    hp = Table[snegbdgelement[H, modes[[i]], modesp[[j]], eta[[i]], eta[[j]], boson],
      {i, 2 n}, {j, 2 n}];
    off = Simplify[hp /. KroneckerDelta[a_, b_] /;
      MemberQ[{kx - px, px - kx}, Expand[a - b]] -> 0];
    If[!MatchQ[Flatten[{off}], {0 ..}],
      Message[bdghamiltonian::transl, off /. {kx -> k}]; Return[$Failed]];
    If[!snegzeroQ[Flatten[h - ConjugateTranspose[h]]],
      Message[bdghamiltonian::herm]];
  ];
  (* The c-number part of the normal-ordered H plus the constant from
     writing the normal part as 1/2 Psi^dagger H(k) Psi. *)
  hc = Total[Select[sum2list[Expand[sumExpand[H]]], FreeQ[#, _?operatorQ[___]] &]];
  E0 = snegsumreduce[hc + If[boson, -1, 1]/2 sum[Tr[h[[1 ;; n, 1 ;; n]]], {kx}]];
  {h, E0} /. kx -> k
];

(* True if every operator product contains an even number of operators with
the given heads. *)
snegevenQ[H_, heads_List] := Module[{cnt},
  cnt[l_List] := Count[l, (h_Symbol)[CR | AN, ___] /; MemberQ[heads, h]];
  FreeQ[Last[Reap[H /. {HoldPattern[nc[x__]] :> (Sow[cnt[{x}]]; 1),
      o:(h_Symbol)[CR | AN, ___] /; MemberQ[heads, h] :> (Sow[cnt[{o}]]; 1)}]],
    n_Integer /; OddQ[n]]];

(* Bogoliubov transformation. In the default mode, M is the BdG matrix of
the modes Psi = (a, a^dagger) and must be particle-hole symmetric,
tau_x M^* tau_x = -M (fermions) or M (bosons); the quasiparticle columns x_m
are then complemented by C x_m = tau_x x_m^*. With "Momentum" -> True, M is
a Bloch BdG matrix H(k) for Psi_k = (c_k, c_-k^dagger), whose particle-hole
symmetry relates k and -k; all 2n columns are then eigenvectors of H(k)
(fermions) or of eta H(k) (bosons), and T^dagger H(k) T is diagonal with
the energies of gamma_k followed by -eps(-k) (fermions) or eps(-k)
(bosons). Internally the conjugate matrix T^* is returned as well; for
symbolic matrices it is formed assuming real diagonal elements and real
energies, which avoids unevaluated Conjugate[] of square roots. *)
bogoliubov::dim = "The BdG matrix must be a square matrix of even dimension.";
bogoliubov::posdef = "The bosonic BdG matrix is not positive definite (dynamical instability or zero modes).";
bogoliubov::phs = "The matrix does not have the particle-hole symmetry of the BdG matrix of the modes (a, a^dagger); for Bloch BdG matrices H(k) use \"Momentum\" -> True.";
bogoliubov::count = "The matrix does not have `1` eigenvalues of each sign.";
bogoliubov::symbolic = "Symbolic BdG matrices are supported only if they decompose into blocks of at most two modes; the block `1` is larger.";
Options[bogoliubov] = {"Tolerance" -> 10^-10, "Momentum" -> False};

bogoliubov[M_?MatrixQ, type:("Fermion" | "Boson"), opts:OptionsPattern[]] :=
  With[{r = snegbogoliubov[M, type, TrueQ[OptionValue["Momentum"]], OptionValue["Tolerance"]]},
    If[r === $Failed, $Failed, Most[r]]];

bogoliubov[H_, specs_List, opts:OptionsPattern[]] := Module[{s, q, r},
  s = snegquadsetup[specs];
  If[s === $Failed, Return[$Failed]];
  q = quadraticform[H, specs];
  If[q === $Failed, Return[$Failed]];
  r = bogoliubov[First[q], Last[s], "Tolerance" -> OptionValue["Tolerance"]];
  If[r === $Failed, Return[$Failed]];
  Append[r, Last[q] + If[Last[s] === "Boson", 1, -1] Total[First[r]]/2]
];

snegNumericMatrix[M_] := If[Precision[M] === Infinity, N[M], M];
snegtaux[A_, n_] := RotateLeft[A, {n, n}];

snegphsymmetricQ[M_, n_, boson_] := snegzeroQ[
  Flatten[snegtaux[Conjugate[M], n] + If[boson, -1, 1] M],
  If[MatrixQ[M, NumericQ], Max[1, Max[Abs[N[M]]]], 1]];

snegbogoliubov[M_, type_, mom_, tol_] := Module[{n, boson = type === "Boson", r},
  If[Length[M] != Length[First[M]] || OddQ[Length[M]],
    Message[bogoliubov::dim]; Return[$Failed]];
  n = Length[M]/2;
  If[!mom && !snegphsymmetricQ[M, n, boson], Message[bogoliubov::phs]; Return[$Failed]];
  If[MatrixQ[M, NumericQ],
    r = If[boson, snegbogoliubovboson[M, n, tol, mom], snegbogoliubovfermion[M, n, tol, mom]];
    If[r === $Failed, $Failed, Append[r, Conjugate[r[[2]]]]],
    snegbogoliubovsymbolic[M, n, boson, mom]]
];

(* Columns {X, C X} with C x = tau_x x^*. *)
snegphconj[x_, n_] := Join[Conjugate[x[[n + 1 ;; 2 n]]], Conjugate[x[[1 ;; n]]]];
snegbogoliubovT[cols_List, n_] := Transpose[Join[cols, Map[snegphconj[#, n] &, cols]]];

snegbogoliubovfermion[M0_, n_, tol0_, mom_] := Module[
  {M = snegNumericMatrix[M0], vals, vecs, tol, pos, neg, zero, z, w, cols, eps, ord, part, hole},
  M = (M + ConjugateTranspose[M])/2;
  {vals, vecs} = Eigensystem[M];
  vals = Re[vals];
  tol = tol0 Max[1, Max[Abs[vals]]];
  pos = Flatten[Position[vals, v_ /; v > tol]];
  neg = Flatten[Position[vals, v_ /; v < -tol]];
  zero = Flatten[Position[vals, v_ /; Abs[v] <= tol]];
  If[Length[pos] > n || Length[neg] > n, Message[bogoliubov::count, n]; Return[$Failed]];
  If[mom,
    (* zero modes are assigned to the particle columns first *)
    part = Join[pos, Take[zero, n - Length[pos]]];
    hole = Join[neg, Drop[zero, n - Length[pos]]];
    part = part[[Ordering[vals[[part]]]]];
    hole = hole[[Ordering[-vals[[hole]]]]];
    Return[{vals[[Join[part, hole]]], Transpose[vecs[[Join[part, hole]]]]}]];
  cols = vecs[[pos]];
  eps = vals[[pos]];
  If[zero =!= {},
    (* Zero modes: a basis invariant under C, combined into pairs. *)
    z = vecs[[zero]];
    w = Flatten[Map[{# + snegphconj[#, n], I (# - snegphconj[#, n])} &, z], 1];
    w = Select[Orthogonalize[w], Norm[#] > 1/2 &];
    If[Length[w] != Length[zero], Message[bogoliubov::phs]; Return[$Failed]];
    cols = Join[cols, Table[(w[[2 j - 1]] + I w[[2 j]])/Sqrt[2], {j, Length[zero]/2}]];
    eps = Join[eps, ConstantArray[0., Length[zero]/2]]];
  ord = Ordering[eps];
  {eps[[ord]], snegbogoliubovT[cols[[ord]], n]}
];

(* Colpa, Physica A 93, 327 (1978): M = K^dagger K, W = K eta K^dagger,
T = K^-1 U |L|^(1/2) with the eigenvectors U and eigenvalues L of W. *)
snegbogoliubovboson[M0_, n_, tol0_, mom_] := Module[
  {M = snegNumericMatrix[M0], eta, K, Ki, W, vals, vecs, pos, neg, eps, cols, ord},
  M = (M + ConjugateTranspose[M])/2;
  If[Min[Re[Eigenvalues[M]]] <= tol0 Max[1, Max[Abs[M]]],
    Message[bogoliubov::posdef]; Return[$Failed]];
  eta = DiagonalMatrix[snegquadeta["Boson", n]];
  K = CholeskyDecomposition[M];
  Ki = Inverse[K];
  W = K . eta . ConjugateTranspose[K];
  {vals, vecs} = Eigensystem[(W + ConjugateTranspose[W])/2];
  vals = Re[vals];
  pos = Flatten[Position[vals, v_ /; v > 0]];
  neg = Flatten[Position[vals, v_ /; v < 0]];
  If[Length[pos] != n || Length[neg] != n, Message[bogoliubov::posdef]; Return[$Failed]];
  pos = pos[[Ordering[vals[[pos]]]]];
  neg = neg[[Ordering[-vals[[neg]]]]];
  If[mom,
    ord = Join[pos, neg];
    Return[{Abs[vals[[ord]]], Ki . Transpose[vecs[[ord]]] . DiagonalMatrix[Sqrt[Abs[vals[[ord]]]]]}]];
  eps = vals[[pos]];
  cols = Transpose[Ki . Transpose[vecs[[pos]]] . DiagonalMatrix[Sqrt[eps]]];
  {eps, snegbogoliubovT[cols, n]}
];

(* Symbolic matrices: blocks of the graph of nonzero matrix elements, each
with at most two indexes. Every block gives its eigenvectors as columns
{kind, label, diagonal element of T^dagger M T, x, x^*}; particle-hole
blocks give a particle column ("+" eigenvector, positive norm for bosons)
and a hole column. *)
snegbogoliubovsymbolic[M_, n_, boson_, mom_] := Module[
  {edges, comps, cols = {}, c, a, b, d, s, dl, e, u, v, i, j, unit, big, part, hole, kind},
  edges = Select[Subsets[Range[2 n], {2}], !PossibleZeroQ[M[[#[[1]], #[[2]]]]] &];
  comps = ConnectedComponents[Graph[Range[2 n], UndirectedEdge @@@ edges]];
  comps = SortBy[Map[Sort, comps], First];
  unit[k_] := UnitVector[2 n, k];
  kind[k_] := If[k <= n, "p", "h"];
  big = Select[comps, Length[#] > 2 &];
  If[big =!= {}, Message[bogoliubov::symbolic, First[big]]; Return[$Failed]];
  Do[
    Which[
      Length[c] == 1,
        AppendTo[cols, {kind[c[[1]]], c[[1]], M[[c[[1]], c[[1]]]], unit[c[[1]]], unit[c[[1]]]}],
      kind[c[[1]]] === kind[c[[2]]],
        (* normal block: Hermitian 2 by 2 matrix *)
        {i, j} = c;
        {a, b, d} = {M[[i, i]], M[[j, j]], M[[i, j]]};
        s = (a + b)/2; dl = (a - b)/2; e = Sqrt[dl^2 + d Conjugate[d]];
        u = Sqrt[(e + dl)/(2 e)]; v = 1/Sqrt[2 e (e + dl)];
        AppendTo[cols, {kind[i], i, s + e, u unit[i] + v Conjugate[d] unit[j], u unit[i] + v d unit[j]}];
        AppendTo[cols, {kind[j], j, s - e, -v d unit[i] + u unit[j], -v Conjugate[d] unit[i] + u unit[j]}],
      True,
        (* particle-hole block *)
        {i, j} = c;
        {a, b, d} = {M[[i, i]], M[[j, j]], M[[i, j]]};
        If[boson,
          s = (a - b)/2; dl = (a + b)/2; e = Sqrt[dl^2 - d Conjugate[d]];
          u = Sqrt[(dl + e)/(2 e)]; v = -1/Sqrt[2 e (dl + e)];
          AppendTo[cols, {"p", i, s + e, u unit[i] + v Conjugate[d] unit[j], u unit[i] + v d unit[j]}];
          AppendTo[cols, {"h", j, e - s, v d unit[i] + u unit[j], v Conjugate[d] unit[i] + u unit[j]}],
          s = (a + b)/2; dl = (a - b)/2; e = Sqrt[dl^2 + d Conjugate[d]];
          u = Sqrt[(e + dl)/(2 e)]; v = 1/Sqrt[2 e (e + dl)];
          AppendTo[cols, {"p", i, s + e, u unit[i] + v Conjugate[d] unit[j], u unit[i] + v d unit[j]}];
          AppendTo[cols, {"h", j, s - e, -v d unit[i] + u unit[j], -v Conjugate[d] unit[i] + u unit[j]}]]
    ],
    {c, comps}];
  part = SortBy[Select[cols, First[#] === "p" &], #[[2]] &];
  hole = SortBy[Select[cols, First[#] === "h" &], #[[2]] &];
  If[mom,
    {Join[part[[All, 3]], hole[[All, 3]]],
     Transpose[Join[part[[All, 4]], hole[[All, 4]]]],
     Transpose[Join[part[[All, 5]], hole[[All, 5]]]]},
    {part[[All, 3]],
     Transpose[Join[part[[All, 4]], Map[RotateLeft[#, n] &, part[[All, 5]]]]],
     Transpose[Join[part[[All, 5]], Map[RotateLeft[#, n] &, part[[All, 4]]]]]}]
];

bogoliubovrules::qp = "The quasiparticle operators `1` do not give `2` modes.";
bogoliubovrules::mode = "The modes must be single operators to define replacement rules.";
bogoliubovrules[T_?MatrixQ, specs_List, qp_] := Module[{s, modes, stat, n, gm},
  s = snegquadsetup[specs];
  If[s === $Failed, Return[$Failed]];
  {modes, stat} = s;
  n = Length[modes];
  If[Dimensions[T] =!= {2 n, 2 n}, Message[quadraticoperator::dim, Dimensions[T], n];
    Return[$Failed]];
  gm = Which[
    ListQ[qp], snegquadmodes[qp],
    TrueQ[bosonQ[qp]], Table[With[{m = m}, Function[t, qp[t, m]]], {m, n}],
    TrueQ[fermionQ[qp]] && spinof[qp] == 0, Table[With[{m = m}, Function[t, qp[t, m, 0]]], {m, n}],
    True, $Failed];
  If[gm === $Failed || Length[gm] != n, Message[bogoliubovrules::qp, qp, n]; Return[$Failed]];
  If[!And @@ Map[MatchQ[#[AN], (h_Symbol)[AN, ___] /; TrueQ[operatorQ[h]]] &, modes],
    Message[bogoliubovrules::mode]; Return[$Failed]];
  Join[
    Table[modes[[i]][AN] -> Sum[T[[i, m]] gm[[m]][AN] + T[[i, n + m]] gm[[m]][CR], {m, n}], {i, n}],
    Table[modes[[i]][CR] -> Sum[T[[n + i, m]] gm[[m]][AN] + T[[n + i, n + m]] gm[[m]][CR], {m, n}], {i, n}]]
];

(* Correlation matrix G_pq = <Psi_p Psi^dagger_q> at temperature temp. For
fermions with numeric M, G = (1 + exp(-M/T))^-1 directly (zero modes are
half occupied at T = 0). Otherwise G = T D T^dagger with the occupations of
the columns of T; at T = 0 symbolic quasiparticle energies of particle
columns are assumed positive. *)
snegoccupation[stat_, eps_, temp_] := Which[
  TrueQ[temp == 0], 0,
  stat === "Boson", 1/(Exp[eps/temp] - 1),
  True, 1/(Exp[eps/temp] + 1)];

snegcorrelation[M_, stat_, temp_, mom_:False, tol_:10^-10] := Module[
  {n = Length[M]/2, vals, vecs, Mn, g, b, occ, d},
  If[stat === "Fermion" && MatrixQ[M, NumericQ],
    Mn = snegNumericMatrix[M];
    {vals, vecs} = Eigensystem[(Mn + ConjugateTranspose[Mn])/2];
    vals = Re[vals];
    g = Map[Which[
        TrueQ[temp == 0], Which[# > tol, 1, # < -tol, 0, True, 1/2],
        True, 1/(1 + Exp[-#/temp])] &, vals];
    Return[Transpose[vecs] . DiagonalMatrix[g] . Conjugate[vecs]]];
  b = snegbogoliubov[M, stat, mom, tol];
  If[b === $Failed, Return[$Failed]];
  occ = Map[snegoccupation[stat, #, temp] &, First[b]];
  d = Which[
    !mom, Join[If[stat === "Boson", 1 + occ, 1 - occ], occ],
    stat === "Boson", Join[1 + occ[[1 ;; n]], occ[[n + 1 ;; 2 n]]],
    TrueQ[temp == 0], Join[ConstantArray[1, n], ConstantArray[0, n]],
    True, 1 - occ];
  b[[2]] . DiagonalMatrix[d] . Transpose[b[[3]]]
];

(* Index map {p, q}: the operator is Psi_p and Psi^dagger_q (None if it is
not a component). For the modes (a, a^dagger) both exist; for
Psi_k = (c_k, c_-k^dagger) each operator is either a component of Psi_k or
of Psi_k^dagger. *)
snegquadindex[psi_List, psid_List] := Module[{idx = Association[]},
  Do[idx[psi[[p]]] = {p, Lookup[idx, Key[psi[[p]]], {None, None}][[2]]}, {p, Length[psi]}];
  Do[idx[psid[[q]]] = {Lookup[idx, Key[psid[[q]]], {None, None}][[1]], q}, {q, Length[psid]}];
  idx];
snegquadindex[modes_List] := snegquadindex[
  Join[Map[#[AN] &, modes], Map[#[CR] &, modes]],
  Join[Map[#[CR] &, modes], Map[#[AN] &, modes]]];

(* Contraction <o1 o2> from G: <Psi_p Psi^dagger_q> = G_pq and
<Psi^dagger_q Psi_p> = delta_pq - G_pq (fermions) or G_pq - eta_p delta_pq
(bosons); other pairs vanish. *)
snegcontraction[G_, idx_, eta_, boson_][o1_, o2_] := Module[
  {i1 = idx[o1], i2 = idx[o2]},
  Which[
    i1[[1]] =!= None && i2[[2]] =!= None, G[[i1[[1]], i2[[2]]]],
    i1[[2]] =!= None && i2[[1]] =!= None,
      If[boson, G[[i2[[1]], i1[[2]]]] - If[i1[[2]] == i2[[1]], eta[[i2[[1]]]], 0],
        If[i1[[2]] == i2[[1]], 1, 0] - G[[i2[[1]], i1[[2]]]]],
    True, 0]];

(* Coefficient and operator list of a single term. *)
snegtermops[t_] := Replace[t, {
  c_. HoldPattern[nc[x__]] /; isnumericQ[c] :> {c, {x}},
  c_. o:(h_Symbol[___]) /; TrueQ[operatorQ[h]] && isnumericQ[c] :> {c, {o}},
  c_ /; isnumericQ[c] :> {c, {}},
  _ :> $Failed}];

snegparity[o_] := If[TrueQ[fermionQ[o]] || TrueQ[majoranaQ[o]], 1, 0];

(* Wick's theorem with the contraction function con[o1, o2]. *)
snegwick[{}, con_] := 1;
snegwick[l_List, con_] /; OddQ[Length[l]] := 0;
snegwick[l_List, con_] := Sum[
  With[{c = con[l[[1]], l[[j]]]},
    If[c === 0, 0,
      (-1)^(snegparity[l[[j]]] Total[Map[snegparity, l[[2 ;; j - 1]]]]) c *
        snegwick[Delete[l, {{1}, {j}}], con]]],
  {j, 2, Length[l]}];

(* Average of an expression from the parsed terms. *)
snegwickeval[e_, idx_, con_] := Module[{terms = Map[snegtermops, sum2list[Expand[e]]], miss},
  If[MemberQ[terms, $Failed], Message[gaussianaverage::op, e]; Return[$Failed]];
  miss = Select[Union[Flatten[terms[[All, 2]]]], !KeyExistsQ[idx, #] &];
  If[miss =!= {}, Message[gaussianaverage::op, First[miss]]; Return[$Failed]];
  Total[Map[#[[1]] snegwick[#[[2]], con] &, terms]]];

gaussianaverage::op = "The operator `1` is not one of the modes.";
gaussianaverage::sum = "Symbolic sums are not supported.";
gaussianaverage::index = "`1` must be a declared momentum index.";
Options[gaussianaverage] = {"Momentum" -> None, "IndexPosition" -> 1, "Tolerance" -> 10^-10};
gaussianaverage[expr_, Mx_, specs_List, temp_, opts:OptionsPattern[]] := Module[
  {k = OptionValue["Momentum"], s, modes, stat, n, M, G, idx},
  If[k === None,
    s = snegquadsetup[specs];
    If[s === $Failed, Return[$Failed]];
    {modes, stat} = s;
    n = Length[modes];
    M = If[MatrixQ[Mx], Mx,
      With[{q = quadraticform[Mx, specs]}, If[q === $Failed, Return[$Failed], First[q]]]];
    idx = snegquadindex[modes],
    (* Bloch BdG matrix H(k) *)
    If[!momentumindexQ[k], Message[gaussianaverage::index, k]; Return[$Failed]];
    modes = snegbdgmodes[specs, k, OptionValue["IndexPosition"]];
    If[modes === $Failed, Return[$Failed]];
    n = Length[modes]/2;
    stat = snegquadstat[modes[[1 ;; n]]];
    If[stat === $Failed, Message[quadraticform::stat]; Return[$Failed]];
    M = If[MatrixQ[Mx], Mx,
      With[{q = bdghamiltonian[Mx, specs, k, "IndexPosition" -> OptionValue["IndexPosition"]]},
        If[q === $Failed, Return[$Failed], First[q]]]];
    idx = snegquadindex[Map[#[AN] &, modes], Map[#[CR] &, modes]]];
  If[!FreeQ[expr, sum], Message[gaussianaverage::sum]; Return[$Failed]];
  If[Length[M] != 2 n, Message[quadraticoperator::dim, Dimensions[M], n]; Return[$Failed]];
  G = snegcorrelation[M, stat, temp, k =!= None, OptionValue["Tolerance"]];
  If[G === $Failed, Return[$Failed]];
  With[{con = snegcontraction[G, idx, snegquadeta[stat, n], stat === "Boson"]},
    If[ListQ[expr], Map[snegwickeval[#, idx, con] &, expr], snegwickeval[expr, idx, con]]]
];

(* Mean-field decoupling: a product of 2m operators is replaced by
Sum_(i<j) s_ij W(rest) o_i o_j - (m - 1) W(all), where W is the Wick
expansion with the allowed contractions <o_a o_b> = avg[nc[o_a, o_b]] and
s_ij the fermionic sign of moving o_i o_j to the front. The average of the
result in a Gaussian state equals the Wick average of the product. *)
meanfield::channel = "Unknown channel `1`; use \"Hartree\", \"Fock\", \"Pairing\", \"Normal\", \"All\", a list of these, or a function.";
meanfield::odd = "Terms with an odd number of operators are not decoupled: `1`.";
meanfield::op = "Terms with operators other than fermions and bosons are not decoupled: `1`.";
meanfield::term = "The term `1` is not a product of operators and is not decoupled.";

snegmfladder[o_] := MatchQ[o, (h_Symbol)[CR | AN, ___] /;
  TrueQ[fermionQ[h]] || TrueQ[bosonQ[h]]];

snegmfchannel["Hartree"] := Function[{o1, o2}, o1[[1]] =!= o2[[1]] && Head[o1] === Head[o2] &&
  Rest[o1] === Rest[o2]];
snegmfchannel["Fock"] := Function[{o1, o2}, o1[[1]] =!= o2[[1]] &&
  !(Head[o1] === Head[o2] && Rest[o1] === Rest[o2])];
snegmfchannel["Pairing"] := Function[{o1, o2}, o1[[1]] === o2[[1]]];
snegmfchannel["Normal"] := Function[{o1, o2}, o1[[1]] =!= o2[[1]]];
snegmfchannel["All"] := Function[{o1, o2}, True];
snegmfchannel[l_List] := With[{fs = Map[snegmfchannel, l]},
  If[MemberQ[fs, $Failed], $Failed, Function[{o1, o2}, Or @@ Map[#[o1, o2] &, fs]]]];
snegmfchannel[f_Function | f_Symbol] /; !StringQ[f] := f;
snegmfchannel[x_] := (Message[meanfield::channel, x]; $Failed);

meanfield[H_, ch_:"All"] := Module[{allowed, con, term},
  allowed = snegmfchannel[ch];
  If[allowed === $Failed, Return[$Failed]];
  con[o1_, o2_] := If[snegparity[o1] == snegparity[o2] &&
      (TrueQ[fermionQ[o1]] === TrueQ[fermionQ[o2]]) && TrueQ[allowed[o1, o2]],
    avg[nc[o1, o2]], 0];
  term[t_] := Module[{c, ops, m, l},
    l = snegtermops[t];
    If[l === $Failed, Message[meanfield::term, t]; Return[t, Module]];
    {c, ops} = l;
    Which[
      Length[ops] <= 2, t,
      !And @@ Map[snegmfladder, ops], Message[meanfield::op, t]; t,
      OddQ[Length[ops]], Message[meanfield::odd, t]; t,
      True,
        m = Length[ops]/2;
        c (Sum[
          (-1)^(snegparity[ops[[i]]] Total[Map[snegparity, ops[[1 ;; i - 1]]]] +
                snegparity[ops[[j]]] Total[Map[snegparity, Delete[ops[[1 ;; j - 1]], i]]]) *
            snegwick[Delete[ops, {{i}, {j}}], con] nc[ops[[i]], ops[[j]]],
          {i, Length[ops]}, {j, i + 1, Length[ops]}] -
          (m - 1) snegwick[ops, con])]];
  Expand[snegmfmap[Expand[H], term]]
];
snegmfmap[sum[a_, it_List], f_] := sum[snegmfmap[Expand[a], f], it];
snegmfmap[c_ s_sum, f_] /; isnumericQ[c] := c snegmfmap[s, f];
snegmfmap[a_Plus, f_] := Map[snegmfmap[#, f] &, a];
snegmfmap[a_, f_] := f[a];

(* Self-consistent solution on a finite set of modes. *)
meanfieldsolve::numeric = "The mean-field Hamiltonian contains symbols other than the averages: `1`.";
meanfieldsolve::conv = "No convergence after `1` iterations; the last change was `2`.";
meanfieldsolve::number = "The particle number `1` must lie between 0 and the number of fermion modes `2`.";
meanfieldsolve::boson = "A fixed particle number is supported only for fermions.";
meanfieldsolve::open = "The level at the Fermi energy is degenerate and partially filled; its states are occupied equally (ensemble average).";
meanfieldsolve::init = "The initial values `1` do not refer to averages in the Hamiltonian.";
Options[meanfieldsolve] = {"Temperature" -> 0, "Mixing" -> 1/2, "Tolerance" -> 10^-10,
  "MaxIterations" -> 1000, "ParticleNumber" -> None};

(* Initial values: rules c avg[x] -> v become avg[x] -> v/c, and the
conjugate average gets the conjugate value unless it is given. *)
snegmfinit[init_List] := Module[{r, c},
  r = Map[Replace[#, {(c0_. a_avg -> v_) /; NumericQ[c0] :> (a -> v/c0), x_ :> x}] &, init];
  c = Map[Function[rule, Replace[Conjugate[First[rule]],
      {(c0_. a_avg) /; NumericQ[c0] :> (a -> Conjugate[Last[rule]]/c0), _ :> Nothing}]],
    Select[r, MatchQ[First[#], _avg] &]];
  Join[r, Select[c, !MemberQ[Map[First, r], First[#]] &]]];

meanfieldsolve[Hmf_, specs_List, init_List:{}, opts:OptionsPattern[]] := Module[
  {s, modes, stat, n, avgs, q, M, E0, rest, idx, parsed, vals, newvals, temp,
   alpha, tol, maxit, nn, nmat, it = 0, diff = Infinity, G, mu = None, Mn, eval,
   corr, energy, eps, extra, init0},
  s = snegquadsetup[specs];
  If[s === $Failed, Return[$Failed]];
  {modes, stat} = s;
  n = Length[modes];
  {temp, alpha, tol, maxit, nn} = OptionValue[{"Temperature", "Mixing", "Tolerance",
    "MaxIterations", "ParticleNumber"}];
  If[nn =!= None && stat === "Boson", Message[meanfieldsolve::boson]; Return[$Failed]];
  If[nn =!= None && !(0 <= nn <= n), Message[meanfieldsolve::number, nn, n]; Return[$Failed]];
  q = quadraticform[Hmf, specs];
  If[q === $Failed, Return[$Failed]];
  {M, E0} = q;
  avgs = Union[Cases[{M, E0}, _avg, Infinity]];
  rest = Select[snegsymbolsof[{M, E0} /. _avg -> 1], !NumericQ[#] &];
  If[rest =!= {}, Message[meanfieldsolve::numeric, rest]; Return[$Failed]];
  init0 = snegmfinit[init];
  extra = Complement[Map[First, init0], avgs];
  If[extra =!= {}, Message[meanfieldsolve::init, extra]];
  idx = snegquadindex[modes];
  parsed = Map[Map[snegtermops, sum2list[Expand[First[#]]]] &, avgs];
  If[MemberQ[parsed, $Failed, {2}] ||
     !SubsetQ[Keys[idx], Union[Flatten[Map[#[[2]] &, parsed, {2}]]]],
    Message[gaussianaverage::op, First[avgs]]; Return[$Failed]];
  eval[g_] := With[{cf = snegcontraction[g, idx, snegquadeta[stat, n], stat === "Boson"]},
    Map[Total[Map[Function[t, t[[1]] snegwick[t[[2]], cf]], #]] &, parsed]];
  vals = N[avgs /. Join[Select[init0, MemberQ[avgs, First[#]] &], Thread[avgs -> 0]]];
  nmat = DiagonalMatrix[Join[ConstantArray[1, n], ConstantArray[-1, n]]];
  corr[m_] := If[nn === None, {snegcorrelation[m, stat, temp], None},
    snegfixednumber[m, n, nn, temp, nmat]];
  While[diff > tol && it < maxit,
    Mn = N[M /. Thread[avgs -> vals]];
    {G, mu} = corr[Mn];
    If[G === $Failed, Return[$Failed, Module]];
    newvals = If[avgs === {}, {}, eval[G]];
    diff = If[avgs === {}, 0, Max[Abs[newvals - vals]]];
    vals = vals + alpha (newvals - vals);
    it++];
  If[diff > tol, Message[meanfieldsolve::conv, it, diff]];
  Mn = N[M /. Thread[avgs -> vals]];
  {G, mu} = corr[Mn];
  energy = N[E0 /. Thread[avgs -> vals]] + If[stat === "Boson",
    (Tr[Mn . G] - Tr[Mn . nmat])/2, (Tr[Mn] - Tr[Mn . G])/2];
  eps = If[diff <= tol, bogoliubov[If[mu === None, Mn, Mn - mu nmat], stat], $Failed];
  Association[
    "Averages" -> Thread[avgs -> vals],
    "Energy" -> Chop[energy],
    "QuasiparticleEnergies" -> If[eps === $Failed, $Failed, First[eps]],
    "ChemicalPotential" -> mu,
    "Iterations" -> it,
    "Converged" -> diff <= tol]
];

(* Correlation matrix at fixed fermion number: chemical potential by
bisection, or by filling the lowest levels for number-conserving
Hamiltonians at T = 0. *)
snegfixednumber[M_, n_, nn_, temp_, nmat_] := Module[
  {h, vals, vecs, ord, occ, shell, ef, nb, lo, hi, mid, num, gmat, k, homo, lumo},
  If[TrueQ[temp == 0] && Max[Abs[M[[1 ;; n, n + 1 ;; 2 n]]]] == 0,
    h = M[[1 ;; n, 1 ;; n]];
    {vals, vecs} = Eigensystem[(h + ConjugateTranspose[h])/2];
    vals = Re[vals];
    ord = Ordering[vals]; vals = vals[[ord]]; vecs = vecs[[ord]];
    occ = ConstantArray[0, n];
    If[nn > 0,
      ef = vals[[nn]];
      shell = Flatten[Position[vals, v_ /; Abs[v - ef] <= 10^-10 Max[1, Abs[ef]]]];
      nb = Count[vals, v_ /; v < ef - 10^-10 Max[1, Abs[ef]]];
      occ[[1 ;; nb]] = 1;
      occ[[shell]] = (nn - nb)/Length[shell];
      If[nn - nb != Length[shell], Message[meanfieldsolve::open]]];
    homo = If[nn > 0, vals[[nn]], vals[[1]] - 1];
    lumo = If[nn < n, vals[[nn + 1]], vals[[n]] + 1];
    (* <a_i^dagger a_j> = Sum_nu occ_nu conj(V_i nu) V_j nu *)
    num = Transpose[Conjugate[vecs]] . DiagonalMatrix[occ] . vecs;
    gmat = ArrayFlatten[{{IdentityMatrix[n] - Transpose[num], ConstantArray[0, {n, n}]},
      {ConstantArray[0, {n, n}], num}}];
    Return[{gmat, (homo + lumo)/2}]];
  k[mu_] := Re[Tr[snegcorrelation[M - mu nmat, "Fermion", temp][[n + 1 ;; 2 n, n + 1 ;; 2 n]]]];
  hi = Max[Abs[Eigenvalues[M]]] + 1 + 50 If[TrueQ[temp == 0], 0, temp];
  lo = -hi;
  Do[mid = (lo + hi)/2; If[k[mid] < nn, lo = mid, hi = mid], {100}];
  mid = (lo + hi)/2;
  {snegcorrelation[M - mid nmat, "Fermion", temp], mid}
];

(**** Schrieffer-Wolff transformation ****)

(* The Hamiltonian H = H0 + V is block-diagonalized with respect to a
projector P (Q = 1 - P) by H_eff = e^S H e^-S, with the anti-Hermitian,
block-off-diagonal canonical generator S = S_1 + S_2 + ... (S_n of order
V^n). Order by order, R_n denotes the part of order n of
e^S' H e^-S' with S' = S_1 + ... + S_(n-1); then [H0, S_n] = O(R_n) and
H_eff^(n) = P R_n P, where O(X) = P X Q + Q X P. The commutators [S_m, H0]
in R_n are replaced by -O(R_m).

H0 must be diagonal in the occupation numbers of the "local" modes (fermion
modes and X-operator sites that appear in number-operator polynomials of
H0, or in P) and its remaining part must have every other operator as an
eigenoperator ([H0, o] = lambda o: band energies, linear bosons, Zeeman
terms). For a normal-ordered monomial M, f(n) M = M f(n + Delta) with the
occupations n of the local modes to the right of M; the modes contained in
M have fixed initial and final occupations, so [H0, M] = M g(n) with a
polynomial g in the occupations of the other ("spectator") modes, and M is
split into eigenoperators with projectors onto the values of the
spectators. Likewise P M = M P(final) and M P = M P(initial). Occupations
are represented by the commuting indicators snegswind[mode, value]. *)

eigenoperators::diag = "H0 is not diagonal in the occupation numbers: [H0, `1`] is not proportional to `1`.";
eigenoperators::term = "The term `1` is not a product of operators.";
eigenoperators::sum = "Expressions with symbolic sums are not supported; decompose the summand.";
eigenoperators::ordering = "The local fermion operators `1` must have the default ordering (EMPTY).";
schriefferwolff::proj = "The projector `1` must be a polynomial in fermion number operators or diagonal X-operators with P^2 = P.";
schriefferwolff::res = "Resonance: the block-off-diagonal component `1` has a vanishing energy denominator, so P and Q are degenerate.";
schriefferwolff::rule = "Projector rules {c, f} (lattice sums) are supported up to second order.";
schriefferwolff::lattice = "With a projector rule, the operators of H0 and V must be in sums over one declared lattice index with sites R + n: `1`.";
schriefferwolff::parity = "With a projector rule, the terms of V must contain an even number of fermion operators: `1`.";

(* Indicator polynomials: n^2 = n, and an X site is in one state at a time. *)
snegswreduce[p_] := FixedPoint[Expand[# //. {
    snegswind[v_, a_]^k_Integer /; k > 1 :> snegswind[v, a],
    snegswind[v_, a_] snegswind[v_, b_] /; a =!= b :> 0}] &, Expand[p], 10];

snegswfkey[op_[t_, j___]] := {op, {j}};
snegswxkey[op_[j___, a_, b_]] := {op, {j}};
snegswxop[o_] := TrueQ[xoperatorQ[Head[o]]];

(* Polynomial of a number-type term {c, ops}: products of fermion number
operators and diagonal X-operators, or $Failed. *)
snegswpolyterm[{c_, ops_List}] := Module[{f, x, cr, an, can, sgn},
  f = Select[ops, TrueQ[fermionQ[#]] &];
  x = Select[ops, snegswxop];
  If[Length[f] + Length[x] != Length[ops], Return[$Failed]];
  If[!And @@ Map[#[[-1]] === #[[-2]] &, x], Return[$Failed]];
  cr = Map[snegswfkey, Select[f, First[#] === CR &]];
  an = Map[snegswfkey, Select[f, First[#] === AN &]];
  If[Length[cr] + Length[an] != Length[f] || Sort[cr] =!= Sort[an] || !DuplicateFreeQ[cr],
    Return[$Failed]];
  sgn = If[f === {}, 1,
    can = Expand[snegswnc[Flatten[Map[{#[[1]][CR, Sequence @@ #[[2]]], #[[1]][AN, Sequence @@ #[[2]]]} &, cr]]]];
    Expand[can/(nc @@ f)]];
  If[!MemberQ[{1, -1}, sgn], Return[$Failed]];
  c sgn Times @@ Map[snegswind[#, 1] &, cr] Times @@ Map[snegswind[snegswxkey[#], #[[-1]]] &, x]];
snegswpolyterm[_] := $Failed;

snegswpoly[expr_] := Module[{p = Map[snegswpolyterm[snegtermops[#]] &, sum2list[Expand[expr]]]},
  If[MemberQ[p, $Failed] || !FreeQ[expr, sum], $Failed, snegswreduce[Total[p]]]];

(* Product of operators that do not need contractions (no creation and
annihilation operator of the same mode): for fermions with the default
ordering, the canonical order is the sort by (CR/AN, operator), with the sign
of the permutation; otherwise nc[] is used. *)
snegswnc[l_List] := If[l =!= {} && And @@ Map[TrueQ[fermionQ[#]] && ordering[Head[#]] === EMPTY &, l],
  With[{perm = Ordering[Map[{First[#], #} &, l]]},
    If[!DuplicateFreeQ[l], 0, Signature[perm] nc @@ l[[perm]]]],
  nc @@ l];

snegswkeyop[snegswind[{op_, {j___}}, 1]] /; TrueQ[fermionQ[op]] := nc[op[CR, j], op[AN, j]];
snegswkeyop[snegswind[{op_, {j___}}, a_]] /; TrueQ[xoperatorQ[op]] := op[j, a, a];

(* c M poly as an operator expression *)
snegswop[c_, ops_List, poly_] := Module[{p = snegswreduce[poly]},
  If[p === 0, 0,
    Expand[Total[Map[Function[t, Module[{ind = Cases[{t}, _snegswind, Infinity]},
      c (t /. _snegswind -> 1) snegswnc[Join[ops, Flatten[Map[snegswkeyop, ind] /. nc -> List]]]]],
      sum2list[p]]]]]];

(* Values of a spectator: {value, projector}. *)
snegswvalues[k:{op_, _}] := If[TrueQ[fermionQ[op]],
  {{1, snegswind[k, 1]}, {0, 1 - snegswind[k, 1]}},
  Map[{#, snegswind[k, #]} &, xstates[op]]];

snegswsetrules[k:{op_, _}, v_] := If[TrueQ[fermionQ[op]],
  {snegswind[k, 1] -> v}, {snegswind[k, a_] :> Boole[a === v]}];

snegswfail[] := Throw[$Failed, snegsw];

(* Context: an Association with the local polynomial of H0 ("f", a function
of the fixed modes), the projector ("P", likewise), the local-mode test, and
the energies of the other operators. *)
snegswcontext[H0_, P_] := Module[{loc = 0, lin = 0, p, pp, keys,
    localQ, rule, R0, tmpl = 0, ppoly, sites, memo},
  If[MatchQ[P, {_Symbol, _}],
    (* projector rule on a lattice *)
    rule = P;
    R0 = snegswlatticeindex[H0];
    Scan[Function[t,
      If[MatchQ[t, (c_. s_sum) /; isnumericQ[c]],
        With[{cf = t /. _sum -> 1, a = snegswrename[First[Cases[t, _sum, {0, 1}]], R0]},
          Scan[Function[u, Module[{l = snegtermops[u], hs},
            p = If[l === $Failed, $Failed, snegswpolyterm[l]];
            hs = If[p === $Failed, {}, Union[Cases[{p}, snegswind[{h_, _}, _] :> h, Infinity]]];
            Which[
              hs === {}, lin += cf sum[u, {R0}],
              hs === {First[rule]}, tmpl += cf p,
              True, Message[schriefferwolff::lattice, u]; snegswfail[]]]],
            sum2list[Expand[a]]]],
        If[!FreeQ[t, First[rule]], Message[schriefferwolff::lattice, t]; snegswfail[]];
        lin += t]],
      sum2list[Expand[H0]]];
    tmpl = snegswreduce[tmpl];
    localQ[o_] := TrueQ[fermionQ[o]] && Head[o] === First[rule];
    ppoly[s_] := ppoly[s] = With[{q = snegswpoly[Last[rule][First[rule][s]]]},
      If[q === $Failed || snegswreduce[q^2 - q] =!= 0,
        Message[schriefferwolff::proj, Last[rule][First[rule][s]]]; snegswfail[], q]];
    sites[keys_] := Union[Map[First[#[[2]]] &, keys]];
    Association[
      "f" -> Function[ks, Module[{off, toff, shifts},
        off = Map[snegswoffset[#, R0] &, sites[ks]];
        toff = Union[Map[snegswoffset[First[#[[2]]], R0] &, Cases[tmpl, snegswind[k_, _] :> k, Infinity]]];
        shifts = Union[Flatten[Outer[Subtract, off, toff]]];
        Total[Map[tmpl /. R0 -> R0 + # &, shifts]]]],
      "P" -> Function[{ks, ss}, Times @@ Map[ppoly, Union[sites[ks], ss]]],
      "localQ" -> localQ, "lin" -> lin, "lattice" -> R0, "memo" -> memo],
    (* explicit modes *)
    Scan[Function[t,
      If[FreeQ[t, sum] && (p = With[{l = snegtermops[t]}, If[l === $Failed, $Failed, snegswpolyterm[l]]]) =!= $Failed,
        loc += p, lin += t]], sum2list[Expand[H0]]];
    loc = snegswreduce[loc];
    pp = If[P === None, 1, snegswpoly[P]];
    If[pp === $Failed || snegswreduce[pp^2 - pp] =!= 0, Message[schriefferwolff::proj, P]; snegswfail[]];
    keys = Union[Cases[{loc, pp}, snegswind[k_, _] :> k, Infinity]];
    localQ[o_] := MemberQ[keys, If[snegswxop[o], snegswxkey[o], snegswfkey[o]]] &&
      (TrueQ[fermionQ[o]] || snegswxop[o]);
    Association["f" -> Function[ks, loc], "P" -> Function[{ks, ss}, pp], "localQ" -> localQ,
      "lin" -> lin, "lattice" -> None, "memo" -> memo]
  ]
];

(* Energy of an operator that is not local: [H0, o] = lambda o. Local
operators must commute with the remaining part of H0. *)
snegswenergy[ctx_, o_] := With[{mem = ctx["memo"]},
  If[!MatchQ[mem[o], _mem], mem[o], mem[o] = Module[{lin, k, l},
    (* summation indexes of H0 that occur in o are renamed *)
    lin = ctx["lin"] /. s:HoldPattern[sum[a_, it_List]] :> Module[{cap = Select[it, !FreeQ[o, #] &], rep},
      rep = Map[# -> ssJWRnewname[#, Join[listfreeindexes, it], {ctx["lin"], o}] &, cap];
      sum[a /. rep, it /. rep]];
    k = snegswsimp[komutator[lin, o]];
    If[k === 0, 0,
      l = Map[snegtermops, sum2list[k]];
      If[!MatchQ[l, {{_, {o}} ..}] || !FreeQ[k, sum], Message[eigenoperators::diag, o]; snegswfail[]];
      Total[l[[All, 1]]]]]]];

(* Internal representation: a term {it, ops, poly} stands for
sum[nc[ops] poly, it], where ops is a normal-ordered product of operators in
which every local fermion mode appears at most once and no diagonal
X-operator appears ("bare" monomial), and poly is a commuting polynomial of
coefficients and occupation indicators of local modes not contained in ops.
Products only normal-order the short bare monomials, and projectors stay
polynomials. In lattice mode the terms are summands at the lattice index R0
(it = {}), and the sum over R0 is implicit. *)

(* Occupations of the local modes of a string before (init) and after (fin)
it acts; a mode that is annihilated is occupied initially, a mode that is
created is occupied finally (normal order). *)
snegswocc[ctx_, ops_List] := Module[{loc, fk, xk, ia = <||>, fa = <||>},
  loc = Select[ops, ctx["localQ"]];
  fk = Select[loc, TrueQ[fermionQ[#]] &];
  xk = Select[loc, snegswxop];
  Scan[Function[k,
    ia[k] = Boole[MemberQ[fk, x_ /; snegswfkey[x] === k && First[x] === AN]];
    fa[k] = Boole[MemberQ[fk, x_ /; snegswfkey[x] === k && First[x] === CR]]],
    Union[Map[snegswfkey, fk]]];
  Scan[Function[x, ia[snegswxkey[x]] = x[[-1]]; fa[snegswxkey[x]] = x[[-2]]], xk];
  {ia, fa, loc}];
snegswrules[a_Association] := Flatten[KeyValueMap[snegswsetrules, a]];

(* Bare term from a coefficient and a normal-ordered string. *)
snegswnormalize[ctx_, it_List, c_, ops_List] := Module[{ia, fa, loc, both, xd, bare, nops, sgn, p},
  {ia, fa, loc} = snegswocc[ctx, ops];
  both = Select[Keys[ia], ia[#] === 1 && fa[#] === 1 && TrueQ[fermionQ[First[#]]] &];
  xd = Select[loc, snegswxop[#] && #[[-1]] === #[[-2]] &];
  bare = Select[ops, !(MemberQ[xd, #] || (TrueQ[fermionQ[#]] && ctx["localQ"][#] &&
    MemberQ[both, snegswfkey[#]])) &];
  sgn = If[both === {}, 1,
    nops = Map[nc[#[[1]][CR, Sequence @@ #[[2]]], #[[1]][AN, Sequence @@ #[[2]]]] &, both];
    Expand[snegswKD[Expand[snegswnc[Join[bare, Flatten[List @@@ nops]]]]]]/(nc @@ ops)];
  If[!MemberQ[{1, -1}, sgn], Message[eigenoperators::term, nc @@ ops]; snegswfail[]];
  p = snegswreduce[sgn (c /. snegswrules[ia]) Times @@ Map[snegswind[#, 1] &, both] *
    Times @@ Map[snegswind[snegswxkey[#], #[[-1]]] &, xd]];
  {it, bare, p}];

(* Terms of an expression with sums. *)
snegnonopQ[snegswnum] ^= True;
snegswmask[e_] := e /. s:HoldPattern[sum[a_, _List]] /; isnumericQ[a] :>
  With[{h = Hash[s]}, snegswnumtab[h] = s; snegswnum[h]];
snegswunmask[e_] := e /. snegswnum[h_] :> snegswnumtab[h];

snegswterms[ctx_, e_] := snegswcombine[ctx, Flatten[Map[snegswterms1[ctx, {}, #] &,
  sum2list[Expand[snegswmask[e]]]], 1]];
snegswterms1[ctx_, it_List, t_] := Which[
  MatchQ[t, (c_. _sum) /; isnumericQ[c]],
    With[{s = First[Cases[t, _sum, {0, 1}]]},
      Flatten[Map[snegswterms1[ctx, Join[it, s[[2]]], (t /. _sum -> 1) #] &,
        sum2list[Expand[s[[1]]]]], 1]],
  True, With[{l = snegtermops[t]},
    If[l === $Failed, Message[eigenoperators::term, t]; snegswfail[]];
    {snegswnormalize[ctx, it, l[[1]], l[[2]]]}]];

(* Terms as an expression *)
snegswexpr[ctx_, terms_List] := Module[{e},
  e = Total[Map[Function[t, With[{x = snegswop[1, t[[2]], t[[3]]]},
    If[t[[1]] === {}, x, sum[x, t[[1]]]]]], terms]];
  If[ctx["lattice"] =!= None, e = sum[e, {ctx["lattice"]}]];
  snegswsimp[snegswunmask[e]]];

(* Kronecker deltas: lattice labels that differ by a constant, and deltas of
summation indexes, which are eliminated. Equal (it, ops) are combined. *)
snegswcombine[ctx_, terms_List] := Module[{t},
  t = Flatten[Map[snegswdelta[ctx, #] &, terms], 1];
  t = Map[{#[[1, 1]], #[[1, 2]], snegswreduce[Total[#[[All, 3]]]]} &,
    GatherBy[t, {Sort[#[[1]]], #[[2]]} &]];
  Select[t, #[[3]] =!= 0 &]];
snegswdelta[ctx_, {it_, ops_, p0_}] := Module[{p = snegswKD[p0], mons, with, without, res = {}},
  If[it === {} || FreeQ[p, KroneckerDelta], Return[{{it, ops, p}}]];
  mons = sum2list[Expand[p]];
  without = Select[mons, FreeQ[#, KroneckerDelta[a_, b_] /; MemberQ[it, a] || MemberQ[it, b]] &];
  with = Complement[mons, without];
  If[without =!= {}, AppendTo[res, {it, ops, Total[without]}]];
  Scan[Function[m, Module[{kd, x, y, it2, ops2, m2},
    kd = First[Cases[{m}, KroneckerDelta[a_, b_] /; MemberQ[it, a] || MemberQ[it, b], Infinity]];
    {x, y} = If[MemberQ[it, kd[[1]]], {kd[[1]], kd[[2]]}, {kd[[2]], kd[[1]]}];
    it2 = DeleteCases[it, x];
    m2 = (m /. kd -> 1) /. x -> y;
    ops2 = ops /. x -> y;
    res = Join[res, Flatten[Map[Function[l, snegswdelta[ctx, snegswnormalize[ctx, it2, l[[1]] m2, l[[2]]]]],
      Map[snegtermops, sum2list[Expand[nc @@ ops2]]]], 1]]]], with];
  res];

snegswscale[terms_List, c_] := Map[{#[[1]], #[[2]], c #[[3]]} &, terms];

(* Product of two terms: M1 p1 M2 p2 = (M1 M2) p1(final occupations of M2) p2. *)
snegswprod[ctx_, t1_List, t2_List] := Module[{it1, o1, p1, it2, o2, p2, ren, fa, pr},
  {it1, o1, p1} = t1; {it2, o2, p2} = t2;
  ren = Map[# -> ssJWRnewname[#, Join[listfreeindexes, it1, it2], {t1, t2}] &,
    Select[it2, !FreeQ[{it1, o1, p1}, #] &]];
  {it2, o2, p2} = {it2, o2, p2} /. ren;
  ren = Map[# -> ssJWRnewname[#, Join[listfreeindexes, it1, it2], {t1, t2, it2, o2, p2}] &,
    Select[it1, !FreeQ[{o2, p2}, #] &]];
  {it1, o1, p1} = {it1, o1, p1} /. ren;
  fa = snegswocc[ctx, o2][[2]];
  pr = sum2list[Expand[nc[nc @@ o1, nc @@ o2]]];
  Map[Function[t, With[{l = snegtermops[t]},
      If[l === $Failed, Message[eigenoperators::term, t]; snegswfail[]];
      snegswnormalize[ctx, Join[it1, it2], l[[1]] (p1 /. snegswrules[fa]) p2, l[[2]]]]], pr]];

(* Commutator of term lists; in lattice mode only the relative offsets of
overlapping supports contribute, since terms of even fermion parity on
disjoint sites commute. *)
snegswkom[ctx_, a_List, b_List] := Module[{R0 = ctx["lattice"], pairs},
  pairs = If[R0 === None,
    Flatten[Outer[List, a, b, 1], 1],
    Flatten[Outer[Function[{x, y}, Module[{ox, oy},
      ox = Map[snegswoffset[#, R0] &, snegswsites[ctx, x]];
      oy = Map[snegswoffset[#, R0] &, snegswsites[ctx, y]];
      Map[{x, y /. R0 -> R0 + #} &, Union[Flatten[Outer[Subtract, ox, oy]]]]]], a, b, 1], 2]];
  snegswcombine[ctx, Flatten[Map[Join[snegswprod[ctx, #[[1]], #[[2]]],
    snegswscale[snegswprod[ctx, #[[2]], #[[1]]], -1]] &, pairs], 1]]];

(* Sites of a term: operator site labels and the sites of the indicators. *)
snegswsites[ctx_, {it_, ops_, p_}] := Union[Join[
  Map[If[TrueQ[fermionQ[#]] || TrueQ[bosonQ[#]], #[[2]], First[#]] &, ops],
  Cases[{p}, snegswind[{_, {s_, ___}}, _] :> s, Infinity]]];

(* Analysis of a bare term: fixed occupations, energies of the other
operators, and checks. *)
snegswanalyze[ctx_, ops_List] := Module[{ia, fa, loc},
  {ia, fa, loc} = snegswocc[ctx, ops];
  Scan[If[TrueQ[fermionQ[#]] && ordering[Head[#]] =!= EMPTY,
    Message[eigenoperators::ordering, Head[#]]; snegswfail[]] &, loc];
  Scan[If[snegswenergy[ctx, #] =!= 0, Message[eigenoperators::diag, #]; snegswfail[]] &, Union[loc]];
  <|"init" -> snegswrules[ia], "fin" -> snegswrules[fa], "keys" -> Keys[ia],
    "lin" -> Select[ops, !ctx["localQ"][#] &]|>];

(* Eigenoperator components {omega, term} of a term. *)
snegswcomponents[ctx_, {it_, ops_, p_}] := Module[{a, f, g, vars, lam},
  a = snegswanalyze[ctx, ops];
  f = ctx["f"][a["keys"]];
  g = snegswreduce[(f /. a["fin"]) - (f /. a["init"])];
  lam = Total[Map[snegswenergy[ctx, #] &, a["lin"]]];
  vars = Union[Cases[{g}, snegswind[k_, _] :> k, Infinity]];
  DeleteCases[Map[Function[asg,
      {Together[(g /. Flatten[MapThread[snegswsetrules, {vars, asg[[All, 1]]}]]) + lam],
       {it, ops, snegswreduce[p Times @@ asg[[All, 2]]]}}],
    Tuples[Map[snegswvalues, vars]]], {_, {_, _, 0}}]];

(* Block projections P X P and P X Q + Q X P of a term. *)
snegswblock[ctx_, which_][{it_, ops_, p_}] := Module[{a, pp, pi, pf},
  a = snegswanalyze[ctx, ops];
  pp = ctx["P"][a["keys"], snegswsites[ctx, {it, ops, p}]];
  {pi, pf} = {pp /. a["init"], pp /. a["fin"]};
  {it, ops, snegswreduce[p If[which === "P", pf pi, pf (1 - pi) + (1 - pf) pi]]}];
snegswproj[ctx_, terms_] := snegswcombine[ctx, Map[snegswblock[ctx, "P"], terms]];
snegswod[ctx_, terms_] := snegswcombine[ctx, Map[snegswblock[ctx, "O"], terms]];

(* L(X): [H0, L(X)] = X for a block-off-diagonal X. *)
snegswL[ctx_, terms_] := Module[{c, zero},
  c = Flatten[Map[snegswcomponents[ctx, #] &, terms], 1];
  zero = snegswcombine[ctx, Cases[c, {w_, t_} /; PossibleZeroQ[w] :> t]];
  If[zero =!= {}, Message[schriefferwolff::res, snegswexpr[ctx, zero]]; snegswfail[]];
  snegswcombine[ctx, Map[{#[[2, 1]], #[[2, 2]], snegswreduce[#[[2, 3]]/#[[1]]]} &,
    Select[c, !PossibleZeroQ[#[[1]]] &]]]];

(* Simplification of expressions: expansion, Kronecker deltas, merging of sums
with equal iterator lists. *)
snegswsimp[x_] := With[{y = snegswKD[x]}, If[FreeQ[y, sum], Expand[y],
  snegswmerge[snegswKD[sumSimplifyKD[Expand[sumExpand[y]]]]]]];
snegswKD[x_] := If[FreeQ[x, KroneckerDelta], x,
  x /. rulelatticeKDreduce /. KroneckerDelta[d_, 0] /; snegswlvQ[d] -> 0];
snegswmerge[x_] := Module[{t = sum2list[Expand[x]], s, o},
  s = Cases[t, (c_. HoldPattern[sum[a_, it_List]]) /; isnumericQ[c] :> {it, c a}];
  o = Select[t, !MatchQ[#, (c_. _sum) /; isnumericQ[c]] &];
  Total[o] + Total[Map[sum[Expand[Total[#[[All, 2]]]], #[[1, 1]]] &, GatherBy[s, First]]]];

(* Lattice sums: the index, renaming, site offsets. *)
snegswlatticeindex[e_] := Module[{ix = Union[Cases[{e}, HoldPattern[sum[_, {i_?latticeindexQ}]] :> i, Infinity]]},
  If[ix === {}, Message[schriefferwolff::lattice, e]; snegswfail[]];
  First[ix]];
snegswrename[sum[a_, {i_}], R0_] /; latticeindexQ[i] := a /. i -> R0;
snegswrename[s_, R0_] := (Message[schriefferwolff::lattice, s]; snegswfail[]);

snegswlvQ[d_] := Module[{v = Select[snegsymbolsof[d], TrueQ[latticevectorQ[#]] &]},
  v =!= {} && FreeQ[Expand[d] /. Thread[v -> 0], _Symbol] &&
    And @@ Map[IntegerQ[Coefficient[Expand[d], #]] &, v] &&
    IntegerQ[Expand[d] /. Thread[v -> 0]]];
snegswoffset[s_, R0_] := With[{d = Expand[s - R0]},
  If[FreeQ[d, R0] && (IntegerQ[d] || snegswlvQ[d]), d,
    Message[schriefferwolff::lattice, s]; snegswfail[]]];

(* Lattice expressions: summand terms at R0 and the constant. *)
snegswlatticeterms[ctx_, e_] := Module[{R0 = ctx["lattice"], t = sum2list[Expand[snegswmask[e]]], s, c},
  s = Select[t, MatchQ[#, (k_. _sum) /; isnumericQ[k]] &];
  c = Complement[t, s];
  If[!FreeQ[c, _?operatorQ[___]], Message[schriefferwolff::lattice, First[Select[c, !FreeQ[#, _?operatorQ[___]] &]]];
    snegswfail[]];
  {snegswcombine[ctx, Flatten[Map[Function[u,
      With[{a = snegswrename[First[Cases[u, _sum, {0, 1}]], R0]},
        Flatten[Map[snegswterms1[ctx, {}, (u /. _sum -> 1) #] &, sum2list[Expand[a]]], 1]]], s], 1]],
   snegswunmask[Total[c]]}];

snegswparitycheck[terms_List] := Scan[Function[t,
  If[OddQ[Count[t[[2]], o_ /; TrueQ[fermionQ[o]] || TrueQ[majoranaQ[o]] ||
      (snegswxop[o] && snegxparity[o] === 1)]],
    Message[schriefferwolff::parity, nc @@ t[[2]]]; snegswfail[]]], terms];

(* Compositions of n into positive parts *)
snegswcompositions[n_] := Join @@ Map[Permutations, IntegerPartitions[n]];

(* Collect equal operator monomials and simplify their coefficients. *)
snegswtidy[x_, simp_] := snegswunmask[snegswtidy1[
  Expand[snegswmask[snegswmerge[snegswdummies[snegswsimp[x]]]]], simp]];

(* Canonical names of summation indexes: the indexes of each sum are renamed,
in order, to root, root1, root2, ... of the same type, where the root is the
index from which an automatically renamed index derives. *)
snegswdummies[x_] := x /. HoldPattern[sum[a_, it_List]] :> Module[{roots, new, free},
  roots = Map[snegswroot, it];
  new = MapIndexed[snegswdummyname[#1, Count[roots[[1 ;; First[#2]]], #1] - 1] &, roots];
  free = Complement[snegsymbolsof[a], it];
  If[!DuplicateFreeQ[new] || Intersection[new, free] =!= {}, sum[a, it],
    sum[a /. Thread[it -> new], new]]];
(* Root of an automatically renamed index (k for k12), found by name among
the declared indexes of the same type, independently of the context in
which the renamed index was created. *)
snegswroot[m_Symbol] := Module[{root = StringReplace[SymbolName[m],
    ShortestMatch[r : (_ ~~ ___)] ~~ DigitCharacter .. ~~ EndOfString :> r], c},
  c = Select[listfreeindexes, SymbolName[#] === root && latticeindextype[#] === latticeindextype[m] &];
  If[c === {} || root === SymbolName[m], m, First[c]]];
snegswroot[m_] := m;
snegswdummyname[r_Symbol, 0] := r;
snegswdummyname[r_Symbol, j_Integer] := snegswdummyname[r, j] =
  With[{s = Symbol[Context[r] <> SymbolName[r] <> ToString[j]]},
    If[!MemberQ[listfreeindexes, s], snegfreeindexes[s]; snegcopyindexproperties[r, s]];
    s];
snegswtidy1[sum[a_, it_List], simp_] := sum[snegswtidy1[snegswcanonical[Expand[a], it], simp], it];
snegswtidy1[c_ s_sum, simp_] /; isnumericQ[c] := c snegswtidy1[s, simp];
snegswtidy1[e_, simp_] := Module[{t = sum2list[e], s, o, l},
  s = Select[t, FreeQ[#, sum] &];
  o = Complement[t, s];
  l = Map[snegtermops, s];
  If[MemberQ[l, $Failed], Return[e]];
  Total[Map[snegswtidy1[#, simp] &, o]] +
    Total[Map[simp[Total[#[[All, 1]]]] nc @@ #[[1, 2]] &, GatherBy[l, Last]]]];

(* Relabel the bound indexes of each term of a summand by the permutation
(within indexes of the same type) that gives the smallest operator string,
so that terms related by exchanging dummy indexes are combined. *)
snegswcanonical[a_, it_List] := Module[{groups, perms},
  groups = GatherBy[it, {latticeindextype[#], Conjugate[#] === #} &];
  perms = Map[Function[p, Thread[Flatten[groups] -> Flatten[p]]],
    Tuples[Map[Permutations, groups]]];
  If[Length[perms] <= 1, a,
    Total[Map[Function[t, First[SortBy[Map[t /. # &, perms],
      Function[u, With[{l = snegtermops[snegswmask[u]]},
        If[l === $Failed, ToString[u, InputForm], ToString[l[[2]], InputForm]]]]]]],
      sum2list[a]]]]];

Options[schriefferwolff] = {"Generator" -> False, "Simplify" -> Simplify};
schriefferwolff[H0_, V_, P_, n_Integer?NonNegative, opts:OptionsPattern[]] := Catch[Module[
  {ctx, lat, v, vc, h0, h0c, R, S, C, A, heff, od, simp = OptionValue["Simplify"],
   gen = TrueQ[OptionValue["Generator"]], out},
  lat = MatchQ[P, {_Symbol, _}];
  If[lat && n > 2, Message[schriefferwolff::rule]; snegswfail[]];
  ctx = snegswcontext[H0, P];
  {v, vc} = If[lat, snegswlatticeterms[ctx, V], {snegswterms[ctx, V], 0}];
  {h0, h0c} = If[lat, snegswlatticeterms[ctx, H0], {snegswterms[ctx, H0], 0}];
  If[lat, snegswparitycheck[v]];
  A[{}, base_] := base;
  A[{m_, rest___}, base_] := A[{m, rest}, base] = snegswkom[ctx, S[m], A[{rest}, base]];
  heff = {snegswproj[ctx, h0]};
  Do[
    (* top order 2: P R_2 P = P [S_1, V_d] P + 1/2 P [S_1, V_od] P = 1/2 P [S_1, V] P,
       since S_1 is block-off-diagonal *)
    If[m == 2 && n == 2 && !gen,
      AppendTo[heff, snegswproj[ctx, snegswscale[snegswkom[ctx, S[1], v], 1/2]]]; Continue[]];
    R[m] = If[m == 1, v, snegswcombine[ctx, Join[
      Flatten[Map[Function[c, snegswscale[A[Most[c], C[Last[c]]], 1/Length[c]!]],
        Select[snegswcompositions[m], Length[#] > 1 &]], 1],
      Flatten[Map[Function[c, snegswscale[A[c, v], 1/Length[c]!]], snegswcompositions[m - 1]], 1]]]];
    AppendTo[heff, snegswproj[ctx, R[m]]];
    If[m < n || gen,
      od = snegswod[ctx, R[m]]; C[m] = snegswscale[od, -1]; S[m] = snegswL[ctx, od]],
    {m, 1, n}];
  out = MapIndexed[snegswtidy[snegswexpr[ctx, #1] + Which[#2 === {1}, h0c, #2 === {2}, vc, True, 0], simp] &, heff];
  If[gen, {out, Table[snegswtidy[snegswexpr[ctx, S[m]], simp], {m, n}]}, out]
], snegsw];

eigenoperators[O_, H0_] := Catch[Module[{ctx, comps},
  If[!FreeQ[O, sum], Message[eigenoperators::sum]; snegswfail[]];
  ctx = snegswcontext[H0, None];
  comps = Flatten[Map[snegswcomponents[ctx, #] &, snegswterms[ctx, O]], 1];
  comps = Map[{#[[1, 1]], snegswexpr[ctx, snegswcombine[ctx, #[[All, 2]]]]} &, GatherBy[comps, First]];
  Select[comps, #[[2]] =!= 0 &]
], snegsw];

(* Fermion operators of a spin-1/2 level in terms of the X-operators of the
Hubbard atom (inverse of xtofermions): c^dagger_UP = X^(UP,0) + X^(2,DO),
c^dagger_DO = X^(DO,0) - X^(2,UP). *)
fermionstox::spin = "`1` must be a spin-1/2 fermion operator.";
fermionstox[expr_, c_Symbol -> x_Symbol] := Module[{},
  If[!TrueQ[fermionQ[c]] || spinof[c] != 1/2, Message[fermionstox::spin, c]; Return[$Failed]];
  If[!TrueQ[xoperatorQ[x]] || Sort[xstates[x]] =!= Sort[First[snegxhubbardatom]] ||
     Map[xparities[x], First[snegxhubbardatom]] =!= Last[snegxhubbardatom],
    Message[xtofermions::states, x]; Return[$Failed]];
  snegswsimp[expr /. {
    c[CR, j___, UP] :> x[j, "UP", "0"] + x[j, "2", "DO"],
    c[CR, j___, DO] :> x[j, "DO", "0"] - x[j, "2", "UP"],
    c[AN, j___, UP] :> x[j, "0", "UP"] + x[j, "DO", "2"],
    c[AN, j___, DO] :> x[j, "0", "DO"] - x[j, "UP", "2"]}]];

(* X-operators of singly occupied sites as spin-1/2 operators. *)
xtospin::out = "Terms with X-operators outside the singly occupied subspace were dropped: `1`.";
xtospin[expr_, x_Symbol -> s_Symbol] := Module[{e = snegswsimp[expr], e0, out},
  If[!TrueQ[xoperatorQ[x]] || Sort[xstates[x]] =!= Sort[First[snegxhubbardatom]],
    Message[xtofermions::states, x]; Return[$Failed]];
  If[!TrueQ[spinQ[s]], Message[xtospin::spinop, s]; Return[$Failed]];
  e0 = Expand[e /. x[___, a_, b_] /; MemberQ[{"0", "2"}, a] || MemberQ[{"0", "2"}, b] :> 0];
  out = snegswtidy[snegswsimp[e - e0], Together];
  If[out =!= 0, Message[xtospin::out, out]];
  snegswsimp[e0 /. {
    x[j___, "UP", "UP"] :> 1/2 + s[j, 3], x[j___, "DO", "DO"] :> 1/2 - s[j, 3],
    x[j___, "UP", "DO"] :> s[j, 4], x[j___, "DO", "UP"] :> s[j, 5]}]];
xtospin::spinop = "`1` must be declared with snegspinoperators.";

(* The canonical transformation for matrices. P is the subspace of the basis
states pind. H0 must not couple P and Q; if it is not diagonal, numerical
blocks are diagonalized first. The results are the P blocks of H_eff^(m),
m = 0, ..., n, in the original basis. *)
schriefferwolffmatrix::block = "H0 couples the subspaces P and Q.";
schriefferwolffmatrix::diag = "A symbolic H0 must be diagonal.";
schriefferwolffmatrix::dim = "H0 and V must be square matrices of equal dimension, and the indexes must be distinct and in range.";
Options[schriefferwolffmatrix] = {"Generator" -> False};
schriefferwolffmatrix[H0_?MatrixQ, V_?MatrixQ, pind_List, n_Integer?NonNegative, opts:OptionsPattern[]] := Catch[Module[
  {d = Length[H0], q, e, u = None, vt, mask, L, kom, R, S, C, A, heff, od, back, tg, i, j, vals, vecs},
  If[Dimensions[H0] =!= {d, d} || Dimensions[V] =!= {d, d} || !DuplicateFreeQ[pind] ||
     !SubsetQ[Range[d], pind], Message[schriefferwolffmatrix::dim]; snegswfail[]];
  q = Complement[Range[d], pind];
  If[!And @@ Map[PossibleZeroQ, Flatten[{H0[[pind, q]], H0[[q, pind]]}]],
    Message[schriefferwolffmatrix::block]; snegswfail[]];
  tg[x_] := If[MatrixQ[x, NumericQ], x, Map[Together, x, {2}]];
  If[And @@ Map[PossibleZeroQ, Flatten[H0 - DiagonalMatrix[Diagonal[H0]]]],
    e = Diagonal[H0]; vt = V,
    If[!MatrixQ[H0, NumericQ], Message[schriefferwolffmatrix::diag]; snegswfail[]];
    u = ConstantArray[0, {d, d}]; e = ConstantArray[0, d];
    Scan[Function[b,
      {vals, vecs} = Eigensystem[(H0[[b, b]] + ConjugateTranspose[H0[[b, b]]])/2];
      e[[b]] = vals; u[[b, b]] = Transpose[vecs]], {pind, q}];
    vt = ConjugateTranspose[u] . V . u];
  mask = Table[Boole[MemberQ[pind, i] =!= MemberQ[pind, j]], {i, d}, {j, d}];
  L[x_] := Table[If[mask[[i, j]] == 0 || PossibleZeroQ[x[[i, j]]], 0,
    If[PossibleZeroQ[e[[i]] - e[[j]]], Message[schriefferwolff::res, x[[i, j]]]; snegswfail[],
      x[[i, j]]/(e[[i]] - e[[j]])]], {i, d}, {j, d}];
  kom[a_, b_] := tg[a . b - b . a];
  A[{}, base_] := base;
  A[{m_, rest___}, base_] := A[{m, rest}, base] = kom[S[m], A[{rest}, base]];
  back[x_] := If[u === None, x, u[[pind, pind]] . x . ConjugateTranspose[u[[pind, pind]]]];
  heff = {back[DiagonalMatrix[e][[pind, pind]]]};
  R[1] = vt;
  Do[
    If[m > 1,
      R[m] = tg[Total[Map[Function[c, A[Most[c], C[Last[c]]]/Length[c]!],
          Select[snegswcompositions[m], Length[#] > 1 &]]] +
        Total[Map[Function[c, A[c, vt]/Length[c]!], snegswcompositions[m - 1]]]]];
    AppendTo[heff, back[R[m][[pind, pind]]]];
    If[m < n || TrueQ[OptionValue["Generator"]],
      od = R[m] mask; C[m] = -od; S[m] = L[od]],
    {m, 1, n}];
  If[TrueQ[OptionValue["Generator"]],
    {heff, Table[If[u === None, S[m], u . S[m] . ConjugateTranspose[u]], {m, n}]}, heff]
], snegsw];

(**** Equations of motion, Green's functions, and sum rules ****)

(* Retarded Green's functions in the notation of Zubarev,
G_AB(z) = <<A; B>>_z = -i Int_0^Infinity dt e^(i z t) <[A(t), B]_eta>,
with the anticommutator (eta = +1) if A is odd in the fermion operators and
the commutator (eta = -1) otherwise. The equation of motion is
z <<A; B>> = <[A, B]_eta> + <<[A, H]; B>>, and the spectral function
A_AB(omega) = -Im G_AB(omega + i0)/Pi has the moments
Int omega^n A_AB(omega) d omega = <[L^n A, B]_eta>, L A = [A, H]. *)

eomclosure::open = "The equations of motion do not close within `1` operators.";
eomclosure::sum = "Symbolic sums are not supported; use explicit modes.";
eomclosure::term = "The term `1` is not a product of operators.";
eomclosure::parity = "The operator `1` has no definite fermion parity.";
greensfunction::parity = eomclosure::parity;
spectralmoments::parity = eomclosure::parity;
lehmann::mode = "Only fermion modes are supported; `1` is not one of the modes.";
lehmann::parity = eomclosure::parity;

(* Fermion parity of an operator expression: 0 (even), 1 (odd), or $Failed. *)
snegeomparity[e_] := Module[{p},
  p = Union[Map[Function[u, With[{l = snegtermops[u]},
      If[l === $Failed, $Failed, Mod[Total[Map[snegeomop, l[[2]]]], 2]]]],
    sum2list[Expand[e //. HoldPattern[sum[a_, _]] :> a]]]];
  If[Length[p] == 1 && p =!= {$Failed}, First[p], $Failed]];
snegeomop[o_] := Which[
  TrueQ[fermionQ[o]] || TrueQ[majoranaQ[o]], 1,
  TrueQ[xoperatorQ[Head[o]]], If[snegxparity[o] === None, 0, snegxparity[o]],
  True, 0];

(* (Anti)commutator according to the parity of A *)
snegeombracket[a_, b_, p_] := If[p === 1, antikomutator[a, b], komutator[a, b]];

snegeomsimp[x_] := If[FreeQ[x, sum], Expand[x], snegswsimp[x]];

(* {A, [A, H], [[A, H], H], ...} up to n commutators *)
liouvillian[a_, h_, n_Integer?NonNegative] :=
  NestList[snegeomsimp[komutator[#, h]] &, snegeomsimp[a], n];

(* Moments <[L^k A, B]_eta>, k = 0, ..., n; the average is avg[] (symbolic)
or the function given by "Average". *)
Options[spectralmoments] = {"Average" -> avg};
spectralmoments[a_, b_, h_, n_Integer?NonNegative, opts:OptionsPattern[]] := Module[
  {p = snegeomparity[a], av = OptionValue["Average"]},
  If[p === $Failed, Message[spectralmoments::parity, a]; Return[$Failed]];
  Map[snegeomsimp[av[snegeomsimp[snegeombracket[#, b, p]]]] &, liouvillian[a, h, n]]];

(* Operator monomials {c, monomial} of an expression without sums. *)
snegeommonomials[e_] := Map[Function[t, With[{l = snegtermops[t]},
    If[l === $Failed, Message[eomclosure::term, t]; Throw[$Failed, snegeom]];
    {l[[1]], nc @@ l[[2]]}]], sum2list[Expand[e]]];

(* Closure of the equations of motion: the operator monomials O_i generated
from A by repeated commutation with H and the matrix M with
[O_i, H] = Sum_j M_ij O_j. Returns {{O_1, ...}, M, a} with A = a . O. *)
Options[eomclosure] = {"MaxOperators" -> 100};
eomclosure[a_, h_, opts:OptionsPattern[]] := Catch[Module[
  {max = OptionValue["MaxOperators"], basis = {}, index = <||>, rows = <||>, queue, m, add, coef, i},
  If[!FreeQ[{a, h}, sum], Message[eomclosure::sum]; Throw[$Failed, snegeom]];
  If[snegeomparity[a] === $Failed, Message[eomclosure::parity, a]; Throw[$Failed, snegeom]];
  add[o_] := If[!KeyExistsQ[index, o],
    If[Length[basis] >= max, Message[eomclosure::open, max]; Throw[$Failed, snegeom]];
    AppendTo[basis, o]; index[o] = Length[basis]; AppendTo[queue, o]];
  queue = {};
  coef = snegeommonomials[a];
  Scan[add[#[[2]]] &, coef];
  While[queue =!= {},
    m = First[queue]; queue = Rest[queue];
    rows[m] = snegeommonomials[komutator[m, h]];
    Scan[add[#[[2]]] &, rows[m]]];
  {basis,
   Table[Total[Map[If[#[[2]] === basis[[j]], #[[1]], 0] &, rows[basis[[i]]]]], {i, Length[basis]}, {j, Length[basis]}],
   Table[Total[Map[If[#[[2]] === basis[[j]], #[[1]], 0] &, coef]], {j, Length[basis]}]}
], snegeom];

(* G_AB(z) = a . (z - M)^-1 . N with N_i = <[O_i, B]_eta>. *)
Options[greensfunction] = {"Average" -> avg, "MaxOperators" -> 100};
greensfunction[a_, b_, h_, z_, opts:OptionsPattern[]] := Module[
  {cl, p = snegeomparity[a], nv, av = OptionValue["Average"]},
  If[p === $Failed, Message[greensfunction::parity, a]; Return[$Failed]];
  cl = eomclosure[a, h, "MaxOperators" -> OptionValue["MaxOperators"]];
  If[cl === $Failed, Return[$Failed]];
  nv = Map[Expand[av[Expand[snegeombracket[#, b, p]]]] &, cl[[1]]];
  (* a . (z - M)^-1 is computed exactly when M is exact; inexact averages
     enter only through the final scalar product. For inexact M the
     eigendecomposition of M is used. *)
  If[Precision[cl[[2]]] === Infinity,
    With[{g = LinearSolve[Transpose[z IdentityMatrix[Length[cl[[1]]]] - cl[[2]]], cl[[3]]] . nv},
      If[Precision[nv] === Infinity, Together[g], Expand[g]]],
    Module[{vals, vecs, v},
      {vals, vecs} = Eigensystem[cl[[2]]];
      v = Transpose[vecs];
      Total[(cl[[3]] . v) LinearSolve[v, nv]/(z - vals)]]]];

(* Exact results for finite fermion systems: matrices in the Fock space of
the modes (Jordan-Wigner order of the modes). *)
snegeomfock[n_, k_] := snegeomfock[n, k] = SparseArray[KroneckerProduct @@ Table[
  Which[j < k, {{1, 0}, {0, -1}}, j == k, {{0, 0}, {1, 0}}, True, IdentityMatrix[2]], {j, n}]];
snegeommatrix[e_, modes_List] := Module[{n = Length[modes], idx},
  idx = Association[Flatten[Table[{modes[[i]][CR] -> {i, CR}, modes[[i]][AN] -> {i, AN}}, {i, n}]]];
  Total[Map[Function[t, With[{l = snegtermops[t]},
    If[l === $Failed || !SubsetQ[Keys[idx], l[[2]]],
      Message[lehmann::mode, First[Complement[Cases[{t}, _?operatorQ[___], Infinity], Keys[idx]] /. {} -> {t}]];
      Throw[$Failed, snegeom]];
    l[[1]] If[l[[2]] === {}, IdentityMatrix[2^n, SparseArray],
      Dot @@ Map[With[{q = idx[#]}, If[q[[2]] === CR, snegeomfock[n, q[[1]]],
        Transpose[snegeomfock[n, q[[1]]]]]] &, l[[2]]]]]], sum2list[Expand[e]]]]];

snegeomsetup[h_, specs_List] := Module[{s = snegquadsetup[specs]},
  If[s === $Failed, Throw[$Failed, snegeom]];
  If[s[[2]] =!= "Fermion", Message[lehmann::mode, First[specs]]; Throw[$Failed, snegeom]];
  First[s]];

(* Thermal weights of the eigenstates; at T = 0 the ground states share
the weight equally. *)
snegeomweights[e_, temp_, tol_] := If[TrueQ[temp == 0],
  With[{g = Boole[Map[# < Min[e] + tol &, e]]}, g/Total[g]],
  With[{w = Exp[-(e - Min[e])/temp]}, w/Total[w]]];

Options[thermalaverage] = {"Tolerance" -> 10^-10};
(* Eigensystem of H in the Fock space, cached for repeated averages. *)
snegeomeigen[h_, modes_List] := snegeomeigen[h, modes] = Module[{vals, vecs},
  {vals, vecs} = Eigensystem[N[Normal[snegeommatrix[h, modes]]]];
  {Re[vals], vecs}];

thermalaverage[x_, h_, specs_List, temp_, opts:OptionsPattern[]] := Catch[Module[
  {modes = snegeomsetup[h, specs], vals, vecs, w, xm},
  {vals, vecs} = snegeomeigen[h, modes];
  w = snegeomweights[vals, temp, OptionValue["Tolerance"]];
  xm = Normal[snegeommatrix[x, modes]];
  Chop[Total[w Map[Conjugate[#] . xm . # &, vecs]]]
], snegeom];

(* Lehmann representation G_AB(z) = Sum_j w_j/(z - omega_j):
w = <m|A|n><n|B|m> (rho_m -+ rho_n) at omega = E_n - E_m (upper sign:
commutator for even A). Poles closer than the tolerance are merged. *)
Options[lehmann] = {"Tolerance" -> 10^-9};
lehmann[a_, b_, h_, specs_List, temp_, opts:OptionsPattern[]] := Catch[Module[
  {modes = snegeomsetup[h, specs], p = snegeomparity[a], tol = OptionValue["Tolerance"],
   vals, vecs, rho, am, bm, w, om, l},
  If[p === $Failed, Message[lehmann::parity, a]; Throw[$Failed, snegeom]];
  {vals, vecs} = snegeomeigen[h, modes];
  rho = snegeomweights[vals, temp, tol];
  am = Conjugate[vecs] . Normal[snegeommatrix[a, modes]] . Transpose[vecs];
  bm = Conjugate[vecs] . Normal[snegeommatrix[b, modes]] . Transpose[vecs];
  w = am Transpose[bm] (If[p === 1, Plus, Subtract] @@ {Outer[#1 &, rho, rho], Outer[#2 &, rho, rho]});
  om = Outer[#2 - #1 &, vals, vals];
  l = Select[Transpose[{Flatten[om], Flatten[w]}], Abs[#[[2]]] > tol &];
  l = Map[{Mean[#[[All, 1]]], Total[#[[All, 2]]]} &, Gather[SortBy[l, First], Abs[#1[[1]] - #2[[1]]] < tol &]];
  Chop[Select[l, Abs[#[[2]]] > tol &]]
], snegeom];

(**** LaTeX output ****)

(* snegtex: operators and operator products are typeset directly; the
remaining expression is converted with TeXForm, in which every operator
product is represented by a placeholder symbol that is substituted
afterwards. *)
snegtexindex[j_] := ToString[TeXForm[j]];
snegtexspin[s_, op_] := Which[
  spinof[op] == 1/2 && s === UP, "\\uparrow",
  spinof[op] == 1/2 && s === DO, "\\downarrow",
  spinof[op] == 0, "",
  True, snegtexindex[s]];
snegtexsub[l_List] := If[l === {}, "", "_{" <> StringRiffle[l, ","] <> "}"];
snegtexname[op_] := With[{n = SymbolName[op]},
  If[StringLength[n] == 1, n, "\\mathrm{" <> n <> "}"]];

snegtexop[o:op_?fermionQ[t_, j___]] := Module[{l = {j}, sp},
  If[spinof[op] == 0,
    sp = ""; l = Map[snegtexindex, l],
    sp = If[l === {}, "", snegtexspin[Last[l], op]];
    l = If[l === {}, {}, Map[snegtexindex, Most[l]]]];
  snegtexname[op] <> If[t === CR, "^\\dagger", ""] <>
    snegtexsub[If[sp === "", l, If[l === {}, {sp}, {StringRiffle[l, ","] <> sp}]]]];
snegtexop[o:op_?bosonQ[t_, j___]] :=
  snegtexname[op] <> If[t === CR, "^\\dagger", ""] <> snegtexsub[Map[snegtexindex, {j}]];
snegtexop[o:op_?spinQ[j___, c_]] := snegtexname[op] <>
  "^{" <> Switch[c, 1, "x", 2, "y", 3, "z", 4, "+", 5, "-", _, snegtexindex[c]] <> "}" <>
  snegtexsub[Map[snegtexindex, {j}]];
snegtexop[o:op_?majoranaQ[j___]] := snegtexname[op] <> snegtexsub[Map[snegtexindex, {j}]];
snegtexop[o:op_[j___, a_, b_]] /; TrueQ[xoperatorQ[op]] := snegtexname[op] <>
  "^{" <> ToString[a] <> ToString[b] <> "}" <> snegtexsub[Map[snegtexindex, {j}]];
snegtexop[x_] := snegtex[x];

snegtexfactor[x_] := If[Head[x] === Plus, "\\left(" <> snegtex[x] <> "\\right)", snegtexop[x]];

snegtex[expr_] := Module[{e = expr, tab = {}, ph, n = 0, s},
  ph[str_] := (n++; AppendTo[tab, "\\text{snegtexph" <> ToString[n] <> "}" -> str];
    "snegtexph" <> ToString[n]);
  e = e /. {
    HoldPattern[sum[a_, it_List]] :> ph["\\sum_{" <> StringRiffle[Map[snegtexindex, it], ","] <> "} " <>
      If[Head[a] === Plus, "\\left(" <> snegtex[a] <> "\\right)", snegtex[a]]],
    HoldPattern[nc[a__]] :> ph[StringRiffle[Map[snegtexfactor, {a}], " "]],
    o:(h_Symbol[___]) /; TrueQ[operatorQ[h]] :> ph[snegtexop[o]]};
  s = ToString[TeXForm[e]];
  StringReplace[s, tab]];

End[]; (* `Private` *)

EndPackage[];

(* Backward-compatible lowercase loader context. *)
BeginPackage["sneg`", {"Sneg`"}];
EndPackage[];

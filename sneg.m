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

snegidstring = "sneg.m 2.0.13 Feb 2026";
snegcopyright = "Copyright (C) 2002-2026 Rok Zitko";

$SnegVersion = Module[{pos, p1, p2},
  pos = StringPosition[snegidstring, " "];
  p1 = pos[[1,1]]+1;
  p2 = pos[[2,1]]-1;
  StringTake[snegidstring, {p1, p2}]
];
$SnegVersion::usage = "$SnegVersion is the version of sneg package used.";

Print["sneg " <> $SnegVersion <> " " <> snegcopyright];

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
(* Add a link to the relevant documentation page. *)
SetAttributes[UsageWithMore, HoldFirst];
UsageWithMore[symbol_, string_, tag_:Null] := (symbol::usage = string <>
"\!\(\*ButtonBox[\(More...\),ButtonStyle->\"AddOnsLink\",ButtonData:>\"" <>
     If[tag === Null, ToString[symbol], tag] <> "\"]\)");

UsageWithMore[snegfermionoperators,
"snegfermionoperators[a, b, c] declares a, b, and c to be
fermionic operators. They obey canonical anti-commutation relations."];
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
"nc[...] represents non-commutative multiplication."];
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
"komutator[a, b] = nc[a, b] - nc[b,a]"];
UsageWithMore[commutator,
"commutator[a, b] = nc[a, b] - nc[b,a]", "komutator"];
UsageWithMore[antikomutator,
"antikomutator[a, b] = nc[a, b] + nc[b, a]"];
UsageWithMore[anticommutator,
"anticommutator[a, b] = nc[a, b] + nc[b, a]", "antikomutator"];
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
"hubbard[c[i]] returns the local (Hubbard) electron-electron repulsion
operator corresponding to operator c[i]."];
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
total spin quantum numbers (Q,S) in creation operator representation."];
UsageWithMore[qsbasisvc,
"qsbasisvc[{ops}] returns the basis with well defined charge and
total spin quantum numbers (Q,S) in occupation number representation."];
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
"spinketbraP[S] returns the spin lowering operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinketbraM,
"spinketbraM[S] returns the spin raising operator for spin S
in the bra-ket representation.", "spinketbraX"];
UsageWithMore[spinbasis,
"spinbasis[S] returns the basis for spin S in the ket representation.", "spinbra"];
UsageWithMore[transformfunc,
"transformfunc[list, rule] applies a rule to the states 'list' in the
occupation number representation and orthogonalizes the result."];
UsageWithMore[transformbasis,
"transformbasis[basis, rule] applies a rule to the states in the full
basis (occupation number representation) and orthogonalizes the result."];
UsageWithMore[mergebasis,
"mergebasis[{basis1, basis2,...}] merges several sets of basis states."];
UsageWithMore[applybasis,
"applybasis[b, fn] applies function fn to each basis state in the
basis b."];
UsageWithMore[transformtoPH,
"transformtoPH[b, Nph] generates a direct product of a basis b for
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
"isoqsz[op[j]] gives the z-component of the on-site isospin operator squared.
This term appears in the expression for e-e repulsion in the Anderson model.",
"hubbard"];
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
KSubsets[l_List,2] := Flatten[Table[{l[[i]], l[[j]]},
                                    {i, Length[l]-1},
                                    {j, i+1, Length[l]}
                              ],
                              1
                      ]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])
KSubsets[s_List, k_Integer] := Prepend[Map[s[[#]] &, KS[Length[s], k]], s[[Range[k] ]] ]

(* Mathematica >6.x required *)
(* OLD 1: snegorthog[m_] := Orthogonalize[m, Method->"Householder"]; *)
(* OLD 2: snegorthog[m_] := Orthogonalize[m, Dot[Conjugate[#1], #2] &]; *)
snegorthog[m_] := Orthogonalize[m, Simplify[Dot[Conjugate[#1], #2]] &];
snegorthog[{{0}}] := {};
snegorthog[m_] /; Tr[Abs[m]] == 0 := {};

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
              (* XXX: TEMPORARY FIX: Should be \[UpArrow] *)
              StyleForm["\[DoubleUpArrow]", FontColor -> Red],
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

snegrealfunctions[l__] := Scan[
{
  snegnonopQ[#] ^= True;
  Conjugate[#[a___]] ^= #[a];
  isnumericQ[#[___]] := True;
}&, {l}];

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

snegspinoperators[l__] := Scan[
{
  operatorQ[#] ^= True;
  spinQ[#] ^= True;
  SetAttributes[#, NHoldAll];

  (* This allows for some tricks later on... *)
  #[i___, -SPININDEXx] = -#[i, SPININDEXx];
  #[i___, -SPININDEXy] = -#[i, SPININDEXy];
  #[i___, -SPININDEXz] = -#[i, SPININDEXz];
  #[i___, 0] = 0;

  If[PrettyOutput, SnegPPspinoperator[#] ];

  addto[listspinoperators, #];
}&, {l}];


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

(* isnumericQ[] return True if the expression does not involve operators. *)

isnumericQ[z_] := And @@ (If[NumericQ@#, True,
  If[MemberQ[Quiet[Attributes@Evaluate@Head@#], NumericFunction] ||
     snegnonopQ[#] === True, True, False]] & /@
  Level[z, {0, Infinity}]);

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
contraction[op_[AN, j1___], op_[CR, j2___]] /; (ordering[op] == NONE) :=
  If[{j1} === {j2}, 1, 0, 0];
contraction[op_[___], op_[___]] /; (ordering[op] == NONE) := 0;

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
  ( (op1 =!= op2) && (i1 == AN && i2 == CR) ) := -nc[a, x2, x1, b];

nc[a___, x1:op1_?bosonQ[i1_,___], x2:op2_?bosonQ[i2_,___], b___] /;
  ( (op1 =!= op2) && (i1 == AN && i2 == CR) ) := nc[a, x2, x1, b];

(* In the case of the same *type* of operators, sort according to Head
(symbol), i.e. in the alphabetical order. *)

nc[a___, x1:op1_?fermionQ[i1_,___], x2:op2_?fermionQ[i2_,___], b___] /;
  ( (op1 =!= op2) && (i1 === i2) && !OrderedQ[{x1,x2}]) :=
    -nc[a, x2, x1, b];

nc[a___, x1:op1_?bosonQ[i1_,___], x2:op2_?bosonQ[i2_,___], b___] /;
  ( (op1 =!= op2) && (i1 === i2) && !OrderedQ[{x1,x2}]) :=
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

nc[a___, b_, z_, c___] /; (fermionQ[b] && grassmanQ[z]) :=
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

acmt[op1_Symbol, op2_Symbol] /; majoranaQ[op1] && majoranaQ[op2] && op1 =!= op2 := 0;


(* Majorana operators are always anti-commuted to the left of the (Dirac)
fermionic operators. This allows for mixed Majorana-Dirac expressions. *)
nc[a___, x1:op1_?fermionQ, x2:op2_?majoranaQ, b___] := -nc[a, x2, x1, b];

(* Coherent states *)
coh[z_, c_[k___]] := nc[Exp[-nc[conj[z], z]/2], Exp[nc[c[CR, k], z]], VACUUM];

(* Count the number of operators with anticommuting properties *)

acmtcount[l_List] := Count[l, _?((fermionQ[#] || grassmanQ[#]) &), 1];


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

cmt[op_[i1__, j1_], op_[i2__, j2_]] :=
  If[Length[{i1}] == Length[{i2}],
    +Inner[KroneckerDelta, {i1}, {i2}, Times], 0] cmt[op[i1, j1], op[i1, j2]];

cmt[op1_?spinQ[i1___], op2_?spinQ[i2___]] /; (op1 =!= op2) = 0;

(* (Anti-)commutators *)
SetAttributes[komutator, Listable];
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
conj[op_?fermionQ[i_, j___]] := op[If[i == CR, AN, CR], j];

(* Boson *)
conj[op_?bosonQ[i_, j___]] := op[If[i == CR, AN, CR], j];

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

(**** Symbolic sums ****)

(* No sum at all *)
sum[z_, {}] := z;

(* The indexes must be sorted for the simplifications to work correctly *)
sum[z_, it_List] := sum[z, Sort[it]] /; Not[OrderedQ[it]];

(* Returns True if expr contains none of symbols in list it *)
FreeOfIndexQ[expr_, it_List] := And @@ Map[FreeQ[expr, #]&, it];

(* Return True if expr can be factored out in front of the sum[] *)
sumFactorizableQ[expr_, it_List] := FreeOfIndexQ[expr, it];

(* It's always safe to extract a number *)
sum[z_ a_, it_List] := z sum[a, it] /; NumberQ[z];

sum[z_ a_, it_List] := z sum[a, it] /; sumFactorizableQ[z, it];

sum[z_, it_List] := z sum[1, it] /; (sumFactorizableQ[z, it] && z != 1);


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
  nn
];

snegsumJoinWithRenaming[a1_, a0___, a2_, it1_List, it2_List, reverse_:False] :=
Module[{l, rule, allnames},
  l = it1;
  rule = {};
  allnames = Union[it1, it2];
  Scan[ If[FreeQ[l, #] && FreeQ[a1, #],
    AppendTo[l, #],
    (* else *)
    nn = ssJWRnewname[#, allnames, a1];
    AppendTo[l, nn];
    AppendTo[allnames, nn];
    AppendTo[rule, # :> nn] ]&, it2];
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

(* Multiple sums *)
sum[sum[a_, it1_List], it2_List] :=
  sum[a, snegsumJoin[it1, it2]];

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

rulesumCollect = {
Plus[z1_. sum[a1_, it_List], z2_. sum[a2_, it_List]] :> sum[z1 a1+z2 a2, it],

Plus[z1_. sum[a1_, it1_List], z2_. sum[a2_, it2_List]] /;
  Intersection[it1, it2] =!= {} :>
    Module[{int},
      int = Intersection[it1, it2];
      sum[sum[z1 a1, Complement[it1, int]]
         +sum[z2 a2, Complement[it2, int]], int]
    ],

nc[a___, sum[b_,it_List], c___] :> sum[nc[a, b, c], it]
};

sumCollect[expr_] := expr //. rulesumCollect;

rulesumSimplifyKD = {
  sum[KroneckerDelta[n1_, n2_] a_., it_List] /; MemberQ[it, n1] :>
    sum[a //. {n1 :> n2}, Complement[it, {n1}]],

  sum[KroneckerDelta[n1_, n2_] a_., it_List] /; MemberQ[it, n2] :>
    sum[a //. {n2 :> n1}, Complement[it, {n2}]],

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

sumSimplifyKD[expr_] := expr //. rulesumSimplifyKD;

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
        rule = Table[it[[i]] :> ndxfunc[i], {i, len}];
        sum[a //. rule, it //. rule]
      ];
  expr /. r
];

sumNameIndex::usage =
"sumNameIndex[expr] is to be used in conjunction with
sumAbstractIndex[expr].";

sumNameIndex[expr_, ndxfunc_, li_List] := Module[ {len},
  len = Length[li];
  expr //. Table[ndxfunc[i] :> li[[i]], {i, len}]
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
  TransformationFunctions -> Prepend[sumsimplfuncs, sumFullSimplifyOLD] ];


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

vev[_?bosonQ[__]] := 0;

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


(* Special rules for bosons *)

  (* VEV of a string of identical bosonic operators must be zero.
  The equivalent rule for fermionic operators is not necessary, due
  to Pauli exclusion rule with is enforced at all times in the definition
  of nc[] multiplication. *)

vev[ HoldPattern[ nc[op_?bosonQ[___]..] ] ] := 0;


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
  KroneckerDelta[op1, op2] KroneckerDelta[{j1}, {j2}] KroneckerDelta[k1, k2] *
  (If[i1 == CR && i2 == AN, UnitStep[-k1], 0]
  + If[i1 == AN && i2 == CR, UnitStep[k1], 0]) /;
  fermionQ[op1] && fermionQ[op2] &&
  ordering[op1] === SEA && ordering[op2] === SEA;


(*** Inner and outer products using nc ***)

inner[a_, b_, c___] := Inner[nc, a, b, c];
outer[a_, b_, c___] := Outer[nc, a, b, c];

(* Vector-matrix-vector product using inner multiplication *)
VMV[v1_, m_, v2_] := (v1 ~ inner ~ m) ~ inner ~ v2;

(* Direct product of two lists. May be used in in basis construction. *)
direct[l1_List, l2_List] := Flatten @ outer[l1, l2];

(* DEPRECATED: mdot[a_, b_] := Inner[nc, a, b]; *)


(*** Auxiliary functions for operator construction ***)

(* Number operator *)
SetAttributes[number, Listable];

number[op_?fermionQ[j___], sigma_] := op[CR, j, sigma] ~ nc ~ op[AN, j, sigma];
number[op_?fermionQ[j___]] /; spinof[op] == 1/2 :=
  number[op[j], UP] + number[op[j], DO];
number[op_?fermionQ[j___]] /; spinof[op] != 1/2 :=
  Sum[number[op[j], s], {s, -spinof[op], +spinof[op]}];

(* Number operator for abstract function argument *)
number[fn_Function, sigma_] := fn[CR, sigma] ~ nc ~ fn[AN, sigma];
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
hubbard[op_?fermionQ[j___]] /; spinof[op] != 1/2 := Module[{smin, smax},
  smin = -spinof[op];
  smax = +spinof[op];
  Sum[Sum[number[op[j], s1] ~ nc ~ number[op[j], s2],
    {s1, s2+1, smax}], {s2, smin, smax}]
];

hubbard[op_?fermionQ, a___] /; AtomQ[op] := hubbard[op[], a];

(* The Hubbard model for operator op[], nn sites and parameters
   U, t and eps *)
hamiltonian[Hubbard, op_[j___], nn_, U_, t_, eps_] :=
  Sum[eps number[op[i, j]] + U hubbard[op[i, j]], {i, 1, nn}] +
  t Sum[hop[op[i, j], op[i+1, j]], {i, 1, nn-1}];

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

spinxyzgen[g_, spin_?halfintegerQ] := Module[{cr, an},
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

spinspin[op1_?spinQ[j1___], op2_?spinQ[j2___]] :=
  Sum[nc[op1[j1, i], op2[j2, i]], {i, 3}];

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
  nambar = conj /@ nam;
  1/2 VMV[nam, #, nambar]& /@ {PauliX, PauliY, PauliZ}
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
manyisospin[ops_List, ns1_:Null] := Module[{len, ns, ixyz, ii},
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
basis[op_?fermionQ[j___]] := 
  {1, op[CR,j,UP], op[CR,j,DO], nc[op[CR,j,DO], op[CR,j,UP]]};

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
  (spinof[op1] == spinof[op2] != 1/2) :=
  Sum[ hop[op1[j1], op2[j2], s], {s, -spinof[op1], spinof[op1]} ];

hop[fn1_Function, fn2_Function, sigma_] :=
  fn1[CR, sigma] ~ nc ~ fn2[AN, sigma] +
  fn2[CR, sigma] ~ nc ~ fn1[AN, sigma];

hop[fn1_Function, fn2_Function] :=
  hop[fn1, fn2, UP] + hop[fn1, fn2, DO];

hop[fn1_Function, op2_?fermionQ[j2___]] := hop[fn1, op2[#1, j2, #2]&];
hop[op1_?fermionQ[j1___], fn2_Function] := hop[op1[#1, j1, #2]&, fn2];

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

anomaloushop[fn1_Function, fn2_Function, sigma_] :=
  fn1[CR, sigma]   ~ nc ~ fn2[CR, 1-sigma] +
  fn2[AN, 1-sigma] ~ nc ~ fn1[AN, sigma];

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

anhop[fn1_Function, fn2_Function, sigma_] :=
  fn1[CR, sigma]   ~ nc ~ fn2[CR, 1-sigma] +
  fn2[AN, 1-sigma] ~ nc ~ fn1[AN, sigma];

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
spinfliphop[op1_?fermionQ[j1___], op2_?fermionQ[j2___]] := Sum[
  op1[CR, j1, sigma] ~ nc ~ op2[AN, j2, 1-sigma] +
  op2[CR, j2, sigma] ~ nc ~ op1[AN, j1, 1-sigma],
{sigma, DO, UP} ];

(* Hopping with hole operators:
c^\dag_{k,\sigma} f_\sigma} + f^\dag_\sigma c_{k\sigma}
\to
h_{k,-\sigma} f_\sigma + f^\dag_\sigma h^\dag_{k,-\sigma}
*)

SetAttributes[holehop, Listable];
holehop[oph_?fermionQ[jh___], opf_?fermionQ[jf___], sigma_] :=
oph[AN, jh, sigma] ~ nc ~ opf[AN, jf, 1-sigma] +
opf[CR, jf, sigma] ~ nc ~ oph[CR, jh, 1-sigma];

holehop[oph_?fermionQ[jh___], opf_?fermionQ[jf___]] :=
  Sum[holehop[oph[jh], opf[jf], sigma], {sigma, DO, UP}];

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
  Sum[Exp[I phi] op1[CR, j1, sigma]~nc~op2[AN, j2, 1 - sigma] +
  Exp[-I phi] op2[CR, j2, sigma]~nc~op1[AN, j1, 1 - sigma],
        {sigma, DO, UP}];

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
mbfunc[op_[j___]] /; spinof[op] != 1/2 :=
  Table[ op[CR, j, s], {s, +spinof[op], -spinof[op], -1} ];


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
vc2ops[vc[l__, ket[k__]]] := nc[vc2ops[vc[l]], ket[k]];

vc2ops::incom = "Incompatible lenghts of vc[] and BASIS.";
vc2ops[x:vc[l__]] := Module[{a},
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

scalarproductvc[a_vc, b_vc] := If[a === b, 1, 0];

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
nonebasisvc[l_List] := Module[{nr},
  makebasis[l];
  nr = Length[l];
  {{{}, Table[ vc @@ IntegerDigits[i, 2, 2nr], {i, 0, 4^nr-1}]}}
];

nonebasis[l_List] := bzvc2bzop @ nonebasisvc[l];

(********** BASIS with U(1) charge conservation symmetry **********)

(* Make a basis with good quantum number Q *)

qbasisvc[l_List] := Module[{nr, b},
  makebasis[l];
  nr = Length[l];
  b = Table[ vc @@ IntegerDigits[i, 2, 2nr], {i, 0, 4^nr-1}];
  b = Map[{{Count[#, 1, {1}] - nr}, {#}}&, b];
  b = mergebasis[b];
  b
];

qbasis[l_List] := bzvc2bzop @ qbasisvc[l];


(************* (Q,S) BASIS ***************)

(* Make a basis of good quantum numbers Q and S *)
(* Convention: always use the highest possible Sz, i.e. Sz == S ! *)

qsbasisvcold[l_List] := bzop2bzvc[ qsbasis[l] ];


qsbasisvc[l_List] := Module[{},
  makebasis[l];
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
qsbasis[l_List] := Module[{},
  makebasis[l];
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
  b / normop[b]
];

(* Smarter spindown: drops parts which annihilate vacuum and "normalizes"
the remainder *)
spindown::Usage = "spindown[v] effectively applies the spin-lowering
operator to the state v in the creation operator representation.";
spindown[a_] := Module[{b},
  b = zeroonvac[SPINDOWN[a]];
  b / normop[b]
];


(* spindown function for states in the occupation number representation *)
spindownvc::Usage = "spindownvc[v] effectively applies the spin-lowering
operator to the state v in the occupation number representation.";

sneglinearoperator[spindownvc1];
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
"keymerge[list1, list2] merges two lists, so that the sublists with
the same first element are combined.";
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
projectorEphi[op_?AtomQ] := projectorEphi[op[]];
projectorOphi[op_?AtomQ] := projectorOphi[op[]];


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
  l = Select[l, Norm[#] != 0 &]; (* Drop zero vectors *)
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
  l = Select[l, Norm[#] != 0 &]; (* Drop zero vectors *)
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

(* Trick: bra and ket from orthogonal Hilbert spaces: we may commute them! *)
nc[a___, x1:bra[i1___], x2:ket[i2___], b___] /;
   compatiblepattern[{i1}, {i2}] := nc[a, x2, x1, b];

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

spinbasis[{a_?halfintegerQ, b__}] :=
  basistensorproduct[spinbasis[a, {1, 0}],
                     spinbasis[b, {0, 1}],
                     Function[{sz1, sz2}, sz1+sz2] ];

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

(* Tensor product of two operator expressions in terms of ket,bra terms *)
ketbratensorproduct[x1_, x2_] := Module[{y1, y2},
  y1 = x1 /. {
    ket[a__] :> ket[a, Null],
    bra[a__] :> bra[a, Null]
  };
  y2 = x2 /. {
    ket[a__] :> ket[Null, a],
    bra[a__] :> bra[Null, a]
  };
  nc[y1, y2]
];

ketbratensorproduct[x_] := x;

(* Merge argument lists of neighboring bra/ket terms. *)
ruletensor = {
  nc[a___, bra[i__], bra[j__], b___] :> nc[a, bra[i, j], b],
  nc[a___, ket[i__], ket[j__], b___] :> nc[a, ket[i, j], b]
};

(* Direct product of a fermionic basis and a (single) phonon basis
with up to Nph phonons. *)

transformtoPH[bvc_List, Nph_Integer] := Module[{vecs, fn, phbasis},
  phbasis = phononbasis[Nph];
  fn[x_] = nc[x, #]&;
  vecs = Map[applybasis[bvc, fn[#]]&, phbasis];
  mergebasis @ Flatten[vecs, 1]
];

phononbasis[Nph_] := Table[ket[i], {i, 0, Nph}];

(* Phonon hamiltonian construction functions *)
phononnumber[Nph_Integer] := Sum[i nc[ket[i], bra[i]], {i, 0, Nph}];
phononplus[Nph_Integer] := Sum[ Sqrt[i+1] nc[ket[i+1], bra[i]], {i, 0, Nph-1}];
phononminus[Nph_Integer] := Sum[ Sqrt[i] nc[ket[i-1], bra[i]], {i, 1, Nph}];
phononx[Nph_Integer] := phononplus[Nph] + phononminus[Nph];

(* Extension to multiple phonons *)
phononbasis[cutoffs : {_Integer ..}] :=
  Flatten[Outer[nc[#1, #2] /. ruletensor &, Sequence @@ Map[phononbasis, cutoffs]], 1];

phononid[Nph_Integer] := Sum[nc[ket[i], bra[i]], {i, 0, Nph}];

phononnumber[i_, cutoffs : {_Integer ..}] := Module[{},
  nr = Length[cutoffs];
  ops = Table[If[j == i, phononnumber, phononid], {j, nr}];
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononplus[i_, cutoffs : {_Integer ..}] := Module[{},
  nr = Length[cutoffs];
  ops = Table[If[j == i, phononplus, phononid], {j, nr}];
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononminus[i_, cutoffs : {_Integer ..}] := Module[{},
  nr = Length[cutoffs];
  ops = Table[If[j == i, phononminus, phononid], {j, nr}];
  ketbratensorproduct @@ MapThread[#1[#2] &, {ops, cutoffs}]
];

phononx[i_, cutoffs : {_Integer ..}] := Module[{},
  nr = Length[cutoffs];
  ops = Table[If[j == i, phononx, phononid], {j, nr}];
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
snegold2newrules[oldbasis_, newbasis_, rules_] := Module[{n, ob, mat, x},
  n = Length[oldbasis];
  If[n != Length[newbasis],
    Print["Mismatching lengths."];
    Return[];
  ];
  ob = Table[oldbasis[[i]] :> enain[i, n], {i, Length[oldbasis]}];
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
    (ordering[op] =!= NONE) :=
       nc[x1, x2] - normalorder[x1 ~ nc ~ x2];

(* Contract one pair.  ctr is an order index that replaces the contracted
operators in the operator string, factor is the prefactor of the string and
ops is the string that we are currently applying the contraction to. *)

contractone[{ctr_, factor_, ops_List}, {i1_, i2_}] :=
  Module[{l = ops, c},
    (* Preserve order! *)
    If[i1 < i2,
      c = contraction[l[[i1]], l[[i2]]],
      c = contraction[l[[i2]], l[[i1]]]
    ];
    l[[i1]] = ctr;
    l[[i2]] = ctr;
    {ctr + 1, c*factor, l}
  ];

(* Return the remaining operator string. The integer number indexed
placeholders are commuted to the beginning of the list and ordered:
this gives the correct sign of the remaining operator string. Sign is
flipped whenever two neighboring integers are transposed, and when an
integer and a neighboring operator are transposed. Note: List2 is a
non-Listable List. *)

remainder[{ops__}] := Module[{ops2},
  ops2 = List2[ops] //. {
    List2[a___, nr1_Integer, nr2_Integer, b___] :>
        -List2[a, nr2, nr1, b] /; nr2 < nr1,
    List2[a___, x : c_[i__], nr_Integer, b___] :>
        -List2[a, nr, x, b] /; operatorQ[c]
  };
  ops2 = ops2 /. {x : List2[i__] :> DeleteCases[x, _Integer, {1}]};
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
vevwick2[HoldPattern[x : nc[l__]]] := Module[{ic, ia, ndc, nda},
  ic = indecesCR[x];
  ia = indecesAN[x];
  If[Length[ic] != Length[ia], Return[0]];
  (* Bug trap *)
  ndc = Check[Extract[x, Map[{#} &, ic]] /. op_[CR, i___] :> {op, i},
              Null];
  nda = Check[Extract[x, Map[{#} &, ia]] /. op_[AN, i___] :> {op, i},
              Null];
  If[ndc === Null || nda === Null, Return[Problem[{l}]] ];
  If[Sort[ndc] =!= Sort[nda], Return[0]];
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
bzQ[{ {{__},{___}}.. }] = True;
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

mambpair[{qn1_, vecs1_}, {qn2_, vecs2_}, op_, FNC_] := Module[{pair, matrep},
  pair = {qn1, qn2};
  matrep = FNC[op, vecs1, vecs2];
  (* TRICK: Drop combinations of subspaces that do not contribute. *)
  tot = Total[Abs[matrep], 2];
  If[tot =!= 0, {pair, matrep}, HoldComplete[Sequence[]]]
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
  expr //. {nc[op_[CR, j___, sigma_], op_[AN, j___, sigma_]] :>
              HoldForm[number[op[j], sigma]] /; operatorQ[op],
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

    rule1  = Thread[(spinxyz[op1[i1___]] /. ir1) :> (l1 /. ir2)];
    rule1a = Thread[(Expand[spinxyz[op1[i1___]]] /. ir1) :> (l1 /. ir2)];
    rule1b = Thread[(Expand[2spinxyz[op1[i1___]]] /. ir1) :> (2l1 /. ir2)];
    rule1c = Thread[(Expand[I spinxyz[op1[i1___]]] /. ir1) :> (I l1 /. ir2)];
    rule1d = Thread[(Expand[2I spinxyz[op1[i1___]]] /. ir1) :> (2I l1 /. ir2)];
    rule1e = Thread[(Simplify[2I spinxyz[op1[i1___]]] /. ir1) :> (2I l1 /. ir2)];
    rule1f = Thread[(Simplify[-2I spinxyz[op1[i1___]]] /. ir1) :> (-2I l1 /. ir2)];

    (* Rules for products of spin operators *)

    rule2 = Thread[
          Flatten[Expand[
                  4 outer[spinxyz[op1[i1___]], spinxyz[op2[i2___]]]] /.
                ir1, 1] :>
            Flatten[Expand[4 outer[l1, l2]] /. ir2, 1]];

    rule2b = Thread[
          Flatten[Expand[
                  outer[spinxyz[op1[i1___]], spinxyz[op2[i2___]]]] /.
                ir1, 1] :>
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
  rule1 = Expand[z_. hop[op1[i___], op2[j___]] /. ir1] :>
    (z HoldForm[hop[op1[i], op2[j]]] /. ir2);
    rule2 = Expand[z_. twohop[op1[i___], op2[j___]] /. ir1] :>
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

snegSeries[f_[expr_], n_:5] := snegSeries[f, expr, n];
snegSeries[E^expr_, n_:5] := snegSeries[Exp, expr, n];

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
  HoldPattern[KroneckerDelta[x__]^n_Integer] :> KroneckerDelta[x],
  HoldPattern[UnitStep[x__]^n_Integer] :> UnitStep[x]
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
(* UNTESTED! *)
matrixrepresentationvcsparse[a_, l_List] := Module[{},
  SparseArray[Select[Flatten[
    Table[
      (sum2list @ Collect[ap[a, l[[i]]], l]) /.
        x_. v_vc :> ({i, Position[l, v][[1, 1]]} :> x),
      {i, Length[l]}],
    1], (#=!=0)&]]
];

(* Similar to matrixrepresentationvc[], but significantly faster. *)
(* NOTE: only applicable for simple monomial vectors!! *)
matrixrepresentationvcfast[a_, l_List] := Module[{n = Length[l]},
  Map[
    indexvalue2list[
      Sort[
        (sum2list @ Collect[Chop[ap[a, #]], l]) /.
          x_. v_vc :> {Position[l, v][[1,1]], x}
      ],
    n] &,
  l] // Transpose
];

matrixrepresentationvcfast[a_, l1_List, l2_List] := Module[{n = Length[l1]},
  Map[
    indexvalue2list[
      Sort[
        (sum2list @ Collect[Chop[ap[a, #]], l1]) /.
        x_. v_vc :> Module[{pos = Position[l1, v]},
          If[pos==={},0,{pos[[1,1]], x}] ]
      ],
    n] &,
  l2] // Transpose
];

(* Transform a list of {index, value} pairs into a vector with given
values at indexed positions. *)
indexvalue2list[iv_List, n_Integer] :=
  Fold[ReplacePart[#1, Last[#2], First[#2]] &, Table[0, {n}],
  Select[iv, (# =!= 0)&] ];

(* Added 25.3.2011 *)
indexvalue2list[{0}, n_Integer]:= Table[0, {n}];
indexvalue2list[{0.}, n_Integer]:= Table[0, {n}];
indexvalue2list[{0.+0. I}, n_Integer]:= Table[0, {n}];

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

SU2combine[op1_, op2_, {s1_, s2_}, fixspin1fnc_, fixspin2fnc_] :=
  Table[
   {s, Sum[
     With[{cg = ClebschGordan[{s1, sz1}, {s2, sz2}, {s, s}]},
      If[cg =!= 0,
         cg  nc[fixspin1fnc[op1, s1, sz1] , fixspin2fnc[op2, s2, sz2]],
         0]
      ], {sz1, -s1, s1}, {sz2, -s2, s2}]},
   {s, Abs[s1 - s2], s1 + s2}
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
 Module[{oplist, l1, l2, crops, conjcrops, pos, l3, contributions},
  oplist = List @@ l;
  l1 = indecesCR[l];
  l2 = indecesAN[l];

  (* Extract creation operators and conjugate them *)
  crops = Map[l[[#]] &, l1];
  conjcrops = conj /@ crops;

  (* Locate these in the operator list *)
  pos = Map[Position[oplist, #, {1}] &, conjcrops];
  pos = Replace[pos, {n_} :> n, {2}];

  (* Generate all allowed permutations *)
  l3 = allcombinations[pos];
  l3 = Select[l3, DuplicateFreeQ];

  (* Calculate the contractions *)
  contributions = Map[contract[oplist, Transpose[{l1, #}]] &, l3];

  Total[contributions /. {dd[_]->0}]
];

EndPackage[];

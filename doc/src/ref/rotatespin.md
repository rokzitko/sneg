---
keywords: spin rotation, local frame, sublattice rotation, antiferromagnet, spiral, canted spins, Dzyaloshinskii-Moriya interaction
see also: spincomponents, holsteinprimakoff, spinspin, stevens, RotationMatrix
---
# rotatespin

## Usage
- `rotatespin[expr, S, R]` replaces the spin vectors $\mathbf S_j$ in *expr* by $R\, \mathbf S_j$ for all sites *j*, with a $3 \times 3$ matrix *R*.
- `rotatespin[expr, S, f]` uses the matrix `f[j...]` for the site with the indexes *j*....

## Details
- Every operator `S[j..., t]` with $t = 1, \ldots, 5$ is replaced by the corresponding component of $R\, (S^x_j, S^y_j, S^z_j)^T$, i.e. $S^a_j \to \sum_b R_{ab} S^b_j$, and $S^\pm \to (R\mathbf S)^x \pm i (R\mathbf S)^y$. The result is expanded and contains Cartesian components; use `spincomponents` to rewrite it in terms of $S^z$ and $S^\pm$.
- For a rotation matrix (orthogonal, determinant 1), the substitution is implemented by a unitary rotation of the spins and preserves the su(2) algebra; $\mathbf S \cdot \mathbf S'$ of two spins rotated by the same matrix is invariant. Other matrices are not rejected and give general linear transformations of the components.
- The function form describes site-dependent rotations: sublattice rotations for antiferromagnets, spirals, and local frames of canted structures. The function receives the site indexes of `S[j..., t]` (without the component) as its arguments; for `S[R, "B", t]` it is called as `f[R, "B"]`.
- If the matrix is not $3 \times 3$, the message `rotatespin::matrix` is issued and `$Failed` is returned.
- The Dzyaloshinskii-Moriya interaction $\mathbf D \cdot (\mathbf S_1 \times \mathbf S_2)$ can be written with `levicivita` or with the cross product of the component vectors.

## Examples
### Basic Examples
Rotation by $\pi$ about the $x$ axis:
```wl
snegspinoperators[S];
rotatespin[{S[1, 3], S[1, 4]}, S, RotationMatrix[Pi, {1, 0, 0}]]
```
Rotation by the angle φ about the $y$ axis. The angle must be declared as a constant:
```wl
snegrealconstants[φ];
rotatespin[S[1, 3], S, RotationMatrix[φ, {0, 1, 0}]]
```

### Scope
Site-dependent rotation given as a function of the site index:
```wl
snegspinoperators[S];
rotatespin[S[1, 3] + S[2, 3], S, If[# == 2, RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &]
```
---
Sites with several indexes, e.g. a unit cell `R` and a sublattice label:
```wl
snegspinoperators[S];
rotatespin[S[R, "A", 3] + S[R, "B", 3], S, If[Last[{##}] === "B", RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &]
```

### Applications
Antiferromagnetic exchange after rotating the second spin by $\pi$ about $x$: $\mathbf S_1 \cdot \mathbf S_2 \to S^x_1 S^x_2 - S^y_1 S^y_2 - S^z_1 S^z_2$, which has the ferromagnetic classical ground state used in the spin-wave expansion of the antiferromagnet (see `holsteinprimakoff`):
```wl
snegspinoperators[S];
rotatespin[spinspin[S[1], S[2]], S, If[# == 2, RotationMatrix[Pi, {1, 0, 0}], IdentityMatrix[3]] &]
```
In terms of $S^z$ and $S^\pm$, the transverse part becomes anomalous, $\frac12 (S^+_1 S^+_2 + S^-_1 S^-_2)$:
```wl
spincomponents[%]
```
---
Spiral with the pitch angle $Q$ in the $xz$ plane: the exchange $\mathbf S_1 \cdot \mathbf S_2$ in the local frames:
```wl
snegspinoperators[S];
snegrealconstants[Q];
Simplify[spincomponents[rotatespin[spinspin[S[1], S[2]], S, RotationMatrix[(# - 1) Q, {0, 1, 0}] &], "XYZ"]]
```

### Properties & Relations
The scalar product is invariant under a global rotation:
```wl
snegspinoperators[S];
snegrealconstants[θ, φ];
rot = RotationMatrix[θ, {0, 0, 1}] . RotationMatrix[φ, {1, 0, 0}];
Simplify[spincomponents[rotatespin[spinspin[S[1], S[2]], S, rot] - spinspin[S[1], S[2]]]]
```
---
The rotated components obey the su(2) algebra:
```wl
snegspinoperators[S];
snegrealconstants[φ];
r = rotatespin[{S[1, 1], S[1, 2], S[1, 3]}, S, RotationMatrix[φ, {1, 1, 0}/Sqrt[2]]];
Simplify[spincomponents[komutator[r[[1]], r[[2]]] - I r[[3]]]]
```

### Possible Issues
Symbols in the rotation matrix that are not declared as constants are treated as operators, so that the products are not simplified:
```wl
snegspinoperators[S];
rotatespin[nc[S[1, 3], S[2, 3]], S, RotationMatrix[α, {0, 1, 0}]]
```
```wl
snegrealconstants[α];
rotatespin[nc[S[1, 3], S[2, 3]], S, RotationMatrix[α, {0, 1, 0}]]
```
---
The matrix must be $3 \times 3$:
```wl
snegspinoperators[S];
rotatespin[S[1, 3], S, IdentityMatrix[2]]
```

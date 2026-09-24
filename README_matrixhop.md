# Matrix-valued hopping

`matrixhop[T, a[i], b[j]]` constructs the spin-dependent hopping operator
`a† T b + b† T† a`. The dense or sparse 2×2 matrix `T` uses the spin order
`{UP, DO}` for both rows and columns; its entries may be complex and
symbolic. For example:

```wl
Needs["sneg`"]
snegfermionoperators[a, b];
snegrealconstants[t, theta];

T = t MatrixExp[I theta PauliY];
H = matrixhop[T, a[], b[]];
```

The constructor attempts to simplify symbolic matrices by decomposing
them into identity and Pauli components. In this example the decomposition
is `t (Cos[theta] IdentityMatrix[2] + I Sin[theta] PauliY)`, which gives
trigonometric hopping coefficients. This also works for inline
`MatrixExp` calls and honors the current `$Assumptions`.

The endpoints are spin-½ fermionic operators with creation/annihilation
and spin indices omitted, as for `hop`. Abstract-orbital functions are
also accepted: `fn[CR, sigma]` defines the creation expression, and its
conjugate defines annihilation. With `IdentityMatrix[2]` as the matrix,
`matrixhop` reproduces `hop`; with `PauliX`, it reproduces `spinfliphop`.

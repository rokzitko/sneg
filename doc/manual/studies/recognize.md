An interaction written as a linear combination of density and spin-flip monomials may hide a rotationally invariant exchange. Ask `SnegSimplify` to identify local building blocks; inspect the heads of the held constructors without expanding them again:
```wl
snegfermionoperators[c];
snegrealconstants[J,U];
raw = Expand[J spinspin[c[1],c[2]] + U hubbard[c[1]]];
compact = SnegSimplify[raw];
Cases[compact, HoldPattern[HoldForm[h_[___]]] :> SymbolName[h], Infinity]
```
Recognition is useful only if it preserves the expression. Release the display wrappers and test the operator identity rather than comparing printed strings:
```wl
Expand[ReleaseHold[compact] - raw]
```

Reflect a three-site molecule about its centre. In the one-electron sector the two end orbitals form even and odd combinations; the central orbital is even. Construct the occupation-vector basis with definite reflection parity from the operator basis and read its dimensions:
```wl
snegfermionoperators[c];
sites = c /@ Range[3];
b = transformtoLRvc[qsbasis[sites],sites,vacuum[]];
one = Select[b, #[[1,1]] == -2 &];
TableForm[{Sequence @@ First[#],Length[Last[#]]} & /@ one,
 TableHeadings -> {None,{"Q","S","Parity","Multiplets"}}]
```
An impurity attached only to the central site hybridizes with the even channel. A calculation in a single parity block would miss an odd bound state altogether; it is the observable's symmetry, not just that of the Hamiltonian, that decides which blocks are needed.

Take $U=\omega_0=1$ and $g=1/2$ in the Anderson--Holstein atom. In the empty sector the predicted lowest energy is $U/2-g^2/\omega_0=1/4$. With a cutoff of two phonons inspect this sector alone. The off-diagonal elements should be $-g$ and $-\sqrt{2}g$:
```wl
snegfermionoperators[d];
ham[n_] := isozsq[d[]]/2 + phononnumber[n] +
  nc[number[d[]]-1, phononx[n]]/2;
empty[n_] := First[keyselect[makematricesbzvc[ham[n],
  transformtoPH[qsbasisvc[{d[]}],{n}]], {-1,0}]][[2]];
empty[2] // MatrixForm
```
Increasing the cutoff grows the variational space, so its lowest energy approaches the exact answer from above. High precision resolves a small truncation error that a rounded energy would hide:
```wl
rows = Table[With[{e = Min[Eigenvalues[N[empty[n],35]]]},
  {n, N[e,8], N[-Log[10,e-1/4],5]}], {n,{1,2,4,8}}];
TableForm[rows, TableHeadings -> {None,{"Cutoff","Energy","-log10(error)"}}]
```

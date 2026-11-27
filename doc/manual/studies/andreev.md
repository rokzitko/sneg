Replace the superconducting continuum by one paired orbital with gap parameter $\Delta$. Unlike a static pair field on the impurity, this model retains a bath quasiparticle. Compare the lowest states with integer and half-integer spin as the repulsion increases, at $\Delta=1$ and hopping $v=1/2$:
```wl
snegfermionoperators[d,f];
snegrealconstants[U];
hp = U isozsq[d[]]/2 - bcs[f[]] + hop[f[],d[]]/2;
mm = makematricesbzvc[hp,szbasisvc[{d[],f[]}]];
emin[sz_,u_] := Min[Eigenvalues[N[First[keyselect[mm,{sz}]][[2]] /. U -> u]]];
rows = Table[{u,emin[0,u],emin[1/2,u]}, {u,0,4}];
TableForm[rows,TableHeadings -> {None,{"U","Lowest Sz = 0","Lowest Sz = 1/2"}}]
```
The ordering reverses between $U=0$ and $U=1$. States in the two columns have opposite fermion parity and may cross without mixing. The energies include the constant in the symmetric charging term $U(n_d-1)^2/2$.

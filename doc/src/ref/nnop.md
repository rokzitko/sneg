---
keywords: bipartite lattice, isospin, Wilson chain, phase convention
see also: quickISObasis, quickISOSZbasis, isospinminus, transformQStoIS
---
# nnop

## Usage
- `nnop[level]` specifies the sublattice phase exponent of *level* for the quick isospin basis constructors.

## Details
- Assign integer values before building the basis. Only the parity matters: the coefficient of the local pair operator is $(-1)^{n_i}$. For real hopping between opposite sublattices choose opposite signs. Equal signs still generate a charge SU(2) algebra but generally not a symmetry of that hopping.
- `nnop` is a model definition. It does not infer a sublattice assignment from the Hamiltonian. Give a value for every participating level, including a local impurity.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
nnop[c[1]]=0; nnop[c[2]]=1;
tm = Total[isospinminus[#,nnop[#]] & /@ {c[1],c[2]}];
Expand[komutator[hop[c[1],c[2]],tm]]
```

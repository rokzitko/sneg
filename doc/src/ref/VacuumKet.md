---
keywords: VACUUM, reference ket, Fermi sea, vacuum state, Fock state
see also: vacuum, vc, conj, nc, vev, ordering, ap
---
# VACUUM

## Usage
- `VACUUM` denotes the reference ket $|0\rangle$ in an operator product; `conj[VACUUM]` denotes its bra.

## Details
- Write the ket at the right end of `nc` and its bra at the left end. Their overlap is one. An annihilator acting on the reference ket gives zero, as does a creator acting to the left on the reference bra.
- What annihilates the reference state depends on `ordering`. For a filled Fermi sea a creator below the Fermi surface kills the sea. In the occupation-vector representation use `vacuum[]` and the current `BASIS`.

## Examples
### Basic Examples
```wl
snegfermionoperators[c];
one = nc[c[CR,UP], VACUUM];
{nc[conj[one],one], nc[conj[one],number[c[]],one]}
```
### Applications
The reference sea has both spins below the Fermi point. Creating another electron there is forbidden:
```wl
snegfermionoperators[d];
ordering[d] = SEA;
{nc[d[CR,-1,UP],VACUUM], nc[conj[VACUUM],number[d[-1]],VACUUM]}
```

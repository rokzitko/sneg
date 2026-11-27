<|"Source" -> "snegsesquilinearoperator.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegrealconstants[t];\nsnegcomplexconstants[z];\n\
snegsesquilinearoperator[sp];\nsp[z a + t b, I a]", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"\[ImaginaryI]", " ", 
         RowBox[{"(", RowBox[{RowBox[{RowBox[{"Conjugate", "[", "z", "]"}], 
               " ", RowBox[{"sp", "[", RowBox[{"a", ",", "a"}], "]"}]}], "+", 
             RowBox[{"t", " ", RowBox[{"sp", "[", RowBox[{"b", ",", "a"}], 
                 "]"}]}]}], ")"}]}], "Out[4]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nsnegrealconstants[t];\nsnegcomplexc\
onstants[z];\nsnegsesquilinearoperator[overlap];\noverlap[c[CR, i_, s_], \
c[CR, j_, r_]] := KroneckerDelta[i, j] KroneckerDelta[s, r];\npsi = z c[CR, \
1, UP] + t c[CR, 2, UP];\n{overlap[psi, psi], overlap[psi, c[CR, 1, UP]], \
overlap[c[CR, 1, UP], psi]}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{SuperscriptBox["t", "2"], "+", 
             RowBox[{"z", " ", RowBox[{"Conjugate", "[", "z", "]"}]}]}], ",", 
           RowBox[{"Conjugate", "[", "z", "]"}], ",", "z"}], "}"}], 
       "Out[7]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[t];\nsnegcomplexconstants[z];\npsi = z c[CR, 1, UP] + t c[CR, 2, \
UP];\nvev[nc[conj[psi], psi]]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{SuperscriptBox["t", "2"], "+", 
         RowBox[{"z", " ", RowBox[{"Conjugate", "[", "z", "]"}]}]}], 
       "Out[5]="}}|>}|>

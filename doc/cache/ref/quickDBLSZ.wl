<|"Source" -> "quickDBLSZ.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfermionoperators[c];\nbz = quickDBLSZ[{c[1]}, \
{c[2]}, qszbasis];\n{First[#], Length[Last[#]]} & /@ bz", "Reset" -> True, 
    "InLabel" -> "In[1]:=", "Cells" -> 
     {{"Output", RowBox[{"{", RowBox[
          {RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], 
                   ",", "0", ",", RowBox[{"-", FractionBox["1", "2"]}]}], 
                 "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"-", "1"}], ",", 
                   "0", ",", FractionBox["1", "2"]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[
                  {RowBox[{"-", "1"}], ",", "1", ",", "0"}], "}"}], ",", 
               "1"}], "}"}], ",", RowBox[{"{", RowBox[{RowBox[
                {"{", RowBox[{"0", ",", RowBox[{"-", "1"}], ",", RowBox[
                    {"-", FractionBox["1", "2"]}]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   RowBox[{"-", "1"}], ",", FractionBox["1", "2"]}], "}"}], 
               ",", "1"}], "}"}], ",", RowBox[
            {"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", 
                   RowBox[{"-", "1"}]}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", 
                   "0"}], "}"}], ",", "2"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", 
                   "1"}], "}"}], ",", "1"}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "1", ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "1", ",", FractionBox["1", "2"]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   RowBox[{"-", "1"}], ",", "0"}], "}"}], ",", "1"}], "}"}], 
           ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", "0", ",", 
                   RowBox[{"-", FractionBox["1", "2"]}]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "0", ",", FractionBox["1", "2"]}], "}"}], ",", "1"}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"1", ",", 
                   "1", ",", "0"}], "}"}], ",", "1"}], "}"}]}], "}"}], 
       "Out[3]="}}|>, <|"Code" -> "snegfermionoperators[c];\nsnegrealconstant\
s[J, B];\nbz = quickDBLSZ[{c[1]}, {c[2]}, qszbasis];\nH = J spinspin[c[1], \
c[2]] - B (spinz[c[1]] + spinz[c[2]]);\nSelect[{First[#], \
Eigenvalues[Last[#]]} & /@ makematricesbzvc[H, bzop2bzvc[bz]], #[[1, 1]] == 0 \
&& #[[1, 2]] == 0 &]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0", ",", RowBox[{"-", "1"}]}], "}"}], ",", RowBox[
                {"{", RowBox[{"B", "+", FractionBox["J", "4"]}], "}"}]}], 
             "}"}], ",", RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", 
                   "0", ",", "0"}], "}"}], ",", RowBox[{"{", RowBox[
                  {RowBox[{"-", FractionBox[RowBox[{"3", " ", "J"}], "4"]}], 
                   ",", FractionBox["J", "4"]}], "}"}]}], "}"}], ",", 
           RowBox[{"{", RowBox[{RowBox[{"{", RowBox[{"0", ",", "0", ",", 
                   "1"}], "}"}], ",", RowBox[{"{", RowBox[{FractionBox["1", 
                    "4"], " ", RowBox[{"(", RowBox[{RowBox[{RowBox[{"-", 
                          "4"}], " ", "B"}], "+", "J"}], ")"}]}], "}"}]}], 
             "}"}]}], "}"}], "Out[5]="}}|>, 
   <|"Code" -> "snegfermionoperators[c];\nbz = bzop2bzvc[quickDBLSZ[{c[1]}, \
{c[2]}, qszbasis]];\nMap[Sort, mergebasis[{{#[[1, 1]] + #[[1, 2]], #[[1, \
3]]}, #[[2]]} & /@ Expand[bz]], {2}] === Map[Sort, qszbasisvc[{c[1], c[2]}], \
{2}]", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", "True", "Out[3]="}}|>}|>

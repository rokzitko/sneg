<|"Source" -> "lab-magnons.tex", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegspinoperators[{S, \
1/2}];\nsnegbosonoperators[b];\nsnegrealconstants[hs];\nh = \
Sum[spinspin[S[i], S[Mod[i, 4] + 1]], {i, 4}] - hs Sum[(-1)^(i + 1) S[i, 3], \
{i, 4}];\nh = rotatespin[h, S, If[EvenQ[First[{##}]], RotationMatrix[Pi, {1, \
0, 0}], IdentityMatrix[3]] &];\nhb = holsteinprimakoff[h, S -> b, \
2];\n{\\[Epsilon], T, Egs} = bogoliubov[hb /. hs -> 1/10, Table[b[i], {i, \
4}]];\n{\\[Epsilon], Egs}", "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[
        {"{", RowBox[{RowBox[{"{", RowBox[{"0.45825756949558444`", ",", 
               "0.4582575694955846`", ",", "1.1`", ",", 
               "1.1000000000000003`"}], "}"}], ",", 
           RowBox[{"-", "1.8417424305044152`"}]}], "}"}], "Out[8]=", 
       <|"Kind" -> "List", "Parts" -> {"\\{0.458258,0.458258,1.1,1.1\\}", 
          "-1.84174"}|>}}|>, 
   <|"Code" -> "N[Sqrt[(1 + 1/10)^2 - Cos[{0, Pi/2, Pi/2, Pi}]^2]]", 
    "Reset" -> False, "InLabel" -> "In[9]:=", 
    "Cells" -> {{"Output", RowBox[{"{", RowBox[{"0.458257569495584`", ",", 
           "1.1`", ",", "1.1`", ",", "0.458257569495584`"}], "}"}], 
       "Out[9]=", <|"Kind" -> "List", "Parts" -> {"0.458258", "1.1", "1.1", 
          "0.458258"}|>}}|>}|>

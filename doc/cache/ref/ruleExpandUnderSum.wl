<|"Source" -> "ruleExpandUnderSum.md", "SnegVersion" -> "2.2.0", 
 "Results" -> {<|"Code" -> "snegfreeindexes[k];\nsnegrealfunctions[f, g, \
h];\ns = sum[(f[k] + g[k])/(1 + h[k])^2, {k}];\ns /. ruleExpandUnderSum", 
    "Reset" -> True, "InLabel" -> "In[1]:=", 
    "Cells" -> {{"Output", RowBox[{"sum", "[", 
         RowBox[{RowBox[{FractionBox[RowBox[{"f", "[", "k", "]"}], 
              SuperscriptBox[RowBox[{"(", RowBox[{"1", "+", RowBox[{"h", "[", 
                     "k", "]"}]}], ")"}], "2"]], "+", FractionBox[
              RowBox[{"g", "[", "k", "]"}], SuperscriptBox[RowBox[
                {"(", RowBox[{"1", "+", RowBox[{"h", "[", "k", "]"}]}], 
                 ")"}], "2"]]}], ",", RowBox[{"{", "k", "}"}]}], "]"}], 
       "Out[4]="}}|>, <|"Code" -> "sumExpand[s]", "Reset" -> False, 
    "InLabel" -> "In[5]:=", "Cells" -> 
     {{"Output", RowBox[{RowBox[{"sum", "[", RowBox[
            {FractionBox[RowBox[{"f", "[", "k", "]"}], RowBox[{"1", "+", 
                RowBox[{"2", " ", RowBox[{"h", "[", "k", "]"}]}], "+", 
                SuperscriptBox[RowBox[{"h", "[", "k", "]"}], "2"]}]], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{FractionBox[RowBox[{"g", "[", "k", 
                "]"}], RowBox[{"1", "+", RowBox[{"2", " ", RowBox[{"h", "[", 
                    "k", "]"}]}], "+", SuperscriptBox[RowBox[{"h", "[", "k", 
                   "]"}], "2"]}]], ",", RowBox[{"{", "k", "}"}]}], "]"}]}], 
       "Out[5]="}}|>, 
   <|"Code" -> "s /. ruleExpandUnderSum //. rulesumDistribute", 
    "Reset" -> False, "InLabel" -> "In[6]:=", 
    "Cells" -> {{"Output", RowBox[
        {RowBox[{"sum", "[", RowBox[{FractionBox[RowBox[{"f", "[", "k", 
                "]"}], SuperscriptBox[RowBox[{"(", RowBox[{"1", "+", 
                   RowBox[{"h", "[", "k", "]"}]}], ")"}], "2"]], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}], "+", 
         RowBox[{"sum", "[", RowBox[{FractionBox[RowBox[{"g", "[", "k", 
                "]"}], SuperscriptBox[RowBox[{"(", RowBox[{"1", "+", 
                   RowBox[{"h", "[", "k", "]"}]}], ")"}], "2"]], ",", 
             RowBox[{"{", "k", "}"}]}], "]"}]}], "Out[6]="}}|>}|>

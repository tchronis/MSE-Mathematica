(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



(* ::Input::Initialization:: *)
ClearAll[Cx];
Cx::usage="Cx[n] creates a list of n variables named x1,x2,...,xn.";
Cx[n_]:=Table[Symbol["x"<>ToString[i]],{i,n}];


(* ::Input::Initialization:: *)
ClearAll[payoff];
payoff::usage="payoff[m,i,j] returns the payoff of i-upstream and j-upstream in the m-market. 
It is used when the streams of u and d agents are imported separately (i.e. not in the precomputed format). It is assumed that u , d , and noAttr have been already assigned.";
payoff[m_,i_,j_]:=(Prepend[Cx[noAttr-1],1]u[[m,i]]).d[[m,j]];


(* ::Input::Initialization:: *)
ClearAll[payoffDM];(*DM from Distance Matrix*)
payoffDM::usage="payoffDM[m,i,j] returns the payoff of i-upstream and j-upstream in the m-market.\r
It is used in the case of precomputed data. It is assumed that noAttr and distanceMatrices have been already assigned.";
payoffDM[m_,i_,j_]:=Prepend[Cx[noAttr-1],1].distanceMatrices[[m,i,j]];


(* ::Input::Initialization:: *)
(*C in front of the name means create*)
ClearAll[CpayoffMatrix];
CpayoffMatrix::usage="CpayoffMatrix[payoff(or payoffDM),noM_:noM,noU_:noU,noD_:noD,parallel_:False] calculates and assigns the payoffMatrix.\r 
payoff is used when input data consist of separate u and d streams.\r
payoffDM is used for precomputed data.\r

CpayoffMatrix[solution_?VectorQ] substitutes the solution to all payoffMatrix's entries.
";
CpayoffMatrix[payoff_,noM_:noM,noU_:noU,noD_:noD,p_:False]:=
payoffMatrix=
If[p==False,
Table[payoff[m,i,j]
,{m,1,noM},{i,noU[[m]]},{j,noD[[m]]}],
ParallelTable[payoff[m,i,j]
,{m,1,noM},{i,noU[[m]]},{j,noD[[m]]}]
]

CpayoffMatrix[solution_?VectorQ]:=
If[(Length@solution)===noAttr-1,
payoffMatrix=(payoffMatrix/.Thread[Cx[noAttr-1]->solution])
,
Print["There is some problem with your input. Couldn't calculate anything meaningful."]
]


(* ::Input::Initialization:: *)
(*C in front of the name means create*)
ClearAll[Ctotalpayoff];
Ctotalpayoff::usage="
Ctotalpayoff[payoffobject,mates] calculates the total payoff (i.e. the sum of payoffs) across all markets for the specific mates arrangement. This function accepts as a first argument the \[OpenCurlyDoubleQuote]payffobject\[CloseCurlyDoubleQuote], which can either be the name of the payoff function or the payoffMatrix (in case we have already calculated all pair payoffs).";
Ctotalpayoff[payoffobject_,mates_]:=
totalpayoff=
Total/@Switch[Head@payoffobject,
(*payoff function*)Symbol,Map[payoffobject@@#&,mates,{2}],
(*payoffMatrix*)List,Map[Part@@Join[{payoffobject},#]&,mates,{2}]
]




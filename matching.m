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
Echo["Loaded matching.m"];


(* ::Input::Initialization:: *)
ClearAll[generateAssignmentMatrix];
Options[generateAssignmentMatrix]={linearProgrammingOptions->Null,rationalizeResult->True,domain->Integers};
generateAssignmentMatrix::usage="generateAssignmentMatrix[payoffs,quotaU,quotaD], generates the optimal assignment of matches from the given matrix of payoffs for each match. The optimal assignment is the one that maximizes the total payoff (i.e. the sum of all payoffs) in a market. In an assignment matrix, each entry (i,j) is 1 if i and j are matched and 0 otherwise. The quota can be a number (the same for all streams) or a list that sets a specific quota per agent.
Notice that the quota is max number of matches per agent ( so in the data the real number of matches could be lower than the max).";
generateAssignmentMatrix[payoffs_,quotaU_:1,quotaD_:1,options___?OptionQ]:=
Block[{realorint,rationalize,lpOptions,numU,numD,m1,m2,m3,m,b,matchMatrix,result},{realorint,rationalize,lpOptions}={domain,rationalizeResult,linearProgrammingOptions}/.Flatten[{options,Options[generateAssignmentMatrix]}];
{numU,numD}=Dimensions[payoffs];

(*In this function,there are two sides to the market,U (pstream) and D (ownstream).i will index upstream firms and j will index downstream firms.*)

(*m1 is the first third of the constraint matrix.It represents the constraint\sum{x_ {ij}}_ {i=1}^n\[LessEqual]quotaD for all j*)

m1=SparseArray[ArrayFlatten[Outer[Times,SparseArray[IdentityMatrix[numU]],{Table[1,{numD}]}]]];

(*m2 is the second third of the constraint matrix.It represents the constraint\sum{x_ {ij}}_ {j=1}^n\[LessEqual]quotaU for all i*)

m2=SparseArray[ArrayFlatten[Outer[Times,{Table[1,{numU}]},SparseArray[IdentityMatrix[numD]]]]];

(*m3 is the last third of the constraint matrix.It represents the constraint that each pair of people can only match with each other once.*)m3=SparseArray[{{i_,i_}->1},{numU*numD,numU*numD}];

m=SparseArray[ArrayFlatten[{{m1},{m2},{m3}}]];

b=Join[
If[ListQ@quotaU,quotaU,Table[quotaU,{numU}]],
If[ListQ@quotaD,quotaD,Table[quotaD,{numD}]],
Table[1,{numU*numD}]];

(*Remember:linear programming will minimize,so we need to negate the "c" vector.Similarly,since the constraint is taken to be m.x\[GreaterEqual]b,we need to negate m and b as well.*)

If[lpOptions!=Null,
matchMatrix=LinearProgramming[-Flatten[payoffs],-m,-b,lpOptions,realorint],
matchMatrix=LinearProgramming[-Flatten[payoffs],-m,-b,0,realorint]];
result=Partition[matchMatrix,numD];
If[rationalize,Map[Rationalize[#,.01]&,result,{2}],result]
];
Information[generateAssignmentMatrix,LongForm->False]


(* ::Input::Initialization:: *)
ClearAll[CmatchMatrix];
CmatchMatrix::usage="CmatchMatrix[payoffMatrix_,quotaU_:1,quotaD_:1,p_:0]  
Calculates and creates/updates the global variable 'matchMatrix'.
If p is set to 0, it creates the matchMatrix by running the generateAssignmentMatrix routine for all markets.
If p is set to an integer from 1 to the 'number of Markets' then the p'th element of the matchMatrix is calculated.";
CmatchMatrix[payoffMatrix_,quotaU_:1,quotaD_:1,p_:False]:=
If[IntegerQ@p,
If[1<= p<=Length@payoffMatrix,
If[Definition@matchMatrix==Null,matchMatrix=Table[Null,{Length@payoffMatrix}]];
matchMatrix[[p]]=
generateAssignmentMatrix[payoffMatrix[[p]],
If[ListQ@quotaU,quotaU[[p]],quotaU],
If[ListQ@quotaD,quotaD[[p]],quotaD]
];matchMatrix
,
Print["Invalid market index!"]
]
,
matchMatrix=
MapIndexed[generateAssignmentMatrix[#1,
If[ListQ@quotaU,quotaU[[First[#2]]],quotaU],
If[ListQ@quotaD,quotaD[[First[#2]]],quotaD]
]&,payoffMatrix]
];
Information[CmatchMatrix,LongForm->False]


(* ::Input::Initialization:: *)
ClearAll[Cquota,quota];
Cquota::usage="Cquota[matchMatrix,function_:<|\"upstream\"\[Rule]True,\"downstream\"\[Rule]True|>] Calculates and creates/updates the global variable 'quota'.By default it calculates both streams quotas. It returns the association list quota = <|\"upstream\"->quotaU,\"downstream\"->quotaD|>. Quota is defined for each stream u and d.";
Cquota[matchMatrix_,function_:<|"upstream"->True,"downstream"->True|>]:=
Block[{},
Switch[Head@quota,
Symbol,
Print["quota is calculated for the first time. Initializing..."];
quota=<|
"upstream"->((Total/@#)&/@matchMatrix),
"downstream"->((Total/@(Transpose@#))&/@matchMatrix)
|>
,
Association,
If[function["upstream"],
quota["upstream"]=((Total/@#)&/@matchMatrix)];
If[function["downstream"],
quota["downstream"]=((Total/@(Transpose@#))&/@matchMatrix)];
];
quota
];
Information[Cquota,LongForm->False]


(* ::Input::Initialization:: *)
(*C in front of the name means create*)
ClearAll[Cmates];
Cmates::usage="Cmates[matchMatrix] simplifies the matchMatrix to a list of triples that define matches across all markets. It provides another way to express all the matching information that is, which upstream is matched with which downstream and in which market. The output consists of a list of lists of triples each of which has the following structure: {market_index,upstream_index,downstream_index}.
Output example:{{{1,1,3},{1,3,1},{1,3,2}},{{2,1,1},{2,2,1},{2,2,3},{2,3,2}}}. In this example we have 2 lists, one per market and each inner list contains the triples. Note that in market 1, upstream agent 2 is not contributing which is fine. 
This function is mainly used for the calculation of the total payoff - see Ctotalpayoff routine.";
Cmates[matchMatrix_]:=mates=GatherBy[Position[matchMatrix,1,{3}],First];
Information[Cmates,LongForm->False]


(* ::Input::Initialization:: *)
(*C in front of the name means create*)
ClearAll[Cmate];
Cmate::usage="Cmate[matchMatrix] simplifies the matchMatrix from the original code by Santiago & Fox, to a matrix format which consists of lists of pairs, one pair per market. Here, each pair has the following structure: {{{1},{2},...,{noU within this market}},{{downstreams that are matched with upstream1},{downstreams that are matched with upstream2},...,{downstreams that are matched with upstream noU}}.
Example : {{{{1},{2},{3}},{{3},{},{1,2}}},{{{1},{2},{3}},{{1},{1,3},{2}}}}.  In this example, there are three upstream and three downstream agents in each market, indexed 1, 2, 3. In the first market, upstream 1 is matched with downstream 3, upstream 2 is not matched, and upstream 3 is matched with downstream 1 and  2. In the second market, upstream 1 is matched with downstream 1, upstream 2 with downstream 1 and 3, and upstream 3 with downstream 2.  
The mate=Cmate[matchMatrix] is later fed into the Cineqmembers routine.";
Cmate[matchMatrix_]:=mate=((Transpose@MapIndexed[{{First@#2},Flatten@Position[#1,1]}&,#])&/@matchMatrix);
Information[Cmate,LongForm->False]




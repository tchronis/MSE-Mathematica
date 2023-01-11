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
Echo["Loaded modifydata.m"];


(* ::Input::Initialization:: *)
ClearAll[store];
store::usage="store[var,printflag] is used for storing all global variables to \"var\" global variable before they are modified.  In that case they can restored later (with the restore[var] command).
Variables: {\"header\",\"noM\",\"noU\",\"noD\",\"noAttr\",\"distanceMatrices\",\"matchMatrix\",\"mate\",\"quota\",\"payoffMatrix\",\"totalpayoff\",\"dataArray\"}}";
SetAttributes[store,HoldFirst];
store[var_:stored,printflag_:False]:=Module[{keys={"header","noM","noU","noD","noAttr","distanceMatrices","matchMatrix","mate","quota","payoffMatrix","totalpayoff","dataArray"}},
(*assign[stored,#,Symbol[#]]&/@keys;*)
var=<||>;
(var[#]=Symbol[#])&/@ keys;
If[printflag,Print["Stored ",ByteCount[var]," bytes to \""<>SymbolName[Unevaluated[var]]<>"\" Association List: \n header, noM, noU, noD, noAttr, distanceMatrices, matchMatrix, mate, quota, payoffMatrix, dataArray"];
];
];
Information[store,LongForm->False]


(* ::Input::Initialization:: *)
ClearAll[restore];
restore::usage="restore[var,printflag] is used to restore all global variables from \"var\" global variable (when the last store[] command was used).
Variables: {\"header\",\"noM\",\"noU\",\"noD\",\"noAttr\",\"distanceMatrices\",\"matchMatrix\",\"mate\",\"quota\",\"payoffMatrix\",\"totalpayoff\",\"dataArray\"}}";
SetAttributes[restore,HoldFirst];
restore[var_:stored,printflag_:False]:=Module[{keys={"header","noM","noU","noD","noAttr","distanceMatrices","matchMatrix","mate","quota","payoffMatrix","totalpayoff","dataArray"}},
(MakeExpression@#/._[s_]:>(s=var[#]))&/@keys;
If[printflag,Print["Restored ",ByteCount[var]," bytes from \""<>SymbolName[Unevaluated[var]]<>"\" Association List: \n header, noM, noU, noD, noAttr, distanceMatrices, matchMatrix, mate, quota, payoffMatrix, dataArray"];
]
];
Information[restore,LongForm->False]


(* ::Input::Initialization:: *)
ClearAll[modify];
modify::usage="modify[m_,u_List,d_List,function_?AssociationQ:<|\"unmatch\"\[Rule]False,\"remove\"\[Rule]True,\"quota_reset\"\[Rule]False,\"quota_update\"\[Rule]False,\"quota_update_upstream\"->False,\"quota_downstream_upstream\"->False,\"rematch\"\[Rule]False|>] 
modifies m's market upstream and/or downstream members. As a consequence payoffMatrix, matchMatrix, quota are modified.
If \"unmatch\"\[Rule]True then it is supposed that the Transpose[u,d] are the matches we need to unmatch.
If \"unmatch\"\[Rule]True and \"quota_update_upstream\"\[Rule]True then the quota of the unmatched upstreams are reduced by one.
If \"unmatch\"\[Rule]True and \"quota_update_downstream\"\[Rule]True then the quota of the unmatched downstreams are reduced by one.
If \"remove\"\[Rule]True and \"quota_reset\"\[Rule]True then the quota of the selected for remove streams becomes equal to 0.
If \"remove\"\[Rule]True and \"quota_update\"\[Rule]True then the quota of the matched opposite stream are reduced because of the stream removal.
If \"rematch\"\[Rule]True then the matchMatrix of the m'th market is re-calculated using the set quota.";
modify[m_,u_List,d_List,function_:<|"unmatch"->False,"remove"->True,"quota_reset"->False,"quota_update"->False,"quota_update_upstream"->False,"quota_update_downstream"->False,"rematch"->False|>]:=Block[{keep,theirdownstream,theirupstream},
If[
function["unmatch"],

If[Length[u]!=Length[d],Print["Error in data. u list and d list must have the same Length."," u = ",u," d = ",d];Return[]];
If[matchMatrix[[m,u[[#]],d[[#]]]]!=1,Print["Warning: "," In Market ",m," upstream ",u[[#]], " does not match with downstream  ",d[[#]]];]&/@Range[Length[u]];

(matchMatrix[[m,u[[#]],d[[#]]]]=0)&/@Range[Length[u]];
Cmate[matchMatrix];

(*Quota update if set*)
If[function["quota_update_upstream"],
Cquota[matchMatrix,<|"upstream"->True,"downstream"->False|>]];
If[function["quota_update_downstream"],
Cquota[matchMatrix,<|"upstream"->False,"downstream"->True|>]];

,

If[function["remove"],
If[u!={},
If[function["quota_update"],
theirdownstream=mate[[m]][[2,u]];
quota[[2,m,#]]--&/@Flatten[theirdownstream]
];

If[function["quota_reset"],
quota[[1,m,u]]=Table[0,{Length@u}];
matchMatrix[[m,u]]=Table[0,{Length[u]},{noD[[m]]}];Cmate[matchMatrix];
,
keep=Complement[Range[Dimensions[payoffMatrix[[m]]][[1]]],u];
payoffMatrix[[m]]=payoffMatrix[[m,keep]];
matchMatrix[[m]]=matchMatrix[[m,keep]];Cmate[matchMatrix];
quota[[1,m]]=quota[[1,m,keep]];
noU[[m]]=noU[[m]]-Length[u];
]
];
If[d!={},
If[function["quota_update"],
theirupstream=Position[matchMatrix[[m,All,#]],1]&/@d;
quota[[1,m,#]]--&/@Flatten[theirupstream]
];
If[function["quota_reset"],
quota[[2,m,d]]=Table[0,{Length@d}];
matchMatrix[[m,All,d]]=Table[0,{noU[[m]]},{Length[d]}];Cmate[matchMatrix];
,
keep=Complement[Range[Dimensions[payoffMatrix[[m]]][[2]]],d];
payoffMatrix[[m]]=payoffMatrix[[m,All,keep]];
matchMatrix[[m]]=matchMatrix[[m,All,keep]];Cmate[matchMatrix];
quota[[2,m]]=quota[[2,m,keep]];
noD[[m]]=noD[[m]]-Length[d];
]
]
]


];

If[function["rematch"],
CmatchMatrix[payoffMatrix,quota["upstream"],quota["downstream"],m];
Cmate[matchMatrix];
];
];
Information[modify,LongForm->False]


(* ::Input::Initialization:: *)
ClearAll[removeU];
removeU::usage="removeU[m_,u_List,match_:False] removes from the payoffMatrix's market m the upstreams that are included in the list u. If the match flag is set to True then they are removed also from the matchMatrix. This is preferred on the precomputed case where we do not have to change noU and noD lists.";
removeU[m_,u_List,match_:False]:=Block[{keep},
keep=Complement[Range[Dimensions[payoffMatrix[[m]]][[1]]],u];
payoffMatrix[[m]]=payoffMatrix[[m,keep]];
quota[[1,m]]=quota[[1,m,keep]];
If[match,matchMatrix[[m]]=matchMatrix[[m,keep]]];
];


(* ::Input::Initialization:: *)
ClearAll[removeD];
removeD::usage="removeD[m_,d_List,match_:False] removes from the payoffMatrix's market m the downstreams that are included in the list d. If the match flag is set to True then they are removed also from the matchMatrix. This is preferred on the precomputed case where we do not have to change noU and noD lists.";
removeD[m_,d_List,match_:False]:=Block[{keep},
keep=Complement[Range[Dimensions[payoffMatrix[[m]]][[2]]],d];
payoffMatrix[[m]]=payoffMatrix[[m,All,keep]];
quota[[2,m]]=quota[[2,m,keep]];
If[match,matchMatrix[[m]]=matchMatrix[[m,All,keep]]];
];


(* ::Input::Initialization:: *)
ClearAll[removeUD];
removeUD::usage="removeUD[m_,u_List,d_List,match_:False] combines removeU and removeD in one step";
removeUD[m_,u_List,d_List,match_:False]:=(
removeU[m,u,match];
removeD[m,d,match];
)




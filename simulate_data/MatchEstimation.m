(* ::Package:: *)

BeginPackage["MatchEstimation`", "Combinatorica`", "Histograms`", "MultivariateStatistics`"]


(*
	vec - Takes a matrix and stacks the columns into a vector. 
*)			 	
vec[mtx_] := Flatten[Transpose[mtx]]; 


(*
	unvec - Takes a vector and "unstacks" it into a matrix with the
			number of rows given by the rows parameter. 
*)
unvec[v_, rows_] := Transpose[Partition[v, rows]]; 


(*
	numCoalitions - Takes a match matrix and returns the total number of coalitions across 
					all markets. 
*)
numCoalitions[matchIdxMtx_]:=
    Fold[Plus, 0, Flatten[Map[If[Total[#] != 0, 1, 0]&, matchIdxMtx[[All, 1, All, All]],{2}]]];
(* This expression works in two parts. It maps a function that returns 1 if there
   is a nonempty coalition in a given slot onto each of the upstream elements. Then this list
   is summed with a Fold. *)


(*
	generateRandomSubsample - Generates a subsample of a given size from a data array.
		
		Parameters: 
			ssSize - Size of the subsample generated, in terms of number of distinct entities
					 that will be represented in the subsample (ie, nests or coalitions). 
			groupIDs - A data map that the routine will use to examine the rows of the data array
					   for possible inclusion into the subsample. 
			dataArray - A data array structure suitable for passing into the objective function. 
*)
generateRandomSubsample[ssSize_, groupIDs_, dataArray_] :=
	Module[{totalGroups, groups, qualifiedIndexes},
		totalGroups = Union[Flatten[groupIDs, 1] ];
		groups = RandomKSubset[totalGroups, ssSize]; 
		(* Get the indexes of the dataArray rows that correspond to selected groups. *)
		qualifiedIndexes = Position[groupIDs, _?(Intersection[groups, #] != {} &), {1}, Heads->False];
		(* We want to extract certain rows, so we need to transpose before and 
			after extracting the rows we want, due to the matrix layout. *)
		Transpose[Extract[(*Transpose[*)dataArray(*]*), qualifiedIndexes]](*TCHRONIS for precomputed*)
	];
  	


    
(*CORRECTED TC*)	
(*
	generateAssignmentMatrix - Generates the optimal assignment of matches from the given matrix of
								payoffs for each match. In an assignment matrix, each entry (i,j) is
								1 if i and j are matched and 0 otherwise. 
					
		Parameters:
			payoffs - A list of matrixes (one for each market) where the (i,j)th element is
						the total production from matching i and j. 
			quotaU - Maximum number of partners each upstream agent can have. Default = 1. 
			quotaD - Maximum number of partners each downstream agent can have. Default = 1. 
*)
Options[generateAssignmentMatrix] = {linearProgrammingOptions -> Null, rationalizeResult -> True}; 
generateAssignmentMatrix[payoffs_, quotaU_:1, quotaD_:1, options___?OptionQ] := 
	Block[{rationalize, lpOptions, numU, numD, m1, m2, m3, m, b, matchMatrix, result},
		{rationalize, lpOptions} = {rationalizeResult, linearProgrammingOptions} 
									/. Flatten[{options, Options[generateAssignmentMatrix]}];
		{numU, numD} = Dimensions[payoffs];
		
		(* In this function, there are two sides to the market, U (pstream) and D (ownstream). i will 
			index upstream firms and j will index downstream firms. *)
		
		(* m1 is the first third of the constraint matrix. It represents the constraint
			\sum{x_ {ij}}_ {i=1}^n <= quotaD for all j *)
		m1 = SparseArray[ArrayFlatten[Outer[Times,SparseArray[IdentityMatrix[numU]], {Table[1, {numD}]}]]];
		
		(* m2 is the second third of the constraint matrix. It represents the constraint
			\sum{x_ {ij}}_ {j=1}^n <= quotaU for all i *)
		m2 = SparseArray[ArrayFlatten[Outer[Times, {Table[1, {numU}]},SparseArray[IdentityMatrix[numD]]]]];
		
		(* m3 is the last third of the constraint matrix. It represents the constraint that
			each pair of people can only match with each other once. *)
		m3 = SparseArray[{{i_,i_}->1}, {numU*numD, numU*numD}]; 
		
		m = SparseArray[ArrayFlatten[{{m1},{m2},{m3}}]];
		
		b = Join[Table[quotaU, {numU}], Table[quotaD, {numD}],Table[1, {numU*numD}]];
		(* Remember: linear programming will minimize, so we need to negate the "c" vector. 
			Similarly, since the constraint is taken to be m.x >= b, we need to negate m 
			and b as well. *)
		If[lpOptions != Null, 
			matchMatrix = LinearProgramming[-Flatten[payoffs], -m, -b, lpOptions],
			matchMatrix = LinearProgramming[-Flatten[payoffs], -m, -b]]; 
		result = Partition[matchMatrix, numD];
		
		If[rationalize, 
			Map[Rationalize[#, .01]&, result, {2}],
			result]
	];
	
	
(*OLD WITH BUG	
(*
	generateAssignmentMatrix - Generates the optimal assignment of matches from the given matrix of
								payoffs for each match. In an assignment matrix, each entry (i,j) is
								1 if i and j are matched and 0 otherwise. 
					
		Parameters:
			payoffs - A list of matrixes (one for each market) where the (i,j)th element is
						the total production from matching i and j. 
			quotaU - Maximum number of partners each upstream agent can have. Default = 1. 
			quotaD - Maximum number of partners each downstream agent can have. Default = 1. 
*)
Options[generateAssignmentMatrix] = {linearProgrammingOptions -> Null, rationalizeResult -> True}; 
generateAssignmentMatrix[payoffs_, quotaU_:1, quotaD_:1, options___?OptionQ] := 
	Block[{rationalize, lpOptions, numU, numD, m1, m2, m3, m, b, matchMatrix, result},
		{rationalize, lpOptions} = {rationalizeResult, linearProgrammingOptions} 
									/. Flatten[{options, Options[generateAssignmentMatrix]}];
		{numU, numD} = Dimensions[payoffs];
		
		(* In this function, there are two sides to the market, U (pstream) and D (ownstream). i will 
			index upstream firms and j will index downstream firms. *)
		
		(* m1 is the first third of the constraint matrix. It represents the constraint
			\sum{x_ {ij}}_ {i=1}^n <= quotaD for all j *)
		m1 = SparseArray[ArrayFlatten[Outer[Times, SparseArray[IdentityMatrix[numD]], {Table[1,{numU}]}]]];
		
		(* m2 is the second third of the constraint matrix. It represents the constraint
			\sum{x_ {ij}}_ {j=1}^n <= quotaU for all i *)
		m2 = SparseArray[ArrayFlatten[Outer[Times, {Table[1,{numD}]}, SparseArray[IdentityMatrix[numU]] ] ] ]; 
		
		(* m3 is the last third of the constraint matrix. It represents the constraint that
			each pair of people can only match with each other once. *)
		m3 = SparseArray[{{i_,i_}->1}, {numU*numD, numU*numD}]; 
		
		m = SparseArray[ArrayFlatten[{{m1},{m2},{m3}}]];
		
		b = Join[Table[quotaD, {numU}], Table[quotaU, {numD}], Table[1,{numU*numD}]];
		(* Remember: linear programming will minimize, so we need to negate the "c" vector. 
			Similarly, since the constraint is taken to be m.x >= b, we need to negate m 
			and b as well. *)
		If[lpOptions != Null, 
			matchMatrix = LinearProgramming[-vec[payoffs], -m, -b, lpOptions],
			matchMatrix = LinearProgramming[-vec[payoffs], -m, -b]]; 
		result = unvec[matchMatrix, numU];
		
		If[rationalize, 
			Map[Rationalize[#, .01]&, result, {2}],
			result]
	];
*)		


(* 
	matchIndexMatrix - Takes a list of assignment matrixes (see generateAssignmentMatrix) and 
					   generates a list of triply-indexed matrixes with the i'th element 
					   containing the index of i's match. Each element in the list is a list 
					   with two elements, and each of those is a list of lists. The two 
					   elements within each market represent the two sides of the market, 
					   upstream and downstream, in that order. In the upstream half, 
					   the i'th element will be a list with an entry for each coalition 
					   in the market; each of these lists will contain all of the 
					   identifiers of the upstream market participants in that coalition. 
					   The downstream half will contain the same number of entries, each of 
					   which is a list of the downstream members of the coalition. 
					   
					   An example will clarify: 
					   
					   {
					   	 {
					   	   {{1},{2},{3}},
					   	   {{4,5},{6,7},{8,9}}
					   	 },
					   	 {
					   	   {{10,11},{12,13},{14}},
					   	   {{15},{16},{17}}					   	 
					   	 }
					   }
					   
					   In this very simple example, there are two markets. In the first, 
					   there are three coalitions, each of which has one upstream agents
					   and two downstream agents. In the second market, there are also 
					   three coalitions, each of which has two upstream agents and one 
					   downstream agent.  
					   
					   If the second argument is given, it makes a match matrix which
   					   has a list of length quotaU in each element. The second for is for 
   					   multiple matching; unfortunately, you need to specify this, because it 
   					   is not always clear from an assignment matrix alone what the quota was 
   					   (it might not have been optimal to take advantage of the possibility of 
   					   multiple matching).
   					   
   		Parameters: 
   			assignmentMatrices - A list of assignment matrixes, which are returned from generateAssignmentMatrix.  
   			quotaU - The maximum number of agents an upstream agent can match with. Default = 1. 
*) 
matchIndexMatrix[assignmentMatrices_, quotaU_:1] :=
    Table[{
    	Table[{i}, {i,Length[assignmentMatrices[[n]] ]}], (* Upstream half is direct *)
    	(* The map generates the second half of each market's match matrix. It maps a 
    	   function that returns the positions of the all of the 1s in the assignment
    	   matrix onto each of the assignment matrixes. *) 
    	Map[
     		Flatten[PadRight[If[Position[#,_?(.9<#<1.1&)] != {},
          	     			    Position[#,_?(.9<#<1.1&)],
          	     			    {0}], 
          	     			 quotaU] ]&,
        	assignmentMatrices[[n]]
        ]}, {n,Length[assignmentMatrices]}
    ];
	


(*
	allPairsInequalityList - Takes a match matrix and returns a list of every possible
							 pairwise swap that can be generated, where each one is
							 of the form {{n, i1, p1}, {n, i2, p2}}, n is a nest index,
							 i1 and i2 are upstream agents, and p1 and p2 index one of
							 one of each of their downstream partners. 
*)
allPairsInequalityList[matchMtx_]:=
  Module[{ineq,a,n,i,j,p,q},
    ineq=Table[{},{Total@Table[Length[matchMtx[[n,2]] ]*(Length[matchMtx[[n,2]]]-1)/2*
	Length[matchMtx[[n,2,1]]]^2,{n,Length[matchMtx]}]}];
    a=1;
    (* The log of the indexing: 
       For each market (n), 
         for each pair of upstream agents (i, j)
            for each pair of downstream matches of i and j (p, q) *) 
    For[n=1,n <= Length[matchMtx],n=n+1,
      For[i=1,i <= Length[matchMtx[[n,2]]],i=i+1,
        For[j=i+1,j <= Length[matchMtx[[n,2]]],j=j+1,
          For[p=1,p <= Length[matchMtx[[n,2,1]]],p=p+1,
            For[q=1,q <= Length[matchMtx[[n,2,1]]],q=q+1,
              ineq[[a]] ={{n,i,p},{n,j,q}};
              a+=1;
              ]
            ]
          ]
        ]
      ];
    ineq
  ]
	


	
(*
	randomInequalityList - Takes a match matrix and generates a random sample of inequalities,
						   in contrast to allPairsInequalityList, which generates all of them. 
						   
		Parameters:
			matchMtx - A match matrix. 
			inequalitiesPerNest - The number of inequalities to pick for each nest. Repeated 
								  inequalities cannot be ruled out without performing the lengthy
								  computation this function is designed to avoid, so this 
								  parameter should be significantly smaller than the number of 
								  possible inequalities per nest. 
*)
randomInequalityList[matchMtx_, inequalitiesPerNest_] :=
	Module[{ineq,n,i,j,p,q,a},ineq=Table[{},{Length[matchMtx]*inequalitiesPerNest}];
		a=1;
		For[n=1,n <= Length[matchMtx],n=n+1,
			For[m=1,m <= inequalitiesPerNest, m=m+1,
				(* Draw two random upstream agents from the n'th market. *)
				i = Random[DiscreteUniformDistribution[{1,Length[matchMtx[[n,2]] ]} ] ];
				j = Random[DiscreteUniformDistribution[{1,Length[matchMtx[[n,2]] ]} ] ];
				(* Draw two random matches of theirs. *)  
				p = Random[DiscreteUniformDistribution[{1,Count[matchMtx[[n,2,i]], _?(#!=0&)]}] ];
				q = Random[DiscreteUniformDistribution[{1,Count[matchMtx[[n,2,j]], _?(#!=0&)]}] ];
				If[i==j, m=m-1;Continue[]];
				ineq[[a]] = {{n,i,p},{n,j,q}};
				a+=1; 
			]
		];
    	ineq
    ]
	


(* 
	pairwiseMSE - Generates an estimate using the pairwise maximum score estimator. 
	
		Parameters:
			q - An objective function, which takes a data array and a sequence of scalar
				arguments as parameters. 
			dataArray - The data array parameter to use in the objective function. 
			args - A list of unique symbols equal in length to the number parameters to
				   estimate. The return value from pairwiseMSE will contain a list of 
				   replacement rules keyed on the elements of args. 
			options - An optional parameter specifying options. The only recognized option
					  is nMaximizeOptions. 
*)
Options[pairwiseMSE] = {nMaximizeOptions -> {Method -> {"DifferentialEvolution"}}}; 
pairwiseMSE[q_, dataArray_, args_, options___?OptionQ] :=
    Block[{qwp, nmaxOptions}, 
    	{nmaxOptions} = {nMaximizeOptions} 
    		/. Flatten[{options, Options[pairwiseMSE]}];
      	
      	(* We need to fool NMaximize into not evaluating q symbolically. Thus,
          we need to create a pattern-matching function that restricts the arguments of q to be numeric.*)
        qwp[x__?NumericQ] := q[dataArray, x];

       	NMaximize[qwp[Sequence@@args], args, Sequence@@nmaxOptions]
    ];
 


(*
	pointIdentifiedCR - Generates a confidence region estimate using subsampling. 
	
		Parameters: 
			ssSize - The size of each subsample to be estimated. 
			numSubsamples - The number of subsamples to use in estimating the confidence region. 
			pointEstimate - The point estimate to build the confidence region around (typically 
							the output of pairwiseMSE). 
			objFunc - The objective function used in pairwiseMSE. 
			args - A list of unique symbols used in pairwiseMSE. 
			groupIDs - A data map used to generate the subsamples.
			dataArray - The dataArray parameter used in pairwiseMSE. 
			options - An optional parameter specifying options. Available options are: 
				progressUpdate - How often to print progress (0 to disable). Default = 0. 
				confidenceLevel - The confidence level of the region. Default = .95. 
				asymptotics - Type of asymptotics to use (nests or coalitions). Default = nests. 
				subsampleMonitor - An expression to evaluate for each subsample. Default = Null. 
				symmetric - True or False. If True, the confidence region will be symmetric. Default = False. 
*)
Options[pointIdentifiedCR] = {progressUpdate->0, confidenceLevel->.95, asymptotics->nests, subsampleMonitor->Null, symmetric->False};
pointIdentifiedCR[ssSize_, numSubsamples_, pointEstimate_, objFunc_, args_, groupIDs_, dataArray_, options___?OptionQ]:=
	Module[{progress, confLevel, asymp, ssDataArray, estimates, estimate, alpha, cr, sym},
		{progress, confLevel, asymp, sym} = {progressUpdate, confidenceLevel, asymptotics, symmetric} 
			/. Flatten[{options,Options[pointIdentifiedCR]}];
			
		(* This block sets variables that are slightly different for each of the two asymptotics. 
		   subNormalization is the standardization multiplier for the subsamples, fullNormalization is the 
		   multiplier for the construction of the final confidence interval from all of the subsamples. *)
		Switch[asymp,
			nests, 
				subNormalization = (ssSize)^(1/3);
				fullNormalization = (Length[Union[Flatten[groupIDs, 1] ] ])^(1/3);
				nextRandomSubsample := generateRandomSubsample[ssSize, groupIDs, dataArray];,
   			coalitions,
    			subNormalization = (ssSize)^(1/2);
				fullNormalization = (Length[Union[Flatten[groupIDs, 1] ] ])^(1/2); (*(numCoalitions[matchIdxMtx])^(1/2);*)
				nextRandomSubsample := generateRandomSubsample[ssSize, groupIDs, dataArray];	
		];
			
    	estimates = Table[0, {numSubsamples}]; (* List of standardized subsample estimates. *)
    	ssEstimates = Table[0, {numSubsamples}]; (* List of raw subsample estimates. *)
    	Do[ssDataArray = nextRandomSubsample;
    		ssEstimate = args /. pairwiseMSE[objFunc, ssDataArray, args, options][[2]];
    		ssEstimates[[i]] = ssEstimate;
    		estimates[[i]] = subNormalization (ssEstimate - pointEstimate);
    		If[progress > 0 && Mod[i, progress] == 0, Print["Iterations completed: "<>ToString[i]]];
    		Block[{}, subsampleMonitor /. Flatten[{options, Options[pointIdentifiedCR]}]];, 
    	{i,1,numSubsamples}];
    	For[i = 1, i <= Length[args], i = i + 1, Histogram[estimates[[All,i]]]];
    	alpha = 1-confLevel;
    	If[sym == True,
    		(* For the symmetric case, we want to add and subtract the 1-alpha'th quantile from the point estimate. 
    			We take the Abs here for simplicity: tn*Abs[x-y] == Abs[tn*(x-y)] *)
    		cr = Table[pointEstimate[[i]] - Reverse[{-1,1}*Quantile[Abs[estimates[[All,i]] ], {1-alpha, 1-alpha}]]
    			/fullNormalization, {i, 1, Length[args]}],
    	(* Else *)
    		(* For the asymmetric case we separately take the alpha/2 and 1-alpha/2 quantiles and subtract them. 
    		   Keep in mind that since estimates has its mean subtracted, only in freakishly unlikely cases 
    		   will these two have the same sign. This is not true for the symmetric case. *)
    		cr = Table[pointEstimate[[i]] - Reverse[Quantile[estimates[[All,i]], {alpha/2, 1-alpha/2}]]
    			/fullNormalization, {i, 1, Length[args]}]
    	];
    	{cr,estimates}
	]
	


(*
	setIdentifiedCR - Generates a confidence region estimate using the set-identified procedure in Shaikh (2006). 
					  The parameters are exactly the same as pointIdentifiedCR except that symmetric is not an option. 
*)	
Options[setIdentifiedCR] = {progressUpdate->0, confidenceLevel->.95, asymptotics->nests, subsampleMonitor->Null, symmetric->False};	
setIdentifiedCR[ssSize_, numSubsamples_, pointEstimate_, objFunc_, args_, groupIDs_, dataArray_, options___?OptionQ] :=
	Module[{progress, confLevel, ineq, asymp, totalGroups, subsamples, estimates, alpha, dhat, sym},
		{progress, confLevel, asymp, sym} = {progressUpdate, confidenceLevel, asymptotics, symmetric} 
			/. Flatten[{options, Options[setIdentifiedCR]}];
		totalGroups = Length[Union[Flatten[groupIDs, 1] ] ]; 
 		qwp[x__?NumericQ] := objFunc[dataArray, x];
 				
 		(* This block sets variables that are slightly different for each of the two asymptotics. 
		   subNormalization is the standardization multiplier for the subsamples, fullNormalization is the 
		   multiplier for the construction of the final confidence interval from all of the subsamples. *)	
		Switch[asymp,
			nests,
				subNormalization = ssSize^(-1/3);
				fullNormalization = totalGroups^(-1/3);
				subsamples = Table[generateRandomSubsample[ssSize, groupIDs, dataArray], {numSubsamples}];,
			coalitions,
				subNormalization = 1;
				fullNormalization = 1;
				subsamples = Table[generateRandomSubsample[ssSize, groupIDs, dataArray], {numSubsamples}];
		];	
			 	
		(* This generates [numSubsamples] subsample estimates. They will be reused throughout the
		   rest of the procedure. *)
    	estimates = Table[0, {numSubsamples}];
    	Do[estimates[[i]] = args /. pairwiseMSE[objFunc, subsamples[[i]], args, options][[2]];
      		If[progress > 0 && Mod[i, progress] == 0,
        	  Print["Iterations completed: "<>ToString[i]]
        	];,
        {i,1,numSubsamples}];
   
   		(* This is the "dhat" quantile function from Shaikh's paper. It returns the 
   		   1-alpha'th quantile of the differences of the objective function between x and
   		   the estimate value, standardized. *)
   		dhat[x__?NumericQ] :=
       		Module[{objDiff, coverageStatistics},
       			objDiff = Table[(-objFunc[subsamples[[i]], x] 
       							- -objFunc[subsamples[[i]], Sequence@@(estimates[[i]])]), 
       							{i, Length[subsamples]}];
        		coverageStatistics = subNormalization * objDiff;
        		Quantile[coverageStatistics, 1-alpha]
        	];
    	
    	alpha=1-confLevel;
    
    	(* For each dimension (length[args]), this does a one-dimensional minimization and a maximization 
    	   from a point known to be in the confidence region, so that we can find the dimension-by-
    	   dimension extents of the region. *)
    	initialRegion= MapIndexed[List[#1, pointEstimate[[First[#2]]]-2, pointEstimate[[First[#2]]]+2]&, args];
    	cr = {};
    	Do[clow = args[[i]] /. 
    		NMinimize[{args[[i]], fullNormalization*(-qwp[Sequence@@args]- -qwp[Sequence@@pointEstimate]) 
                  			  	  <= dhat[Sequence@@args ]}, initialRegion, 
              	  	  StepMonitor :> Print["Minimize candidate value: "<>ToString[args[[i]] ] ], 
              	  	  Method->"DifferentialEvolution", AccuracyGoal->1, PrecisionGoal->1][[2]];
      	   chigh = args[[i]] /.
      		NMaximize[{args[[i]], fullNormalization*(-qwp[Sequence@@args]- -qwp[Sequence@@pointEstimate ])
      						  	  <= dhat[Sequence@@args]}, initialRegion, 
              	  	  StepMonitor :> Print["Maximize candidate value: "<>ToString[args[[i]] ] ], 
              	  	  Method->"DifferentialEvolution", AccuracyGoal->1, PrecisionGoal->1][[2]];
      	   AppendTo[cr, {clow, chigh}];,
      	   {i, 1, Length[args]}
    	]; 
    	If[sym,
    		(* We take the maximum distance from the point estimate to either end, and then make the
    			CR have that distance from the point estimate. *)
    		dist = Abs[cr - pointEstimate];
    		deltas = Map[Max, dist];
    		pointEstimate + Table[{-1, 1}, {Length[pointEstimate]}] * deltas,
    	(* Else *) 
    		cr
    	]
	]
	


	
(*
	sampleSetIdentifiedCR - Generates a random set of points that are inside a set-identified confidence region. 
	
		Parameters: 
			ssSize - The size of each subsample to be estimated. 
			numSubsamples - The number of subsamples to use in estimating the confidence region.
			numSamplePoints - The number of points to randomly sample. Some subset of these points will
							  hopefully be found to be inside the confidence region, and thus returned in 
							  the output.  
			pointEstimate - The point estimate to build the confidence region around (typically 
							the output of pairwiseMSE). 
			objFunc - The objective function used in pairwiseMSE. 
			args - A list of unique symbols used in pairwiseMSE. 
			groupIDs - A data map used to generate the subsamples.
			dataArray - The dataArray parameter used in pairwiseMSE. 
			options - An optional parameter specifying options. Available options are: 
				progressUpdate - How often to print progress (0 to disable). Default = 0. 
				confidenceLevel - The confidence level of the region. Default = .95. 
				asymptotics - Type of asymptotics to use (nests or coalitions). Default = nests. 
				subsampleMonitor - An expression to evaluate for each subsample. Default = Null. 
				samplingVariance - The variance of the distribution used to draw the sample points. Default = 20.  
*)
Options[sampleSetIdentifiedCR] = {progressUpdate->0, confidenceLevel->.95, asymptotics->nests, subsampleMonitor->Null, samplingVariance->20};	
sampleSetIdentifiedCR[ssSize_, numSubsamples_, numSamplePoints_, pointEstimate_, objFunc_, args_, groupIDs_, dataArray_, options___?OptionQ] :=
	Module[{progress, confLevel, ineq, asymp, totalGroups, subsamples, estimates, alpha, dhat, svar},
		{progress, confLevel, asymp, svar} = {progressUpdate, confidenceLevel, asymptotics, samplingVariance} 
			/. Flatten[{options, Options[sampleSetIdentifiedCR]}];
		totalGroups = Length[Union[Flatten[groupIDs, 1] ] ]; 
 		qwp[x__?NumericQ] := objFunc[dataArray, x];
 
 		(* This block sets variables that are slightly different for each of the two asymptotics. 
		   subNormalization is the standardization multiplier for the subsamples, fullNormalization is the 
		   multiplier for the construction of the final confidence interval from all of the subsamples. *)	 					
		Switch[asymp,
			nests,
				subNormalization = ssSize^(-1/3);
				fullNormalization = totalGroups^(-1/3);
				subsamples = Table[generateRandomSubsample[ssSize, groupIDs, dataArray], {numSubsamples}];,
			coalitions,
				subNormalization = 1;
				fullNormalization = 1;
				subsamples = Table[generateRandomSubsample[ssSize, groupIDs, dataArray], {numSubsamples}];
		];	

		(* This generates [numSubsamples] subsample estimates. They will be reused throughout the
		   rest of the procedure. *)			 	
    	estimates = Table[0, {numSubsamples}];
    	Do[estimates[[i]] = args /. pairwiseMSE[objFunc, subsamples[[i]], args, options][[2]];
      		If[progress > 0 && Mod[i, progress] == 0,
        	  Print["Iterations completed: "<>ToString[i]]
        	];,
        {i,1,numSubsamples}];
    
   		(* This is the "dhat" quantile function from Shaikh's paper. It returns the 
   		   1-alpha'th quantile of the differences of the objective function between x and
   		   the estimate value, standardized. *)    
       	dhat[x__?NumericQ] :=
       		Module[{objDiff, coverageStatistics},
       			objDiff = Table[(-objFunc[subsamples[[i]], x] 
       							 - -objFunc[subsamples[[i]], Sequence@@(estimates[[i]])]), 
       							{i, Length[subsamples]}];
        		coverageStatistics = subNormalization * objDiff;
        		Quantile[coverageStatistics, 1-alpha]
        	];
    	(* This alternate version of dhat is worth remembering. 
    	dhat[x__?NumericQ] := 
    		Module[{coverageStatistics},
        		coverageStatistics = subNormalization 
        			* MapIndexed[(-objFunc[#1, x]- -objFunc[#1, Sequence@@(estimates[[First[#2] ]])])&,
                    subsamples];
        		Quantile[coverageStatistics, 1-alpha]
        	];
    	*) 
    	
    	isInCR[point_] := 
    			fullNormalization * (-qwp[Sequence@@point] - -qwp[Sequence@@pointEstimate]) <= dhat[Sequence@@point];
    		
    	alpha=1-confLevel;
    
   		cr = {};
    	Do[pt = Random[MultinormalDistribution[pointEstimate, svar*IdentityMatrix[Length[args]]]];
    	   	If[isInCR[pt],
      	   		AppendTo[cr, pt];
      	   	],
      	   {i, 1, numSamplePoints}
    	]; 
		cr
	]	


EndPackage[]; 

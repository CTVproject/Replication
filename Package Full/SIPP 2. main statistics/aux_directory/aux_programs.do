
/* programs
 OCCMAT_DIM_EXE (args: occupation_var) : mapping from occupational codes to matrix rows
 XSNET_CALCX : mobility stats, including correction, net/gross/excess
 reg2xls_exe (args filename sheetname) : save regression output (as displayed in stata) to an xls sheet
 star_exe (args xlscol xlsrow table_pos) : save coeff (starred) in a xls cell, and the standard error below 
 excel_col_exe   for columns beyond 26
	star_exe v2
	tstar_exe transpose star_exe						
*/
		
		capture program drop occmat_dim_exe
		** version 14/7/2017
		program define occmat_dim_exe, rclass
				args occ
		display "entering occmat_dim_exe"
		local maxoccno=1
		quietly su `occ'
		local maxoccno=r(max) 

		quietly tab `occ'
		local rcount=r(r)

		matrix define tempoccmat=J(`rcount', 1, 0)

		local occdim=0
		forvalues i=1(1)`maxoccno' {
						cap count if (`occ'==`i')
						if r(N)>0 {
						local occdim = `occdim' +1 
						matrix tempoccmat[`occdim',1]=`i'
						}
						}
		matrix define occno_matrix=J(`occdim',1,0)
		forvalues j=1(1)`occdim' {
						matrix occno_matrix[`j',1]=tempoccmat[`j',1]
		}

		*matrix list occno_matrix
		return matrix occno_matrix=occno_matrix
		return scalar occno=`occdim'
		
		
		end 

		
			capture program drop reg2xls_exe
			program define reg2xls_exe
						args filename sheetname
						
			cap putexcel set "`filename'.xls", sheet("`sheetname'", replace)
			cap n putexcel set "`filename'.xls", modify sheet("`sheetname'", replace)

			putexcel F1=("Number of obs") G1=(e(N))
			putexcel F2=("F") G2=(e(F))
			putexcel F3=("Prob > F") G3=(Ftail(e(df_m), e(df_r), e(F)))
			putexcel F4=("R-squared") G4=(e(r2))
			putexcel F5=("Adj R-squared") G5=(e(r2_a))
			putexcel F6=("Root MSE") G6=(e(rmse))
			matrix ab = r(table)
			matrix a=ab'
			matrix a = a[., 1..6]
			putexcel A8=matrix(a, names)

			end 


		
		
		capture program drop star_exe  
				/* run directly after regression
					input [1] column in xls file [2] row in xls file [3] regressor position in table
					note: needs two rows in regression table, one for coefficient, one for s.e.
				*/
			program star_exe 
				args xlscol xlsrow table_pos
			matrix rtable=r(table)
			*matrix list rtable
			*display "`table_pos'"
			local pval=rtable[4,`table_pos']
			local coeffx=rtable[1,`table_pos']
			local stderrx=rtable[2, `table_pos']
			*display "`pval'"
			if `pval'<=0.1 & `pval'>0.05 {
			local star="*" 
			}	
			if `pval'>0.01 & `pval'<=0.05 {
			local star="**" 
			}	
			if `pval'<=0.01 {
			local star="***" 
			}	
			if `pval'>0.1 {
			local star="" 
			}	
			local coeffx=string(`coeffx', "%8.4f")
			local coeffstar="`coeffx'`star'"
			local stderr2=string(`stderrx', "%8.4f")
			*display "`stderr2'"
			local stderr3="(`stderr2')"
			display "`coeffstar'"
			display "`stderr3'"


			putexcel `xlscol'`xlsrow'=("`coeffstar'")
			local xlsrowplus=`xlsrow'+1
			putexcel `xlscol'`xlsrowplus'=("`stderr3'")

			end

			
/*			
capture program drop xsnet_calcx
program define xsnet_calcx, rclass

	
		*** input: xsnet_calcx MAT, statistics for matrix
		***	input: xsnet_calcx MAT GAMMA, statistics for gamma corrected matrix: CHECK WHETHER THE TRANSPOSES WORK OUT



*matrix matrik=durmat2
						*display "entering program"
						
						if "`2'"!="" {
						matrix prepost=`2'
						matrix matrikspp=`1'
						matrix matriks=prepost'*matrikspp*prepost
						}
						if "`2'"=="" {
						matrix matriks=`1'
						}
						local matdim=colsof(matriks)
						*display "matdim: ", `matdim'
						
						*display "continuing program"
						
						matrix unitx= J(1,`matdim', 1)
						*matrix list unitx
						
						
						
						matrix temp_outflowvector=matriks*unitx'  // warning outflow includes self-flows
						matrix temp_inflowvector=unitx*matriks    // warning inflow includes self-flows 
						*matrix list temp_outflowvector
						*matrix list temp_inflowvector
						
						matrix temp_netflowvector=temp_inflowvector'-temp_outflowvector
						matrix temp_netflowvector_abs=temp_netflowvector
						forvalues i=1(1)`matdim' {
									if temp_netflowvector_abs[`i',1]<0{
									matrix temp_netflowvector_abs[`i',1]=-temp_netflowvector_abs[`i',1]
									}
						}
						*matrix list temp_netflowvector
						*matrix list temp_netflowvector_abs
						
						
						matrix sum_all=unitx*temp_outflowvector
						*matrix list sum_all
						
						
						
						
						global sum_all=sum_all[1,1]
						*display $sum_all
						
						global trace_temp=trace(matriks)
						*display $trace_temp
						
						global staying_temp=$trace_temp / $sum_all
						display "STAYING PROPORTION", $staying_temp
						
						
						
						
						
						matrix netflow_temp=unitx*temp_netflowvector_abs
						*matrix list netflow_temp
						
						
						global netflow_temp=netflow_temp[1,1]
						global excessflow_temp=${sum_all} - ${trace_temp} - (0.5*netflow_temp[1,1])
						global excessflowprop_temp=${excessflow_temp}/(${sum_all}-${trace_temp})
						global netflowprop_temp=(0.5*${netflow_temp})/(${sum_all}-${trace_temp})
						global netflow_all_u_temp=(0.5*${netflow_temp})/(${sum_all})
					
						
						/*
						display "TOTAL MOVERS  " $excessflow_temp
						display "STAYER  " $trace_temp
						display "NET FLOW  " $netflow_temp
						display "ALL INDIV  " $sum_all
						display "PROP OF U NEEDED  " $netflow_all_u_temp
						display "PROP NET REALL  ", $excessflowprop_temp
						
						*/
						
						
						// calculate transition matrix
						matrix transmat_temp=J(`matdim', `matdim',0)
						forvalues i=1(1)`matdim' {
						forvalues j=1(1)`matdim' {
										matrix transmat_temp[`i',`j']=matriks[`i', `j']/temp_outflowvector[`i',1]
						}
						}
						
						*matrix temp_inflowvector=I(13)*temp_inflowvector'
						matrix temp_inflowvector=temp_inflowvector'
						
						
						
						// adjust gross inflow and outflow vectors
						forvalues i=1(1)`matdim' {
									matrix temp_outflowvector[`i', 1]=temp_outflowvector[`i', 1]-matriks[`i', `i']
									matrix temp_inflowvector[`i', 1]=temp_inflowvector[`i', 1]-matriks[`i', `i']
									}
									
						// calculate net reallocation rate
						matrix netreallocc_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netreallocc_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/(temp_outflowvector[`i',1]+temp_inflowvector[`i',1])
						}
						// calculate flow normalized by spells
						matrix netflownorm_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflownorm_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all)
						}
								// calculate flow normalized by spells
						matrix netflowoflow_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflowoflow_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all-$trace_temp)
						}
				
						
				
					
					
						matrix pairwise_netflow=matriks
						
						matrix pairwise_netreallocation=matriks
						
						
						
						forvalues i=1(1)`matdim' {
						forvalues j=`i'(1)`matdim' {
						matrix pairwise_netflow[`i', `j']=(pairwise_netflow[`i', `j']-pairwise_netflow[`j', `i'])*100/${sum_all}
						matrix pairwise_netflow[`j', `i']=0
						if (matriks[`i', `j']+ matriks[`j', `i'])>=0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=(matriks[`i', `j']-matriks[`j', `i'])/(matriks[`i', `j']+ matriks[`j', `i'])
						matrix pairwise_netreallocation[`j', `i']=0
						}
						if (matriks[`i', `j']+ matriks[`j', `i'])<0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=0
						matrix pairwise_netreallocation[`j', `i']=0
						}
						}
						}
						
						* fill out the net flow matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netflow[`j', `i']!=0 {
								matrix pairwise_netflow[`i', `j']=-pairwise_netflow[`j', `i']
						}
						}
						}
						
						* fill out the net reallocation matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netreallocation[`j', `i']!=0 {
								matrix pairwise_netreallocation[`i', `j']=-pairwise_netreallocation[`j', `i']
						}
						}
						}
						
						
													
							// relative flow heterogeneity matrix
							/*
							captures proportion of outflows from occ i that go to occupation j, relative to the proportion of all occ's that go to occupation j (excluding self-flows, i.e. excluding all flows from k to k, and all flows from j)
							*/
							
							matrix flowdev_rel=matriks
							matrix sumoutflows_excl_j=J(1,1,0)
							matrix flowdev_abs=J(`matdim',`matdim',0)
							
							
							forvalues i=1(1)`matdim' {
							forvalues j=1(1)`matdim' {
													matrix flowdev_rel[`i',`j']=0
													
													if `j'!=`i' {
													local rel_ijflow=(matriks[`i', `j']) /(temp_outflowvector[`i',1]) // prop of outflows from i that go to j (excl. self-flows)
													matrix sumoutflows_excl_j[1,1]=unitx*temp_outflowvector-temp_outflowvector[`j',1] // all outflows that are not self-flows, excl outflows from j
													*display "test"
													*matrix list sumoutflows_excl_j
													local rel_j_inflow=temp_inflowvector[`j',1] / (sumoutflows_excl_j[1,1]) // prop of inflows into j (excl self-flows) of overall non-self-flows
													matrix flowdev_rel[`i',`j']=(`rel_ijflow'-`rel_j_inflow')/`rel_j_inflow'
													matrix flowdev_abs[`i',`j']=(`rel_ijflow'-`rel_j_inflow')
													
													}
							}
							}

							
							

						
						
						
					
					
					
						
						
						
					
						********RETURNED VARS AND MATRICES
						return matrix toutflowvector=temp_outflowvector  // outflows destionation per source occupation
						return matrix tinflowvector=temp_inflowvector    // for each destination occupation, vector of source occupations
						return matrix tnetflowvector=temp_netflowvector  // net flows per occupation
						return matrix tnetflowvector_abs=temp_netflowvector_abs // absolute value of net flows per occupation
						return matrix tnetflownorm=netflownorm_temp // net flows relative to unemployment spells
						return matrix tnetreallocc=netreallocc_temp // net reallocation rate per occupation (outflows-inflow)/(outflows+inflows)
						return matrix tnetflowoflow=netflowoflow_temp // net flows relative to total unemplyoed workers changing occupations
						return matrix ttransmat=transmat_temp      // transition matrix
						return matrix tpairwise_netflow=pairwise_netflow // net flow across i,j occupation pairs
						return matrix tpairwise_netreallrate=pairwise_netreallocation // net reallocation rate between i, j occupation pairs
						return matrix tflowdev_rel=flowdev_rel
						return matrix tflowdev_abs=flowdev_abs
												
						return scalar tsum_all=$sum_all
						return scalar tnetflow=$netflow_temp
						return scalar texcessflow=$excessflow_temp
						return scalar texcessflowprop=$excessflowprop_temp
						return scalar tnetflowprop=$netflowprop_temp
						return scalar tnetflow_all_u=$netflow_all_u_temp
						return scalar tmobrate=1.0-$staying_temp 
						return scalar tstaying=$staying_temp 
						
			
						
						
end
*/
			
capture program drop xsnet_calcx
program define xsnet_calcx, rclass

	
		*** input: xsnet_calcx MAT, statistics for matrix
		***	input: xsnet_calcx MAT GAMMA, statistics for gamma corrected matrix: CHECK WHETHER THE TRANSPOSES WORK OUT



*matrix matrik=durmat2
						*display "entering program"
						
						if "`2'"!="" {
						matrix prepost=`2'
						matrix matrikspp=`1'
						matrix matriks=prepost'*matrikspp*prepost
						}
						if "`2'"=="" {
						matrix matriks=`1'
						}
						local matdim=colsof(matriks)
						*display "matdim: ", `matdim'
						
						*display "continuing program"
						
						matrix unitx= J(1,`matdim', 1)
						*matrix list unitx
						
						
						
						matrix temp_outflowvector=matriks*unitx'  // warning outflow includes self-flows
						matrix temp_inflowvector=unitx*matriks    // warning inflow includes self-flows 
						*matrix list temp_outflowvector
						*matrix list temp_inflowvector
						
						matrix temp_netflowvector=temp_inflowvector'-temp_outflowvector
						matrix temp_netflowvector_abs=temp_netflowvector
						forvalues i=1(1)`matdim' {
									if temp_netflowvector_abs[`i',1]<0{
									matrix temp_netflowvector_abs[`i',1]=-temp_netflowvector_abs[`i',1]
									}
						}
						*matrix list temp_netflowvector
						*matrix list temp_netflowvector_abs
						
						
						matrix sum_all=unitx*temp_outflowvector
						*matrix list sum_all
						
						
						
						
						global sum_all=sum_all[1,1]
						*display $sum_all
						
						global trace_temp=trace(matriks)
						*display $trace_temp
						
						global staying_temp=$trace_temp / $sum_all
						display "STAYING PROPORTION", $staying_temp
						
						
						
						
						
						matrix netflow_temp=unitx*temp_netflowvector_abs
						*matrix list netflow_temp
						
						
						global netflow_temp=netflow_temp[1,1]
						global excessflow_temp=${sum_all} - ${trace_temp} - (0.5*netflow_temp[1,1])
						global excessflowprop_temp=${excessflow_temp}/(${sum_all}-${trace_temp})
						global netflowprop_temp=(0.5*${netflow_temp})/(${sum_all}-${trace_temp})
						global netflow_all_u_temp=(0.5*${netflow_temp})/(${sum_all})
					    global netflow_temp=${netflow_temp}/2						
						
						/*
						display "TOTAL MOVERS  " $excessflow_temp
						display "STAYER  " $trace_temp
						display "NET FLOW  " $netflow_temp
						display "ALL INDIV  " $sum_all
						display "PROP OF U NEEDED  " $netflow_all_u_temp
						display "PROP NET REALL  ", $excessflowprop_temp
						
						*/
						
						
						// calculate transition matrix
						matrix transmat_temp=J(`matdim', `matdim',0)
						forvalues i=1(1)`matdim' {
						forvalues j=1(1)`matdim' {
										matrix transmat_temp[`i',`j']=matriks[`i', `j']/temp_outflowvector[`i',1]
						}
						}
						
						*matrix temp_inflowvector=I(13)*temp_inflowvector'
						matrix temp_inflowvector=temp_inflowvector'
						matrix tinflowvector_incl=temp_inflowvector
						matrix toutflowvector_incl=temp_outflowvector
						
						
						// adjust gross inflow and outflow vectors
						forvalues i=1(1)`matdim' {
									matrix temp_outflowvector[`i', 1]=temp_outflowvector[`i', 1]-matriks[`i', `i']
									matrix temp_inflowvector[`i', 1]=temp_inflowvector[`i', 1]-matriks[`i', `i']
									}
									
						// calculate net reallocation rate
						matrix netreallocc_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netreallocc_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/(temp_outflowvector[`i',1]+temp_inflowvector[`i',1])
						}
						// calculate flow normalized by spells
						matrix netflownorm_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflownorm_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all)
						}
								// calculate flow normalized by spells
						matrix netflowoflow_temp=J(`matdim',1,0)
						forvalues i=1(1)`matdim' {
										matrix netflowoflow_temp[`i',1]=(temp_outflowvector[`i',1]-temp_inflowvector[`i',1])/($sum_all-$trace_temp)
						}
				
						
				
					
					
						matrix pairwise_netflow=matriks
						
						matrix pairwise_netreallocation=matriks
						
						
						
						forvalues i=1(1)`matdim' {
						forvalues j=`i'(1)`matdim' {
						matrix pairwise_netflow[`i', `j']=(pairwise_netflow[`i', `j']-pairwise_netflow[`j', `i'])*100/${sum_all}
						matrix pairwise_netflow[`j', `i']=0
						if (matriks[`i', `j']+ matriks[`j', `i'])>=0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=(matriks[`i', `j']-matriks[`j', `i'])/(matriks[`i', `j']+ matriks[`j', `i'])
						matrix pairwise_netreallocation[`j', `i']=0
						}
						if (matriks[`i', `j']+ matriks[`j', `i'])<0.002* ${sum_all} {
						matrix pairwise_netreallocation[`i', `j']=0
						matrix pairwise_netreallocation[`j', `i']=0
						}
						}
						}
						
						* fill out the net flow matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netflow[`j', `i']!=0 {
								matrix pairwise_netflow[`i', `j']=-pairwise_netflow[`j', `i']
						}
						}
						}
						
						* fill out the net reallocation matrix below the diagonal
						forvalues i=2(1)`matdim' {
						local m=`i'-1
						forvalues j=1(1)`m' {
						
						if pairwise_netreallocation[`j', `i']!=0 {
								matrix pairwise_netreallocation[`i', `j']=-pairwise_netreallocation[`j', `i']
						}
						}
						}
						
						
													
							// relative flow heterogeneity matrix
							/*
							captures proportion of outflows from occ i that go to occupation j, relative to the proportion of all occ's that go to occupation j (excluding self-flows, i.e. excluding all flows from k to k, and all flows from j)
							*/
							
							matrix flowdev_rel=matriks
							matrix sumoutflows_excl_j=J(1,1,0)
							matrix flowdev_abs=J(`matdim',`matdim',0)
							
							
							forvalues i=1(1)`matdim' {
							forvalues j=1(1)`matdim' {
													matrix flowdev_rel[`i',`j']=0
													
													if `j'!=`i' {
													local rel_ijflow=(matriks[`i', `j']) /(temp_outflowvector[`i',1]) // prop of outflows from i that go to j (excl. self-flows)
													matrix sumoutflows_excl_j[1,1]=unitx*temp_outflowvector-temp_outflowvector[`j',1] // all outflows that are not self-flows, excl outflows from j
													*display "test"
													*matrix list sumoutflows_excl_j
													local rel_j_inflow=temp_inflowvector[`j',1] / (sumoutflows_excl_j[1,1]) // prop of inflows into j (excl self-flows) of overall non-self-flows
													matrix flowdev_rel[`i',`j']=(`rel_ijflow'-`rel_j_inflow')/`rel_j_inflow'
													matrix flowdev_abs[`i',`j']=(`rel_ijflow'-`rel_j_inflow')
													
													}
							}
							}

							
							

						
						
						
					
					
					
						
						
						
					
						********RETURNED VARS AND MATRICES
						return matrix toutflowvector=temp_outflowvector  // outflows destionation per source occupation
						return matrix tinflowvector=temp_inflowvector    // for each destination occupation, vector of source occupations
						return matrix tnetflowvector=temp_netflowvector  // net flows per occupation
						return matrix tnetflowvector_abs=temp_netflowvector_abs // absolute value of net flows per occupation
						return matrix tnetflownorm=netflownorm_temp // net flows relative to unemployment spells
						return matrix tnetreallocc=netreallocc_temp // net reallocation rate per occupation (outflows-inflow)/(outflows+inflows)
						return matrix tnetflowoflow=netflowoflow_temp // net flows relative to total unemplyoed workers changing occupations
						return matrix ttransmat=transmat_temp      // transition matrix
						return matrix tpairwise_netflow=pairwise_netflow // net flow across i,j occupation pairs
						return matrix tpairwise_netreallrate=pairwise_netreallocation // net reallocation rate between i, j occupation pairs
						return matrix tflowdev_rel=flowdev_rel
						return matrix tflowdev_abs=flowdev_abs
						return matrix tsocc_distr=toutflowvector_incl // outflows destionation per source occupation, including self-flows
						return matrix tdocc_distr=tinflowvector_incl    // for each destination occupation, vector of source occupations, including self-flows
												
						return scalar tsum_all=$sum_all
						return scalar tnetflow=$netflow_temp
						return scalar texcessflow=$excessflow_temp
						return scalar texcessflowprop=$excessflowprop_temp
						return scalar tnetflowprop=$netflowprop_temp
						return scalar tnetflow_all_u=$netflow_all_u_temp
						return scalar tmobrate=1.0-$staying_temp 
						return scalar tstaying=$staying_temp 
						
			
						
						
end




//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP


cap n program drop excel_col_exe
	program define excel_col_exe, rclass
			args lnumber

	local excelcc=`lnumber'
	tokenize "`c(alpha)'"
	display "`lnumber', and resulting number `excelcc'"
	if `excelcc'<=26 {
	return local xlscol "``excelcc''"
	}
	if `excelcc'>26 {
	local firstletter=floor((`excelcc'-1)/26)
	local secondletter=`excelcc'-26*`firstletter'
	return local xlscol "``firstletter''``secondletter''"
	}

	end 

	
	//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
capture program drop star_exe
program star_exe 
	args xlscol xlsrow table_pos matrixname

	
	
if "`matrixname'"=="" {
	matrix rtablel=r(table)
}
if "`matrixname'"!="" {
	matrix rtablel=`matrixname'
}

*matrix list rtablel
*display "`table_pos'"
local pval=rtablel[4,`table_pos']
local coeffx=rtablel[1,`table_pos']
local stderrx=rtablel[2, `table_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"


putexcel `xlscol'`xlsrow'=("`coeffstar'")
local xlsrowplus=`xlsrow'+1
putexcel `xlscol'`xlsrowplus'=("`stderr3'")

end


capture program drop tstar_exe
program tstar_exe   // COLUMN FIRST, THEN ROW 
	args xlscol xlsrow rtable_pos matrixname
	
if "`matrixname'"=="" {
	matrix localrtablexxxx=r(table)
}
if "`matrixname'"!="" {
	matrix localrtablexxxx=`matrixname'
}

*matrix list rtable
*display "`table_pos'"
local pval=localrtablexxxx[4,`rtable_pos']
local coeffx=localrtablexxxx[1,`rtable_pos']
local stderrx=localrtablexxxx[2, `rtable_pos']
*display "`pval'"
if `pval'<=0.1 & `pval'>0.05 {
local star="*" 
}	
if `pval'>0.01 & `pval'<=0.05 {
local star="**" 
}	
if `pval'<=0.01 {
local star="***" 
}	
if `pval'>0.1 {
local star="" 
}	
local coeffx=string(`coeffx', "%8.4f")
local coeffstar="`coeffx'`star'"
local stderr2=string(`stderrx', "%8.4f")
*display "`stderr2'"
local stderr3="(`stderr2')"
display "`coeffstar'"
display "`stderr3'"

tokenize "`c(alpha)'"

putexcel ``xlscol''`xlsrow'=("`coeffstar'")
local xlscolplus=`xlscol'+1

putexcel ``xlscolplus''`xlsrow'=("`stderr3'")

end



//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
// rep_sh REPORT SHEET PROGRAMS
//PPPPPPPPPPPPPPPPPPPPPPPPPPPP
capture program drop rep_sh1
program rep_sh1
			args name counter_repsh regressionmatrixname no_obs marginsmatrixname commandlineglobal

			/* input into this program
			
			NAME = identifier name of the summary statistic 
			COUNTER_REPSH = row of the spreadsheet in which information appears
			REGRESSIONMATRIXNAME = rtable of the regression on which one wants to report
			NO_OBS = number of observations one wants to report // NEED TO PUT  0 if it is still available under e(N)
			MARGINSMATRIXNAME = rtable of the margins command
			COMMANDLINEGLOBAL = points to global where the command line is stored
			*/
			
			
			
			
	cap n putexcel set "${filename}.xls", sheet("${sheetname1}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname1}")

	putexcel A`counter_repsh'=("`name'")
	
	if "`regressionmatrixname'"=="" {
	tstar_exe 4 `counter_repsh' 1 
	putexcel F`counter_repsh'=(e(N)) 
	}
	if "`regressionmatrixname'"!="" {
	tstar_exe 4 `counter_repsh' 1 `regressionmatrixname' 
	
	if "`no_obs'"=="" {
	putexcel F`counter_repsh'=(e(N)) 
	}
	if "`no_obs'"=="0" {
	putexcel F`counter_repsh'=(e(N)) 
	}
    if "`no_obs'"!="0" & "`no_obs'"!="" {
	putexcel F`counter_repsh'=("`no_obs'") 
	}
	}
	
	if "`commandlineglobal'"=="" {
	putexcel G$ctr=(e(command)) 
	}
	if "`commandlineglobal '"!="" {
	putexcel G$ctr=("${`commandlineglobal'}") 
	}
	
	if "`marginsmatrixname'"== "" {
	display " no margins information given ----- running margins now "
	margins 
	tstar_exe 2 `counter_repsh' 1  
	putexcel F$ctr=(r(N)) 
	}
	
	if "`marginsmatrixname'"!= "" {
	display " margins information"
	tstar_exe 2 `counter_repsh' 1  `marginsmatrixname'
	}

end 

capture program drop rep_sh2
program define rep_sh2
			args name headingglobal col_c row_c  local_regtable estimatesname 
	
	local local_restore_indic=0
	
	 if "`headingglobal'"==""{
	 local headinglocal ""
	 }
	
	 if "`headingglobal'"!=""{
	 local headinglocal "${`headingglobal'}"
	 }
	
	if "`local_regtable'"=="" {
	display "USING CURRENT R(TABLE)"
	matrix local_rtable=r(table)
	}
	
	if "`local_regtable'"!="" {
	matrix local_rtable=`local_regtable'
	}
	
	if "`estimatesname'"!="" {
	cap n estimates store estimates_tempxxxx
	estimates restore `estimatesname'
	local local_restore_indic=1
	}
	
	cap n putexcel set "${filename}.xls", sheet("${sheetname2}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname2}")
	tokenize "`c(alpha)'"
	
	excel_col_exe `col_c'
	local firstletter=r(xlscol)
	
	 excel_col_exe `col_c'+1
	local secondletter=r(xlscol)

	excel_col_exe `col_c'+6
	local stlastletter=r(xlscol)

	excel_col_exe `col_c'+7
	local lastletter=r(xlscol)

	local row0=`row_c'+1
	local row1=`row_c'+2
	local row2=`row_c'+3
	local row3=`row_c'+4
	local row4=`row_c'+5
	local row5=`row_c'+6
	local row6=`row_c'+7
	local row8=`row_c'+9
	
	cap n putexcel `firstletter'`row_c'=("****** `headinglocal'")
	
	cap n putexcel `firstletter'`row0'=("`name'")
	cap n putexcel `secondletter'`row0'=(e(command))
	putexcel `stlastletter'`row1'=("Number of obs") `lastletter'`row1'=(e(N))
	putexcel `stlastletter'`row2'=("F") `lastletter'`row2'=(e(F))
	putexcel `stlastletter'`row3'=("Prob > F") `lastletter'`row3'=(Ftail(e(df_m), e(df_r), e(F)))
	putexcel `stlastletter'`row4'=("R-squared") `lastletter'`row4'=(e(r2))
	cap n putexcel `stlastletter'`row5'=("Adj R-squared") `lastletter'`row5'=(e(r2_a))
	cap n putexcel `stlastletter'`row6'=("Root MSE") `lastletter'`row6'=(e(rmse))
	matrix ab = local_rtable
	matrix a=ab'
	matrix a = a[., 1..6]
	putexcel `firstletter'`row8'=matrix(a, names)

	local nocols=colsof(ab)
	local lastrow=`row8'+`nocols'

	local bottomrowl=max($bottomrow, `lastrow')
	display "bottomrow equals `bottomrowl', max of $bottomrow and `lastrow'"

	
	
	*display "bottomrow = `bottomrowl'"
	global bottomrow=`bottomrowl'
	global leftcol=`col_c'+9
	*display "leftcol=$leftcol"
	
	
	if "`local_restore_indic'"=="1" {
	*display "restoring current estimates"
	estimates restore estimates_tempxxxx
	local local_restore_indic=0
	}
	
	end 

/*
capture program drop rep_sh3
program define rep_sh3
				args name number column controlindicator 




end
*/

capture program drop rep_sh4
program rep_sh4
			args name counter_repsh   commandlineglobal  est_commandlineglobal // NEEDS TO BE RUN IMMEDIATELY AFTER F-TEST

			/* input into this program
			
			NAME = identifier name of the summary statistic 
			COUNTER_REPSH = row of the spreadsheet in which information appears
			COMMANDLINE_GLOBAL = captures the direct command related to the F-test
			EST_COMMANDLINE_GLOBAL = captures the underlying estimation command, if different (e.g. in the case of previous use of margins command) 
			*/
			
	cap n putexcel set "${filename}.xls", sheet("${sheetname4}")
	cap n putexcel set "${filename}.xls", modify sheet("${sheetname4}")

	putexcel A`counter_repsh'=("`name'")
	
	*local coeffx=string(`coeffx', "%8.4f")
	local df_temp=r(df)
	local df_r_temp=r(df_r)
	local Fsetup="F(`df_temp', `df_r_temp' )="
	putexcel B`counter_repsh'=("`Fsetup'")
	putexcel C`counter_repsh'=(r(F))
	putexcel D`counter_repsh'=(r(p))
	
	if "`commandlineglobal'"=="" {
	cap n putexcel E`counter_repsh'=(e(command)) 
	cap n putexcel E`counter_repsh'=(e(cmdline)) 
	}
	if "`commandlineglobal'"!="" {
	putexcel E`counter_repsh'=("${`commandlineglobal'}") 
	}
	
	
	if "`est_commandlineglobal'"=="" {
	cap n putexcel F`counter_repsh'=(e(est_cmdline)) 
	}
	if "`est_commandlineglobal'"!="" {
	cap n putexcel F`counter_repsh'=("${`commandlineglobal'}") 
	}
end 









//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
		capture program drop regression_report_maintext
		program regression_report_maintext
			args name ctr colpos rowpos
		matrix rtable=r(table)
		local obscount=e(N)
		global obscount=e(N)
		global commandline=e(command)

		margins 
		matrix marginstable=r(table)

		rep_sh1 `name' `ctr' rtable `obscount' marginstable commandline
		rep_sh2 `name' heading `colpos' `rowpos'  rtable 

		end 
		



//PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
	capture program drop occmap_exe
	program define occmap_exe, rclass
				args occ
				syntax varlist [if/]

	display "`if'"
	if "`if'"=="" {
	local if="`occ'!=."
	}
	local maxoccno=1
	cap su `occ' if `if'
	local maxoccno=r(max) 

	quietly tab `occ'
	local rcount=r(r)
	local rcount=2*`rcount'

	matrix define tempoccmat=J(`rcount', 1, 0)

	local occdim=0
	forvalues i=1(1)`maxoccno' {
						*if "`if'"==""{
						cap count if `occ'==`i' & `if'
						*}
						*if "`if'"!=""{
						*cap count `if' & `occ'==`i' 
						*}
						if r(N)>0 {
						local occdim = `occdim' +1 
						matrix tempoccmat[`occdim',1]=`i'
						}
						}
	matrix define occno_matrix=J(`occdim',1,0)
	forvalues j=1(1)`occdim' {
						matrix occno_matrix[`j',1]=tempoccmat[`j',1]
	}

	matrix list occno_matrix
	return matrix mappingmatrix=occno_matrix
	return scalar dim=`occdim'
	end

	

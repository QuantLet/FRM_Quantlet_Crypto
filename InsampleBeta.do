
clear
clear matrix
clear mata
global date = "20230914"
global root = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All/Output/Crypto/Add_EfficiencyTest"


set maxvar  120000


/**************** 1. data generating ****************/
use $root/InsampleBetaTest_20230514.dta, clear

gen Date = date(date, "YMD")
tsset Date

global laglist = "10 25 63 110"
global risk = " FRM_5P FRM_25P FRM_50P GDC DAG PCA_1 TCI"

local nstart 1
foreach measure in $risk {
	foreach lag in $laglist{
		newey MKVola `measure'_L`lag', lag(5)  
		
		if `nstart'  == 1 {
			outreg2 using $root/InsampleBeta/InsampleBeta.xls, ///
			stat(coef se) bdec(4) sdec(3) r2 title("FRM") replace drop(_I** _est* o.**)
						} 
		else {
			 outreg2 using $root/InsampleBeta/InsampleBeta.xls, ///
			stat(coef se) bdec(4) sdec(3) r2 title("FRM") append drop(_I** _est* o.**)
			  }
		
		local nstart = `nstart' + 1
		
			
	}

}	


gen str_Year = substr(date, 1, 4)

* numeric
destring str_Year, generate(Year)
drop str_Year
matrix coef_container = J(8, 4, .)

* Newey-West Estimator

newey MKVola FRM_5P_L10, lag(5)
reg MKVola FRM_5P_L10
local nstart 1
* Loop over the years
forval iY = 2015/2022 {
    newey MKVola FRM_5P_L10 if Year == `iY', lag(5) 
    *reg MKVola_63 FRM_5P_L10 if Year == `iY'
    
    * Store regression coefficients in the matrix
	matrix coef_container[`nstart', 1] = `iY'
    matrix coef_container[`nstart', 2] = _b[FRM_5P_L10]
    matrix coef_container[`nstart', 3] = _b[FRM_5P_L10] - 1.96 * _se[FRM_5P_L10]
	matrix coef_container[`nstart', 4] = _b[FRM_5P_L10] + 1.96 * _se[FRM_5P_L10]
    
	local nstart = `nstart' + 1
	
    * Clear temporary results
    *drop _all
}

* Display the regression coefficients
matrix list coef_container

putexcel set $root/InsampleBeta/InsampleBeta_dynamic.xls, replace

putexcel A1 = matrix(coef_container)



/*
foreach lag in $laglist{
	
	local MKVola MKVola_`lag'
	
	local measure FRM_5P_L`lag'
	
	reg `MKVola'  `measure' 

	outreg2 using $root/InsampleBeta.xls, ///
					stat(coef se) bdec(4) sdec(3) adjr2 title("FRM") append drop(_I** _est* o.**)
	

}
	
			

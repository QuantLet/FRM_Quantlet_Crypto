
clear
clear matrix
clear mata
global date = "20230914"
global root = "/Users/ruting/Documents/Github/FRM_Crypto_Github/FRM_All/Output/Crypto/Add_EfficiencyTest/Sharpe"

/*global control = "lag_lnAsset lag_lnDebt lag_ROA Interbank list_num rate_bond_AAA  guarantee_dummy Eect01  GDPgrowth SOE_province SOE_city Admin_choice_province Admin_choice_city Admin_choice_county POE "
*/

global control = "BVIndex_growth_lag CVIXIndex_growth_lag DXYCurncy_growth_lag SPXIndex_growth_lag VIXIndex_growth_lag mktcap_lag date_new"


set maxvar  120000


/**************** 1. data generating ****************/
use $root/Sharpe_volatility.dta, clear

gen date_new = date(date, "YMD")
encode Type, generate(Type_code)
xtset date_new Type_code

gen FRM5P_lag5 = l5.FRM_5P
gen FRM5P_lag10 = l10.FRM_5P
gen FRM5P_lag25 = l25.FRM_5P
gen FRM5P_lag63 = l63.FRM_5P
gen FRM5P_lag110 = l110.FRM_5P

gen FRM25P_lag5 = l5.FRM_25P
gen FRM25P_lag10 = l10.FRM_25P
gen FRM25P_lag25 = l25.FRM_25P
gen FRM25P_lag63 = l63.FRM_25P
gen FRM25P_lag110 = l110.FRM_25P

gen FRM50P_lag5 = l5.FRM_50P
gen FRM50P_lag10 = l10.FRM_50P
gen FRM50P_lag25 = l25.FRM_50P
gen FRM50P_lag63 = l63.FRM_50P
gen FRM50P_lag110 = l110.FRM_50P

gen FRM5P_lag = l.FRM_5P


gen BVIndex_lag = l.BVIndex
gen CVIXIndex_lag = l.CVIXIndex
gen DXYCurncy_lag = l.DXYCurncy
gen SPXIndex_lag = l.SPXIndex
gen VIXIndex_lag = l.VIXIndex
gen mktcap_lag = l.mktcap


gen BVIndex_growth = log(BVIndex) - log(BVIndex_lag)
gen CVIXIndex_growth = log(CVIXIndex) - log(CVIXIndex_lag)
gen DXYCurncy_growth = log(DXYCurncy) - log(DXYCurncy_lag)
gen SPXIndex_growth = log(SPXIndex) - log(SPXIndex_lag)
gen VIXIndex_growth = log(VIXIndex) - log(VIXIndex_lag)

gen BVIndex_growth_lag = l.BVIndex_growth
gen CVIXIndex_growth_lag = l.CVIXIndex_growth
gen DXYCurncy_growth_lag = l.DXYCurncy_growth
gen SPXIndex_growth_lag = l.SPXIndex_growth
gen VIXIndex_growth_lag = l.VIXIndex_growth


*winsor2 Sharpe_vol_10 Sharpe_vol_25 Sharpe_vol_63 Sharpe_vol_110  , replace cuts (1 99)
 

asdoc sum Sharpe_vol_10 Sharpe_vol_25 Sharpe_vol_63 Sharpe_vol_110 FRM_5P FRM_25P FRM_50P mktcap BVIndex_growth CVIXIndex_growth DXYCurncy_growth SPXIndex_growth VIXIndex_growth,  decimals(4), save($root/summary_FRMCC.doc) replace



global laglist = "10 25 63 110"
global FRMPara = "5 25 50"


foreach Para in $FRMPara{
	
	local nstart 1
	foreach lag in $laglist{

		local measure FRM`Para'P_lag`lag'
		
		reghdfe Sharpe_vol_63  `measure' $control, absorb(Type_code)
		
		if `nstart'  == 1 {
			outreg2 using $root/Fixed_Vol_Sharpe_vol_FRM_`Para'P.xls, ///
						stat(coef se) bdec(4) sdec(3) adjr2 title("FRM") replace drop(_I** _est* o.**) 
						} 
						else {
							outreg2 using $root/Fixed_Vol_Sharpe_vol_FRM_`Para'P.xls, ///
						stat(coef se) bdec(4) sdec(3) adjr2 title("FRM") append drop(_I** _est* o.**) 
						}
		
		local nstart = `nstart' + 1
	}
}
			
foreach Para in $FRMPara{
	
	
		local measure FRM_`Para'P
		
		reghdfe Sharpe_vol_63  `measure' $control, absorb(Type_code)
		
		
		outreg2 using $root/Fixed_Vol_Sharpe_vol_FRM_`Para'P.xls, ///
	stat(coef se) bdec(4) sdec(3) adjr2 title("FRM") append drop(_I** _est* o.**) 
	

		
}				
			
				
		



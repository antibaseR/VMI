log using "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Code/VMI/VMI.log", replace

use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear

/*
Descriptive analysis: 
*/
* 1. The association between total HS and PBR 5-25
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine {
    egen z_`var' = std(`var')
}
mixed z_PBR5_25 z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine|| PatientID: , variance

* 2. Comparison of markers of glycocalyx denudation, microvascular dysfunction, platelet count and inflammation in the following groups: without AKI, with AKI, and within the AKI patients, those with transient AKI, persistent severe AKI and AKD. 

/*
1. Primary analysis
Hypothesis: Glycocalyx denudation is associated with higher risk of persistent severe AKI. 
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine {
    egen z_`var' = std(`var')
}

* Check the correlation of the data
corr z_total_HS z_PBR5_25 z_X__24_hours_Total_Fluids z_Age_At_Enrollment z_APACHEIII_baseline z_Charlson z_IL6Val z_ref_Creatinine

** AKI vs No AKI
logit AKI z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

logit AKI z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

** Persistent severe AKI vs No Persistent Severe AKI
keep if AKI == 1

* Check the correlation of the data
corr z_total_HS z_PBR5_25 z_X__24_hours_Total_Fluids z_Age_At_Enrollment z_APACHEIII_baseline z_Charlson z_IL6Val z_ref_Creatinine Persistent_Severe_AKI Sex_Scr

logit Persistent_Severe_AKI z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

logit Persistent_Severe_AKI z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

** Persistent Severe AKI vs No AKI
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear

gen Persistent_Severe_AKI_vs_No_AKI = .
replace Persistent_Severe_AKI_vs_No_AKI = 0 if AKI == 0
replace Persistent_Severe_AKI_vs_No_AKI = 1 if Persistent_Severe_AKI == 1
keep if !missing(Persistent_Severe_AKI_vs_No_AKI)

foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine {
    egen z_`var' = std(`var')
}

logit Persistent_Severe_AKI_vs_No_AKI total_HS i.hour X__24_hours_Total_Fluids Age_At_Enrollment i.Sex_Scr Charlson ref_Creatinine, vce(robust) or
estat auc

logit Persistent_Severe_AKI_vs_No_AKI PBR5_25 i.hour X__24_hours_Total_Fluids Age_At_Enrollment i.Sex_Scr Charlson ref_Creatinine, vce(robust) or
estat auc


/*
2. Secondary clinical analyses
A. Hypothesis: The change in glycocalyx denudation (72h – 6h/24h - 6h) is associated with higher risk of persistent severe AKI. 
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
keep PatientID hour AKI Persistent_Severe_AKI total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment Sex_Scr APACHEIII_baseline Charlson IL6Val ref_Creatinine
reshape wide total_HS PBR5_25 IL6Val, i(PatientID) j(hour)

gen total_HS_diff_72_6 = total_HS72 - total_HS6
gen PBR5_25_diff_72_6 = PBR5_2572 - PBR5_256
gen total_HS_diff_24_6 = total_HS24 - total_HS6
gen PBR5_25_diff_24_6 = PBR5_2524 - PBR5_256

gen total_HS_diff = total_HS_diff_72_6
replace total_HS_diff = total_HS_diff_24_6 if missing(total_HS_diff)
gen PBR5_25_diff = PBR5_25_diff_72_6
replace PBR5_25_diff = PBR5_25_diff_24_6 if missing(PBR5_25_diff)

foreach var of varlist total_HS_diff PBR5_25_diff X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val6 ref_Creatinine {
    egen z_`var' = std(`var')
}

gen APACHEIII_baseline_log = log(APACHEIII_baseline)
hist(APACHEIII_baseline_log)

egen z_APACHEIII_baseline_log = std(APACHEIII_baseline_log)
hist(z_APACHEIII_baseline_log)

gen ref_Creatinine_log = log(ref_Creatinine)
egen z_ref_Creatinine_log = std(ref_Creatinine_log)

** AKI vs No AKI
logit AKI z_total_HS_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline_log z_Charlson z_ref_Creatinine_log, vce(robust) or
estat auc

logit AKI z_PBR5_25_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline_log z_Charlson z_ref_Creatinine_log, vce(robust) or
estat auc


/*
B. Hypothesis: Glycocalyx denudation is associated with higher risk of no renal recovery. 
Outcome: Renal recovery
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine {
    egen z_`var' = std(`var')
}

logit AKI_Recovery_Total z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

logit AKI_Recovery_Total z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc



/*
C. Hypothesis: Glycocalyx denudation is associated with higher risk of MAKE 30.
Outcome: MAKE 30/MAKE 30 v2
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine {
    egen z_`var' = std(`var')
}


logit MAKE30 z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc


logit MAKE30 z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc


/*
D. Hypothesis: Glycocalyx denudation is associated with higher risk of AKD.
Outcome: AKD
*/

logit AKD z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

logit AKD z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc


/*
E. Hypothesis: Glycocalyx denudation is associated with higher risk of 90-day mortality.
*/
logit death_90days z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc

logit death_90days z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson z_ref_Creatinine, vce(robust) or
estat auc


/*
3. Secondary mechanistic analyses
A. Hypothesis: Glycocalyx denudation is associated with microvascular dysfunction.  
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine SummaryPPV_Small SummaryPVD_Small MfiSmall_n DeBackerScore HeteroIndexSmall {
    egen z_`var' = std(`var')
}

mixed z_SummaryPPV_Small z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_SummaryPVD_Small z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_MfiSmall_n z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_DeBackerScore z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_HeteroIndexSmall z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance


mixed z_SummaryPPV_Small z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_SummaryPVD_Small z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_MfiSmall_n z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_DeBackerScore z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_HeteroIndexSmall z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

/*
B. Hypothesis: Glycocalyx denudation is associated with endothelial activation.  
Glycocalyx denudation has to happen before Endothelial activation.
•	Endothelial activation definition:
o	Ang 2
o	ICAM
o	VCAM
o	E-selectin
o	P-selectin
o	VEGF
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
gen Ang_2Val_log = log(Ang_2Val)
gen ICAMVal_log = log(ICAMVal)
gen VCAMVal_log = log(VCAMVal)
gen E_SelectinVal_log = log(E_SelectinVal)
gen P_SelectinVal_log = log(P_SelectinVal)

foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine Ang_2Val_log ICAMVal_log VCAMVal_log E_SelectinVal_log P_SelectinVal_log VEGF {
    egen z_`var' = std(`var')
}

mixed z_Ang_2Val_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_ICAMVal_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_VCAMVal_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_E_SelectinVal_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_P_SelectinVal_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_VEGF z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance


mixed z_Ang_2Val_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_ICAMVal_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_VCAMVal_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_E_SelectinVal_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_P_SelectinVal_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance

mixed z_VEGF z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson|| PatientID: , variance



/*
C. Hypothesis: Glycocalyx denudation is associated with increased platelet rolling and/or adhesion.  
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
gen Adhered_log = log(Adhered)

foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine Adhered_log Rolling{
    egen z_`var' = std(`var')
}

regress z_Rolling z_total_HS z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

regress z_Adhered_log z_total_HS z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson


regress z_Rolling z_PBR5_25 z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

regress z_Adhered_log z_PBR5_25 z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

/*
D. Hypothesis: Glycocalyx denudation is associated decreased platelet count.  
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
gen Platelet_Count_log = log(Platelet_Count)

foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine Platelet_Count_log{
    egen z_`var' = std(`var')
}

mixed z_Platelet_Count_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson || PatientID: , variance
estat icc

mixed z_Platelet_Count_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson || PatientID: , variance
estat icc

/*
E. Hypothesis: Glycocalyx denudation is associated with tubular dysfunction.
•	Tubular dysfunction measures as a continuous variable using: 
o	TIMP2/IGFBP7
o	Kim1
o	NGAL
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
gen TIMP2_0_log = log(TIMP2_0)
gen IGFBP7_0_log = log(IGFBP7_0)
gen KIM1Val_log = log(KIM1Val)
gen NGALVal_log = log(NGALVal)

foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine TIMP2_0_log IGFBP7_0_log KIM1Val_log NGALVal_log{
    egen z_`var' = std(`var')
}

regress z_TIMP2_0_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

regress z_IGFBP7_0_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

mixed z_KIM1Val_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

mixed z_NGALVal_log z_total_HS i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson


regress z_TIMP2_0_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

regress z_IGFBP7_0_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

mixed z_KIM1Val_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson

mixed z_NGALVal_log z_PBR5_25 i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson



/*
F. Hypothesis: A positive change in microvascular dysfunction is associated with decreased risk of persistent AKI.
The change to look at is 24h – 6h/72h - 6h. Create this variable. The outcome could be pAKI or renal recovery.
*/
use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
keep PatientID hour AKI Persistent_Severe_AKI AKI_Recovery_Total total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment Sex_Scr APACHEIII_baseline Charlson IL6Val ref_Creatinine MfiSmall_n HeteroIndexSmall SummaryTVD_Small SummaryPPV_Small SummaryPVD_Small DeBackerScore
reshape wide total_HS PBR5_25 IL6Val MfiSmall_n HeteroIndexSmall SummaryTVD_Small SummaryPPV_Small SummaryPVD_Small DeBackerScore, i(PatientID) j(hour)

gen MfiSmall_n_diff_24_6 = MfiSmall_n24 - MfiSmall_n6
gen HeteroIndexSmall_diff_24_6 = HeteroIndexSmall24 - HeteroIndexSmall6
gen SummaryTVD_Small_diff_24_6 = SummaryTVD_Small24 - SummaryTVD_Small6
gen SummaryPPV_Small_diff_24_6 = SummaryPPV_Small24 - SummaryPPV_Small6
gen SummaryPVD_Small_diff_24_6 = SummaryPVD_Small24 - SummaryPVD_Small6
gen DeBackerScore_diff_24_6 = DeBackerScore24 - DeBackerScore6

gen MfiSmall_n_diff_72_6 = MfiSmall_n72 - MfiSmall_n6
gen HeteroIndexSmall_diff_72_6 = HeteroIndexSmall72 - HeteroIndexSmall6
gen SummaryTVD_Small_diff_72_6 = SummaryTVD_Small72 - SummaryTVD_Small6
gen SummaryPPV_Small_diff_72_6 = SummaryPPV_Small72 - SummaryPPV_Small6
gen SummaryPVD_Small_diff_72_6 = SummaryPVD_Small72 - SummaryPVD_Small6
gen DeBackerScore_diff_72_6 = DeBackerScore72 - DeBackerScore6

gen MfiSmall_n_diff = MfiSmall_n_diff_72_6
gen HeteroIndexSmall_diff = HeteroIndexSmall_diff_72_6
gen SummaryTVD_Small_diff = SummaryTVD_Small_diff_72_6
gen SummaryPPV_Small_diff = SummaryPPV_Small_diff_72_6
gen SummaryPVD_Small_diff = SummaryPVD_Small_diff_72_6
gen DeBackerScore_diff = DeBackerScore_diff_72_6

replace MfiSmall_n_diff = MfiSmall_n_diff_24_6 if missing(MfiSmall_n_diff)
replace HeteroIndexSmall_diff = HeteroIndexSmall_diff_24_6 if missing(HeteroIndexSmall_diff)
replace SummaryTVD_Small_diff = SummaryTVD_Small_diff_24_6 if missing(SummaryTVD_Small_diff)
replace SummaryPPV_Small_diff = SummaryPPV_Small_diff_24_6 if missing(SummaryPPV_Small_diff)
replace SummaryPVD_Small_diff = SummaryPVD_Small_diff_24_6 if missing(SummaryPVD_Small_diff)
replace DeBackerScore_diff = DeBackerScore_diff_24_6 if missing(DeBackerScore_diff)

foreach var of varlist X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson ref_Creatinine MfiSmall_n_diff HeteroIndexSmall_diff SummaryTVD_Small_diff SummaryPPV_Small_diff SummaryPVD_Small_diff DeBackerScore_diff {
    egen z_`var' = std(`var')
}

logit AKI_Recovery_Total z_MfiSmall_n_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI_Recovery_Total z_HeteroIndexSmall_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI_Recovery_Total z_SummaryTVD_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI_Recovery_Total z_SummaryPPV_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI_Recovery_Total z_SummaryPVD_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI_Recovery_Total z_DeBackerScore_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc


logit AKI z_MfiSmall_n_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_HeteroIndexSmall_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_SummaryTVD_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_SummaryPPV_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_SummaryPVD_Small_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_DeBackerScore_diff z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc


/*
G. Hypothesis: If microvascular dysfunction is associated with AKI, persistent AKI or AKD.
The way to set this up would be using PPV or MFI as predictors, and AKI, persistent AKI or AKD as outcomes. 
*/

use "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_Complete_Data.dta", clear
foreach var of varlist total_HS PBR5_25 X__24_hours_Total_Fluids Age_At_Enrollment APACHEIII_baseline Charlson IL6Val ref_Creatinine SummaryPPV_Small SummaryPVD_Small MfiSmall_n DeBackerScore HeteroIndexSmall {
    egen z_`var' = std(`var')
}

** AKD
logit AKD z_SummaryPPV_Small i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKD z_MfiSmall_n i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

** AKI vs No AKI
logit AKI z_SummaryPPV_Small i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit AKI z_MfiSmall_n i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

/*
** Persistent Severe AKI vs No Persistent Severe AKI among everyone
gen X__24_hours_Total_Fluids_log = log(X__24_hours_Total_Fluids)
egen z_X__24_hours_Total_Fluids_log = std(X__24_hours_Total_Fluids_log)
gen Age_At_Enrollment_log = log(Age_At_Enrollment)
egen z_Age_At_Enrollment_log = std(Age_At_Enrollment_log)
gen APACHEIII_baseline_log = log(APACHEIII_baseline)
egen z_APACHEIII_baseline_log = std(APACHEIII_baseline_log)
gen SummaryPPV_Small_log = log(SummaryPPV_Small)
egen z_SummaryPPV_Small_log = std(SummaryPPV_Small_log)

logit Persistent_Severe_AKI z_SummaryPPV_Small i.hour X__24_hours_Total_Fluids Age_At_Enrollment i.Sex_Scr APACHEIII_baseline Charlson, vce(robust) or
estat auc

logit Persistent_Severe_AKI z_MfiSmall_n i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

** Persistent Severe AKI vs No Persistent Severe AKI among those with AKI
keep if AKI == 1
logit Persistent_Severe_AKI z_SummaryPPV_Small i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc

logit Persistent_Severe_AKI z_MfiSmall_n i.hour z_X__24_hours_Total_Fluids z_Age_At_Enrollment i.Sex_Scr z_APACHEIII_baseline z_Charlson, vce(robust) or
estat auc
*/
log close

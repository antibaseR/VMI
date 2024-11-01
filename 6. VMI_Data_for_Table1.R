library(tidyverse)
library(readxl)


demographics = c("Age_At_Enrollment",
                 "Sex_Scr",
                 "race",
                 "ethnicity",
                 "weight",
                 "height",
                 "residence_location",
                 "employment_status",
                 "lifestyle",
                 "Charlson",
                 "APACHEIII_baseline")

comorbidities = c("comorbidities_mental",
                  "comorbidities_comat",
                  "comorbidities_mvent",
                  "comorbidities_chmvent",
                  "comorbidities_mvent_comment",
                  "comorbidities_abrsmavail",
                  "comorbidities_abrsm",
                  "comorbidities_IVFvol",
                  "comorbidities_education",
                  "comorbidities_HTN",
                  "comorbidities_mi",
                  "comorbidities_achf",
                  "comorbidities_cad",
                  "comorbidities_cerebral",
                  "comorbidities_pvd",
                  "comorbidities_respdis",
                  "comorbidities_resptype",
                  "comorbidities_dementia",
                  "comorbidities_neuro",
                  "comorbidities_endo",
                  "comorbidities_canc",
                  "comorbidities_leukmm",
                  "comorbidities_lymphoma",
                  "comorbidities_nonmettumor",
                  "comorbidities_metcanc",
                  "comorbidities_immsupp",
                  "comorbidities_aids",
                  "comorbidities_renad",
                  "comorbidities_renadtype",
                  "comorbidities_rendis",
                  "comorbidities_Ulcer",
                  "comorbidities_cirrhos",
                  "comorbidities_portalHTN",
                  "comorbidities_variceal",
                  "comorbidities_hepdis",
                  "comorbidities_ochrol",
                  "comorbidities_musskel",
                  "comorbidities_kidney",
                  "comorbidities_oralhypoglycemics",
                  "comorbidities_aspirin7",
                  "comorbidities_steriods",
                  "comorbidities_anticoags",
                  "comorbidities_statins")

others = c("hour",
           "total_HS",
           "PBR5_25",
           "PBR5_9",
           "PBR10_19",
           "PBR20_25",
           "MfiSmall_n",
           "HeteroIndexSmall",
           "SummaryTVD_Small",
           "SummaryPPV_Small",
           "SummaryPVD_Small",
           "DeBackerScore",
           "Ang_2Val",
           "ICAMVal",
           "VCAMVal",
           "E_SelectinVal",
           "P_SelectinVal",
           "VEGFVal",
           "Rolling",
           "Adhered",
           "X__24_hours_Total_Fluids",
           "Platelet_Count.6",
           "Platelet_Count.24",
           "Platelet_Count72",
           "IL6Val6",
           "IL6Val24",
           "IL6Val72",
           "TNFVal6",
           "TNFVal24",
           "TNFVal72",
           "IL10Val6",
           "IL10Val24",
           "IL10Val72",
           "CRPVal6",
           "CRPVal24",
           "CRPVal72",
           "ref_Creatinine",
           "death_30days",
           "death_90days",
           "death", 
           "RRT_30days",
           "TIMP2_0",
           "IGFBP7_0",
           "KIM1Val0",
           "KIM1Val6",
           "KIM1Val24",
           "KIM1Val72",
           "NGALVal0",
           "NGALVal6",
           "NGALVal24",
           "NGALVal72",
           "Vasopressin_1St_24Hrs_Yn",
           "Icu_Los",
           "Hosp_LOS")

variables = c("PatientID",demographics, comorbidities, others)

VMI_renal = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/SepsisAKI_Molinari_02112021.xlsx")
VMI_biomarker = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_biomarker.csv") 

names(VMI_renal)[names(VMI_renal) == "Stnum"] = "PatientID"

VMI_renal = VMI_renal[, names(VMI_renal) %in% variables]
VMI_biomarker = VMI_biomarker[, names(VMI_biomarker) %in% variables]
VMI_biomarker = VMI_biomarker %>%
  select(-c(race, Charlson))

# rename columns
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.6"] = "Platelet_Count6"
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.24"] = "Platelet_Count24"

# arrange data
VMI_renal = VMI_renal %>% arrange(PatientID)
VMI_biomarker = VMI_biomarker %>% arrange(PatientID, hour)

VMI_biomarker_wide_var = c(
  "total_HS", "PBR5_25", "PBR5_9", "PBR10_19", "PBR20_25", "Rolling", 
  "Adhered", "DeBackerScore", "SummaryTVD_Small", "SummaryPVD_Small", 
  "SummaryPPV_Small", "HeteroIndexSmall", "MfiSmall_n", "ICAMVal", 
  "VCAMVal", "E_SelectinVal", "P_SelectinVal", "Ang_2Val", "VEGFVal"
)
VMI_biomarker_wide = VMI_biomarker %>%
  pivot_wider(
    names_from = hour,     # Use 'hour' column to create new column names
    values_from = all_of(VMI_biomarker_wide_var) # Replace with the actual columns (e.g., variable name)
  )

merged_data = merge(VMI_renal, VMI_biomarker_wide, by = c("PatientID"))

# load the data with AKI flag and the previously merged data
Molinari_AKI_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/AKI_Flag_Molinari_Data.csv")
Molinari_MAKE_flag = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/MAKE30_Flag_Molinari_Data.csv")
# merge the AKI flag into the data
data = merge(merge(merged_data, Molinari_AKI_flag, by = c("PatientID")), Molinari_MAKE_flag, by = c("PatientID"))
# create a AKI Category variable
data = data %>%
  mutate(AKI_Category = ifelse(Persistent_Severe_AKI == 1, "Persistent Severe AKI",
                               ifelse(Transient_AKI == 1, "Transient AKI",
                                      ifelse(AKI == 1 & No_Persistent_Severe_AKI == 1, "No Persistent Severe AKI", "No AKI"))))

data$AKI_Category = factor(data$AKI_Category, levels = c("No AKI", "No Persistent Severe AKI", "Transient AKI", "Persistent Severe AKI"))
write.csv(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/Table1_Complete_Data.csv", row.names = F)


library(tidyverse)
library(readxl)

VMI_biomarker_raw = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/mass spec analysis 090617 combo.xlsx")
VMI_fulldata = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/ProCESS-full dataset-Gomez.xlsx")

# remove the healthy volunteers for the statistical analysis but keep the patients in case
healthy_vulunteers_data = VMI_biomarker_raw[!(VMI_biomarker_raw$PatientID %in% VMI_fulldata$stnum),]

# Count the number of occurrences for each combination of PatientID and hour
duplicate_counts = VMI_biomarker_raw %>%
  group_by(PatientID, hour) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Filter for combinations that have more than one occurrence (duplicates)
duplicates = duplicate_counts %>%
  filter(count > 1)

# subset the data to only the duplicates
duplicate_data = merge(duplicates, VMI_biomarker_raw, all.x = T)
duplicate_remove = duplicate_data %>%
  filter(is.na(vesselLength))

VMI_biomarker_raw_no_dup = anti_join(VMI_biomarker_raw, duplicate_remove)
write.csv(VMI_biomarker_raw_no_dup, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_biomarker.csv", row.names = F)

# mass spec as the mass dataset
## include pAKI, mortality and other outcomes
VMI_renal_variable = c("Stnum",
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
                       "APACHEIII_baseline",
                       "Charlson",
                       "death_30days",
                       "death_90days",
                       "death", 
                       "day_alive", 
                       "day_alive_cens800", 
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
                       "Age_At_Enrollment",
                       "Sex_Scr")

VMI_renal_time_variant_variable = c("PatientID",
                                    "Platelet_Count6",
                                    "Platelet_Count24",
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
                                    "KIM1Val6",
                                    "KIM1Val24",
                                    "KIM1Val72",
                                    "NGALVal6",
                                    "NGALVal24",
                                    "NGALVal72")

VMI_renal_time_invariant_variable = c("PatientID", 
                                      "APACHEIII_baseline",
                                      "Charlson",
                                      "ref_Creatinine",
                                      "death_30days",
                                      "death_90days",
                                      "death", 
                                      "day_alive", 
                                      "day_alive_cens800", 
                                      "RRT_30days",
                                      "TIMP2_0",
                                      "IGFBP7_0",
                                      "Age_At_Enrollment",
                                      "Sex_Scr")

VMI_biomarker_variable = c("PatientID",
                           "hour",
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
                           "X__24_hours_Total_Fluids")

VMI_renal = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/SepsisAKI_Molinari_02112021.xlsx") %>%
  select(all_of(VMI_renal_variable))
VMI_biomarker = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_biomarker.csv") %>%
  select(all_of(VMI_biomarker_variable))
VMI_fulldata = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/Final_Complete_ProCESS_Dataset_03062019.xlsx") %>%
  select(Stnum_all, Enrdte)

# rename columns
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.6"] = "Platelet_Count6"
names(VMI_renal)[names(VMI_renal) == "Platelet_Count.24"] = "Platelet_Count24"
names(VMI_fulldata)[names(VMI_fulldata) == "Stnum_all"] = c("PatientID")
names(VMI_renal)[names(VMI_renal) == "Stnum"] = "PatientID"

# arrange data
VMI_renal = VMI_renal %>% arrange(PatientID)
VMI_biomarker = VMI_biomarker %>% arrange(PatientID, hour)

# convert wide to long format
VMI_renal_wide = VMI_renal %>%
  select(all_of(VMI_renal_time_variant_variable))

VMI_renal_long = pivot_longer(VMI_renal_wide, 
                              cols = -PatientID,  # All columns except 'id'
                              names_to = c(".value", "hour"),  # Split into 'variable' and 'time'
                              names_pattern = "(.*)(6|24|72)$") %>%  # Capture the variable name and the time points
  mutate(hour = as.numeric(hour))

VMI_renal_rest = VMI_renal %>%
  select(all_of(VMI_renal_time_invariant_variable))

data = merge(merge(merge(VMI_biomarker, VMI_renal_long, by = c("PatientID", "hour")), VMI_renal_rest, by = "PatientID"), VMI_fulldata, by = "PatientID") %>%
  select(PatientID, hour, Enrdte, everything()) %>%
  arrange(PatientID, hour)

data = merge(merge(merge(VMI_biomarker, VMI_renal_rest, by = c("PatientID")), VMI_fulldata, by = c("PatientID")), VMI_renal_long, by = c("PatientID", "hour"))

write.csv(data, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/merged_data.csv", row.names = F)


# entire ProCESS data (either Molinari data or the final complete ProCESS)
## descriptive and missingness and the method of imputation





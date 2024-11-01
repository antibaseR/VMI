library(tidyverse)
library(readxl)
source("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Code/VMI/0. VMI_Functions.R")

data = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/SepsisAKI_Molinari_02112021.xlsx")
names(data)


# Data for identifying the flag for AKI
data_AKI = data %>%
  select(Stnum, death, day_alive, day_alive_cens800, RRT_30days, 
         AKIbase_stage, matches("^AKI_Daily([1-9]|1[0-9]|2[0-8])$")) %>%
  mutate(death = death-1)

data_AKI$AKI_29Day = apply(data_AKI[, c("AKIbase_stage", paste0("AKI_Daily", 1:28))], 1, function(row) paste0(row, collapse = ","))

# Index for Persistent Severe AKI
## 1. Persistent KDIGO stage 3 AKI ≥ 72h
Persistent_Severe_AKI_Index1 = grep("3,3,3", data_AKI$AKI_29Day)
## 2. Dialysis
Persistent_Severe_AKI_Index2 = which(data_AKI$RRT_30days == 1)
## 3. Death following KDIGO stage 3 AKI (24 hrs)
data_AKI$stage3_last_date = as.numeric(lapply(data_AKI$AKI_29Day, find_last_position))
data_AKI$day_alive_rounded = round(data_AKI$day_alive, 0)
Persistent_Severe_AKI_Index3 = which(data_AKI$day_alive_rounded <= (data_AKI$stage3_last_date))
## 4. If in stage 2, patient must progress to stage 3 within 48h, and then persist in stage 3 for 72h
Persistent_Severe_AKI_Index4 = c(grep("2,3,3,3", data_AKI$AKI_29Day),
                                 grep("2,NA,3,3,3", data_AKI$AKI_29Day),
                                 grep("2,0,3,3,3", data_AKI$AKI_29Day),
                                 grep("2,1,3,3,3", data_AKI$AKI_29Day),
                                 grep("2,2,3,3,3", data_AKI$AKI_29Day))
## 5. Relapse of stage 3 (stage 3 to stage 2 to stage 3) was allowed if relapse occurred within 48h and subsequent stage 3 lasted for 72h.
Persistent_Severe_AKI_Index5 = c(grep("3,NA,3,3,3", data_AKI$AKI_29Day),
                                 grep("3,1,3,3,3", data_AKI$AKI_29Day),
                                 grep("3,2,3,3,3", data_AKI$AKI_29Day))

Persistent_Severe_AKI_Index = sort(unique(c(Persistent_Severe_AKI_Index1,
                                            Persistent_Severe_AKI_Index2,
                                            Persistent_Severe_AKI_Index3,
                                            Persistent_Severe_AKI_Index4,
                                            Persistent_Severe_AKI_Index5)))

# Index for Transient AKI
Transient_AKI_Index = seq(1, nrow(data_AKI), 1)[sapply(data_AKI$AKI_29Day, check_zero_after_non_zero)]

# Index for AKI recovery
## Among patients with AKI and patients without Transient AKI, those recover to stage 0
AKI_Recovery_Index1 = seq(1, nrow(data_AKI), 1)[sapply(data_AKI$AKI_29Day, check_zero_after_first_non_zero_non_na)]
AKI_Recovery_Index = AKI_Recovery_Index1[!(AKI_Recovery_Index1 %in% Transient_AKI_Index)]

# Index for Acute kidney disease (AKD)
## No stage 0 in any of the 7 days after first AKI
AKD_Index = seq(1, nrow(data_AKI), 1)[sapply(data_AKI$AKI_29Day, no_zero_in_first_7)]


# Construct variables
## AKI (yes/no): if the patient has AKI any day from enrollment to day 28 (AKI Daily > 0 in any day)
AKI_Index = seq(1, nrow(data_AKI), 1)[sapply(data_AKI$AKI_29Day, check_123_in_row)]
data_AKI$AKI = 0
data_AKI[AKI_Index, "AKI"] = 1
## First AKI date
data_AKI$FIRST_AKI_DATE = sapply(data_AKI$AKI_29Day, find_first_non_zero_non_na_position) - 1
## First AKI stage
data_AKI$FIRST_AKI_STAGE = sapply(data_AKI$AKI_29Day, find_first_non_zero_non_na_value)
## Persistent Severe AKI
data_AKI$Persistent_Severe_AKI = 0
data_AKI[Persistent_Severe_AKI_Index, "Persistent_Severe_AKI"] = 1
## Transient AKI
data_AKI$Transient_AKI = 0
data_AKI[Transient_AKI_Index, "Transient_AKI"] = 1
## No Persistent Severe AKI
data_AKI$No_Persistent_Severe_AKI = ifelse((data_AKI$Persistent_Severe_AKI == 0 & data_AKI$Transient_AKI == 0), 1, 0)
## Total AKI recovery
data_AKI$AKI_Recovery_Total = 0
data_AKI[AKI_Recovery_Index1, "AKI_Recovery_Total"] = 1
## AKI recovery
data_AKI$AKI_Recovery = 0
data_AKI[AKI_Recovery_Index, "AKI_Recovery"] = 1
data_AKI[Transient_AKI_Index, "AKI_Recovery"] = NA
## Acute kidney disease (AKD)
data_AKI$AKD = 0
data_AKI[AKD_Index, "AKD"] = 1
## Overlap between Persistent and Transient AKI
data_AKI$Persistent_Transient_Overlap = ifelse((data_AKI$Persistent_Severe_AKI==1 & data_AKI$Transient_AKI==1), 1, 0)


# Subset the data
data_AKI_sub = data_AKI %>%
  select(Stnum, AKI, FIRST_AKI_DATE, FIRST_AKI_STAGE, 
         Persistent_Severe_AKI, Transient_AKI, Persistent_Transient_Overlap, No_Persistent_Severe_AKI, 
         AKI_Recovery, AKI_Recovery_Total, AKD) 

names(data_AKI_sub)[names(data_AKI_sub) == "Stnum"] = "PatientID"

write.csv(data_AKI_sub, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/AKI_Flag_Molinari_Data.csv", row.names = F)

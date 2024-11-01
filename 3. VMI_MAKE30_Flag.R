library(tidyverse)
library(readxl)
source("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Code/VMI/0. VMI_Functions.R")

data = read_xlsx("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/SepsisAKI_Molinari_02112021.xlsx")
names(data)


# Data for identifying the flag for AKI
data_MAKE = data %>%
  select(Stnum, death_30days, RRT_30days, AKIbase_stage, matches("^AKI_Daily([1-9]|1[0-9]|2[0-8])$"))

data_MAKE$AKI_29Day = apply(data_MAKE[, c("AKIbase_stage", paste0("AKI_Daily", 1:28))], 1, function(row) paste0(row, collapse = ","))
data_MAKE$LAST_AKI = sapply(data_MAKE$AKI_29Day, last_non_na_string)
data_MAKE$Persistent_Renal_Dysfunction = ifelse(data_MAKE$LAST_AKI>0, 1, 0)
data_MAKE$MAKE30 = ifelse((data_MAKE$death_30days==1 | data_MAKE$RRT_30days==1 | data_MAKE$Persistent_Renal_Dysfunction==1), 1, 0)

# Subset the data
data_MAKE_sub = data_MAKE %>%
  select(Stnum, MAKE30) 
names(data_MAKE_sub)[names(data_MAKE_sub) == "Stnum"] = "PatientID"
write.csv(data_MAKE_sub, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/MAKE30_Flag_Molinari_Data.csv", row.names = F)


library(tidyverse)
library(vtable)

data = read.csv("/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Data/VMI/VMI_biomarker.csv")

missing = data.frame(colSums(is.na(data)) / nrow(data))
colnames(missing) = c("missingness")
missing$variable = rownames(missing)
missing = missing %>% arrange(variable)

sort(names(data)[2:length(names(data))])



missing = data.frame(colSums(is.na(data)) / nrow(data))
colnames(missing) = c("missingness")
write.csv(missing, "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Result/VMI/VMI_missing.csv")
st(data[, 2:ncol(data)], out="csv", file = "/Users/chenxinlei/Library/Mobile Documents/com~apple~CloudDocs/Projects/AKI/Result/VMI/VMI_summary_statistics.csv")



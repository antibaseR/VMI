library(tidyverse)

# Function to find positions of 3 in a row
find_positions = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  which(values == 3)
}

# Function to find the last position of 3 in a row
find_last_position = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  zero_positions = which(values == 3)
  if (length(zero_positions) > 0) {
    return(tail(zero_positions, n = 1))  # Return the last position
  } else {
    return(NA)  
  }
}

# Function to check if the 2nd or 3rd position is 0
check_second_third_zero = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  return((length(values) >= 2 && values[2] == 0) | (length(values) >= 3 && values[3] == 0))
}

# Function to find the position of the first non-zero and non-NA value
find_first_non_zero_non_na_position = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  for (i in seq_along(values)) {
    if (!is.na(values[i]) && values[i] != 0) {
      return(i)  
    }
  }
  return(NA)
}

# Function to check if any value in the row is 1, 2, or 3
check_123_in_row = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  return(any(values %in% c(1, 2, 3), na.rm = TRUE))
}

# Function to find the first number that is not 0 or NA
find_first_non_zero_non_na_value = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  for (value in values) {
    if (!is.na(value) && value != 0) {
      return(value)  
    }
  }
  return(NA)
}

# Function to check if there's a 0 after the first non-zero, non-NA value
check_zero_after_first_non_zero_non_na = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  first_non_zero_non_na_index = which(!is.na(values) & values != 0)[1]
  if (is.na(first_non_zero_non_na_index)) {
    return(FALSE)
  }
  return(any(values[(first_non_zero_non_na_index + 1):length(values)] == 0, na.rm = TRUE))
}

# Function to check if there is no 0 in the first 7 positions
no_zero_in_first_7 = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  return(!any(values[1:min(7, length(values))] == 0, na.rm = TRUE))
}

# Function to check if there is any 0 in the first 2 positions after the first non-zero, non-NA value
check_zero_after_non_zero = function(row) {
  values = unlist(strsplit(row, ","))
  values[values == "NA"] = NA
  values = as.numeric(values)
  first_non_zero_non_na_index = which(!is.na(values) & values != 0)[1]
  if (is.na(first_non_zero_non_na_index)) {
    return(FALSE)
  }
  next_two_values = values[(first_non_zero_non_na_index + 1):(first_non_zero_non_na_index + 2)]
  return(any(next_two_values == 0, na.rm = TRUE))
}

# Function to find the last non-NA value in a string
last_non_na_string <- function(x) {
  # Split the string by commas
  elements <- unlist(strsplit(x, ","))
  
  # Remove "NA" and find the last element
  elements_non_na <- elements[elements != "NA"]
  
  # Return the last non-NA value
  tail(elements_non_na, 1)
}


# Function to calculate the p-value for table 1
pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- kruskal.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

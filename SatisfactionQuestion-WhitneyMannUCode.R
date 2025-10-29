if(!requireNamespace("haven", quietly = TRUE))
  install.packages("haven", quiet = TRUE, dependencies = TRUE)
library(haven)
library(dplyr)
library(ggplot2)

file_path <-"/Users/jessicawoskett/Library/CloudStorage/OneDrive-UNBC/Woskett_Analysis/spssdata.sav"#Change this path using you account
data<-read_sav(file_path) #data reading SPSS file
str(data) #Data structure checking

#since both groups are in a different column, Create a dataset for Fishers
fishers_data <- data.frame(
  response = data$q0019_0001,
  group = "Fishers"
)
# Create a dataset for Non-Fishers
non_fishers_data <- data.frame(
  response = data$q0052_0001,
  group = "Non-Fishers"
)
# Combine the two datasets
combined_data <- rbind(fishers_data, non_fishers_data)

# Make sure group is a factor
combined_data$group <- as.factor(combined_data$group)

# Run the Mann-Whitney U Test
wilcox.test(response ~ group, data = combined_data)



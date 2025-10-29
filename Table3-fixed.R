# Install packages if needed
if(!requireNamespace("haven", quietly = TRUE)) install.packages("haven", dependencies = TRUE)
if(!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable", dependencies = TRUE)

# Load packages
library(haven)
library(dplyr)
library(flextable)

# Load data
file_path <-"/Users/jessicawoskett/Library/CloudStorage/OneDrive-UNBC/Woskett_Analysis/spssdata.sav"#Change this path using you account
data<-read_sav(file_path) #data reading SPSS file
str(data) #Data structure checking''

# Convert SPSS labelled variables to numeric
# Fisher_status: convert to numeric
data$Fisher_status <- as.numeric(data$Fisher_status)

# Survey response columns
fisher_cols <- c("q0036_0001","q0036_0002","q0036_0003","q0036_0004","q0036_0005","q0036_0006")
nonfisher_cols <- c("q0069_0001","q0069_0002","q0069_0003","q0069_0004","q0069_0005","q0069_0006")

# Convert all question columns to numeric
for(col in c(fisher_cols, nonfisher_cols)) {
  data[[col]] <- as.numeric(data[[col]])
}

# storage vectors
p_values <- numeric(length(fisher_cols))
fisher_means <- numeric(length(fisher_cols))
fisher_sds <- numeric(length(fisher_cols))
nonfisher_means <- numeric(length(fisher_cols))
nonfisher_sds <- numeric(length(fisher_cols))

# Loop through question pairs
for(i in seq_along(fisher_cols)) {
  
  # Subset responses
  fisher_responses <- data[[fisher_cols[i]]][data$Fisher_status == 1]       # Fisher = 1
  nonfisher_responses <- data[[nonfisher_cols[i]]][data$Fisher_status == 2] # Non-fisher = 2
  
  # Remove NAs
  fisher_responses <- fisher_responses[!is.na(fisher_responses)]
  nonfisher_responses <- nonfisher_responses[!is.na(nonfisher_responses)]
  
  # Means and SDs
  fisher_means[i] <- mean(fisher_responses, na.rm = TRUE)
  fisher_sds[i] <- sd(fisher_responses, na.rm = TRUE)
  nonfisher_means[i] <- mean(nonfisher_responses, na.rm = TRUE)
  nonfisher_sds[i] <- sd(nonfisher_responses, na.rm = TRUE)
  
  # Mann-Whitney U test (only if both groups have >1 value)
  if(length(fisher_responses) > 1 && length(nonfisher_responses) > 1) {
    test <- wilcox.test(fisher_responses, nonfisher_responses, exact = FALSE)
    p_values[i] <- test$p.value
  } else {
    p_values[i] <- NA
    warning(paste("Not enough data for:", fisher_cols[i]))
  }
}

# Create summary table
results <- data.frame(
  question_fisher = fisher_cols,
  question_nonfisher = nonfisher_cols,
  fisher_mean = fisher_means,
  fisher_sd = fisher_sds,
  nonfisher_mean = nonfisher_means,
  nonfisher_sd = nonfisher_sds,
  p_values = p_values
)

# Create "Mean ± SD" columns
results$Fishers <- paste0(round(results$fisher_mean, 2), " ± ", round(results$fisher_sd, 2))
results$Non_fishers <- paste0(round(results$nonfisher_mean, 2), " ± ", round(results$nonfisher_sd, 2))

# Add significance column - probability that its rejecting the null hypothesis effect fix p_values instead of p_adj and then round 
results$p_values <- round(p_values, 2)
results$Significance <- ifelse(results$p_values < 0.05, "*", "")

# APA-style table
apa_table <- results[, c("question_fisher", "Fishers", "Non_fishers", "p_values", "Significance")]
colnames(apa_table) <- c("Statement", "Fishers (M ± SD)", "Non-fishers (M ± SD)", "p_values", "Significance")

# Format p-values for APA
apa_table$p_values <- ifelse(!is.na(apa_table$p_values) & apa_table$p_values < 0.001, "< .001",
                             ifelse(!is.na(apa_table$p_values), round(apa_table$p_values, 2), NA))

# Replace question codes with readable statements
apa_table$Statement <- c(
  "To feed family",
  "To feed myself",
  "To catch allowed limit of fish",
  "To catch a lot of fish",
  "To catch big fish",
  "For culture or tradition"
)

# View final table
print(apa_table)

# Create and export flextable
ft <- flextable(apa_table) %>%
  autofit() %>%
  bold(j = "Statement") %>%
  fontsize(size = 12) %>%
  align(j = 2:5, align = "center") %>%
  set_caption(caption = "Fishers’ and Non-Fishers’ Desired Outcomes with Fishing Experiences")

save_as_docx("Table" = ft, path = "APA_table3-DesiredOutcomes2.docx")



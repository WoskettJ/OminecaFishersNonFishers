# Setup
if(!requireNamespace("haven", quietly = TRUE))
  install.packages("haven", quiet = TRUE, dependencies = TRUE)
install.packages("flextable")  # if you haven’t installed it yet

library(haven)
library(dplyr)
library(flextable)             # load the package

file_path <-"/Users/jessicawoskett/Library/CloudStorage/OneDrive-UNBC/Woskett_Analysis/spssdata.sav"#Change this path using you account
data<-read_sav(file_path) #data reading SPSS file
str(data) #Data structure checking''

# Make sure Fisher_status column has "Fisher" and "Non-fisher"
fisher_cols <- c(
  "q0037_0001",
  "q0037_0002",
  "q0037_0003",
  "q0037_0004",
  "q0037_0005",
  "q0037_0006",
  "q0037_0007",
  "q0037_0008",
  "q0037_0009",
  "q0037_0010",
  "q0037_0011",
  "q0037_0012"
)

nonfisher_cols <- c(
  "q0070_0001",
  "q0070_0002",
  "q0070_0003",
  "q0070_0004",
  "q0070_0005",
  "q0070_0006",
  "q0070_0007",
  "q0070_0008",
  "q0070_0009",
  "q0070_0010",
  "q0070_0011",
  "q0070_0012"
)

# Initialize vectors for results
p_values <- numeric(length(fisher_cols))
fisher_means <- numeric(length(fisher_cols))
fisher_sds <- numeric(length(fisher_cols))
nonfisher_means <- numeric(length(fisher_cols))
nonfisher_sds <- numeric(length(fisher_cols))

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

data$Fisher_status <- trimws(as.character(data$Fisher_status))
data$Fisher_status <- ifelse(data$Fisher_status == "1", "Fisher",
                             ifelse(data$Fisher_status == "2", "Non-fisher", NA))
unique(data$Fisher_status)
# Should print: "Fisher" "Non-fisher" NA

# Loop through each question pair one to one comparison 
for(i in 1:length(fisher_cols)) {
  
  # Subset responses
  fisher_responses <- as.numeric(data[data$Fisher_status == "Fisher", fisher_cols[i], drop = TRUE])
  nonfisher_responses <- as.numeric(data[data$Fisher_status == "Non-fisher", nonfisher_cols[i], drop = TRUE])
  
  # Mann-Whitney U test non-parametric because it's not normal distribution -> continuous variable
  test <- wilcox.test(fisher_responses, nonfisher_responses, exact = FALSE)
  results$p_values[i] <- test$p.value
  
  # Store p-value in both vectors
  p_values[i] <- test$p.value
  results$p_values[i] <- test$p.value
  
  # Means and SDs
  results$fisher_mean[i] <- mean(fisher_responses, na.rm = TRUE)
  results$fisher_sd[i] <- sd(fisher_responses, na.rm = TRUE)
  results$nonfisher_mean[i] <- mean(nonfisher_responses, na.rm = TRUE)
  results$nonfisher_sd[i] <- sd(nonfisher_responses, na.rm = TRUE)
}

# Create "Mean ± SD" columns
results$Fishers <- paste0(round(results$fisher_mean, 2), " ± ", round(results$fisher_sd, 2))
results$Non_fishers <- paste0(round(results$nonfisher_mean, 2), " ± ", round(results$nonfisher_sd, 2))

# Add significance column - probability that its rejecting the null hypothesis effect fix p_values instead of p_adj and then round 
results$p_values <- round(p_values, 2)
results$Significance <- ifelse(results$p_values < 0.05, "*", "")

# Create final APA-style table
apa_table <- results[, c("question_fisher", "Fishers", "Non_fishers", "p_values", "Significance")]
colnames(apa_table) <- c("Statement", "Fishers (M ± SD)", "Non-fishers (M ± SD)", "p_values", "Significance")

# Format p-values for APA
apa_table$p_values <- ifelse(as.numeric(apa_table$p_values) < 0.001, "< .001", as.character(apa_table$p_values))

# Replace statement column with real text labels
apa_table$Statement <- c(
  "Keep and eat fish",
  "Catch and release fish",
  "Fish with family and friends",
  "Fish alone",
  "Fish from the shoreline",
  "Fish from watercraft",
  "Fish at stocked lakes",
  "Fish wild fish in lakes",
  "Fish wild fish in rivers",
  "Fish to feel relaxed",
  "Fish to feel excited",
  "Catch challenging fish"
)

# View the final clean APA-style table
print(apa_table)

# Create flextable from apa_table
ft <- flextable(apa_table)

# Format for APA style
ft <- ft %>%
  autofit() %>%                                 # Auto adjust column widths
  fontsize(size = 12) %>%                       # Set APA-friendly font size
  align(j = 2:5, align = "center") %>%         # Center numeric columns
  set_caption(caption = "Fishers' and Non-Fishers' Preferences for Fishing")

# Export to Word
save_as_docx("Table" = ft, path = "APA_table2-Preferences.docx")
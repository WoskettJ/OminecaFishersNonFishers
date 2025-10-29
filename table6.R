# Setup
if(!requireNamespace("haven", quietly = TRUE))
  install.packages("haven", quiet = TRUE, dependencies = TRUE)

library(haven)
library(dplyr)
# Replace 'data' with the name of your dataframe
# Make sure Fisher_status column has "Fisher" and "Non-fisher"

fisher_cols <- c(
  "q0029_0001",
  "q0029_0002",
  "q0029_0003",
  "q0029_0004",
  "q0029_0005",
  "q0029_0006",
  "q0029_0007"
)

nonfisher_cols <- c(
  "q0062_0001",
  "q0062_0002",
  "q0062_0003",
  "q0062_0004",
  "q0062_0005",
  "q0062_0006",
  "q0062_0007"
)

# Initialize vectors for results
p_values <- numeric(length(fisher_cols))
fisher_means <- numeric(length(fisher_cols))
fisher_sds <- numeric(length(fisher_cols))
nonfisher_means <- numeric(length(fisher_cols))
nonfisher_sds <- numeric(length(fisher_cols))

# Loop through each question pair
for(i in 1:length(fisher_cols)) {
  
  # Subset responses
  fisher_responses <- data[data$Fisher_status == "Fisher", fisher_cols[i]]
  nonfisher_responses <- data[data$Fisher_status == "Non-fisher", nonfisher_cols[i]]
  
  # Make sure these are numeric 
  fisher_responses <- as.numeric(fisher_responses[[1]])
  nonfisher_responses <- as.numeric(nonfisher_responses[[1]])
  
  # Mann-Whitney U test
  test <- wilcox.test(fisher_responses, nonfisher_responses, exact = FALSE)
  p_values[i] <- test$p.value
  
  # Means and SDs
  fisher_means[i] <- mean(fisher_responses, na.rm = TRUE)
  fisher_sds[i] <- sd(fisher_responses, na.rm = TRUE)
  nonfisher_means[i] <- mean(nonfisher_responses, na.rm = TRUE)
  nonfisher_sds[i] <- sd(nonfisher_responses, na.rm = TRUE)
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

# Create final APA-style table
apa_table <- results[, c("question_fisher", "Fishers", "Non_fishers", "p_values", "Significance")]
colnames(apa_table) <- c("Statement", "Fishers (M ± SD)", "Non-fishers (M ± SD)", "p_values", "Significance")

# Format p-values for APA
apa_table$p_values <- ifelse(as.numeric(apa_table$p_values) < 0.001, "< .001", as.character(apa_table$p_values))

# Replace statements with actual text
apa_table$Statement <- c(
  "All genders well represented",
  "Inspired to fish (hear about women)",
  "Inspired to fish (hear about men)",
  "Inspired to fish (hear about gender diverse people)",
  "Felt discriminated against (gender identity)",
  "Witness gendered discrimination",
  "Experience gendered discrimination"
)

# View final APA-style table
print(apa_table)

#This will quickly show which column(s) have 0 valid observations for one group — those are the ones breaking your loop.
sapply(fisher_cols, function(x) {
  paste0(
    "Fishers valid in ", x, ": ", sum(!is.na(data[data$Fisher_status == "Fisher", x])),
    " | Non-fishers valid: ", sum(!is.na(data[data$Fisher_status == "Non-fisher", x]))
  )
})

if (length(na.omit(fisher_responses)) == 0 | length(na.omit(nonfisher_responses)) == 0) {
  p_values[i] <- NA
  next
}

# Create flextable
library(flextable)
ft <- flextable(apa_table)
ft <- ft %>%
  autofit() %>%
  fontsize(size = 12) %>%
  align(j = 2:5, align = "center") %>%
  set_caption(caption = "Fishers’ and Non-Fishers’ Perceptions of Gender within Fishing")

# Export to Word
save_as_docx("Table" = ft, path = "APA_table6-Gender.docx")
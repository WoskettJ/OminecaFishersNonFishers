# Setup
if(!requireNamespace("haven", quietly = TRUE))
  install.packages("haven", quiet = TRUE, dependencies = TRUE)

library(haven)
library(dplyr)

# Convert haven_labelled variables to factors
data <- data %>%
  mutate(across(starts_with("q0040_"), as_factor)) %>%
  mutate(across(starts_with("q0073_"), as_factor))

fisher_cols <- c(
  "q0040_0001",
  "q0040_0002",
  "q0040_0003",
  "q0040_0004",
  "q0040_0005",
  "q0040_0006",
  "q0040_0007",
  "q0040_0008",
  "q0040_0009",
  "q0040_0010",
  "q0040_0011"
)

nonfisher_cols <- c(
  "q0073_0001",
  "q0073_0002",
  "q0073_0003",
  "q0073_0004",
  "q0073_0005",
  "q0073_0006",
  "q0073_0007",
  "q0073_0008",
  "q0073_0009"
  ,"q0073_0010",
  "q0073_0011"
)

# Initialize results 
p_values <- numeric(length(fisher_cols))
fisher_props <- numeric(length(fisher_cols))
nonfisher_props <- numeric(length(fisher_cols))

# Loop through paired questions
for(i in 1:length(fisher_cols)) {
  
  fisher_responses <- na.omit(data[[fisher_cols[i]]])
  nonfisher_responses <- na.omit(data[[nonfisher_cols[i]]])
  
  # Skip if empty
  if(length(fisher_responses) == 0 | length(nonfisher_responses) == 0) {
    p_values[i] <- NA
    fisher_props[i] <- NA
    nonfisher_props[i] <- NA
    next
  }
  
  # Build combined dataset
  combined <- data.frame(
    group = factor(c(rep("Fisher", length(fisher_responses)),
                     rep("Non-fisher", length(nonfisher_responses)))),
    response = factor(c(as.character(fisher_responses),
                        as.character(nonfisher_responses)))
  )
  
  # Contingency table
  tbl <- table(combined$group, combined$response)
  
  # Run Chi-square
  if(all(dim(tbl) > 1)) {
    test <- suppressWarnings(chisq.test(tbl))
    p_values[i] <- test$p.value
  } else {
    p_values[i] <- NA
  }
  
  # % choosing the most frequent category
  fisher_tab <- prop.table(table(fisher_responses))
  nonfisher_tab <- prop.table(table(nonfisher_responses))
  
  fisher_props[i] <- round(max(fisher_tab) * 100, 1)
  nonfisher_props[i] <- round(max(nonfisher_tab) * 100, 1)
}

# Results table 
results <- data.frame(
  question_fisher = fisher_cols,
  question_nonfisher = nonfisher_cols,
  fisher_prop = fisher_props,
  nonfisher_prop = nonfisher_props,
  p_values = round(p_values, 2)
)

results$Significance <- ifelse(!is.na(results$p_values) & results$p_values < 0.05, "*", "")

# APA-style table
apa_table <- results[, c("question_fisher", "fisher_prop", "nonfisher_prop", "p_values", "Significance")]
colnames(apa_table) <- c("Statement", "Fishers (%)", "Non-fishers (%)", "p_values", "Significance")

# Format p-values
apa_table$p_values <- ifelse(is.na(apa_table$p_values), "",
                             ifelse(apa_table$p_values < 0.001, "< .001",
                                    as.character(round(apa_table$p_values, 2))))

# Replace question labels
apa_table$Statement <- c(
  "To find companionship",
  "To be challenged",
  "To improve skills",
  "To bring friends together",
  "To get away",
  "To be close to nature",
  "To catch fish for eating",
  "To relax",
  "To catch large fish",
  "To bring family closer together",
  "To catch many fish"
)

# --- Print APA-style table ---
print(apa_table)

# Create flextable
library(flextable)
ft <- flextable(apa_table)
ft <- ft %>%
  autofit() %>%
  fontsize(size = 12) %>%
  align(j = 2:5, align = "center") %>%
  set_caption(caption = "Fishers’ and Non-Fishers’ Reasons for Going Fishing")

# Export to Word
save_as_docx("Table" = ft, path = "APA_table4CHI-SQ-Fishers’ and Non-Fishers’ Reasons for Going Fishing.docx")

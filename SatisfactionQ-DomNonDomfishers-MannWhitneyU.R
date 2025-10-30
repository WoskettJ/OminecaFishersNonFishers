if(!requireNamespace("haven", quietly = TRUE))
  install.packages("haven", quiet = TRUE, dependencies = TRUE)
library(haven)
library(dplyr)
library(ggplot2)

file_path <-"/Users/jessicawoskett/Library/CloudStorage/OneDrive-UNBC/Woskett_Analysis/spssdata.sav"#Change this path using you account
data<-read_sav(file_path) #data reading SPSS file
str(data) #Data structure checkingif(!requireNamespace("haven", quietly = TRUE))
install.packages("haven", quiet = TRUE, dependencies = TRUE)

library(haven)
library(dplyr)
library(ggplot2)

file_path <-"/Users/jessicawoskett/Library/CloudStorage/OneDrive-UNBC/Woskett_Analysis/spssdata.sav" #Change this path using you account
data<-read_sav(file_path) #data reading SPSS file

str(data) #Data structure checking
View(data)

#####Getting the Fisher groups by reclassifying answers Q12

data<-data%>%
  mutate(Fisher_status=as.factor(case_when(
    q0012==2 ~ "Non-fisher",
    TRUE ~ "Fisher"
  )))

######

non_dominant_gender_columns<-c("q0007_0003", "q0007_0002","q0007_0004", "q0007_0005", "q0007_0006", "q0007_0008", "q0007_0009",
                               "q0007_0010", "q0007_0011", "q0007_0012", "q0007_0013", "q0007_0014")
data <- data %>% 
  mutate(Gender_status = as.factor(case_when(
    q0007_0001 == 1  ~ "Dominant",
    TRUE ~ if_else(rowSums(select(., all_of(non_dominant_gender_columns)), na.rm = TRUE) > 0, "Non_Dominant", NA_character_)
  )))

###Jessica's attempt below to create the sexuality & race column

non_dominant_sexuality_columns<-c("q0008_0001", "q0008_0002","q0008_0003", "q0008_0004", "q0008_0005", "q0008_0006",
                                  "q0008_0008", "q0008_0009")
data <- data %>%
  mutate(Sexuality_status = as.factor(case_when(
    q0008_0007 == 1  ~ "Dominant",
    TRUE ~ if_else(rowSums(select(., all_of(non_dominant_sexuality_columns)), na.rm = TRUE) > 0, "Non_Dominant", NA_character_)
  ))) 

non_dominant_race_columns<-c("q0009_0001", "q0009_0002","q0009_0003", "q0009_0004", "q0009_0005", "q0009_0006", "q0009_0007",
                             "q0009_0008", "q0009_0009", "q0009_0010", "q0009_0011", "q0009_0012", "q0009_0013", "q0009_0015", "q0009_0016")
data <- data %>%
  mutate(race_status = as.factor(case_when(
    q0009_0014 == 1  ~ "Dominant",
    TRUE ~ if_else(rowSums(select(., all_of(non_dominant_race_columns)), na.rm = TRUE) > 0, "Non_Dominant", NA_character_)
  )))

# Sort overall status dominant/non-dominant
data <- data %>%
  mutate(non_dominant_all_columns = as.factor(case_when(
    Gender_status == "Dominant" & 
      race_status == "Dominant" & 
      Sexuality_status == "Dominant" ~ "Dominant",
    TRUE ~ "Non_Dominant"
  )))

View(data)

#fishers only
fishers_only <- data %>%
  filter(Fisher_status == "Fisher")

#satisfaction question
fisher_satisfaction <- fishers_only %>%
  select(response = q0019_0001,  # Satisfaction score
         dominance = non_dominant_all_columns) %>%
  filter(!is.na(response), !is.na(dominance))  # Remove missing values

# Run the Mann-Whitney U Test
wilcox.test(response ~ dominance, data = fisher_satisfaction)


fisher_satisfaction %>%
  group_by(dominance) %>%
  summarise(
    mean_satisfaction = mean(response),
    sd_satisfaction = sd(response),
    n = n()
  )

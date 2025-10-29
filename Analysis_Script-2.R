if(!requireNamespace("haven", quietly = TRUE))
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

##end of attempt, I think it worked 

#combine the fisher and non-fisher satisfaction responses into one new variable.
data <- data %>%
  mutate(satisfaction = case_when(
    Fisher_status == "Fisher" ~ q0019_0001,
    Fisher_status == "Non-fisher" ~ q0052_0001,
    TRUE ~ NA_real_
  ))

#Create a combined group variable (4 groups)
data <- data %>%
  mutate(group_4cat = case_when(
    Fisher_status == "Fisher" & non_dominant_all_columns == "Dominant" ~ "Dominant Fishers",
    Fisher_status == "Fisher" & non_dominant_all_columns == "Non_Dominant" ~ "Non-Dominant Fishers",
    Fisher_status == "Non-fisher" & non_dominant_all_columns == "Dominant" ~ "Dominant Non-Fishers",
    Fisher_status == "Non-fisher" & non_dominant_all_columns == "Non_Dominant" ~ "Non-Dominant Non-Fishers",
    TRUE ~ NA_character_
  ))

# First create a combined group variable for dominant and non-dominant fishers
data <- data %>%
  mutate(fisher_dominance_group = case_when(
    Fisher_status == "Fisher" & non_dominant_all_columns == "Dominant" ~ "Dominant Fishers",
    Fisher_status == "Fisher" & non_dominant_all_columns == "Non_Dominant" ~ "Non-Dominant Fishers",
    TRUE ~ NA_character_
  ))
#combine those two satisfaction variables into a single column
data <- data %>%
  mutate(satisfaction = case_when(
    Fisher_status == "Fisher" ~ q0019_0001,
    Fisher_status == "Non-fisher" ~ q0052_0001,
    TRUE ~ NA_real_
  ))
#run the kruskal-wallis test
kruskal.test (satisfaction ~ group_4cat, data = data)

#ggplot 
library(ggplot2)

ggplot(data, aes(x = group_4cat, y = satisfaction, fill = group_4cat)) +
  geom_boxplot() +
  labs(title = "Satisfaction Scores by Group",
       x = "Group",
       y = "Satisfaction Score") +
  scale_fill_manual(values = c("Dominant Fishers" = "blue", 
                               "Non-Dominant Fishers" = "lightblue", 
                               "Dominant Non-Fishers" = "green", 
                               "Non-Dominant Non-Fishers" = "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "none")

# Count how many NAs are in the final variables used for the plot/test
sum(is.na(data$satisfaction))
sum(is.na(data$group_4cat))

# Find the total count of non-missing satisfaction scores
sum(!is.na(data$satisfaction))

###Replace Values by labels

###You can use this approach for the main columns in which you are more interested on the label. 
age_labels<-c("Under 18", "18-24 years old","25-34 years old","35-44 years old","45-54 years old", "55-64 years old","65-74 years old","75 years or older","Prefer not to answer")
scholarity<-c("Less than high school graduation", "Graduated high school or equivalent","Some college or university, no degree or diploma","Trades/technical/vocational diploma or certification",
              "Bachelor’s Degree","Master’s Degree","Doctoral Degree","Other (please specify)")

##old from 2024: 
#replacing values using factor and Frequency table calculation by Level of School
Fisher_Gender <- data %>%
  group_by(Fisher_status, Gender_status) %>%
  summarise(Frequency = n()) %>%
  mutate(Percent = round(Frequency/sum(Frequency)*100, 1))

chisq.test(Fisher_Gender)

##drop the NAs before chi square will work 

####Plot Age and Scholarity

Fisher_Gender %>%
  ggplot(aes(Gender_status, Frequency, 
             fill = Gender_status, 
             color = Gender_status)) +
  facet_grid(~Fisher_status) +
  geom_bar(stat="identity", position=position_dodge()) +
  theme(axis.text.x=element_blank())
 
##filter fishers
Fisher_data<-data %>%
  filter(Fisher_status=="Fisher")

##filter non-fishers did not work
Non_fisher_Data<-data %>%
  filter(Fisher_status=="Non_fisher")

##t-test did not work it has to be used to compare means of two groups
t.test(Fisher_status~Gender_status,data=data)


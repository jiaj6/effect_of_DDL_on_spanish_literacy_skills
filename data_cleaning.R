### Preamble ###
# Purpose: explore the effect of DLL (a literacy training program) on the Spanish literacy skills of 
# first-grade Spanish-speaking students who have poor literacy skills
# Author: Jia Jia Ji
# Email: jiajia.ji@mail.utoronto.ca
# Date: April 19, 2021
# To do: read in the raw data and clean it by removing the missing values and replacing the outliers with average values


### install and load the packages ###
#install.packages('tidyverse')
#install.packages('ggplot2')
library(tidyverse) 
library(ggplot2)

### R and R packages citations ###
citation()
citation('tidyverse')
citation('ggplot2')

# read the data in the csv file format
lgrm_data <- read_csv('Inputs/data/raw_data.csv')

### Data Cleaning ###

# remove the redundant column: Group
lgrm_data <- lgrm_data[, -3]

# row 113 and 145 are outliers (having extremely low pretest scores), 
# replace each pretest score with the average of its corresponding variable for this instance's assigned group
# both rows are in the control group, so calculate the mean of each pretest score for control group 
# (when calculating the mean, ignore the missing values and these 2 outliers for now)
control_post <- lgrm_data[-c(113, 145),]%>%
  filter(T_assignment == '0') %>%
  select(Posttest_READING, Posttest_LANGUAGE, Posttest_VOCABULARY, Posttest_ELA.TOTAL)
ctrl_post_re_avg <- mean(na.omit(control_post$Posttest_READING))
ctrl_post_la_avg <- mean(na.omit(control_post$Posttest_LANGUAGE))
ctrl_post_vo_avg <- mean(na.omit(control_post$Posttest_VOCABULARY))
ctrl_post_total_avg <- mean(na.omit(control_post$Posttest_ELA.TOTAL))
# replace each pre-test score of row 113 and 145 with the average
lgrm_data[113, 3] <- ctrl_post_re_avg
lgrm_data[113, 4] <- ctrl_post_la_avg
lgrm_data[113, 5] <- ctrl_post_vo_avg
lgrm_data[113, 6] <- ctrl_post_total_avg

lgrm_data[145, 3] <- ctrl_post_re_avg
lgrm_data[145, 4] <- ctrl_post_la_avg
lgrm_data[145, 5] <- ctrl_post_vo_avg
lgrm_data[145, 6] <- ctrl_post_total_avg

# ignore the missing values for pretest score and posttest score of each subtest
clean_data_reading <- lgrm_data %>%
  select(STUDENTID, SCHOOLID, T_assignment, Pretest_READING, Posttest_READING) %>%
  na.omit()
clean_data_language <- lgrm_data %>%
  select(STUDENTID, SCHOOLID, T_assignment, Pretest_LANGUAGE , Posttest_LANGUAGE) %>%
  na.omit()
clean_data_vocabulary <- lgrm_data %>%
  select(STUDENTID, SCHOOLID, T_assignment, Pretest_VOCABULARY , Posttest_VOCABULARY) %>%
  na.omit()
clean_data_overall <- lgrm_data %>%
  select(STUDENTID, SCHOOLID, T_assignment, Pretest_ELA.TOTAL, Posttest_ELA.TOTAL) %>%
  na.omit()

# write the clean data for each subtest in a csv file and put it in the input folder
write_csv(clean_data_reading, "Inputs/data/clean_data_reading.csv")
write_csv(clean_data_language, "Inputs/data/clean_data_language.csv")
write_csv(clean_data_vocabulary, "Inputs/data/clean_data_vocabulary.csv")
write_csv(clean_data_overall, "Inputs/data/clean_data_overall.csv")

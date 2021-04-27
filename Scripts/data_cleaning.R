### Preamble ###
# Purpose: explore the effect of DLL (a literacy training program) on 
# first-grade Spanish-speaking students who have poor literacy skills
# Author: Jia Jia Ji
# Email: jiajia.ji@mail.utoronto.ca
# Date: April 19, 2021
# To do: draw some graphs to visualize the differences of 
# Logramos test scores between the control group and treatment group, 
# and fit a linear regression model to see if this effect is statistically significant


### install and load the packages ###
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('gridExtra')
library(tidyverse) 
library(ggplot2)
library(gridExtra)

### R and R packages citations ###
citation()
citation('tidyverse')
citation('ggplot2')


# read the data in the csv file format
lgrm_data <- read_csv('Inputs/data/raw_data.csv')

### Data Cleaning ###

# row 113 and 145 are outliers (having extremely low pre-test scores), 
# replace each pre-test score with its corresponding average of this instance's assigned group
# both rows are in the control group, so calculate the mean of each pre-test score for control group (when calculating the mean, ignore the missing values and these 2 outliers for now)
control_post <- lgrm_data[-c(113, 145),]%>%
  filter(T_assignment == '0') %>%
  select(Posttest_READING, Posttest_LANGUAGE, Posttest_VOCABULARY, Posttest_ELA.TOTAL)
ctrl_post_re_avg <- mean(na.omit(control_post$Posttest_READING))
ctrl_post_la_avg <- mean(na.omit(control_post$Posttest_LANGUAGE))
ctrl_post_vo_avg <- mean(na.omit(control_post$Posttest_VOCABULARY))
ctrl_post_total_avg <- mean(na.omit(control_post$Posttest_ELA.TOTAL))
# replace each pre-test score of row 113 and 145 with the average
lgrm_data[113, 4] <- ctrl_post_re_avg
lgrm_data[113, 5] <- ctrl_post_la_avg
lgrm_data[113, 6] <- ctrl_post_vo_avg
lgrm_data[113, 7] <- ctrl_post_total_avg

lgrm_data[145, 4] <- ctrl_post_re_avg
lgrm_data[145, 5] <- ctrl_post_la_avg
lgrm_data[145, 6] <- ctrl_post_vo_avg
lgrm_data[145, 7] <- ctrl_post_total_avg


# replace the other rows that have missing values for some specific variables (related to pre-test or post-test scoress) with the corresponding mean
# also calculate the mean of 
control_pre <- lgrm_data %>%
  filter(T_assignment == '0') %>%
  select(Pretest_READING, Pretest_LANGUAGE, Pretest_VOCABULARY, Pretest_ELA.TOTAL)
ctrl_pre_re_avg <- mean(na.omit(control_pre$Pretest_READING))
ctrl_pre_la_avg <- mean(na.omit(control_pre$Pretest_LANGUAGE))
ctrl_pre_vo_avg <- mean(na.omit(control_pre$Pretest_VOCABULARY))
ctrl_pre_total_avg <- mean(na.omit(control_pre$Pretest_ELA.TOTAL))

# 
trt_pre_post <- lgrm_data %>%
  filter(T_assignment == '1') %>%
  select(Pretest_READING, Pretest_LANGUAGE, Pretest_VOCABULARY, Pretest_ELA.TOTAL, Posttest_READING, Posttest_LANGUAGE, Posttest_VOCABULARY, Posttest_ELA.TOTAL)
trt_pre_re_avg <- mean(na.omit(trt_pre_post$Pretest_READING))
trt_pre_la_avg <- mean(na.omit(trt_pre_post$Pretest_LANGUAGE))
trt_pre_vo_avg <- mean(na.omit(trt_pre_post$Pretest_VOCABULARY))
trt_pre_total_avg <- mean(na.omit(trt_pre_post$Pretest_ELA.TOTAL))

trt_post_re_avg <- mean(na.omit(trt_pre_post$Posttest_READING))
trt_post_la_avg <- mean(na.omit(trt_pre_post$Posttest_LANGUAGE))
trt_post_vo_avg <- mean(na.omit(trt_pre_post$Posttest_VOCABULARY))
trt_post_total_avg <- mean(na.omit(trt_pre_post$Posttest_ELA.TOTAL))

# row 35: control group: postest_reading, posttest_vocabulary and posttest_total are missing
lgrm_data[35, 4] <- ctrl_post_re_avg
lgrm_data[35, 6] <- ctrl_post_vo_avg
lgrm_data[35, 7] <- ctrl_post_total_avg

# row 38, 63, 81, 83: control group: posttest_reading, posttest_total, pretest_reading and pretest_total are missing
lgrm_data[38, 4] <- ctrl_post_re_avg
lgrm_data[38, 7] <- ctrl_post_total_avg
lgrm_data[38, 8] <- ctrl_pre_re_avg
lgrm_data[38, 11] <- ctrl_pre_total_avg

lgrm_data[63, 4] <- ctrl_post_re_avg
lgrm_data[63, 7] <- ctrl_post_total_avg
lgrm_data[63, 8] <- ctrl_pre_re_avg
lgrm_data[63, 11] <- ctrl_pre_total_avg

lgrm_data[81, 4] <- ctrl_post_re_avg
lgrm_data[81, 7] <- ctrl_post_total_avg
lgrm_data[81, 8] <- ctrl_pre_re_avg
lgrm_data[81, 11] <- ctrl_pre_total_avg

lgrm_data[83, 4] <- ctrl_post_re_avg
lgrm_data[83, 7] <- ctrl_post_total_avg
lgrm_data[83, 8] <- ctrl_pre_re_avg
lgrm_data[83, 11] <- ctrl_pre_total_avg
# row 43, 67, 80, 85, 93: treatment group: pretest_reading, pretest_total are missing
lgrm_data[43, 8] <- trt_pre_re_avg
lgrm_data[43, 11] <- trt_pre_total_avg

lgrm_data[67, 8] <- trt_pre_re_avg
lgrm_data[67, 11] <- trt_pre_total_avg

lgrm_data[80, 8] <- trt_pre_re_avg
lgrm_data[80, 11] <- trt_pre_total_avg

lgrm_data[85, 8] <- trt_pre_re_avg
lgrm_data[85, 11] <- trt_pre_total_avg

lgrm_data[93, 8] <- trt_pre_re_avg
lgrm_data[93, 11] <- trt_pre_total_avg

# row 44: treatment group: posttest_language, posttest_total, pretest_reading, pretest-total are missing
lgrm_data[44, 5] <- trt_post_la_avg
lgrm_data[44, 7] <- trt_post_total_avg
lgrm_data[44, 8] <- trt_pre_re_avg
lgrm_data[44, 11] <- trt_pre_total_avg
# row 65, 139: control group: posttest_language, posttest_total, pretest_reading, pretest_total are missing
lgrm_data[65, 5] <- ctrl_post_la_avg
lgrm_data[65, 7] <- ctrl_post_total_avg
lgrm_data[65, 8] <- ctrl_pre_re_avg
lgrm_data[65, 11] <- ctrl_pre_total_avg

lgrm_data[139, 5] <- ctrl_post_la_avg
lgrm_data[139, 7] <- ctrl_post_total_avg
lgrm_data[139, 8] <- ctrl_pre_re_avg
lgrm_data[139, 11] <- ctrl_pre_total_avg
# row 71, 79: control group: pretest_reading, pretest_total are missing
lgrm_data[71, 8] <- ctrl_pre_re_avg
lgrm_data[71, 11] <- ctrl_pre_total_avg

lgrm_data[79, 8] <- ctrl_pre_re_avg
lgrm_data[79, 11] <- ctrl_pre_total_avg
# row 73, 91: treatment group: posttest_reading, posttest_total, pretest_reading, pretest_total are missing
lgrm_data[73, 4] <- trt_post_re_avg
lgrm_data[73, 7] <- trt_post_total_avg
lgrm_data[73, 8] <- trt_pre_re_avg
lgrm_data[73, 11] <- trtl_pre_total_avg

lgrm_data[91, 4] <- trt_post_re_avg
lgrm_data[91, 7] <- trt_post_total_avg
lgrm_data[91, 8] <- trt_pre_re_avg
lgrm_data[91, 11] <- trtl_pre_total_avg
# row 82: control group: posttest_reading, posttest_language, posttest_total, pretest_reading, pretest_total are missing
lgrm_data[82, 4] <- ctrl_post_re_avg
lgrm_data[82, 5] <- ctrl_post_la_avg
lgrm_data[82, 7] <- ctrl_post_total_avg
lgrm_data[82, 8] <- ctrl_pre_re_avg
lgrm_data[82, 11] <- ctrl_pre_total_avg
# row 84: treatment group: posttest_reading, posttest_language, posttest_total, pretest_reading, pretest_total are missing
lgrm_data[84, 4] <- trt_post_re_avg
lgrm_data[84, 5] <- trt_post_la_avg
lgrm_data[84, 7] <- trt_post_total_avg
lgrm_data[84, 8] <- trt_pre_re_avg
lgrm_data[84, 11] <- trt_pre_total_avg


# remove the rows that have missing values for either all pre-test scores or all post-test scores
clean_data <- lgrm_data[-c(7, 25, 29, 30, 32, 45, 64, 68, 126, 152),]

# write the raw data in a csv file and put it in the input folder
write_csv(clean_data, "Inputs/data/clean_data.csv")
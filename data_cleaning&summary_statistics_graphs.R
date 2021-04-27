### Preamble ###
# Purpose: explore the effect of DLL (a literacy training program) on the Spanish literacy skills of 
# first-grade Spanish-speaking students who have poor literacy skills
# Author: Jia Jia Ji
# Email: jiajia.ji@mail.utoronto.ca
# Date: April 19, 2021
# To do: draw some graphs to visualize the differences in Logramos test scores between the control group and treatment group


### install and load the packages ###
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('knitr')
library(tidyverse) 
library(ggplot2)
library(gridExtra)
library(knitr)

### R and R packages citations ###
citation()
citation('tidyverse')
citation('ggplot2')
citation('gridExtra')
toBibtex(citation('knitr'))

# read the clean data 
clean_data_reading <- read_csv('Inputs/data/clean_data_reading.csv')
clean_data_language <- read_csv('Inputs/data/clean_data_language.csv')
clean_data_vocabulary <- read_csv('Inputs/data/clean_data_vocabulary.csv')
clean_data_overall <- read_csv('Inputs/data/clean_data_overall.csv')

### Summary statistics & graphs ###

# convert the variable T_assignment from the numerical variable to categorical variable that has 2 levels (0 and 1)
clean_data_reading$T_assignment <- as.factor(clean_data_reading$T_assignment)
clean_data_language$T_assignment <- as.factor(clean_data_language$T_assignment)
clean_data_vocabulary$T_assignment <- as.factor(clean_data_vocabulary$T_assignment)
clean_data_overall$T_assignment <- as.factor(clean_data_overall$T_assignment)

# For the pretest scores of the 3 subtests and an overall score:

# 1. pretest score for Reading (Pretest_READING):
# draw the histogram of this variable for 2 groups (0 means control group, 1 means treatment group)
pre_reading_plot1 <- clean_data_reading %>%
  ggplot(mapping = aes(x = Pretest_READING, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Pretest score in Reading', y = 'Count/Number of students',
       title = 'Histogram of pretest scores in Reading for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
pre_reading_plot2 <- clean_data_reading %>%
  ggplot(mapping = aes(x = T_assignment, y = Pretest_READING, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Pretest score for Reading',
       title = 'Boxplot of pretest scores in Reading for the students in control group and treatment group') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# 2. pretest score for Language (Pretest_LANGUAGE):
# draw the histogram for 2 groups 
pre_language_plot1 <- clean_data_language %>%
  ggplot(mapping = aes(x = Pretest_LANGUAGE, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Pretest score of Language', y = 'Count/Number of students', 
       title = 'Histogram of pretest scores in Language for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
pre_language_plot2 <- clean_data_language %>%
  ggplot(mapping = aes(x = T_assignment, y = Pretest_LANGUAGE, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Pretest score in Language',
       title = 'Boxplot of pretest scores in Language for the students in control group and treatment group') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# put the histograms and boxplots for the pretest_reading and pretest_language together
grid.arrange(arrangeGrob(pre_reading_plot1, pre_reading_plot2, ncol = 2),
             arrangeGrob(pre_language_plot1, pre_language_plot2, ncol = 2))

# 3. pre-test score for Vocabulary (Pretest_VOCABULARY):
# draw the histogram for 2 groups 
pre_vocabulary_plot1 <- clean_data_vocabulary %>%
  ggplot(mapping = aes(x = Pretest_VOCABULARY, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Pretest score in Vocabulary', y = 'Count/Number of students', 
       title = 'Histogram of pretest scores in Vocabulary for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
pre_vocabulary_plot2 <- clean_data_vocabulary %>%
  ggplot(mapping = aes(x = T_assignment, y = Pretest_VOCABULARY, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Pretest score in Vocabulary',
       title = 'Boxplot of pretest scores in Vocabulary for the students in control group and treatment group:',
       subtitle = '') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# 4. pretest overall score (Pretest_ELA.TOTAL):
# draw the histogram for 2 groups 
pre_overall_plot1 <- clean_data_overall %>%
  ggplot(mapping = aes(x = Pretest_ELA.TOTAL, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Pretest overall score', 
       y = 'Count/Number of students', 
       title = 'Histogram of pretest overall scores for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
pre_overall_plot2 <- clean_data_overall %>%
  ggplot(mapping = aes(x = T_assignment, y = Pretest_ELA.TOTAL, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Pretest overall score',
       title = 'Boxplot of pretest overall scores for the students in control group and treatment group') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# put the histograms and boxplots for teh pretest_Vocabulary and pretetst total 
grid.arrange(arrangeGrob(pre_vocabulary_plot1, pre_vocabulary_plot2, ncol = 2),
             arrangeGrob(pre_overall_plot1, pre_overall_plot2, ncol = 2))


# make a table of the average value of all 3 subtests and 1 overall score
# filter the pretest scores for control group
control_pre_re_avg <- clean_data_reading %>%
  filter(T_assignment == '0') %>%
  select(Pretest_READING) %>%
  sapply(mean)
control_pre_la_avg <- clean_data_language %>%
  filter(T_assignment == '0') %>%
  select(Pretest_LANGUAGE) %>%
  sapply(mean)
control_pre_vo_avg <- clean_data_vocabulary %>%
  filter(T_assignment == '0') %>%
  select(Pretest_VOCABULARY) %>%
  sapply(mean)
control_pre_overall_avg <- clean_data_overall %>%
  filter(T_assignment == '0') %>%
  select(Pretest_ELA.TOTAL) %>%
  sapply(mean)
# filter the pretest scores for treatment group
trt_pre_re_avg <- clean_data_reading %>%
  filter(T_assignment == '1') %>%
  select(Pretest_READING) %>%
  sapply(mean)
trt_pre_la_avg <- clean_data_language %>%
  filter(T_assignment == '1') %>%
  select(Pretest_LANGUAGE) %>%
  sapply(mean)
trt_pre_vo_avg <- clean_data_vocabulary %>%
  filter(T_assignment == '1') %>%
  select(Pretest_VOCABULARY) %>%
  sapply(mean)
trt_pre_overall_avg <- clean_data_overall %>%
  filter(T_assignment == '1') %>%
  select(Pretest_ELA.TOTAL) %>%
  sapply(mean)

# combine the mean of 2 groups in 1 table
pre_avg_summary <- matrix(c(control_pre_re_avg, control_pre_la_avg, control_pre_vo_avg, control_pre_overall_avg,
                            trt_pre_re_avg, trt_pre_la_avg, trt_pre_vo_avg, trt_pre_overall_avg), ncol = 4, byrow = TRUE)
pre_avg_table <- as.table(pre_avg_summary) 
rownames(pre_avg_table) <- c('Control', 'Treatment')
colnames(pre_avg_table) <- c('Reading' ,'Language', 'Vocabulary', 'Overall score')
pre_avg_table <- pre_avg_table %>%
  knitr::kable(caption = 'Average value of the pretest scores for 3 subtests and the overall score for the students in control group and treatment group',
               align = c('l', 'l', 'l', 'l'), digits = 2)
pre_avg_table


# For the posttest scores of the 3 subtests and an overall score:

# 1. posttest score for Reading (Posttest_READING):
# draw the histogram of this variable for 2 groups (0 means control group, 1 means treatment group)
post_reading_plot1 <- clean_data_reading %>%
  ggplot(mapping = aes(x = Posttest_READING, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Posttest score in Reading', y = 'Count/Number of students',
       title = 'Histogram of posttest scores in Reading for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
post_reading_plot2 <- clean_data_reading %>%
  ggplot(mapping = aes(x = T_assignment, y = Posttest_READING, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Posttest score in Reading',
       title = 'Boxplot of posttest scores in Reading for the students in control group and treatment group') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# 2. posttest score for Language (Posttest_LANGUAGE):
# draw the histogram for 2 groups 
post_language_plot1 <- clean_data_language %>%
  ggplot(mapping = aes(x = Posttest_LANGUAGE, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Posttest score in Language', y = 'Count/Number of students', 
       title = 'Histogram of posttest scores in Language for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
post_language_plot2 <- clean_data_language %>%
  ggplot(mapping = aes(x = T_assignment, y = Posttest_LANGUAGE, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Posttest score in Language',
       title = 'Boxplot of posttest scores in Language for the students in control group and treatment group:',
       subtitle = '') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')


# 3. post-test score for Vocabulary (Posttest_VOCABULARY):
# draw the histogram for 2 groups 
post_vocabulary_plot1 <- clean_data_vocabulary %>%
  ggplot(mapping = aes(x = Posttest_VOCABULARY, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Posttest score in Vocabulary', y = 'Count/Number of students', 
       title = 'Histogram of posttest scores in Vocabulary for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
post_vocabulary_plot2 <- clean_data_vocabulary %>%
  ggplot(mapping = aes(x = T_assignment, y = Posttest_VOCABULARY, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Posttest score in Vocabulary',
       title = 'Boxplot of posttest scores in Vocabulary for the students in control group and treatment group:',
       subtitle = '') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')


# 4. posttest overall score (Posttest_ELA.TOTAL):
# draw the histogram for 2 groups 
post_overall_plot1 <- clean_data_overall %>%
  ggplot(mapping = aes(x = Posttest_ELA.TOTAL, fill = T_assignment)) + 
  geom_histogram(position = 'dodge', bins = 30) +
  labs(x = 'Posttest overall score', 
       y = 'Count/Number of students', 
       title = 'Histogram of posttest overall scores for the students in control group and treatment group') +
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()

# draw the boxplot for 2 groups
post_overall_plot2 <- clean_data_overall %>%
  ggplot(mapping = aes(x = T_assignment, y = Posttest_ELA.TOTAL, fill = T_assignment)) + 
  geom_boxplot() +
  labs(x = 'Group', y = 'Posttest overall score',
       title = 'Boxplot of posttest overall scores for the students in control group and treatment group') + 
  scale_fill_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  scale_x_discrete(breaks = c('0', '1'), labels = c('Control', 'Treatment')) +
  theme(legend.position = 'none')

# put the histogram and boxplot together horizontally
grid.arrange(arrangeGrob(post_reading_plot1, post_reading_plot2, ncol = 2),
             arrangeGrob(post_vocabulary_plot1, post_vocabulary_plot2, ncol = 2),
             arrangeGrob(post_language_plot1, post_language_plot2, ncol = 2),
             arrangeGrob(post_overall_plot1, post_overall_plot2, ncol = 2), ncol = 2)

            
# make a table of the average value of all 3 subtests and 1 overall score
# filter the posttest scores for control group
control_post_re_avg <- clean_data_reading %>%
  filter(T_assignment == '0') %>%
  select(Posttest_READING) %>%
  sapply(mean)
control_post_la_avg <- clean_data_language %>%
  filter(T_assignment == '0') %>%
  select(Posttest_LANGUAGE) %>%
  sapply(mean)
control_post_vo_avg <- clean_data_vocabulary %>%
  filter(T_assignment == '0') %>%
  select(Posttest_VOCABULARY) %>%
  sapply(mean)
control_post_overall_avg <- clean_data_overall %>%
  filter(T_assignment == '0') %>%
  select(Posttest_ELA.TOTAL) %>%
  sapply(mean)

# filter the posttest scores for treatment group
trt_post_la_avg <- clean_data_reading %>%
  filter(T_assignment == '1') %>%
  select(Posttest_READING) %>%
  sapply(mean)
trt_post_la_avg <- clean_data_language %>%
  filter(T_assignment == '1') %>%
  select(Posttest_LANGUAGE) %>%
  sapply(mean)
trt_post_vo_avg <- clean_data_vocabulary %>%
  filter(T_assignment == '1') %>%
  select(Posttest_VOCABULARY) %>%
  sapply(mean)
trt_post_overall_avg <- clean_data_overall %>%
  filter(T_assignment == '1') %>%
  select(Posttest_ELA.TOTAL) %>%
  sapply(mean)

# combine the mean of 2 groups in 1 table
post_avg_summary <- matrix(c(control_post_re_avg, control_post_la_avg, control_post_vo_avg, control_post_overall_avg,
                             trt_post_re_avg, trt_post_la_avg, trt_post_vo_avg, trt_post_overall_avg), ncol = 4, byrow = TRUE)
post_avg_table <- as.table(post_avg_summary) 
rownames(post_avg_table) <- c('Control', 'Treatment')
colnames(post_avg_table) <- c('Reading' ,'Language', 'Vocabulary', 'Overall score')
post_avg_table <- post_avg_table %>%
  knitr::kable(caption = 'Average value of the posttest scores for 3 subtests and the overall score for the students in control group and treatment group',
               align = c('l', 'l', 'l', 'l'), digits = 2)
post_avg_table


# For the correlation between pretest and posttest scores: 
# (we only check this relationship for the overall score, because it combines all 3 subtests and we want to see the overall relationship)
# draw a scatter plot between pretest overall score and posttest overall score for 2 groups, and add the best fit line (linear)
clean_data_overall %>%
  ggplot(mapping = aes(x = Pretest_ELA.TOTAL, y = Posttest_ELA.TOTAL, color = T_assignment)) + 
  geom_point() + 
  geom_smooth(method = lm, aes(color = T_assignment)) +
  labs(x = 'Pretest overall score', y = 'Posttest overall score',
       title = 'Scatter plot between pretest overall score and posttest overall score for the students in control group and treatment group',
       subtitle = '') +
  scale_color_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme()

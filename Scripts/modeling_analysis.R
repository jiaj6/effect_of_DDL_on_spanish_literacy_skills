### Preamble ###
# Purpose: explore the effect of DLL (a literacy training program) on the Spanish literacy skills of 
# first-grade Spanish-speaking students who have poor literacy skills
# Author: Jia Jia Ji
# Email: jiajia.ji@mail.utoronto.ca
# Date: April 19, 2021
# To do: fit a linear mixed model to see whether the effect of DLL on the posttest score is statistically significant,
# also check model assumptions


### install and load the packages ###
#install.packages('tidyverse')
#install.packages('ggplot2')
#install.packages('nlme')
#install.packages('knitr')
#install.packages('Pmisc')
#install.packages('emmeans')
#install.packages('lme4')
#install.packages('gridExtra')
library(tidyverse) 
library(ggplot2)
library(nlme)
library(knitr)
library(Pmisc)
library(emmeans)
library(lme4)
library(gridExtra)

### R and R packages citations ###
citation()
citation('tidyverse')
citation('ggplot2')
citation('nlme')
toBibtex(citation('knitr'))
citation('emmeans')
citation('Pmisc')
citation('lme4')
citation('gridExtra')

### Model ###

# for each subtest (including the overall score), fit a linear mixed model, because
# 1. the continuous value for response variables (ie. posttest scores)
# 2. school_id can be treated as the random effect, because there are repeated measurements and there are correlations within a school,
# and some schools may have higher teaching quality than others
# 3. T_assignment and pretest score are the fixed effects

# 1. linear mixed model for scores in Reading
reading_model <- lme(Posttest_READING ~ T_assignment + Pretest_READING, random = ~1|SCHOOLID, method = 'REML', data = clean_data_reading)
# summary statistics
knitr::kable(Pmisc::lmeTable(reading_model), caption ='The summary statistics for the fitted Reading model', digits = 4)
# estimated marginal means for T_assignment
knitr::kable(summary(emmeans(reading_model, ~ T_assignment)), caption = 'The estimated marginal means for T.assignment in the fitted Reading model', digits = 2)

# scatter plot of pretest score and predicted posttest score 
fit1 <- fitted(reading_model)
clean_data_reading[,6] <- fit1
p1 <-clean_data_reading %>%
  ggplot(mapping = aes(x = Pretest_READING, y = fit1, color = T_assignment)) + 
  geom_point() +
  labs(x = 'Pretest score in Reading', y = 'Predicted posttest score',
       title = 'Scatter plot between pretest scores and predicted posttest scores in Reading for the students in control group and treatment group') + 
  scale_color_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()


# check assumptions
# random effect
random_effect1 <- lme4::ranef(reading_model)[[1]]
# errors 
error1 <- residuals(reading_model)

# for random effect
par(mfrow = c(2, 2))
# histogram 
hist(random_effect1, xlab = 'Random effect', main = 'Histogram of random effect')
# qq plot
qqnorm(random_effect1, main = 'QQ plot of random effect')
qqline(random_effect1)
# plot the random effect
plot(random_effect1, main = 'Scatter plot of random effect vs index')
abline(0, 0)

# for errors: conditional residuals
par(mfrow = c(2, 2))
# histogram 
hist(error1, main = 'Histogram of errors')
# qq plot
qqnorm(error1, main = 'QQ plot of errors')
qqline(error1)
# plot errors
plot(error1, main = 'Scatter plot of errors vs index')
abline(0, 0)
# scatter plot of fitted value and errors
plot(fit1, error1, main = 'Scatter plot of fitted value vs errors')
abline(0, 0)

# marginal residual
marginal_res1 <- resid(reading_model, level = 0)
# qq plot
qqnorm(marginal_res1, main = 'QQ plot of marginal residuals')
qqline(marginal_res1)

# variance-covariance
knitr::kable(var(cbind(clean_data_reading$Posttest_READING[clean_data_reading$T_assignment == '0'], 
                       clean_data_reading$Posttest_READING[clean_data_reading$T_assignment == '1'])), caption = 'The table of variance-covariance martix for 2 groups')


# 2. linear mixed model for scores in Language
language_model <- lme(Posttest_LANGUAGE ~ T_assignment + Pretest_LANGUAGE, random = ~1|SCHOOLID, method = 'REML', data = clean_data_language)
# summary statistics
knitr::kable(Pmisc::lmeTable(language_model), caption ='The summary statistics for the fitted Language model', digits = 4)
# estimated marginal means for T_assignment
sknitr::kable(summary(emmeans(language_model, ~ T_assignment)), caption = 'The estimated marginal means for T.assignment in the fitted Language model', digits = 2)

# scatter plot of pretest score and predicted posttest score 
fit2 <- fitted(language_model)
clean_data_language[,6] <- fit2
p2 <- clean_data_language %>%
  ggplot(mapping = aes(x = Pretest_LANGUAGE, y = fit2, color = T_assignment)) + 
  geom_point() +
  labs(x = 'Pretest score in Language', y = 'Predicted posttest score in Language',
       title = 'Scatter plot between pretest scores and predicted posttest scores in Language for the students in control group and treatment group') + 
  scale_color_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()


# check assumptions
# random effect
random_effect2 <- lme4::ranef(language_model)[[1]]
# errors 
error2 <- residuals(language_model)

# for random effect
par(mfrow = c(1, 3))
# histogram 
hist(random_effect2, xlab = 'Random effect', main = 'Histogram of random effect')
# qq plot
qqnorm(random_effect2, main = 'QQ plot of random effect')
qqline(random_effect2)
# plot the random effect
plot(random_effect2, main = 'Scatter plot of random effect vs index')
abline(0, 0)

# for errors: conditional residuals
par(mfrow = c(2, 2))
# histogram 
hist(error2, main = 'Histogram of errors')
# qq plot
qqnorm(error2, main = 'QQ plot of errors')
qqline(error2)
# plot errors
plot(error2, main = 'Scatter plot of errors vs index')
abline(0, 0)
# scatter plot of fitted value and errors
plot(fit2, error2, main = 'Scatter plot of fitted value vs errors')
abline(0, 0)

# marginal residual
marginal_res2 <- resid(language_model, level = 0)
# qq plot
qqnorm(marginal_res2, main = 'QQ plot of marginal residuals')
qqline(marginal_res2)

# variance-covariance
knitr::kable(var(cbind(clean_data_language$Posttest_LANGUAGE[clean_data_language$T_assignment == '0'], 
                       clean_data_language$Posttest_LANGUAGE[clean_data_language$T_assignment == '1'])), caption = 'The table of variance-covariance martix for 2 groups')


# 3. linear mixed model for scores in Vocabulary
vocabulary_model <- lme(Posttest_VOCABULARY ~ T_assignment + Pretest_VOCABULARY, random = ~1|SCHOOLID, method = 'REML', data = clean_data_vocabulary)
# summary statistics
knitr::kable(Pmisc::lmeTable(vocabulary_model), caption ='The summary statistics for the fitted Vocabulary model', digits = 4)
# estimated marginal means for T_assignment
knitr::kable(summary(emmeans(vocabulary_model, ~ T_assignment)), caption = 'The estimated marginal means for T.assignment in the fitted Vocabulary model', digits = 2)

# scatter plot of pretest score and predicted posttest score 
fit3 <- fitted(vocabulary_model)
clean_data_vocabulary[,6] <- fit3
p3 <- clean_data_vocabulary %>%
  ggplot(mapping = aes(x = Pretest_VOCABULARY, y = fit3, color = T_assignment)) + 
  geom_point() +
  labs(x = 'Pretest score in Vocabulary', y = 'Predicted posttest score in Vocabulary',
       title = 'Scatter plot between pretest scores and predicted posttest scores in Vocabulary for the students in control group and treatment group') + 
  scale_color_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()


# check assumptions
# random effect
random_effect3 <- lme4::ranef(vocabulary_model)[[1]]
# errors 
error3 <- residuals(vocabulary_model)

# for random effect
par(mfrow = c(1, 3))
# histogram 
hist(random_effect3, xlab = 'Random effect', main = 'Histogram of random effect')
# qq plot
qqnorm(random_effect3, main = 'QQ plot of random effect')
qqline(random_effect3)
# plot the random effect
plot(random_effect3, main = 'Scatter plot of random effect vs index')
abline(0, 0)

# for errors: conditional residuals
par(mfrow = c(2, 2))
# histogram 
hist(error3, main = 'Histogram of errors')
# qq plot
qqnorm(error3, main = 'QQ plot of errors')
qqline(error3)
# plot errors
plot(error3, main = 'Scatter plot of errors vs index')
abline(0, 0)
# scatter plot of fitted value and errors
plot(fit3, error3, main = 'Scatter plot of fitted value vs errors')
abline(0, 0)

# marginal residual
marginal_res3 <- resid(vocabulary_model, level = 0)
# qq plot
qqnorm(marginal_res3, main = 'QQ plot of marginal residuals')
qqline(marginal_res3)

# variance-covariance
knitr::kable(var(cbind(clean_data_vocabulary$Posttest_VOCABULARY[clean_data_vocabulary$T_assignment == '0'], 
                       clean_data_vocabulary$Posttest_VOCABULARY[clean_data_vocabulary$T_assignment == '1'])), caption = 'The table of variance-covariance martix for 2 groups')


# 4. linear mixed model for overall scores
overall_model <- lme(Posttest_ELA.TOTAL ~ T_assignment + Pretest_ELA.TOTAL, random = ~1|SCHOOLID, method = 'REML', data = clean_data_overall)
# summary statistics
knitr::kable(Pmisc::lmeTable(overall_model), caption ='The summary statistics for the fitted Overall Score model', digits = 4)
# estimated marginal means for T_assignment
knitr::kable(summary(emmeans(overall_model, ~ T_assignment)), caption = 'The estimated marginal means for T.assignment in the fitted Overall Score model', digits = 2)

# scatter plot of pretest score and predicted posttest score 
fit4 <- fitted(overall_model)
clean_data_overall[,6] <- fit4
p4 <- clean_data_overall %>%
  ggplot(mapping = aes(x = Pretest_ELA.TOTAL, y = fit4, color = T_assignment)) + 
  geom_point() +
  labs(x = 'Pretest overall score', y = 'Predicted posttest overall score',
       title = 'Scatter plot between pretest oervall scores and predicted posttest overall scores for the students in control group and treatment group') + 
  scale_color_discrete(name = 'Group', labels = c('Control', 'Treatment')) +
  theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol = 2)


# check assumptions
# random effect
random_effect4 <- lme4::ranef(overall_model)[[1]]
# errors 
error4 <- residuals(overall_model)

# for random effect
par(mfrow = c(1, 3))
# histogram 
hist(random_effect4, xlab = 'Random effect', main = 'Histogram of random effect')
# qq plot
qqnorm(random_effect4, main = 'QQ plot of random effect')
qqline(random_effect4)
# plot the random effect
plot(random_effect4, main = 'Scatter plot of random effect vs index')
abline(0, 0)

# for errors: conditional residuals
par(mfrow = c(2, 2))
# histogram 
hist(error4, main = 'Histogram of errors')
# qq plot
qqnorm(error4, main = 'QQ plot of errors')
qqline(error4)
# plot errors
plot(error4, main = 'Scatter plot of errors vs index')
abline(0, 0)
# scatter plot of fitted value and errors
plot(fit4, error4, main = 'Scatter plot of fitted value vs errors')
abline(0, 0)

# marginal residual
marginal_res4 <- resid(overall_model, level = 0)
# qq plot
qqnorm(marginal_res4, main = 'QQ plot of marginal residuals')
qqline(marginal_res4)

# variance-covariance
knitr::kable(var(cbind(clean_data_overall$Posttest_ELA.TOTAL[clean_data_overall$T_assignment == '0'], 
                       clean_data_overall$Posttest_ELA.TOTAL[clean_data_overall$T_assignment == '1'])), caption = 'The table of variance-covariance martix for 2 groups')

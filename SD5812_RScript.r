
setwd("C:/Users/Administrator/Documents/data")


install.packages("readstata13")
install.packages("tidyverse")
install.packages("MASS")
install.packages("brant")
install.packages("ordinal")
install.packages("nnet")
install.packages("stats")
install.packages("readr")
install.packages("haven")
install.packages("dplyr")
install.packages("visdat")
install.packages("car")
install.packages("ggplot2")
install.packages("rbibutils")
install.packages("lme4")
install.packages("VGAM")
install.packages("lmerTest")
install.packages("modelsummary")
install.packages("pandoc")


library(readstata13)
library(tidyverse)
library(MASS)
library(brant)
library(ordinal)
library(nnet)
library(stats)
library(readr)
library(haven)
library(dplyr)
library(visdat)
library(car)
library(ggplot2)
library(lme4)
library(VGAM)
library(lmerTest)
library(modelsummary)
library(pandoc)


ess_data <- read_dta("ess7.dta")

glimpse(ess_data)
head(ess_data)

To select variables of interest

ess_ess <- ess_data %>% 
  dplyr::select(idno, gndr, agea, cntry,
                edulvlb, eduyrs,
                hinctnta,imwbcnt, imueclt,
                cgtsmke, dosprt)

Data Cleaning
Because variables are entered as labelled survey not numeric variables

ess_data <- ess_data %>% 
  mutate(
    imwbcnt = as.numeric(zap_labels(imwbcnt)),
    imueclt = as.numeric(zap_labels(imueclt)),
    cgtsmke = as.numeric(zap_labels(cgtsmke)),
    dosprt = as.numeric(zap_labels(dosprt)),
    gndr   = as.numeric(zap_labels(gndr))
  )

To remove invalid responses from the outcome variables (dont know, refusal, no answer)

ess_data <- ess_data %>% 
  mutate(across(c(imwbcnt, imueclt),
                ~ ifelse(.x %in% c(7, 8, 9), NA, .x)))


Selecting countries of interest

countries <- c("SE", "DE", "GB", "HU")

ess_subset <- ess_data %>% 
  dplyr::filter(cntry %in% countries)

Descriptive statistics by gender to understand group patterns

ess_subset <- ess_data %>% 
  filter(gndr %in% c(1, 2)) %>%   # keep valid genders only
  group_by(gndr) %>% 
  summarise(
    mean_same = mean(imwbcnt, na.rm = TRUE),
    sd_same   = sd(imwbcnt, na.rm = TRUE),
    mean_diff = mean(imueclt, na.rm = TRUE),
    sd_diff   = sd(imueclt, na.rm = TRUE),
    n = n()
  )

ess_subset

differences between means- same ethnic groups

t.test(imwbcnt ~ gndr, data = ess_data)

differences between means- different ethnic immigrants

t.test(imueclt ~ gndr, data = ess_data)

a cross-country comparison for the two immigration outcomes
discriptive comparison table

c("SE","DE","GB","HU")

four_cty <- ess_data %>% 
  filter(cntry %in% countries)

ess_subset <- four_cty %>% 
  group_by(cntry) %>% 
  summarise(
    mean_same = mean(imwbcnt, na.rm = TRUE),
    sd_same   = sd(imwbcnt, na.rm = TRUE),
    mean_diff = mean(imueclt, na.rm = TRUE),
    sd_diff   = sd(imueclt, na.rm = TRUE),
    n = n()
  )

ess_subset


boxplots for showing distributions, same-ethnic immigrants


ggplot(four_cty, aes(x = cntry, y = imwbcnt)) +
  geom_boxplot() +
  labs(
    x = "Country",
    y = "Attitudes (same ethnicity)",
    title = "Attitudes toward same-ethnic immigrants across countries"
  ) +
  theme_minimal()

Different ethnic immigrants

ggplot(four_cty, aes(x = cntry, y = imueclt)) +
  geom_boxplot() +
  labs(
    x = "Country",
    y = "Attitudes (different ethnicity)",
    title = "Attitudes toward different-ethnic immigrants across countries"
  ) +
  theme_minimal()

mean comparison bar chart

ggplot(four_cty, aes(x = cntry, y = imueclt)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(
    x = "Country",
    y = "Mean attitude",
    title = "Mean attitudes toward different-ethnic immigrants"
  ) +
  theme_minimal()

ggplot(four_cty, aes(x = cntry, y = imwbcnt)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2) +
  labs(
    x = "Country",
    y = "Mean attitude",
    title = "Mean attitudes toward different-ethnic immigrants"
  ) +
  theme_minimal()



statistical test across countries
same ethnic immigrants

anova_same <- aov(imwbcnt ~ cntry, data = four_cty)
summary(anova_same)


different ethnic immigrants

anova_diff <- aov(imueclt ~ cntry, data = four_cty)
summary(anova_diff)

Clean the variable cigarette smoke

clean_ess <- ess_subset %>% 
  filter(cgtsmke %in% c(1:6)) %>% 
  mutate(cig = 7 - cgtsmke)

head(clean_ess)

inspect whether this has been done that correctly by comparing with old cigarette

with(clean_ess,
     table(cig, cgtsmke))

Convert the numerical variables into categorical variables

clean_ess <- clean_ess %>% 
  mutate(
    gndr = factor(gndr, labels = c("Male", "Female")),
    cntry = factor(cntry),
    edulvlb = factor(edulvlb, ordered = TRUE),
    hinctnta = factor(hinctnta, ordered = TRUE)
  )

Simplify categories to for clarity

education into 3 groups
clean_ess <- clean_ess %>% 
  mutate(edu3 = case_when(
    edulvlb %in% 1:2 ~ "Low",
    edulvlb %in% 3:4 ~ "Medium",
    edulvlb %in% 5:6 ~ "High"
  ))

Income into 3 groups
clean_ess <- clean_ess %>% 
  mutate(inc3 = case_when(
    hinctnta %in% 1:3 ~ "Low",
    hinctnta %in% 4:7 ~ "Middle",
    hinctnta %in% 8:10 ~ "High"
  ))

We also clean the ouctome variables and remove missing values

clean_ess <- clean_ess %>% 
  mutate(
    imwbcnt = as.numeric(haven::zap_labels(imwbcnt)),
    imueclt = as.numeric(haven::zap_labels(imueclt))
  )

clean_ess <- clean_ess %>% 
  mutate(
    imwbcnt = ifelse(imwbcnt %in% c(7, 8, 9), NA, imwbcnt),
    imueclt = ifelse(imueclt %in% c(7, 8, 9), NA, imueclt)
  )
table(clean_ess$imwbcnt, useNA = "ifany")
table(clean_ess$imueclt, useNA = "ifany")


Prepare outcomes as ordered factors


clean_ess <- clean_ess %>% 
  mutate(
    imwbcnt_ord = factor(imwbcnt, ordered = TRUE),
    imueclt_ord = factor(imueclt, ordered = TRUE)
  )

Fitting the ordinal regression models

model_same <- polr(
  imwbcnt_ord ~ cntry + gndr + agea + edulvlb + hinctnta + dosprt,
  data = clean_ess,
  Hess = TRUE
)

model_diff <- polr(
  imueclt_ord ~ cntry + gndr + agea + edulvlb + hinctnta + dosprt,
  data = clean_ess,
  Hess = TRUE
)

summary(model_same)
summary(model_diff)

Conduct the Brant test (proportional odds assumption)

brant(model_same)
brant(model_diff)

Brant test shows sparse cells

Because the proportional odds ratio is violated, partial proportional odds ratio
to fix the country variable which is mostly violated

clean_same <- na.omit(clean_ess[, c("imwbcnt_ord",
                                    "cntry",
                                    "gndr",
                                    "agea",
                                    "edulvlb",
                                    "hinctnta",
                                    "dosprt")])

model_same_clm <- clm(
  imwbcnt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt,
  nominal = ~ cntry,
  data = clean_same
)

summary(model_same_clm)

To compare the models after fixing the violation

model_parallel <- clm(
  imwbcnt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt + cntry,
  data = clean_same
)

anova(model_parallel, model_same_clm)


Multilevel Modelling; Leveraging variance partitioning

Do countries differ in their baseline immigration attitudes?
fit a null model


null_ord <- clmm(imwbcnt_ord ~ 1 + (1 | cntry), data = clean_ess)
summary(null_ord)


What % of variation is due to countries?
  Variance Partition Coefficient (ICC)

clean_ess$imwbcnt_ord <- factor(clean_ess$imwbcnt,
                                ordered = TRUE)

clean_ess$imueclt_ord <- factor(clean_ess$imueclt,
                                ordered = TRUE)

Same race immigrants

null_same_ord <- clmm(
  imwbcnt_ord ~ 1 + (1 | cntry),
  data = clean_ess,
  link = "logit"
)
summary(null_same_ord)

Difference race immigrants

null_diff_ord <- clmm(
  imueclt_ord ~ 1 + (1 | cntry),
  data = clean_ess,
  link = "logit"
)
summary(null_diff_ord)

To extract the country level variance

#Same
var_country_same <- as.numeric(VarCorr(null_same_ord)$cntry)

#Different
var_country_diff <- as.numeric(VarCorr(null_diff_ord)$cntry)

To compute the ICC

# Extract country-level variance
var_country_same <- as.numeric(VarCorr(null_same_ord)$cntry)
var_country_diff <- as.numeric(VarCorr(null_diff_ord)$cntry)

# Define level-1 variance for logistic models
var_level1 <- (pi^2) / 3

# Compute ICCs
ICC_same_ord <- var_country_same /
  (var_country_same + var_level1)

ICC_diff_ord <- var_country_diff /
  (var_country_diff + var_level1)

ICC_same_ord
ICC_diff_ord


Proper likelihood ratio test for multilevel vs single-level

null_same <- lmer(imwbcnt ~ 1 + (1 | cntry), data = clean_ess)
ranova(null_same)

var_country <- as.numeric(VarCorr(null_same)$cntry)
var_resid <- attr(VarCorr(null_same), "sc")^2

ICC_same <- var_country / (var_country + var_resid)
ICC_same

proper likelihood ratio test using ordinal models

single_same_ord <- clm(
  imwbcnt_ord ~ 1,
  data = clean_ess,
  link = "logit"
)

multi_same_ord <- clmm(
  imwbcnt_ord ~ 1 + (1 | cntry),
  data = clean_ess,
  link = "logit"
)

anova(single_same_ord, multi_same_ord)

for different race

single_diff_ord <- clm(
  imueclt_ord ~ 1,
  data = clean_ess,
  link = "logit"
)

summary(single_diff_ord)

multi_diff_ord <- clmm(
  imueclt_ord ~ 1 + (1 | cntry),
  data = clean_ess,
  link = "logit"
)

summary(multi_diff_ord)

anova(single_diff_ord, multi_diff_ord)

estimate the full multilevel ordinal model with predictors.

Same race

full_same_ord <- clmm(
  imwbcnt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt + (1 | cntry),
  data = clean_ess,
  link = "logit"
)

summary(full_same_ord)

Difference Race

full_diff_ord <- clmm(
  imueclt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt + (1 | cntry),
  data = clean_ess,
  link = "logit"
)

summary(full_diff_ord)

Compare with null model
Same race


Use same dataset to complete the comparison

clean_same_complete <- clean_ess %>%
  dplyr::select(imwbcnt_ord, gndr, agea, edulvlb, hinctnta, dosprt, cntry) %>%
  na.omit()

fit the models again

multi_same_ord <- clmm(imwbcnt_ord ~ 1 + (1 | cntry),
                       data = clean_same_complete)


full_same_ord <- clmm(imwbcnt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt +
                        (1 | cntry),
                      data = clean_same_complete)

anova(multi_same_ord, full_same_ord)

for different races

clean_diff_complete <- clean_ess %>%
  dplyr::select(imueclt_ord, gndr, agea, edulvlb, hinctnta, dosprt, cntry) %>%
  na.omit()

multi_diff_ord <- clmm(imueclt_ord ~ 1 + (1 | cntry),
                       data = clean_diff_complete)


full_diff_ord <- clmm(imueclt_ord ~ gndr + agea + edulvlb + hinctnta + dosprt +
                        (1 | cntry),
                      data = clean_diff_complete)

anova(multi_diff_ord, full_diff_ord)


Calculate the new ICC
Same race

var_country <- as.numeric(VarCorr(full_same_ord)$cntry)

ICC_same_full <- var_country / (var_country + (pi^2 / 3))

ICC_same_full

different race

var_country2 <- as.numeric(VarCorr(full_diff_ord)$cntry)

ICC_diff_full <- var_country2 / (var_country2 + (pi^2 / 3))

ICC_diff_full

Publication ready output presentations

modelsummary(
  list("Same Race Model" = full_same_ord,
       "Different Race Model" = full_diff_ord)
)

modelsummary(
  list(
    "Same Race Model" = full_same_ord,
    "Different Race Model" = full_diff_ord
  ),
  output = "immigration_models.docx"
)








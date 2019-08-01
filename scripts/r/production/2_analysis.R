## @knitr loadData

# cleanup global environment
rm(list = ls(all = TRUE))

setwd("~/Box\ Sync/Spanish\ English\ Coronals\ Simonet\ Casillas\ Diaz/stats/1_production/")

library(stringr); library(ggplot2); library(lme4)
library(multcomp); library(MuMIn); library(lmerTest)
library(caret); library(MASS); library(car)
library(QuantPsyc); library(relaimpo); library(rms)
library(randomForest); library(pander); library(dplyr)


# read data
df <- read.csv("coronals_clean.csv", header = TRUE, na.strings = "")



str(df)
dim(df)
summary(df)


















## @knitr descriptives

# get descriptive statistics of subsets of factors
m <- aggregate(vot ~ group + phon + lang, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))
# rename column
names(m)[names(m)=="vot.mean"] <- "mean"
names(m)[names(m)=="vot.sd"] <- "sd"

pandoc.table(m, justify = c('left', 'right', 'center', 'right', 'right'))





## @knitr spanishCoronals

spanish <- df %>%
  filter(lang == "spanish") %>%
  select(participant, group, word, phon, vot, cog, sd)

# VOT
spanishVOTmod <- lmer(vot ~ group * phon + (1|participant) + (1|word), data = spanish)
spanishVOTadd <- lmer(vot ~ group + phon + (1|participant) + (1|word), data = spanish)
spanishVOTgrp <- lmer(vot ~ group        + (1|participant) + (1|word), data = spanish)
spanishVOTphn <- lmer(vot ~         phon + (1|participant) + (1|word), data = spanish)
spanishVOTnul <- lmer(vot ~ 1 + (1|participant) + (1|word), data = spanish)

summary(spanishVOTmod)

# group x phon interaction = x2(2) = 19.35; p < 0.001
anova(spanishVOTmod, spanishVOTadd, test = "Chisq")

# main effect group = x2(2) = 0.86; p > 0.05
anova(spanishVOTphn, spanishVOTadd, test = "Chisq")

# main effect phon = x2(1) = 67.38; p < 0.001
anova(spanishVOTgrp, spanishVOTadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(vot ~ group + phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(vot ~ phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))



# SD
spanishSDmod <- lmer(sd ~ group * phon + (1|participant) + (1|word), data = spanish)
spanishSDadd <- lmer(sd ~ group + phon + (1|participant) + (1|word), data = spanish)
spanishSDgrp <- lmer(sd ~ group        + (1|participant) + (1|word), data = spanish)
spanishSDphn <- lmer(sd ~         phon + (1|participant) + (1|word), data = spanish)
spanishSDnul <- lmer(sd ~ 1 + (1|participant) + (1|word), data = spanish)

summary(spanishSDmod)

# group x phon interaction = x2(2) = 26.21; p < 0.001
anova(spanishSDmod, spanishSDadd, test = "Chisq")

# main effect group = x2(2) = 0.97; p > 0.05
anova(spanishSDphn, spanishSDadd, test = "Chisq")

# main effect phon = x2(1) = 3.46; p = 0.06
anova(spanishSDgrp, spanishSDadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(sd ~ group + phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(sd ~ phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))






# COG
spanishCOGmod <- lmer(cog ~ group * phon + (1|participant) + (1|word), data = spanish)
spanishCOGadd <- lmer(cog ~ group + phon + (1|participant) + (1|word), data = spanish)
spanishCOGgrp <- lmer(cog ~ group        + (1|participant) + (1|word), data = spanish)
spanishCOGphn <- lmer(cog ~         phon + (1|participant) + (1|word), data = spanish)
spanishCOGnul <- lmer(cog ~ 1 + (1|participant) + (1|word), data = spanish)

summary(spanishCOGmod)

# group x phon interaction = x2(2) = 5.19; p = 0.07
anova(spanishCOGmod, spanishCOGadd, test = "Chisq")

# main effect group = x2(2) = 7.59; p < 0.03
anova(spanishCOGphn, spanishCOGadd, test = "Chisq")

# main effect phon = x2(1) = 3.76; p = 0.052
anova(spanishCOGgrp, spanishCOGadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(cog ~ group + phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cog ~ phon, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cog ~ group, data = spanish, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Results
# 3 groups distinguish between /d/-/t/ using vot
# 3 groups distinguish between /d/-/d/ using SD (marginal effect)
# Group differences for SD... bilinguals differ from monolingual control
# more inglish dominance = larger difference






















## @knitr englishCoronals

english <- df %>%
  filter(lang == "english") %>%
  select(participant, group, word, phon, vot, cog, sd)

# VOT
englishVOTmod <- lmer(vot ~ group * phon + (1|participant) + (1|word), data = english)
englishVOTadd <- lmer(vot ~ group + phon + (1|participant) + (1|word), data = english)
englishVOTgrp <- lmer(vot ~ group        + (1|participant) + (1|word), data = english)
englishVOTphn <- lmer(vot ~         phon + (1|participant) + (1|word), data = english)
englishVOTnul <- lmer(vot ~ 1 + (1|participant) + (1|word), data = english)

summary(englishVOTmod)

# group x phon interaction = x2(2) = 31.84; p < 0.001
anova(englishVOTmod, englishVOTadd, test = "Chisq")

# main effect group = x2(2) = 4.73; p > 0.05
anova(englishVOTphn, englishVOTadd, test = "Chisq")

# main effect phon = x2(1) = 79.96; p < 0.001
anova(englishVOTgrp, englishVOTadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(vot ~ group + phon, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(vot ~ group, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Group differences for /d/... vot decreases from mono avg with increased dominance
# in spanish


# SD
englishSDmod <- lmer(sd ~ group * phon + (1|participant) + (1|word), data = english)
englishSDadd <- lmer(sd ~ group + phon + (1|participant) + (1|word), data = english)
englishSDgrp <- lmer(sd ~ group        + (1|participant) + (1|word), data = english)
englishSDphn <- lmer(sd ~         phon + (1|participant) + (1|word), data = english)
englishSDnul <- lmer(sd ~ 1 + (1|participant) + (1|word), data = english)

summary(englishSDmod)

# group x phon interaction = x2(2) = 61.70; p < 0.001
anova(englishSDmod, englishSDadd, test = "Chisq")

# main effect group = x2(2) = 1.24; p > 0.05
anova(englishSDphn, englishSDadd, test = "Chisq")

# main effect phon = x2(1) = 64.35; p = 0.001
anova(englishSDgrp, englishSDadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(sd ~ group + phon, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(sd ~ phon, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Groups distinguish between /d/-/t/ using sd (no group differences)





# COG
englishCOGmod <- lmer(cog ~ group * phon + (1|participant) + (1|word), data = english)
englishCOGadd <- lmer(cog ~ group + phon + (1|participant) + (1|word), data = english)
englishCOGgrp <- lmer(cog ~ group        + (1|participant) + (1|word), data = english)
englishCOGphn <- lmer(cog ~         phon + (1|participant) + (1|word), data = english)
englishCOGnul <- lmer(cog ~ 1 + (1|participant) + (1|word), data = english)

summary(englishCOGmod)

# group x phon interaction = x2(2) = 107.55; p = 0.001
anova(englishCOGmod, englishCOGadd, test = "Chisq")

# main effect group = x2(2) = 0.49; p > 5
anova(englishCOGphn, englishCOGadd, test = "Chisq")

# main effect phon = x2(1) = 73.27; p = 0.001
anova(englishCOGgrp, englishCOGadd, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(cog ~ group + phon, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cog ~ phon, data = english, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Results
# Group differences for /d/... vot decreases from mono avg with increased dominance
# in spanish
# Groups distinguish between /d/-/t/ using sd (no group differences)
# all three groups distinguish between /d/-/t/ using cog
# no group differences











## @knitr langSpecSegments

bilinguals <- df %>%
  filter(group == "biEsp" | group == "biEng") %>%
  filter(lang == "spanish" & phon == "t" | lang == "english" & phon == "d") %>%
  select(participant, group, lang, word, phon, vot, cog, sd)
bilinguals <- droplevels(bilinguals)


# VOT
biVOTmod <- lmer(vot ~ group * phon + (1|participant) + (1|word), data = bilinguals)
biVOTadd <- lmer(vot ~ group + phon + (1|participant) + (1|word), data = bilinguals)
biVOTgrp <- lmer(vot ~ group        + (1|participant) + (1|word), data = bilinguals)
biVOTphn <- lmer(vot ~         phon + (1|participant) + (1|word), data = bilinguals)
biVOTnul <- lmer(vot ~ 1            + (1|participant) + (1|word), data = bilinguals)

summary(biVOTmod)

# group x phon interaction = x2(1) 1.55; p > 0.05
anova(biVOTmod, biVOTadd, test = "Chisq")

# main effect group = x2(1) = 0.36; p > 0.05
anova(biVOTadd, biVOTphn, test = "Chisq")

# main effect phon = x2(1) = 26.67; p < 0.001
anova(biVOTadd, biVOTgrp, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(vot ~ group + phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(vot ~ phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# RESULTS
# No group differences between spanish /t/ and english /d/
# but two phones are significantly different from each other
# spanish /t/ has higher VOT; english /d/ has lower VOT
# likely due to bilingual tendency to prevoice english /d/






# SD
biSDmod <- lmer(sd ~ group * phon + (1|participant) + (1|word), data = bilinguals)
biSDadd <- lmer(sd ~ group + phon + (1|participant) + (1|word), data = bilinguals)
biSDgrp <- lmer(sd ~ group        + (1|participant) + (1|word), data = bilinguals)
biSDphn <- lmer(sd ~         phon + (1|participant) + (1|word), data = bilinguals)
biSDnul <- lmer(sd ~ 1            + (1|participant) + (1|word), data = bilinguals)

summary(biSDmod)

# group x phon interaction = x2(1) 0.53; p > 0.05
anova(biSDmod, biSDadd, test = "Chisq")

# main effect group = x2(1) = 0.02; p > 0.05
anova(biSDadd, biSDphn, test = "Chisq")

# main effect phon = x2(1) = 66.48; p < 0.001
anova(biSDadd, biSDgrp, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(sd ~ group + phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(sd ~ phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# RESULTS
# No group differences; but spanish /t/ is different from english /d/
# likely reflects POA differences




# COG
biCOGmod <- lmer(cog ~ group * phon + (1|participant) + (1|word), data = bilinguals)
biCOGadd <- lmer(cog ~ group + phon + (1|participant) + (1|word), data = bilinguals)
biCOGgrp <- lmer(cog ~ group        + (1|participant) + (1|word), data = bilinguals)
biCOGphn <- lmer(cog ~         phon + (1|participant) + (1|word), data = bilinguals)
biCOGnul <- lmer(cog ~ 1            + (1|participant) + (1|word), data = bilinguals)

summary(biCOGmod)

# group x phon interaction = x2(1) 6.09; p < 0.02
anova(biCOGmod, biCOGadd, test = "Chisq")

# main effect group = x2(1) = 0.04; p > 0.05
anova(biCOGadd, biCOGphn, test = "Chisq")

# main effect phon = x2(1) = 56.86; p < 0.001
anova(biCOGadd, biCOGgrp, test = "Chisq")

# get descriptive statistics of subsets of factors
aggregate(cog ~ group + phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))
aggregate(cog ~ phon, data = bilinguals, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# RESULTS
# No group differnces (interaction due to direction of change between group)
# however both groups maintain distinction between spanish /t/ and english /d/
# likely related to POA differences


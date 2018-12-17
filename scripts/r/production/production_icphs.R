

# cleanup global environment
rm(list = ls(all = TRUE))

here::here()

setwd("~/Box\ Sync/Simonet\ Casillas\ Spanish\ English\ Coronals/stats/1_production")

library(stringr); library(ggplot2); library(lme4)
library(multcomp); library(MuMIn); library(lmerTest)
library(caret); library(MASS); library(car)
library(QuantPsyc); library(relaimpo); library(rms)
library(randomForest)


# read data
temp <- read.csv("coronals.csv", header = TRUE, na.strings = "")

# create participant column
temp$participant <- substr(temp$prefix, start=1, stop=11)
temp$participant <- gsub("[BIL]|[NSP]|[NEN]|_", "", paste(temp$participant))
temp$participant <- as.factor(temp$participant)


# create group column
temp$group <- substr(temp$prefix, start=1, stop=3)
temp$group <- as.factor(temp$group)

# create lang column
temp$lang <- str_sub(temp$prefix, start= -12)
temp$lang <- gsub("_|[0-9]", "", paste(temp$lang))
temp$lang <- str_sub(temp$lang, start= -7)
temp$lang <- as.factor(temp$lang)

# create word column
temp$word <- temp$prefix
temp$word <- gsub("_[0-9]+$|spanish|english", "", paste(temp$word))
temp$word <- gsub("\\_(.*?)\\_", "", paste(temp$word))
temp$word <- gsub("[BIL]|[NSP]|[NEN]|_", "", paste(temp$word))
temp$word <- gsub("[0-9]", "", paste(temp$word))
temp$word <- as.factor(temp$word)

# create consonant column
temp$phon <- substr(temp$word, start=1, stop=1)
temp$phon <- as.factor(temp$phon)

# create phoneme type column
temp$phonType <- NA
temp[temp$lang == "spanish" & temp$phon == "d", 16] <- "dSp"
temp[temp$lang == "spanish" & temp$phon == "t", 16] <- "tSp"
temp[temp$lang == "english" & temp$phon == "d", 16] <- "dEn"
temp[temp$lang == "english" & temp$phon == "t", 16] <- "tEn"
temp$phonType <- as.factor(temp$phonType)

# reorder
temp <- temp[,c(1, 11:16, 2:10)]

# remove bilinguals
df <- temp[temp$group == "NSP" | temp$group == "NEN", ]
df$group <- droplevels(df$group)
df$participant <- droplevels(df$participant)

str(df)
summary(df$word)

dim(df)
nrow(df[df$participant == "ainaGom", ])








# random intercept and random slope model
vot.mod <- lmer(vot ~ group * phon + (1+phon|participant) + (1|word), data = df, REML = FALSE)
vot.int <- lmer(vot ~ group + phon + (1+phon|participant) + (1|word), data = df, REML = FALSE)
vot.grp <- lmer(vot ~         phon + (1+phon|participant) + (1|word), data = df, REML = FALSE)
vot.phn <- lmer(vot ~ group        + (1+phon|participant) + (1|word), data = df, REML = FALSE)
summary(vot.mod)
r.squaredGLMM(vot.mod)

# interaction (X(1) = 10.54; p < 0.002)
anova(vot.mod, vot.int)
# Group (X(2) = 44.07; p < 0.001)
anova(vot.mod, vot.grp)
# phon (X(2) = 56.39; p < 0.001)
anova(vot.mod, vot.phn)

vot.phonType <- lmer(vot ~ phonType + (1+phon|participant) + (1|word), data = df)
summary(vot.phonType)
r.squaredGLMM(vot.phonType)
summary(glht(vot.phonType, linfct = mcp(phonType = "Tukey")))

hist(residuals(vot.mod))
qqnorm(residuals(vot.mod))
qqline(residuals(vot.mod))

summary(lm(vot ~ phonType, data = df))





# Check correlations

# cor(df$vot, df$ri)
# cor(df$vot, df$cog)
# cor(df$vot, df$sd)
# cor(df$vot, df$sk)
# cor(df$vot, df$kt)

# cor(df$ri, df$cog)
# cor(df$ri, df$sd)
# cor(df$ri, df$sk)
# cor(df$ri, df$kt)

# cor(df$cog, df$sd)
# cor(df$cog, df$sk)
# cor(df$cog, df$kt)

# cor(df$sd, df$sk)
# cor(df$sd, df$kt)

# cor(df$sk, df$kt)

# par(mfrow = c(3, 5))
# plot(df$vot, df$ri)
# plot(df$vot, df$cog)
# plot(df$vot, df$sd)
# plot(df$vot, df$sk)
# plot(df$vot, df$kt)

# plot(df$ri, df$cog)
# plot(df$ri, df$sd)
# plot(df$ri, df$sk)
# plot(df$ri, df$kt)

# plot(df$cog, df$sd)
# plot(df$cog, df$sk)
# plot(df$cog, df$kt)

# plot(df$sd, df$sk)
# plot(df$sd, df$kt)

# plot(df$sk, df$kt)



# Residualize predictors
mri <- lm(ri ~ vot, data = df)
df$residRI <- residuals(mri)

mcog <- lm(cog ~ vot, data = df)
df$residCOG <- residuals(mcog)

msd <- lm(sd ~ vot, data = df)
df$residSD <- residuals(msd)

mskew <- lm(sk ~ vot, data = df)
df$residSK <- residuals(mskew)

mkurt <- lm(kt ~ vot, data = df)
df$residKT <- residuals(mkurt)



# create subset
dt <- subset(df, group == "NEN" & phon == "d" | group == "NSP" & phon == "t")
dt$participant <- droplevels(dt$participant)
dt$group <- droplevels(dt$group)
dt$phon <- droplevels(dt$phon)
dt$phonType <- droplevels(dt$phonType)
dim(dt)


# Linear regression model RI
mri.mod <- lmer(residRI ~ phonType + (1|participant) + (1|word), data = dt, REML = FALSE)
summary(mri.mod)
r.squaredGLMM(mri.mod)

# Linear regression model COG
cog.mod <- lmer(residCOG ~ phonType + (1|participant) + (1|word), data = dt, REML = FALSE)
summary(cog.mod)
r.squaredGLMM(cog.mod)

# Linear regression model SD
sd.mod <- lmer(residSD ~ phonType + (1|participant) + (1|word), data = dt, REML = FALSE)
summary(sd.mod)
r.squaredGLMM(sd.mod)

# Linear regression model skew
sk.mod <- lmer(residSK ~ phonType + (1|participant) + (1|word), data = dt, REML = FALSE)
summary(sk.mod)
r.squaredGLMM(sk.mod)

# Linear regression model kurt
kt.mod <- lmer(residKT ~ phonType + (1|participant) + (1|word), data = dt, REML = FALSE)
summary(kt.mod)
r.squaredGLMM(kt.mod)


# Check correlations

# cor(df$vot, df$residRI)
# cor(df$vot, df$residCOG)
# cor(df$vot, df$residSD)
# cor(df$vot, df$residSK)
# cor(df$vot, df$residKT)

# cor(df$residRI, df$residCOG)
# cor(df$residRI, df$residSD)
# cor(df$residRI, df$residSK)
# cor(df$residRI, df$residKT)

# cor(df$residCOG, df$residSD)
# cor(df$residCOG, df$residSK)
# cor(df$residCOG, df$residKT)

# cor(df$residSD, df$residSK)
# cor(df$residSD, df$residKT)

# cor(df$residSK, df$residKT)

# par(mfrow = c(3, 5))
# plot(df$vot, df$residRI)
# plot(df$vot, df$residCOG)
# plot(df$vot, df$residSD)
# plot(df$vot, df$residSK)
# plot(df$vot, df$residKT)

# plot(df$residRI, df$residCOG)
# plot(df$residRI, df$residSD)
# plot(df$residRI, df$residSK)
# plot(df$residRI, df$residKT)

# plot(df$residCOG, df$residSD)
# plot(df$residCOG, df$residSK)
# plot(df$residCOG, df$residKT)

# plot(df$residSD, df$residSK)
# plot(df$residSD, df$residKT)

# plot(df$residSK, df$residKT)



# Logistic regression
# use forward selection giving causal priority to 
# best predictors in Sundara 2005

# Used in this way, linear regression offers a simple 
# model of parsing, a theoretical viewpoint developed 
# by Fowler (1984) and Gow (2003; see McMurray et al., 
# in review for a more complete discussion of the 
# regression approach). Linear regression models use 
# the available variation in the input to identify one 
# contributing factor (e.g., the identity of the target 
# vowel). They then compute a residual (the difference 
# between the prototype value for that target vowel and 
# the current input) and use this residual to identify 
# other factors that influence the signal (e.g., the 
# context vowel). We can control which factors are 
# entered into the model at any given time, allowing 
# us to model the sequential uptake of information.

# test regression (residualized)
dt.null <- glm(phon ~ 1, data = dt, family = "binomial")
dt.full <- glm(phon ~ residSD + residCOG + residRI + residSK + residKT, data = dt, family = "binomial")
summary(dt.null)
summary(dt.full)
step2 <- stepAIC(dt.null, direction = "forward", scope = ~ residSD + residRI + residCOG + residSK + residKT)
step2$anova 

dt.final <- glm(phon ~ residSD + residRI + residCOG, data = dt, family = "binomial")
summary(dt.final)
vif(dt.final)
lm.beta(dt.final) 
varImp(dt.final, scale = FALSE)


dtm6.r2 <- glm(phon ~ residSD + residCOG + residRI + residSK + residKT, data = dt, family = "binomial")
dtm5.r2 <- glm(phon ~ residSD + residCOG + residRI + residSK, data = dt, family = "binomial")
dtm4.r2 <- glm(phon ~ residSD + residCOG + residRI, data = dt, family = "binomial")
dtm3.r2 <- glm(phon ~ residSD + residCOG, data = dt, family = "binomial")
dtm2.r2 <- glm(phon ~ residSD, data = dt, family = "binomial")
dtm1.r2 <- glm(phon ~ 1, data = dt, family = "binomial")

summary(dtm1.r2)
summary(dtm2.r2)
summary(dtm3.r2)
summary(dtm4.r2)
summary(dtm5.r2)
summary(dtm6.r2)

anova(dtm1.r2, dtm2.r2, test = "Chisq")
anova(dtm2.r2, dtm3.r2, test = "Chisq")
anova(dtm3.r2, dtm4.r2, test = "Chisq")
anova(dtm4.r2, dtm5.r2, test = "Chisq")
anova(dtm5.r2, dtm6.r2, test = "Chisq")


# Nagelkerke R2
dt1.r2 <- lrm(phon ~ 1, data = dt)
dt2.r2 <- lrm(phon ~ residSD, data = dt)
dt3.r2 <- lrm(phon ~ residSD + residCOG, data = dt)
dt4.r2 <- lrm(phon ~ residSD + residCOG + residRI, data = dt)
dt5.r2 <- lrm(phon ~ residSD + residCOG + residRI + residSK, data = dt)
dt6.r2 <- lrm(phon ~ residSD + residCOG + residRI + residSK + residKT, data = dt)

print(dt1.r2) 
print(dt2.r2) 
print(dt3.r2) 
print(dt4.r2) 
print(dt5.r2) 
print(dt6.r2) 

#         R2  R2 change      F       P 
# sd  = .353       .353 175.39 < 0.001
# cog = .376       .023  14.01 < 0.001
# ri  = .425       .049  30.01 < 0.001
# sk  = .426       .001   0.08 > 0.05
# kt  = .432       .006   4.36 < 0.04





# training 75% of shortlag stops
# use forward selection giving causal priority to 
# best predictors from above regression
# use best model to predict testing 25%

# create data sets
set.seed(1)
inTrain <- createDataPartition(y = dt$phon, p = 0.75, list = FALSE)
training <- dt[inTrain, ]
testing <- dt[-inTrain, ]
dim(training); dim(testing); dim(dt)

# train data with forward selection
modelFit <- train(phon ~ residSD + residCOG + residRI + residSK + residKT, data = training, method = "glmStepAIC", direction = "forward")
modelFit

modelFit$finalModel
varImp(modelFit$finalModel, scale = FALSE)

predictions <- predict(modelFit, newdata = testing)
predictions

confusionMatrix(predictions, testing$phon)

# random forests
modFitRF      <- randomForest(phon ~ residSD + residCOG + residRI + residSK + residKT, data = training)
predictionsRF <- predict(modFitRF, testing, type = "class")

confusionMatrix(predictionsRF, testing$phon)
varImp(modFitRF, scale = FALSE)









# # Add point to show mean for COG
# residCOGnew <- dt[dt$phonType == "dEn", 18]
# phonType <- "dEn"
# hi <- data.frame(phonType = phonType, residCOGnew = residCOGnew)
# meanCOG <- mean(hi$residCOGnew)

# require(tikzDevice)
#     options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
#     "\\usepackage{tipa}"))
#     tikz("figures/prod.tex", 
#     standAlone = TRUE, width = 9, height = 4.5)

# # VOT plot
# par(mfrow = c(2, 3), mar = c(4, 4, 1.5, 2))
# with(df, boxplot(vot ~ phon * group, 
#     col=c("white", "darkgray", "lightgray", "white"), 
#     main = "VOT", horizontal = TRUE, yaxt = "n", 
#     ylim = c(-150, 150), cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2, 3, 4), las = 2,
#     labels = c("/d/", "/t/", "/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1.5, 3.5), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # abline(h = 0, v = 0, col = "lightgray", lty = 3)
# legend("topleft", c("Lead", "Short-lag", "Long-lag"), 
#     fill = c("lightgray", "white", "darkgray"), 
#     cex = 1.0)
# # legend("topright",  "")
# text(153, 4.5, "A", cex = 1.5)

# # RI
# with(dt, boxplot(residRI ~ phonType, 
#     col = "white", yaxt = "n",
#     main = "Relative Intensity", horizontal = TRUE, 
#     ylim = c(-15, 20), cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2), las = 2,
#     labels = c("/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1, 2), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # legend("topright",  "")
# text(20.35, 2.5, "B", cex = 1.5)

# COG
# with(dt, boxplot(residCOG ~ phonType, 
#     col = "white", main = "Center of Gravity", 
#     horizontal = TRUE, yaxt = "n", 
#     cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2), las = 2,
#     labels = c("/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1, 2), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # legend("topright",  "")
# text(5000, 2.5, "C", cex = 1.5)
# points(meanCOG,1, pch=19, col="black")

# # SD
# with(dt, boxplot(residSD ~ phonType, 
#     col = "white", main = "Standard Deviation", 
#     horizontal = TRUE, yaxt = "n", 
#     cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2), las = 2,
#     labels = c("/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1, 2), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # legend("topright",  "")
# text(2875, 2.5, "D", cex = 1.5)

# # SKewness
# with(dt, boxplot(residSK ~ phonType, 
#     col = "white", main = "Skewness", 
#     horizontal = TRUE, yaxt = "n", 
#     cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2), las = 2,
#     labels = c("/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1, 2), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # legend("topright",  "")
# text(14.0, 2.5, "E", cex = 1.5)

# # Kurtosis
# with(dt, boxplot(residKT ~ phonType, 
#     col = "white", main = "Kurtosis", 
#     horizontal = TRUE, yaxt = "n", 
#     cex.lab = 1.5, cex.main = 1.5))
# axis(2, at = c(1, 2), las = 2,
#     labels = c("/d/", "/t/"), cex.axis = 1.25)
# axis(2, at = c(1, 2), tick = FALSE, padj = -2,
#     labels = c("English", "Spanish"), cex.axis = 1.5)
# # legend("topright",  "")
# text(455, 2.5, "F", cex = 1.5)

# dev.off()




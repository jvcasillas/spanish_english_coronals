## Import/Clean data

# cleanup global environment
rm(list = ls(all = TRUE))

# Set working directory
setwd("~/Box Sync/Spanish English Coronals Simonet Casillas Diaz/axb")

# load packages
library(stargazer); library(ggplot2); library(lme4); library(psyphy)



# Combine files vertically into large data frame
temp1 <- list.files(path="./monolinguals/", full.names = TRUE, pattern = ".txt")
myfiles = lapply(temp1, read.delim, sep = "\t")
mo <- do.call("rbind",myfiles)

temp2 <- list.files(path="./bilinguals/", full.names = TRUE, pattern = ".txt")
myfiles = lapply(temp2, read.delim, sep = "\t")
bi <- do.call("rbind",myfiles)

axb <- rbind(mo, bi)

# create consonant column
axb$cons <- axb$stimulus

# remove all characters except those indicating consonant
axb$cons <- gsub("[^dt]", "", paste(axb$cons))

# create language column
axb$lang <- axb$stimulus

# remove all characters except those indicating language
axb$lang <- gsub("[^se]", "", paste(axb$lang))

# add correct answer column
axb$corrAns <- NA

# Mark correct answer as three if 3rd cons is different
axb[with(axb, grep("[d][d][^d]|[t][t][^t]", axb$cons)), 8] <- 3

# Mark correct answer as one if 1st cons is different
axb[with(axb, grep("[^d][d][d]|[^t][t][t]", axb$cons)), 8] <- 1

# Mark correct answer as three if 3rd lang is different
axb[with(axb, grep("[e][e][^e]|[s][s][^s]", axb$lang)), 8] <- 3

# Mark correct answer as one if 1st lang is different
axb[with(axb, grep("[^e][e][e]|[^s][s][s]", axb$lang)), 8] <- 1

# Check for NAs (to see if all corrAns filled)
length(na.omit(axb$corrAns))
length(axb$corrAns)

# Add answer column (1 = correct, 0 = incorrect)
axb$answer <- NA

# If response and corrAns are the same then mark answer as 1 (correct)
axb[axb$response == 3 & axb$corrAns == 3, 9] <- 1
axb[axb$response == 1 & axb$corrAns == 1, 9] <- 1

# If response and corrAns are not the same then mark answer a 0 (incorrect)
axb[axb$response == 3 & axb$corrAns == 1, 9] <- 0
axb[axb$response == 1 & axb$corrAns == 3, 9] <- 0

# Check for NAs (to see if all answer filled)
length(na.omit(axb$answer))
length(axb$answer)

# sort (reorder) columns
axb <- axb[,c(4, 5, 1, 6, 7, 2, 8, 9, 3)]

# Descriptives
summary(axb); str(axb); head(axb, 40)

# weird outlier
# remove?
min <- min(axb$reactionTime)
subset(axb, reactionTime == min)






# RT as a function of group
with(axb, plot(group, log10(reactionTime)))

# Proportion correct by group
gProp <- aggregate(answer ~ group, data = axb, FUN = mean)
groups <- gProp$group
p <- with(gProp, barplot(answer, ylim = c(0,1), 
      xlab = "Group", ylab = "Proportion correct"))
axis(1, at = p, label = groups)








mod <- glm(answer ~ group, data = axb, family = "binomial")
mod.null <- glm(answer ~ 1, data = axb, family = "binomial")

anova(mod, mod.null, test = "Chisq")


# random intercept model
mod2 <- glmer(answer ~ group + (1|participant), data = axb, family = "binomial")
mod2.null <- glmer(answer ~ 1 + (1|participant), data = axb, family = "binomial")

anova(mod2, mod2.null, test = "Chisq")

# d prime
prop <- aggregate(answer ~ participant, data = axb, FUN = mean)
aggregate(answer ~ participant, data = prop, FUN = dprime.oddity)








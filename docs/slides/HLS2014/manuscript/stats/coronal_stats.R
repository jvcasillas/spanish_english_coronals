######################
#                    #
# Joseph V. Casillas #
# 8/27/2013          #
# Span 583a          #
# coronal project    #
#                    #
######################



# load_data
setwd("~/Dropbox/shared/HLS2014/manuscript")

coronals_data <- read.csv("./stats/data_all.csv", quote="")

# new column based on chars of old column
coronals_data$phon <- substr(coronals_data$word, start=1, stop=1)

coronals_data$phon <- as.factor(coronals_data$phon)

#reorder columns
coronals_data <- coronals_data[,c(1:6,13,7:12)]

summary(coronals_data)

library(GGally)
ggpairs(coronals_data[, c("vot", "stress", "phon", "language")])








# descriptive stats all data
# get means of subsets of factors
library(xtable)
descriptives <- aggregate(cbind(vot, cgrav, sd, skew, kurt, cm) ~ language + phon, data = coronals_data, FUN = mean)
print(xtable(descriptives), floating=TRUE, type="latex", 
      include.rownames=FALSE,
      file='~/Google_Drive/11_fall_2013/classes/span_583c_phonetics_3_fall_2013/trabajo_final/coronals/manuscript/tables/descriptives.tex')










# Research question: Is there an acoustic correlate that distinguishes
# English /d/ from Spanish /t/?

# Remove row so that there are equal groups of 7
df <- coronals_data[-c(1004:2074),]
nrow(df)

# multiply vot values by 1000
df$vot <- df$vot*1000

# log transform cgrav
df$cgrav <- log(df$cgrav)

# log transform CM
df$cm <- log(df$cm)

# Make subset of english /d/ and spanish /t/
english_d = subset(df,language=="english" & phon=="d")
spanish_t = subset(df,language=="spanish" & phon=="t")
summary(english_d)
summary(spanish_t)

final_df <- rbind(english_d,spanish_t)
final_df$prefix <- as.factor(final_df$prefix)
summary(final_df)

# remove outliers greater than 242 (2sds) from kurt 
final_df$kurt.new <- final_df$kurt
final_df$kurt.new[final_df$kurt.new >= 242.0106] <- NA

# remove outliers greater than 3000 from sd
final_df$sd.new <- final_df$sd
final_df$sd.new[final_df$sd.new >= 3000] <- NA








eng = subset(coronals_data,language=="english")

# two-way within subjects ANOVA
aov.test = aov(vot ~ (phon*stress) + Error(prefix/(phon*stress)), data=eng)
summary(aov.test)

# subset multiple factors at once
hi = subset(eng,phon=="d")

# t-test: # independent 2-group, 2 level IV
t <- t.test(vot~stress, data=hi, paired=FALSE)
t

# Tukey test
TukeyHSD(aov(vot ~ stress, data=hi))

library(xtable)
table <- aggregate(vot ~ phon + stress, data = eng, FUN = mean)
table1 <- aggregate(vot ~ phon + stress + prefix, data = eng, FUN = mean)


# One-way withing subjects ANOVA
aov.hiagain = aov(vot ~ stress + Error(prefix/stress), data=hi)
summary(aov.hiagain)


t <- t.test(vot~stress, data=table, paired=TRUE)
t






#########
# plots #
#########

# plot vot all phonemes
library(ggplot2)
vot.all <- ggplot(df, aes(group, vot)) +
    geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-150, 250)) +
    labs(list(title = "", 
              x = "Language", y = "VOT (ms)")) +
    #theme(legend.position = c(0.94, 0.86)) +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    coord_flip() +
    # facet_wrap(~language)
    scale_fill_manual(values=c("#FF2860", "#0066FF"),
                      name="Phoneme",
                      breaks=c("d", "t"),
                      labels=c("/d/", 
                               "/t/"))

require(tikzDevice)
  options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
  "\\usepackage{tipa}"))
  tikz("figures/vot.tex", standAlone=TRUE, width=6, height=5)

  vot.all

dev.off()


# plot_all_data
p1.leg <- ggplot(final_df, aes(phon, vot)) +
    geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-0.05, 0.15)) +
    labs(list(title = "", 
              x = "Phoneme", y = "VOT (ms)")) +
    #theme(legend.position = c(0.94, 0.86)) +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    # coord_flip() +
   # facet_wrap(~group)
    scale_fill_manual(values=c("#FF2860", "#0066FF"),
                      name="Phoneme",
                      breaks=c("d", "t"),
                      labels=c("English /d/", 
                               "Spanish /t/"))

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg<-g_legend(p1.leg)

p1 <- ggplot(final_df, aes(phon, vot))
p1 <- p1 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5, 150)) +
    labs(list(title = "VOT (ms)", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


p2 <- ggplot(final_df, aes(phon, cgrav))
p2 <- p2 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(5,10)) +
    labs(list(title = "log Center of Gravity", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


p3 <- ggplot(final_df, aes(phon, sd))
p3 <- p3 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(0,4000)) +
    labs(list(title = "Standard Deviation", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


p4 <- ggplot(final_df, aes(phon, skew))
p4 <- p4 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5,40)) +
    labs(list(title = "Skew", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


p5 <- ggplot(final_df, aes(phon, kurt.new))
p5 <- p5 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5,300)) +
    labs(list(title = "Kurtosis", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


p6 <- ggplot(final_df, aes(phon, cm))
p6 <- p6 +  geom_jitter(size=.5) +
    geom_boxplot(aes(fill = phon),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(15,30)) +
    labs(list(title = "log Central Moment" , 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~stress)


require(tikzDevice)
options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
    "\\usepackage{tipa}"))
tikz("figures/dt_all.tex", standAlone=TRUE, width=10.5, height=5)
library(ggplot2)
library(gridExtra)

grid.arrange(arrangeGrob(p1,p4),arrangeGrob(p2,p5),arrangeGrob(p3,p6), ncol=3, widths=c(1/3,1/3,1/3))

dev.off()









# get descriptive statistics of subsets of factors
aggregate(cm ~ phon + stress + language, data = final_df, FUN = mean)



# stats

# get means of subsets of factors
library(xtable)
descriptives <- aggregate(cbind(vot, cgrav, sd.new, skew, kurt.new, cm) ~ stress*phon, data = final_df, FUN = mean)
sd <- aggregate(cbind(vot*1000, cgrav, sd, skew, kurt.new, cm) ~ stress*phon, data = final_df, FUN = sd)

print(xtable(descriptives, digits=c(0,0,0,5,2,2,2,2,2)), floating=TRUE, type="latex",
      include.rownames=FALSE,
      file='~/Google_Drive/11_fall_2013/classes/span_583c_phonetics_3_fall_2013/trabajo_final/coronals/manuscript/tables/dt_descriptives.tex')



# Save plot as pdf
pdf(file="~/Desktop/intercepts.pdf")
plot(vot~prefix,data=final_df, xlim=c(1, 14), xaxt="n", xlab="Participantes", ylab="VOT (ms)")
axis(1,at=c(1:14),labels=c(1:14))
dev.off()

lm1 = lm(vot~prefix,data=final_df)
plot(lm1)

### GLM for each measure ###
library(lme4)
library(MuMIn)
# vot
lme.vot.full = lmer(vot ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.vot.noint = lmer(vot ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.vot.null = lmer(vot ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.vot.reduced = lmer(vot ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.vot.full))
qqline(residuals(lme.vot.full))

summary(lme.vot.full)
r.squaredGLMM(lme.vot.full)
summary(lme.vot.noint)
r.squaredGLMM(lme.vot.noint)
summary(lme.vot.null)
r.squaredGLMM(lme.vot.null)
summary(lme.vot.reduced)
r.squaredGLMM(lme.vot.reduced)

# check for interaction phon * stress
anova(lme.vot.full,lme.vot.noint)
# check main effect of stress
anova(lme.vot.noint,lme.vot.null)
# check main effect of phon
anova(lme.vot.null,lme.vot.reduced)


# cgrav
lme.cgrav.full = lmer(cgrav ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cgrav.noint = lmer(cgrav ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cgrav.null = lmer(cgrav ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cgrav.reduced = lmer(cgrav ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.cgrav.full))
qqline(residuals(lme.cgrav.full))

summary(lme.cgrav.full)
r.squaredGLMM(lme.cgrav.full)
summary(lme.cgrav.noint)
r.squaredGLMM(lme.cgrav.noint)
summary(lme.cgrav.null)
r.squaredGLMM(lme.cgrav.null)
summary(lme.cgrav.reduced)
r.squaredGLMM(lme.cgrav.reduced)

# check for interaction phon * stress
anova(lme.cgrav.full,lme.cgrav.noint)
# check main effect of stress
anova(lme.cgrav.noint,lme.cgrav.null)
# check main effect of phon
anova(lme.cgrav.null,lme.cgrav.reduced)



# sd
lme.sd.full = lmer(sd.new ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.sd.noint = lmer(sd.new ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.sd.null = lmer(sd.new ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.sd.reduced = lmer(sd.new ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.sd.full))
qqline(residuals(lme.sd.full))

summary(lme.sd.full)
r.squaredGLMM(lme.sd.full)
summary(lme.sd.noint)
r.squaredGLMM(lme.sd.noint)
summary(lme.sd.null)
r.squaredGLMM(lme.sd.null)
summary(lme.sd.reduced)
r.squaredGLMM(lme.sd.reduced)

# check for interaction phon * stress
anova(lme.sd.full,lme.sd.noint)
# check main effect of stress
anova(lme.sd.noint,lme.sd.null)
# check main effect of phon
anova(lme.sd.null,lme.sd.reduced)



# skew
lme.skew.full = lmer(skew ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.skew.noint = lmer(skew ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.skew.null = lmer(skew ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.skew.reduced = lmer(skew ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.skew.full))
qqline(residuals(lme.skew.full))

summary(lme.skew.full)
r.squaredGLMM(lme.skew.full)
summary(lme.skew.noint)
r.squaredGLMM(lme.skew.noint)
summary(lme.skew.null)
r.squaredGLMM(lme.skew.null)
summary(lme.skew.reduced)
r.squaredGLMM(lme.skew.reduced)

# check for interaction phon * stress
anova(lme.skew.full,lme.skew.noint)
# check main effect of stress
anova(lme.skew.noint,lme.skew.null)
# check main effect of phon
anova(lme.skew.null,lme.skew.reduced)



# kurt
lme.kurt.full = lmer(kurt.new ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.kurt.noint = lmer(kurt.new ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.kurt.null = lmer(kurt.new ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.kurt.reduced = lmer(kurt.new ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.kurt.full))
qqline(residuals(lme.kurt.full))

summary(lme.kurt.full)
r.squaredGLMM(lme.kurt.full)
summary(lme.kurt.noint)
r.squaredGLMM(lme.kurt.noint)
summary(lme.kurt.null)
r.squaredGLMM(lme.kurt.null)
summary(lme.kurt.reduced)
r.squaredGLMM(lme.kurt.reduced)

# check for interaction phon * stress
anova(lme.kurt.full,lme.kurt.noint)
# check main effect of stress
anova(lme.kurt.noint,lme.kurt.null)
# check main effect of phon
anova(lme.kurt.null,lme.kurt.reduced)



# cm
lme.cm.full = lmer(cm ~ phon * stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cm.noint = lmer(cm ~ phon + stress + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cm.null = lmer(cm ~ phon + (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)
lme.cm.reduced = lmer(cm ~ (1+phon|prefix) + (1+phon|word), data=final_df, REML=FALSE)

qqnorm(residuals(lme.cm.full))
qqline(residuals(lme.cm.full))

summary(lme.cm.full)
r.squaredGLMM(lme.cm.full)
summary(lme.cm.noint)
r.squaredGLMM(lme.cm.noint)
summary(lme.cm.null)
r.squaredGLMM(lme.cm.null)
summary(lme.cm.reduced)
r.squaredGLMM(lme.cm.reduced)

# check for interaction phon * stress
anova(lme.cm.full,lme.cm.noint)
# check main effect of stress
anova(lme.cm.noint,lme.cm.null)
# check main effect of phon
anova(lme.cm.null,lme.cm.reduced)










# w/o mixed effects
# single logits for each predictor
logtest1 <- glm(phon~vot, family=binomial, data=final_df)
summary(logtest1)
anova(logtest1,test="Chisq")
r.squaredGLMM(logtest1)
qqnorm(residuals(logtest1))
qqline(residuals(logtest1))


logtest2 <- glm(phon~cgrav, family=binomial, data=final_df)
summary(logtest2)
anova(logtest2,test="Chisq")
r.squaredGLMM(logtest2)
qqnorm(residuals(logtest2))
qqline(residuals(logtest2))


logtest3 <- glm(phon~sd.new, family=binomial, data=final_df)
summary(logtest3)
anova(logtest3,test="Chisq")
r.squaredGLMM(logtest3)
qqnorm(residuals(logtest3))
qqline(residuals(logtest3))


logtest4 <- glm(phon~skew, family=binomial, data=final_df)
summary(logtest4)
anova(logtest4,test="Chisq")
r.squaredGLMM(logtest4)
qqnorm(residuals(logtest4))
qqline(residuals(logtest4))


logtest5 <- glm(phon~kurt.new, family=binomial, data=final_df)
summary(logtest5)
anova(logtest5,test="Chisq")
r.squaredGLMM(logtest5)
qqnorm(residuals(logtest5))
qqline(residuals(logtest5))


logtest6 <- glm(phon~cm, family=binomial, data=final_df)
summary(logtest6)
anova(logtest6,test="Chisq")
r.squaredGLMM(logtest6)
qqnorm(residuals(logtest6))
qqline(residuals(logtest6))


# additive models
lmod1 <- glm(phon~vot, family=binomial, data=final_df)
summary(lmod1)
anova(lmod1,test="Chisq")
r.squaredGLMM(lmod1)


lmod2 <- glm(phon~vot+cgrav, family=binomial, data=final_df)
summary(lmod2)
anova(lmod2,test="Chisq")
r.squaredGLMM(lmod2)


lmod3 <- glm(phon~vot+cgrav+sd.new, family=binomial, data=final_df)
summary(lmod3)
anova(lmod3,test="Chisq")
r.squaredGLMM(lmod3)


lmod4 <- glm(phon~vot+cgrav+sd.new+skew, family=binomial, data=final_df)
summary(lmod4)
anova(lmod4,test="Chisq")
r.squaredGLMM(lmod4)


lmod5 <- glm(phon~vot+cgrav+sd.new+skew+kurt.new, family=binomial, data=final_df)
summary(lmod5)
anova(lmod5,test="Chisq")
r.squaredGLMM(lmod5)


lmod6 <- glm(phon~vot+cgrav+sd.new+skew+kurt.new+cm, family=binomial, data=final_df)
summary(lmod6)
anova(lmod6,test="Chisq")
r.squaredGLMM(lmod6)


par(mfrow=c(2,3))
qqnorm(residuals(lmod1))
qqline(residuals(lmod1))
qqnorm(residuals(lmod2))
qqline(residuals(lmod2))
qqnorm(residuals(lmod3))
qqline(residuals(lmod3))
qqnorm(residuals(lmod4))
qqline(residuals(lmod4))
qqnorm(residuals(lmod5))
qqline(residuals(lmod5))
qqnorm(residuals(lmod6))
qqline(residuals(lmod6))








cor(final_df[sapply(final_df, is.numeric)], use="complete.obs")



sd_res <- lm(sd~cgrav,data=final_df)
summary(sd_res)

sd.res <- residuals(sd_res)
cor <- cbind(final_df,sd.res)

cor(cor[sapply(cor, is.numeric)], use="complete.obs")

skew_res <- lm(skew~kurt,data=final_df)
summary(skew_res)

skew.res <- residuals(skew_res)
cor <- cbind(cor,skew.res)

cor(cor[sapply(cor, is.numeric)], use="complete.obs")



# no vot
lmodnovot1 <- glm(phon~cgrav, family=binomial, data=cor)
summary(lmodnovot1)
anova(lmodnovot1,test="Chisq")
r.squaredGLMM(lmodnovot1)


lmodnovot2 <- glm(phon~cgrav+sd.res, family=binomial, data=cor)
summary(lmodnovot2)
anova(lmodnovot2,test="Chisq")
r.squaredGLMM(lmodnovot2)


lmodnovot3 <- glm(phon~cgrav+sd.res+skew.res, family=binomial, data=cor)
summary(lmodnovot3)
anova(lmodnovot3,test="Chisq")
r.squaredGLMM(lmodnovot3)


lmodnovot4 <- glm(phon~cgrav+sd.res+skew.res+cm, family=binomial, data=cor)
summary(lmodnovot4)
anova(lmodnovot4,test="Chisq")
r.squaredGLMM(lmodnovot4)


lmodnovot5 <- glm(phon~cgrav+sd.res+skew.res+cm+kurt.new, family=binomial, data=cor)
summary(lmodnovot5)
anova(lmodnovot5,test="Chisq")
r.squaredGLMM(lmodnovot5)

lmodnovot6 <- glm(phon~cgrav+sd.res+skew.res+cm+kurt.new+vot, family=binomial, data=cor)
summary(lmodnovot6)
anova(lmodnovot6,test="Chisq")
r.squaredGLMM(lmodnovot6)

bestmod <- glm(phon~cgrav+sd.res+skew.res+vot, family=binomial, data=cor)
summary(bestmod)
anova(bestmod,test="Chisq")
r.squaredGLMM(bestmod)
# 68%

par(mfrow=c(2,3))
qqnorm(residuals(lmodnovot1))
qqline(residuals(lmodnovot1))
qqnorm(residuals(lmodnovot2))
qqline(residuals(lmodnovot2))
qqnorm(residuals(lmodnovot3))
qqline(residuals(lmodnovot3))
qqnorm(residuals(lmodnovot4))
qqline(residuals(lmodnovot4))
qqnorm(residuals(lmodnovot5))
qqline(residuals(lmodnovot5))
qqnorm(residuals(bestmod))
qqline(residuals(bestmod))




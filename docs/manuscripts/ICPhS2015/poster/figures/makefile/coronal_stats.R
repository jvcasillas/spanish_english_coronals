######################
#                    #
# Joseph V. Casillas #
# 8/27/2013          #
# Span 583a          #
# coronal project    #
#                    #
######################



# cleanup global environment
rm(list = ls(all = TRUE))

# load_data
setwd("~/Dropbox/shared/HLS2014/poster/figures/makefile")

# read data
coronalsTemp = read.csv("data_all.csv", header=TRUE, quote="")

# take random sample of NSP
set.seed(13)
rnsp <- coronalsTemp[sample(which(coronalsTemp$group == "NSP"), 7, replace = TRUE), ]
rnsp <- as.character(rnsp$subject)

nsp <- coronalsTemp[coronalsTemp$subject %in% rnsp, ]
nen <- subset(coronalsTemp, group == "NEN")

# rbind random sample with NEN group
coronalsALL <- rbind(nsp, nen)
coronals <- aggregate(cbind(vot, cog, sd, skew, kurt, cm, mint) ~ subject + language + word + cons, data = coronalsALL, FUN = mean)
str(coronals)

table(coronals$language, coronals$word)


# remove unwanted characters from column    
coronalsALL$cons <- gsub("da", "d", paste(coronalsALL$cons))
coronalsALL$cons <- gsub("ta", "t", paste(coronalsALL$cons))

# remove unwanted characters from column    
coronals$cons <- gsub("da", "d", paste(coronals$cons))
coronals$cons <- gsub("ta", "t", paste(coronals$cons))


#########
# plots #
#########

# plot vot all phonemes
library(ggplot2)
(vot.all <- ggplot(coronalsALL, aes(language, vot)) +
    geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    ylim(-130, 130) +
    labs(list(title = "", 
              x = "Language", y = "VOT (ms)")) +
    # theme(legend.position = c(0.94, 0.86)) +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    coord_flip() +
    scale_fill_manual(values=c("#FF2860", "#0066FF"),
                      name="Phoneme",
                      breaks=c("d", "t"),
                      labels=c("/d/", 
                               "/t/")))


require(tikzDevice)
  options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
  "\\usepackage{tipa}"))
  tikz("tikz/vot/vot.tex", standAlone=TRUE, width=6, height=5)

  vot.all

dev.off()








# plot_all_data
(p1.leg <- ggplot(coronalsALL, aes(cons, vot)) +
    geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    labs(list(title = "", 
              x = "Phoneme", y = "VOT (ms)")) +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    facet_wrap(~language) +
    scale_fill_manual(values=c("#FF2860", "#0066FF"),
                      name="Phoneme",
                      breaks=c("d", "t"),
                      labels=c("English /d/", 
                               "Spanish /t/")))

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg<-g_legend(p1.leg)

p1 <- ggplot(coronalsALL, aes(cons, vot))
p1 <- p1 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-150, 150)) +
    labs(list(title = "VOT (ms)", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


p2 <- ggplot(coronalsALL, aes(cons, cog))
p2 <- p2 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    labs(list(title = "Center of Gravity", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    facet_wrap(~language)


p3 <- ggplot(coronalsALL, aes(cons, sd))
p3 <- p3 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(0,4000)) +
    labs(list(title = "Standard Deviation", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


p4 <- ggplot(coronalsALL, aes(cons, skew))
p4 <- p4 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5,20)) +
    labs(list(title = "Skewness", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    facet_wrap(~language)


p5 <- ggplot(coronalsALL, aes(cons, kurt))
p5 <- p5 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5,300)) +
    labs(list(title = "Kurtosis", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    facet_wrap(~language)


p6 <- ggplot(coronalsALL, aes(cons, cm))
p6 <- p6 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(15,30)) +
    labs(list(title = "Central Moment" , 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)

p7 <- ggplot(coronalsALL, aes(cons, mint))
p7 <- p7 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(15,30)) +
    labs(list(title = "Mean Intensity" , 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


require(tikzDevice)
options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
    "\\usepackage{tipa}"))
tikz("tikz/all/all.tex", standAlone=TRUE, width=10.5, height=5)
library(ggplot2)
library(gridExtra)

grid.arrange(arrangeGrob(p2,p5),arrangeGrob(p3,p6),arrangeGrob(p4,p7), ncol=3, widths=c(1/3,1/3,1/3))

dev.off()










# models 


# Linear regression model COG
mcog <- lm(cog~vot,data=coronals)
summary(mcog)

coronals$residcog <- residuals(mcog)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.cog = aov(cog ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.cog)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residcog = aov(residcog ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residcog)




# Linear regression model SD
msd <- lm(sd~vot,data=coronals)
summary(msd)

coronals$residsd <- residuals(msd)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.sd = aov(sd ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.sd)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residsd = aov(residsd ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residsd)





# Linear regression model skew
mskew <- lm(skew~vot,data=coronals)
summary(mskew)

coronals$residskew <- residuals(mskew)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.skew = aov(skew ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.skew)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residskew = aov(residskew ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residskew)





# Linear regression model kurt
mkurt <- lm(kurt~vot,data=coronals)
summary(mkurt)

coronals$residkurt <- residuals(mkurt)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.kurt = aov(kurt ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.kurt)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residkurt = aov(residkurt ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residkurt)





# Linear regression model cm
mcm <- lm(cm~vot,data=coronals)
summary(mcm)

coronals$residcm <- residuals(mcm)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.cm = aov(cm ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.cm)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residcm = aov(residcm ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residcm)





# Linear regression model mint
mmint <- lm(mint~vot,data=coronals)
summary(mmint)

coronals$residmint <- residuals(mmint)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.mint = aov(mint ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.mint)

# Mixed design ANOVA: 1 between subjects factor and 1 withing subjects factor
aov.residmint = aov(residmint ~ language*cons + Error(subject/cons), data=coronals)
summary(aov.residmint)





##################
# PLOT RESIDUALS #
##################


r1 <- ggplot(coronals, aes(cons, residcog))
r1 <- r1 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(-150, 150)) +
    labs(list(title = "Center of Gravity", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


r2 <- ggplot(coronals, aes(cons, residsd))
r2 <- r2 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(-150, 150)) +
    labs(list(title = "Standard Deviation", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


r3 <- ggplot(coronals, aes(cons, residskew))
r3 <- r3 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5, 10)) +
    labs(list(title = "Skewness", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


r4 <- ggplot(coronals, aes(cons, residkurt))
r4 <- r4 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    scale_y_continuous(limits = c(-5, 10)) +
    labs(list(title = "Kurtosis", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


r5 <- ggplot(coronals, aes(cons, residcm))
r5 <- r5 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(-5, 10)) +
    labs(list(title = "Central Moment", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


r6 <- ggplot(coronals, aes(cons, residmint))
r6 <- r6 + geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    # scale_y_continuous(limits = c(-5, 10)) +
    labs(list(title = "Mean Intensity", 
              x = "", y = "")) +
    theme(legend.position = "none") +
    scale_fill_manual(values=c("#FF2860", "#0066FF")) +
    # coord_flip() +
    facet_wrap(~language)


# plot_all_data
(r1.leg <- ggplot(coronalsALL, aes(cons, vot)) +
    geom_boxplot(aes(fill = cons),  outlier.size = .5, notch=FALSE) + 
    labs(list(title = "", 
              x = "Phoneme", y = "VOT (ms)")) +
    theme(plot.title = element_text(size = rel(1.5))) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.title = element_text(size = 12)) +
    facet_wrap(~language) +
    scale_fill_manual(values=c("#FF2860", "#0066FF"),
                      name="Phoneme",
                      breaks=c("d", "t"),
                      labels=c("English /d/", 
                               "Spanish /t/")))

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg<-g_legend(r1.leg)




require(tikzDevice)
options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
    "\\usepackage{tipa}"))
tikz("tikz/residuals/residuals.tex", standAlone=TRUE, width=10.5, height=5)
library(ggplot2)
library(gridExtra)

grid.arrange(arrangeGrob(r1,r4),arrangeGrob(r2,r5),arrangeGrob(r3,r6), ncol=3, widths=c(1/3,1/3,1/3))

dev.off()










# Table for results of all models


# descriptive stats all data
# get means of subsets of factors
library(xtable)
descriptives <- aggregate(cbind(vot, cog, sd, skew, kurt, cm, mint) ~ 
                      language + cons, data = coronalsALL, FUN = mean)
print(xtable(descriptives), floating=FALSE, type="latex", 
      include.rownames=FALSE,
      file='./tables/descriptives.tex')


# cog tables
print(xtable(mcog), floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/1_cog.tex")
print(xtable(aov.residcog),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/1_residcog.tex")

# sd tables
print(xtable(msd),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/2_sd.tex")
print(xtable(aov.residsd),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/2_residsd.tex")

# skew tables
print(xtable(mskew),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/3_skew.tex")
print(xtable(aov.residskew),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/3_residskew.tex")

# kurt tables
print(xtable(mkurt),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/4_kurt.tex")
print(xtable(aov.residkurt),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/4_residkurt.tex")

# cm tables
print(xtable(mcm),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/4_cm.tex")
print(xtable(aov.residcm),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/4_residcm.tex")

# mint tables
print(xtable(mmint),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/5_mint.tex")
print(xtable(aov.residmint),floating=FALSE, type = "latex", 
  include.rownames = FALSE,
  file = "./tables/5_residmint.tex")








########
library(MuMIn)


dEng = subset(coronals,language=="English" & cons == "d")
tEsp = subset(coronals,language=="Spanish" & cons == "t")
dt <- rbind(dEng, tEsp)
dt$cons <- as.factor(dt$cons)
str(dt)



# Logistic regression model
mod.sd <- glm(cons~residsd, data=dt, family="binomial")
mod.sk <- glm(cons~residskew, data=dt, family="binomial")
mod.cog <- glm(cons~residcog, data=dt, family="binomial")
mod.cm <- glm(cons~residcm, data=dt, family="binomial")
mod.mint <- glm(cons~residmint, data=dt, family="binomial")

r.squaredGLMM(mod.sd)
r.squaredGLMM(mod.sk)
r.squaredGLMM(mod.cog)
r.squaredGLMM(mod.cm)
r.squaredGLMM(mod.mint)


ad.mod.cog <- glm(cons~residcog+residmint, data=dt, family="binomial")
ad.mod.cogmintsd <- glm(cons~residcog+residmint+residsd, data=dt, family="binomial")
ad.mod.cogmintsdsk <- glm(cons~residcog+residmint+residsd+residskew, data=dt, family="binomial")


library(MuMIn)
r.squaredGLMM(ad.mod.cogsd)

cor(dt$residcog, dt$residsd)
cor(dt$residcog, dt$residmint)




predTest <- predict(mod.cog, dt)
predD <- rep("t", dim(coronals)[1])

predD[mod.cog$fitted > 0.5] = "d"

table(predD, predTest)


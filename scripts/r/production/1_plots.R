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











## @knitr plot1

# PLOTS
engPlot <- df[df$lang == 'english', ]
engPlot$group <- droplevels(engPlot$group)

spPlot  <- df[df$lang == 'spanish', ]
spPlot$group <- droplevels(spPlot$group)

par(mfrow = c(1, 2))
boxplot(vot ~ group * phon, data = engPlot, 
        main = "English", horizontal = FALSE, 
        ylim = c(-250, 250), axes = F)

# x and y axis
axis(side = 1, at = (1:6), labels = c("NEN", "bi Eng", "bi Esp", "NEN", "bi Eng", "bi Esp")) 
mtext("/d/", side = 1, line = 2.5, at = 2)
mtext("/t/", side = 1, line = 2.5, at = 5)
axis(side = 2, las = 1, at = seq(-220, 220, by = 60))

boxplot(vot ~ group * phon, data = spPlot, 
        main = "Spanish", horizontal = FALSE, 
        ylim = c(-250, 250), axes = F)

# x and y axis
axis(side = 1, at = (1:6), labels = c("NSP", "bi Eng", "bi Esp", "NSP", "bi Eng", "bi Esp")) 
mtext("/d/", side = 1, line = 2.5, at = 2)
mtext("/t/", side = 1, line = 2.5, at = 5)
axis(side = 2, las = 1, at = seq(-220, 220, by = 60))






## @knitr plot2


avg <- aggregate(cbind(vot, ri, cog) ~ participant + group + lang + phon, data = df, FUN = mean)

g <- ggplot(df, aes(x = ri, y = vot, colour = phon))
g  + geom_point(aes(size = cog)) + 
     coord_flip() +
     facet_grid(lang ~ group)











## @knitr plot3

# require(tikzDevice)
#     options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
#     "\\usepackage{tipa}"))
#     tikz("figures/all/all.tex", 
#     standAlone = TRUE, width = 9, height = 6)

layout(matrix(c(1,2,3,4,5,6,7,7,7), ncol = 3, byrow = TRUE), heights = c(5, 5, 1), 
  widths = c(5, 5, 5.5))
par(mai = c(0.6, 0.6, 0.3, 0.3))

nen <- df %>% 
  filter(group == "NEN" & lang == "english") %>%
  select(phon, vot, ri, cog)

with(nen, plot(vot, ri, main = "Monolingual English", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(nen$vot[nen$phon == "d"], nen$ri[nen$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * nen$cog[nen$phon == "d"])
     points(nen$vot[nen$phon == "t"], nen$ri[nen$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * nen$cog[nen$phon == "t"])

bieng <- df %>% 
  filter(group == "biEng" & lang == "english") %>%
  select(phon, vot, ri, cog)

with(bieng, plot(vot, ri, main = "English dominant", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(bieng$vot[bieng$phon == "d"], bieng$ri[bieng$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * bieng$cog[bieng$phon == "d"])
     points(bieng$vot[bieng$phon == "t"], bieng$ri[bieng$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * bieng$cog[bieng$phon == "t"])

par(mai = c(0.6, 0.6, 0.3, 0.6))
biesp <- df %>% 
  filter(group == "biEsp" & lang == "english") %>%
  select(phon, vot, ri, cog)

with(biesp, plot(vot, ri, main = "Spanish dominant", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(biesp$vot[biesp$phon == "d"], biesp$ri[biesp$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * biesp$cog[biesp$phon == "d"])
     points(biesp$vot[biesp$phon == "t"], biesp$ri[biesp$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * biesp$cog[biesp$phon == "t"])
     mtext('English', side = 4, srt = 1, line = 1.5)


par(mai = c(0.6, 0.6, 0.3, 0.3))
nsp <- df %>% 
  filter(group == "NSP" & lang == "spanish") %>%
  select(phon, vot, ri, cog)

with(nsp, plot(vot, ri, main = "Monolingual Spanish", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(nsp$vot[nsp$phon == "d"], nsp$ri[nsp$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * nsp$cog[nsp$phon == "d"])
     points(nsp$vot[nsp$phon == "t"], nsp$ri[nsp$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * nsp$cog[nsp$phon == "t"])

bieng <- df %>% 
  filter(group == "biEng" & lang == "spanish") %>%
  select(phon, vot, ri, cog)

with(bieng, plot(vot, ri, main = "English dominant", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(bieng$vot[bieng$phon == "d"], bieng$ri[bieng$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * bieng$cog[bieng$phon == "d"])
     points(bieng$vot[bieng$phon == "t"], bieng$ri[bieng$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * bieng$cog[bieng$phon == "t"])

par(mai = c(0.6, 0.6, 0.3, 0.6))
biesp <- df %>% 
  filter(group == "biEsp" & lang == "spanish") %>%
  select(phon, vot, ri, cog)

with(biesp, plot(vot, ri, main = "Spanish dominant", ylim = c(-25, 25),
     xlim = c(-175, 175), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(biesp$vot[biesp$phon == "d"], biesp$ri[biesp$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * biesp$cog[biesp$phon == "d"])
     points(biesp$vot[biesp$phon == "t"], biesp$ri[biesp$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * biesp$cog[biesp$phon == "t"])
     mtext('Spanish', side = 4, srt = 1, line = 1.5)


par(mai=c(0,0,0,0))
plot.new()
legend(0.375, 0.9, ncol=2, title = "Phoneme",
        legend = c("/d/", "/t/"), bty = "n",
        col = c(rgb(220, 20, 60, 150, maxColorValue = 255), 
        rgb(0, 0, 204, 200, maxColorValue = 255)), lwd = 3)
legend(0.5, 0.9, ncol=3, title = "COG",
        legend = c(2000, 4000, 6000), 
        pch = 21, col = "darkgray", bty = "n",
        pt.cex = c(0.0005 * 2000, 0.0005 * 4000, 0.0005 * 6000))


# dev.off()












## @knitr plot4

biEsp <- df %>%
  filter(group == "biEsp") %>%
  filter(lang == "spanish" & phon == "t" | lang == "english" & phon == "d") %>%
  select(participant, group, lang, word, phon, vot, cog, ri)
biEsp <- droplevels(biEsp)

biEng <- df %>%
  filter(group == "biEng") %>%
  filter(lang == "spanish" & phon == "t" | lang == "english" & phon == "d") %>%
  select(participant, group, lang, word, phon, vot, cog, ri)
biEng <- droplevels(biEng)








# require(tikzDevice)
#     options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
#     "\\usepackage{tipa}"))
#     tikz("figures/dt_compare/dt_compare.tex", 
#     standAlone = TRUE, width = 9, height = 4.5)


layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE), heights = c(4, 0.5))
par(mai = c(0.8, 0.8, 0.3, 0.3))

with(biEng, plot(vot, ri, main = "English dominant", ylim = c(-25, 25),
     xlim = c(-200, 200), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(biEng$vot[biEng$phon == "d"], biEng$ri[biEng$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * biEng$cog[biEng$phon == "d"])
     points(biEng$vot[biEng$phon == "t"], biEng$ri[biEng$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * biEng$cog[biEng$phon == "t"])

with(biEsp, plot(vot, ri, main = "Spanish dominant", ylim = c(-25, 25),
     xlim = c(-200, 200), xlab = "VOT (ms)", ylab = "RI", type = "n"))
     points(biEsp$vot[biEsp$phon == "d"], biEsp$ri[biEsp$phon == "d"], 
            pch = 21, col = "black", bg = rgb(220, 20, 60, 150, maxColorValue = 255), 
            cex = .0007 * biEsp$cog[biEsp$phon == "d"])
     points(biEsp$vot[biEsp$phon == "t"], biEsp$ri[biEsp$phon == "t"], 
            pch = 21, col = "black", bg = rgb(0, 0, 204, 200, maxColorValue = 255), 
            cex = .0007 * biEsp$cog[biEsp$phon == "t"])

par(mai=c(0,0,0,0))
plot.new()
legend(0.25, 1, ncol=2, title = "Phoneme", bty = "n",
        legend = c("English /d/", "Spanish /t/"), 
        col = c(rgb(220, 20, 60, 150, maxColorValue = 255), 
        rgb(0, 0, 204, 200, maxColorValue = 255)), lwd = 3)
legend(0.55, 1, ncol=3, title = "COG",
        legend = c(2000, 4000, 6000), bty = "n",
        pch = 21, col = "darkgray",
        pt.cex = c(0.0005 * 2000, 0.0005 * 4000, 0.0005 * 6000))

# dev.off()







## @knitr plot5

library(htmltools)
library(htmlwidgets)
library(metricsgraphics)
library(RColorBrewer)


p1 <- df %>%
  filter(group == "NEN") %>%
  mjs_plot(x=vot, y=ri, width=300, height=300) %>%
  mjs_point(color_accessor=phon,
            x_rug=TRUE, y_rug=TRUE,
            size_accessor=cog,
            size_range=c(5, 10),
            color_type="category") %>%
  mjs_labs(x="", y="") %>%
  mjs_add_legend(legend="X")

p2 <- df %>%
  filter(group == "biEng" & lang == "english") %>%
  mjs_plot(x=vot, y=ri, width=300, height=300) %>%
  mjs_point(color_accessor=phon,
            x_rug=TRUE, y_rug=TRUE,
            size_accessor=cog,
            size_range=c(5, 10),
            color_type="category") %>%
  mjs_labs(x="", y="") %>%
  mjs_add_legend(legend="X")

p3 <- df %>%
  filter(group == "biEsp" & lang == "english") %>%
  mjs_plot(x=vot, y=ri, width=300, height=300) %>%
  mjs_point(color_accessor=phon,
            x_rug=TRUE, y_rug=TRUE,
            size_accessor=cog,
            size_range=c(5, 10),
            color_type="category") %>%
  mjs_labs(x="", y="") %>%
  mjs_add_legend(legend="X")


mjs_grid(p1, p2, p3, ncol = 3)



## @knitr plot6

## Spider/radar plots

glimpse(df)

df %>%
  filter(group == "biEng", lang == "english") %>%
  aggregate(cbind(vot, ri, cog, sd, sk, kt) ~ group + phon, data = ., FUN = mean) ->
  radPlotEngEng

par(mar = c(1, 1, 2, 1))
radPlotEngEng %>%
  select(., vot:kt) %>%
  webplot(., data.row = 2, main = "Bilinguals")
radPlotEngEng %>%
  select(., vot:kt) %>%
  webplot(., 2, add = T, col = "blue", lty = 2)
par(new = T)
par(mar = c(0, 0, 0, 0))
plot(0, type = "n", axes = F)
legend("bottomright", lty = c(1, 2), lwd = 2, col = c("red", "blue"), c("/d/", 
    "/t/"), bty = "n")





par(mar = c(1, 1, 2, 1))
webplot(mtcars, "Mazda RX4", main = "Compare Cars")
webplot(mtcars, "Mazda RX4 Wag", add = T, col = "blue", lty = 2)
par(new = T)
par(mar = c(0, 0, 0, 0))
plot(0, type = "n", axes = F)
legend("bottomright", lty = c(1, 2), lwd = 2, col = c("red", "blue"), c("Mazda RX4", 
    "Mazda RX4 Wag"), bty = "n")




library(fmsb)


require(tikzDevice)
    options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
    "\\usepackage{tipa}"))
    tikz("figures/dt_compare/new.tex", 
    standAlone = TRUE, width = 9, height = 4.5)


par(mfrow=c(1,2))
par(mar = c(1, 1, 2, 1))

df %>%
  aggregate(cbind(vot, ri, cog, sd, sk, kt) ~ group + phon + lang, data = ., FUN = mean) %>%
  filter(., lang == "spanish", phon == "t") %>%
  dplyr::select(., vot:kt) %>%
  radarchart(., maxmin = FALSE, axistype = 0, plwd = 1.5, pcol = c(1, 2, 4), 
    cglty = 3, cglwd = 1, cglcol = "grey", axislabcol = "black", 
    title = "Spanish /t/")

df %>%
  aggregate(cbind(vot, ri, cog, sd, sk, kt) ~ group + phon + lang, data = ., FUN = mean) %>%
  filter(., lang == "english", phon == "d") %>%
  dplyr::select(., vot:kt) %>%
  radarchart(., maxmin = FALSE, axistype = 0, plwd = 1.5, pcol = c(1, 2, 4), 
    cglty = 3, cglwd = 1, cglcol = "grey", axislabcol = "black", 
    title = "English /d/")

par(new = T)
par(mar = c(0, 0, 0, 0))
plot(0, type = "n", axes = F)
legend("bottomright", lty = c(1, 2, 3), lwd = 2, col = c("black", "red", "blue"), c("Native", 
    "Eng dominant", "Sp dominant"), bty = "n")

dev.off()


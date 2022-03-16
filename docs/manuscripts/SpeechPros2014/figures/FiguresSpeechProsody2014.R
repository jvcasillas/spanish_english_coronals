# setwd("~/Dropbox/SpeechPros/figures/")

# read data
vot = read.delim("SP2014_data_bil.txt", header=TRUE)
summary(vot)

# create subsets for plots
bil_eng <- subset(vot,lang=="English")
eng_t = subset(bil_eng, cons=="t")
eng_d = subset(bil_eng, cons=="d")

bil_sp <- subset(vot,lang=="Spanish")
sp_t <- subset(bil_sp, cons=="t")
sp_d <- subset(bil_sp, cons=="d")



# english
require(tikzDevice)
	options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
	"\\usepackage{tipa}"))
	tikz("~/Dropbox/SpeechPros/figures/bil_eng.tex", standAlone=TRUE, width=5, height=3.5)

	par(mfrow=c(1,2),mar=c(2,5,2,2)+0.1, cex.lab=1, cex.axis=1, oma = c(0, 0, 0, 0), mgp = c(3, 1, 0))
	# english t	
	plot(as.numeric(eng_t$stress),eng_t$VOT,  axes = F, type="n",
	     ylim=c(-45,120), xlim=c(0.8,2.2), ylab="VOT (ms)", xlab="Stress", main="English /t/")
	box()
	axis(side=1, at=(1:2), labels=c("Stressed","Unstressed")) 
	axis(side=2,las=1, at=seq(-45, 120, by=20))
	points(eng_t$stress[eng_t$stress=="Stressed"], eng_t$VOT[eng_t$stress=="Stressed"], col="blue", cex=2)
	points(eng_t$stress[eng_t$stress=="Unstressed"], eng_t$VOT[eng_t$stress=="Unstressed"], col="red", cex=2)
	abline(lm(VOT~stress,data=eng_t), col="darkgreen", lty=2, lwd=2)
	abline(h=0, lty=6)
	
	# english d
	plot(as.numeric(eng_d$stress),eng_d$VOT,  axes = F, type="n",
	     ylim=c(-45,120), xlim=c(0.8,2.2), ylab="", xlab="Stress", main="English /d/")
	box()
	axis(side=1, at=(1:2), labels=c("Stressed","Unstressed")) 
	axis(side=2,las=1, at=seq(-45, 120, by=20))
	points(eng_d$stress[eng_d$stress=="Stressed"], eng_d$VOT[eng_d$stress=="Stressed"], col="blue", cex=2)
	points(eng_d$stress[eng_d$stress=="Unstressed"], eng_d$VOT[eng_d$stress=="Unstressed"], col="red", cex=2)
	abline(lm(VOT~stress,data=eng_d), col="darkgreen", lty=2, lwd=2)
	abline(h=0, lty=6)
dev.off()



# Spanish
require(tikzDevice)
	options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
	"\\usepackage{tipa}"))
	tikz("~/Dropbox/SpeechPros/figures/bil_sp.tex", standAlone=TRUE, width=5, height=3.5)

	par(mfrow=c(1,2),mar=c(2,5,2,2)+0.1, cex.lab=1, cex.axis=1, oma = c(0, 0, 0, 0), mgp = c(3, 1, 0))
	# Spanish t	
	plot(as.numeric(sp_t$stress),sp_t$VOT,  axes = F, type="n",
	     ylim=c(-130,50), xlim=c(0.8,2.2), ylab="VOT (ms)", xlab="Stress", main="Spanish /t/")
	box()
	axis(side=1, at=(1:2), labels=c("Stressed","Unstressed")) 
	axis(side=2,las=1, at=seq(-130, 50, by=20))
	points(sp_t$stress[sp_t$stress=="Stressed"], sp_t$VOT[sp_t$stress=="Stressed"], col="blue", cex=2)
	points(sp_t$stress[sp_t$stress=="Unstressed"], sp_t$VOT[sp_t$stress=="Unstressed"], col="red", cex=2)
	abline(lm(VOT~stress,data=sp_t), col="darkgreen", lty=2, lwd=2)
	abline(h=0, lty=6)
	
	# Spanish d
	plot(as.numeric(sp_d$stress),sp_d$VOT,  axes = F, type="n",
	     ylim=c(-130,50), xlim=c(0.8,2.2), ylab="", xlab="Stress", main="Spanish /d/")
	box()
	axis(side=1, at=(1:2), labels=c("Stressed","Unstressed")) 
	axis(side=2,las=1, at=seq(-130,50, by=20))
	points(sp_d$stress[sp_d$stress=="Stressed"], sp_d$VOT[sp_d$stress=="Stressed"], col="blue", cex=2)
	points(sp_d$stress[sp_d$stress=="Unstressed"], sp_d$VOT[sp_d$stress=="Unstressed"], col="red", cex=2)
	abline(lm(VOT~stress,data=sp_d), col="darkgreen", lty=2, lwd=2)
	abline(h=0, lty=6)
dev.off()




vot %>% 
  as_tibble() %>% 
  filter(cons == "t", lang == "English") %>% 
  group_by(stress) %>% 
  summarize(mean_vot = mean(VOT), sd_vot = sd(VOT))


read.delim("SP2014_data_eng.txt", header=TRUE, sep = " ") %>% 
  as_tibble() %>% 
  filter(cons == "t") %>% 
  group_by(stress) %>% 
  summarize(mean_vot = mean(vot), sd_vot = sd(vot))

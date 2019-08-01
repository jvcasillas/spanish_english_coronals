attach(mydata)
span = subset(mydata, lang=="spanish")
eng = subset(mydata, lang=="english")
bil = subset(mydata, group=="biEng"|group=="biEsp")
mono = subset(mydata, group=="NSP"|group=="NEN")
summary(bil)
detach(mydata)

agspan = aggregate(cbind(vot,ri,cog,sd,sk,kt)~participant*group*phon,FUN=mean,data=span)
ageng = aggregate(cbind(vot,ri,cog,sd,sk,kt)~participant*group*phon,FUN=mean,data=eng)
agbil = aggregate(cbind(vot,ri,cog,sd,sk,kt)~participant*group*phon*lang,FUN=mean,data=bil)
agmono = aggregate(cbind(vot,ri,cog,sd,sk,kt)~participant*group*phon,FUN=mean,data=mono)

summary(agspan)
summary(ageng)
summary(agbil)
summary(agmono)

library(gplots)

###################################################################
# Monolinguals VOT: Are Spanish and English monolinguals different?
par(mfcol=c(1,2))
plotmeans(vot~phon,data=subset(mono,group=="NSP"),main="Spanish",ylim=c(-100,100),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(mono,group=="NEN"),main="English",ylim=c(-100,100),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
summary(aov(vot~phon*group+Error(participant/phon),data=agmono))

# Monolinguals COG: Are Spanish and English monolinguals different?
par(mfcol=c(1,2))
plotmeans(cog~phon,data=subset(mono,group=="NSP"),main="Spanish",lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
plotmeans(cog~phon,data=subset(mono,group=="NEN"),main="English",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
summary(aov(cog~phon*group+Error(participant/phon),data=agmono))

# Monolinguals RI: Are Spanish and English monolinguals different?
par(mfcol=c(1,2))
plotmeans(ri~phon,data=subset(mono,group=="NSP"),main="Spanish", lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Relative Intensity (dB)",ylim=c(-10,10))
abline(h=0,lty=6)
plotmeans(ri~phon,data=subset(mono,group=="NEN"),main="English",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Relative Intensity (dB)",ylim=c(-10,10))
abline(h=0,lty=6)
summary(aov(ri~phon*group+Error(participant/phon),data=agmono))

#########################################################
# Spanish VOT: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(vot~phon,data=subset(span,group=="NSP"),main="Span. mono",ylim=c(-100,100),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(span,group=="biEsp"),main="Span.-dominant",ylim=c(-100,100),lwd=3,col="black",barwidth=3,barcol="black",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(span,group=="biEng"),main="Eng.-dominant",ylim=c(-100,100),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
summary(aov(vot~phon*group+Error(participant/phon),data=agspan))

# Spanish COG: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(cog~phon,data=subset(span,group=="NSP"),main="Span. mono",lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
plotmeans(cog~phon,data=subset(span,group=="biEsp"),main="Span.-dominant",lwd=3,col="black",barwidth=3,barcol="black",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
plotmeans(cog~phon,data=subset(span,group=="biEng"),main="Eng.-dominant",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
summary(aov(cog~phon*group+Error(participant/phon),data=agspan))
attach(agspan)
t.test(cog[group=="NSP"],cog[group=="biEsp"])
t.test(cog[group=="NSP"],cog[group=="biEng"])
detach(agspan)

# Spanish RI: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(ri~phon,data=subset(span,group=="NSP"),main="Span. mono",lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Relative Intensity (dB)",ylim=c(-10,10))
abline(h=0,lty=6)
plotmeans(ri~phon,data=subset(span,group=="biEsp"),main="Span.-dominant",lwd=3,col="black",barwidth=3,barcol="black",ylab="Relative Intensity (dB)",ylim=c(-10,10))
abline(h=0,lty=6)
plotmeans(ri~phon,data=subset(span,group=="biEng"),main="Eng.-dominant",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Relative Intensity (dB)",ylim=c(-10,10))
abline(h=0,lty=6)
summary(aov(ri~phon*group+Error(participant/phon),data=agspan))

#########################################################
# English VOT: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(vot~phon,data=subset(eng,group=="NEN"),main="Eng. mono",ylim=c(-100,100),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(eng,group=="biEsp"),main="Span.-dominant",ylim=c(-100,100),lwd=3,col="black",barwidth=3,barcol="black",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(eng,group=="biEng"),main="Eng.-dominant",ylim=c(-100,100),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
summary(aov(vot~phon*group+Error(participant/phon),data=ageng))

# English COG: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(cog~phon,data=subset(eng,group=="NEN"),main="Eng. mono",lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
plotmeans(cog~phon,data=subset(eng,group=="biEsp"),main="Span.-dominant",lwd=3,col="black",barwidth=3,barcol="black",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
plotmeans(cog~phon,data=subset(eng,group=="biEng"),main="Eng.-dominant",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Center of Gravity (Hz)",ylim=c(0,5000))
summary(aov(cog~phon*group+Error(participant/phon),data=ageng))

# English RI: Are monolinguals and bilinguals different?
par(mfcol=c(1,3))
plotmeans(ri~phon,data=subset(eng,group=="NEN"),main="Eng. mono",lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Center of Gravity (Hz)",ylim=c(-10,10))
abline(h=0,lty=6)
plotmeans(ri~phon,data=subset(eng,group=="biEsp"),main="Span.-dominant",lwd=3,col="black",barwidth=3,barcol="black",ylab="Center of Gravity (Hz)",ylim=c(-10,10))
abline(h=0,lty=6)
plotmeans(ri~phon,data=subset(eng,group=="biEng"),main="Eng.-dominant",lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Center of Gravity (Hz)",ylim=c(-10,10))
abline(h=0,lty=6)
summary(aov(ri~phon*group+Error(participant/phon),data=ageng))

###############################################################
# Bilinguals VOT: Do bilinguals have two separate (sub)systems?
par(mfcol=c(1,2))
plotmeans(vot~phon,data=subset(bil,lang=="spanish"),main="Spanish",ylim=c(-100,100),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
plotmeans(vot~phon,data=subset(bil,lang=="english"),main="English",ylim=c(-100,100),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Voice Onset Times (ms)")
abline(h=0,lty=6)
summary(aov(vot~phon*lang+Error(participant/(phon*lang)),data=agbil))

# Bilinguals COG: Do bilinguals have two separate (sub)systems?
par(mfcol=c(1,2))
plotmeans(cog~phon,data=subset(bil,lang=="spanish"),main="Spanish",ylim=c(0,6000),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Center of Gravity (Hz)")
plotmeans(cog~phon,data=subset(bil,lang=="english"),main="English",ylim=c(0,6000),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Center of Gravity (Hz)")
summary(aov(cog~phon*lang+Error(participant/(phon*lang)),data=agbil))

# Bilinguals COG: Do bilinguals have two separate (sub)systems?
par(mfcol=c(1,2))
plotmeans(ri~phon,data=subset(bil,lang=="spanish"),main="Spanish",ylim=c(-10,10),lwd=3,col="dark red",barwidth=3,barcol="dark red",ylab="Relative Intensity (Hz)")
plotmeans(ri~phon,data=subset(bil,lang=="english"),main="English",ylim=c(-10,10),lwd=3,col="navy blue",barwidth=3,barcol="navy blue",ylab="Relative Intensity (Hz)")
summary(aov(ri~phon*lang+Error(participant/(phon*lang)),data=agbil))
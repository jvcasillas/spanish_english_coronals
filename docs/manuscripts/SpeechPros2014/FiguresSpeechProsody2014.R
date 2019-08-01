par(mfrow=c(1,2)) # 1 row, 2 cols

plot(as.numeric(bil_en$stress),bil_en$VOT,  axes = F, type="n",
     ylim=c(-45,125), xlim=c(0.95,2.05), ylab="VOT (ms)", xlab="Stress", main="English /t/")

attach(bil_en)
t = subset(bil_en, cons=="t")
d = subset(bil_en, cons=="d")
detach(bil_en)
attach(t)
box()
axis(side = 1, at=(1:2))
axis(side=2,las=1, at=seq(-45, 125, by=20))
legend("top", legend=c("str", "unstr"), pch=c("1","2"), col=c("blue","red"))
points(stress[stress=="stressed"], VOT[stress=="stressed"], col="blue", cex=2)
points(stress[stress=="unstressed"], VOT[stress=="unstressed"], col="red", cex=2)
abline(lm(VOT~stress), col="darkgreen", lty=2, lwd=2)
abline(h=0, lty=6)
detach(t)

attach(d)
plot(as.numeric(bil_en$stress),bil_en$VOT,  axes = F, type="n",
     ylim=c(-45,125), xlim=c(0.95,2.05), ylab="VOT (ms)", xlab="Stress", main="English /d/")
box()
axis(side = 1, at=(1:2))
axis(side=2,las=1, at=seq(-45, 125, by=20))
legend("top", legend=c("str", "unstr"), pch=c("1","2"), col=c("blue","red"))
points(stress[stress=="stressed"], VOT[stress=="stressed"], col="blue", cex=2)
points(stress[stress=="unstressed"], VOT[stress=="unstressed"], col="red", cex=2)
abline(lm(VOT~stress), col="darkgreen", lty=2, lwd=2)
abline(h=0, lty=6)
detach(d)
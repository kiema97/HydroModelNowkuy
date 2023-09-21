setwd("D:/Recherche/Article_KIEMA")
Sys.setenv(TZ = "UTC")

library(biwavelet)
sig.level = 0.9

df <- read.csv("nowkuy_data.csv", header = TRUE, sep = ",", dec = ".")
df <- data.frame(sapply(df, as.numeric, simplify=T))
colnames(df) <- c("year","P", "PET", "Q")

pr <- df$P
pet <- df$PET
sq <- df$Q

prn <- (pr - mean(pr))/sd(pr)
petn <- (pet - mean(pet))/sd(pet)
sqn <- (sq - mean(sq))/sd(sq)
years <- df$year

syear <- head(years,1)
eyear <- tail(years,1)
period_label <- paste0(" (",syear,"-",eyear,")")
ylims <- range(prn, petn, sqn)

#dev.off()

png("graphs/CWT.png", width = 15, height = 6, units = 'in', res = 500)

par(mfrow=c(2,3))
par(mar = c(0.5,5,3,3))
par(font.axis=2,font.lab=2)

text_y <- 2.2
prn_plot <- plot(years,prn,type="b",ylim=ylims,
                 main = paste0("Annual P", period_label),
                 ylab = "P (norm)", col="blue", lty=1, lwd=1, pch=19)
text(syear+1, text_y,"a)", cex = 2, font = 2)
prn_plot <- plot(years,petn,type="b",ylim=ylims,
                 main = paste0("Annual PET", period_label), 
                 ylab = "PET (norm)", col="orange", lty=1, lwd=1, pch=19)
text(syear+1, text_y,"b)", cex = 2, font = 2)
sqn_plot <- plot(years,sqn,type="b",ylim=ylims,
                 main = paste0("Annual Q", period_label), 
                 ylab = "Q (norm)", col="green", lty=1, lwd=1, pch=19)
text(syear+1, text_y,"c)", cex = 2, font = 2)


minp <- 2
maxp <- 32
lperiods <- c(2,4,8,16)
ylab <- "Period (years)"

CWT_pr <- wt(data.frame(years = years, pr = pr),sig.level=sig.level,max.scale = maxp)
CWT_pet <- wt(data.frame(years = years, pet = pet),sig.level=sig.level,max.scale = maxp)
CWT_sq <- wt(data.frame(years = years, sq = sq),sig.level=sig.level,max.scale = maxp)

par(mar = c(6,5,0.5,3))
zmax <- 2

plot(CWT_pr, tol=sig.level, ncol=1000,ylim=c(minp,maxp), type="power.norm", plot.cb = F, plot.phase = F,
     lty.sig=1,lwd.sig=2,lty.coi=1,lwd.coi=2, legend.horiz = T,
     col.coi="black",yaxt = "n",xlab = "", ylab = ylab, zlim=range(-1,zmax))
axis(side = 2, at=seq(1,4,by=1), labels = lperiods)
text(syear+2, 1.3,"d)", cex = 2, font = 2, col = "white")

plot(CWT_pet, tol=sig.level, ncol=1000,ylim=c(minp,maxp), type="power.norm", plot.cb = T, plot.phase = F,
     lty.sig=1,lwd.sig=2,lty.coi=1,lwd.coi=2, legend.horiz = T,
     col.coi="black",yaxt = "n",xlab = "", ylab = ylab, zlim=range(-1,zmax))
axis(side = 2, at=seq(1,4,by=1), labels = lperiods)
text(syear+2, 1.3,"e)", cex = 2, font = 2, col = "white")

plot(CWT_sq, tol=sig.level, ncol=1000,ylim=c(minp,maxp), type="power.norm", plot.cb = F, plot.phase = F,
     lty.sig=1,lwd.sig=2,lty.coi=1,lwd.coi=2, legend.horiz = T,
     col.coi="black",yaxt = "n",xlab = "", ylab = ylab, zlim=range(-1,zmax))
axis(side = 2, at=seq(1,4,by=1), labels = lperiods)
text(syear+2, 1.3,"f)", cex = 2, font = 2, col = "white")

dev.off()

png("graphs/WTC.png", width = 15, height = 6, units = 'in', res = 500)
par(mfrow=c(1,2))
par(mar = c(3,5,3,5))
par(font.axis=2,font.lab=2)

WTC_pr_sq <- wtc(data.frame(years = years, pr = pr),
                 data.frame(years = years, sq = sq),
                 sig.level=sig.level,max.scale = maxp, 
                 mother = "morlet", nrands = 300)
WTC_pet_sq <- wtc(data.frame(years = years, pet = pet),
                 data.frame(years = years, sq = sq),
                 sig.level=sig.level,max.scale = maxp, 
                 mother = "morlet", nrands = 300)

plot(WTC_pr_sq, tol=sig.level, ncol=1000,ylim=c(minp,maxp), 
     type="power.corr.norm", plot.cb = T,plot.phase = T,
     lty.sig=1,lwd.sig=2,lty.coi=1,lwd.coi=2,
     col.coi="black",yaxt = "n",xlab = "", ylab = ylab, zlim=c(0,1),
     main = paste0("WTC P~Q", period_label))
axis(side = 2, at=seq(1,4,by=1), labels = lperiods)
text(syear+2, 1.2,"a)", cex = 2, font = 2, col = "white")

plot(WTC_pet_sq, tol=sig.level, ncol=1000,ylim=c(minp,maxp), 
     type="power.corr.norm", plot.cb = T,plot.phase = T,
     lty.sig=1,lwd.sig=2,lty.coi=1,lwd.coi=2,
     col.coi="black",yaxt = "n",xlab = "", ylab = "", zlim=c(0,1),
     main = paste0("WTC PET~Q", period_label))
axis(side = 2, at=seq(1,4,by=1), labels = lperiods)
text(syear+2, 1.2,"b)", cex = 2, font = 2, col = "white")

dev.off()
print("terminated.")

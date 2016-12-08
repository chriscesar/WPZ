### an.all.01.blk.seas.R

### analyse data across sites

############################
### WITH BLOCKING OF DATA ##
### Blocking by SEASON    ##
############################

### load data
## save data
load(file = "data/processed/use.pt.50900149.Rdat")
load(file = "data/processed/use.pt.50900387.Rdat")
load(file = "data/processed/use.pt.50950106.Rdat")
load(file = "data/processed/use.pt.50950125.Rdat")
load(file = "data/processed/use.pt.50950217.Rdat")
load(file = "data/processed/use.pt.50950249.Rdat")
load(file = "data/processed/mydat.trm.Rdata")

#pt.50900149$seas <- as.numeric(pt.50900149$seas)
#pt.50900387$seas <- as.numeric(pt.50900387$seas)
#pt.50950106$seas <- as.numeric(pt.50950106$seas)
#pt.50950125$seas <- as.numeric(pt.50950125$seas)
#pt.50950217$seas <- as.numeric(pt.50950217$seas)
#pt.50950249$seas <- as.numeric(pt.50950249$seas)

require(rkt)
require(ggplot2); require(ggthemes)
################################################################
################################################################
#########
## NO3 ##
#########
## ID trends in no3 WITHOUT serial correction
sKenb.no3.149 <- rkt(correct=F,date = pt.50900149$timestamp, y=pt.50900149$no3, block=pt.50900149$month,rep = "m")
sKenb.no3.387 <- rkt(correct=F,date = pt.50900387$timestamp, y=pt.50900387$no3, block=pt.50900387$month,rep = "m")
sKenb.no3.106 <- rkt(correct=F,date = pt.50950106$timestamp, y=pt.50950106$no3, block=pt.50950106$month,rep = "m")
sKenb.no3.125 <- rkt(correct=F,date = pt.50950125$timestamp, y=pt.50950125$no3, block=pt.50950125$month,rep = "m")
sKenb.no3.217 <- rkt(correct=F,date = pt.50950217$timestamp, y=pt.50950217$no3, block=pt.50950217$month,rep = "m")##SIG
sKenb.no3.249 <- rkt(correct=F,date = pt.50950249$timestamp, y=pt.50950249$no3, block=pt.50950249$month,rep = "m")
## ID trends in no3 WITH serial correction
ssKenb.no3.149 <- rkt(correct=T,date = pt.50900149$timestamp, y=pt.50900149$no3, block=pt.50900149$month,rep = "m")
ssKenb.no3.387 <- rkt(correct=T,date = pt.50900387$timestamp, y=pt.50900387$no3, block=pt.50900387$month,rep = "m")
ssKenb.no3.106 <- rkt(correct=T,date = pt.50950106$timestamp, y=pt.50950106$no3, block=pt.50950106$month,rep = "m")
ssKenb.no3.125 <- rkt(correct=T,date = pt.50950125$timestamp, y=pt.50950125$no3, block=pt.50950125$month,rep = "m")
ssKenb.no3.217 <- rkt(correct=T,date = pt.50950217$timestamp, y=pt.50950217$no3, block=pt.50950217$month,rep = "m")
ssKenb.no3.249 <- rkt(correct=T,date = pt.50950249$timestamp, y=pt.50950249$no3, block=pt.50950249$month,rep = "m")
## ID trends in no3 WITHOUT serial correction & salinity covariable
cvsKenb.no3.149 <- rkt(correct=F,date = pt.50900149$timestamp, y=pt.50900149$no3, block=pt.50900149$month,rep = "m", cv = pt.50900149$sal)
cvsKenb.no3.387 <- rkt(correct=F,date = pt.50900387$timestamp, y=pt.50900387$no3, block=pt.50900387$month,rep = "m", cv = pt.50900387$sal)
cvsKenb.no3.106 <- rkt(correct=F,date = pt.50950106$timestamp, y=pt.50950106$no3, block=pt.50950106$month,rep = "m", cv = pt.50950106$sal)
cvsKenb.no3.125 <- rkt(correct=F,date = pt.50950125$timestamp, y=pt.50950125$no3, block=pt.50950125$month,rep = "m", cv = pt.50950125$sal)
cvsKenb.no3.217 <- rkt(correct=F,date = pt.50950217$timestamp, y=pt.50950217$no3, block=pt.50950217$month,rep = "m", cv = pt.50950217$sal)##SIG
cvsKenb.no3.249 <- rkt(correct=F,date = pt.50950249$timestamp, y=pt.50950249$no3, block=pt.50950249$month,rep = "m", cv = pt.50950249$sal)
## ID trends in no3 WITH serial correction & salinity covariable
cvssKenb.no3.149 <- rkt(correct=T,date = pt.50900149$timestamp, y=pt.50900149$no3, block=pt.50900149$month,rep = "m", cv = pt.50900149$sal)
cvssKenb.no3.387 <- rkt(correct=T,date = pt.50900387$timestamp, y=pt.50900387$no3, block=pt.50900387$month,rep = "m", cv = pt.50900387$sal)
cvssKenb.no3.106 <- rkt(correct=T,date = pt.50950106$timestamp, y=pt.50950106$no3, block=pt.50950106$month,rep = "m", cv = pt.50950106$sal)
cvssKenb.no3.125 <- rkt(correct=T,date = pt.50950125$timestamp, y=pt.50950125$no3, block=pt.50950125$month,rep = "m", cv = pt.50950125$sal)
cvssKenb.no3.217 <- rkt(correct=T,date = pt.50950217$timestamp, y=pt.50950217$no3, block=pt.50950217$month,rep = "m", cv = pt.50950217$sal)
cvssKenb.no3.249 <- rkt(correct=T,date = pt.50950249$timestamp, y=pt.50950249$no3, block=pt.50950249$month,rep = "m", cv = pt.50950249$sal)
## Plot all
p <- ggplot(data = mydat.trm, aes(x= date, y = no3, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = expression(paste("NO"[3], " (mg/l)")))

a <- ggplot(data=mydat.trm, aes(x=date,y=no3))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
a <- a + theme_few()
a <- a + labs(x = "", y = expression(paste("NO"[3], " (mg/l)")))
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
a <- a + ylim(0,8)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a
#a <- a + geom_rect(xmin = -Inf, xmax = Inf, ymin = -1, ymax = 1,
#                   fill = "red", alpha = I(0.5),data=mydat.trm[2.])
## plot significant
q <- ggplot(data = pt.50950217, aes(x= timestamp, y = no3))
q <- q + geom_point()
q <- q + geom_line()
q <- q + theme_few()
q <- q + labs(x = "", y = expression(paste("NO"[3], " (mg/l)")))
##add slope
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.no3.217$tau,3),"\nslope = ",
             round(sKenb.no3.217$B,3), "\np = ",
             round(sKenb.no3.217$sl,5))
q <- q + annotate(geom="text", x=2013, y=2.75, label=lb1,
                  color="red", hjust = 0)

ppi <- 300
png(file = "output/figs/pt.all.NO3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.NO3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();
png(file = "output/figs/PT50950217.blseas.NO3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off()
rm(sKenb.no3.149,sKenb.no3.387,sKenb.no3.106,sKenb.no3.125,sKenb.no3.217,
   sKenb.no3.249, ssKenb.no3.149,ssKenb.no3.387,ssKenb.no3.106,
   ssKenb.no3.125,ssKenb.no3.217,ssKenb.no3.249)
rm(p,a,q,lb1)
################################################################
################################################################
#########
## NO2 ##
#########
## ID trends in no2 WITHOUT Serial correction
sKenb.no2.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$no2,block=pt.50900149$month, rep = "m")#SIG!
sKenb.no2.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$no2,block=pt.50900387$month, rep = "m")
sKenb.no2.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$no2,block=pt.50950106$month, rep = "m")
sKenb.no2.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$no2,block=pt.50950125$month, rep = "m")
sKenb.no2.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$no2,block=pt.50950217$month, rep = "m")##SIG
sKenb.no2.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$no2,block=pt.50950249$month, rep = "m")
## ID trends in no2 WITH Serial correction
ssKenb.no2.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$no2,block=pt.50900149$month, rep = "m")#SIG!
ssKenb.no2.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$no2,block=pt.50900387$month, rep = "m")
ssKenb.no2.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$no2,block=pt.50950106$month, rep = "m")
ssKenb.no2.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$no2,block=pt.50950125$month, rep = "m")
ssKenb.no2.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$no2,block=pt.50950217$month, rep = "m")
ssKenb.no2.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$no2,block=pt.50950249$month, rep = "m")
## ID trends in no2 WITHOUT Serial correction & salinity covariable
cvsKenb.no2.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$no2,block=pt.50900149$month, rep = "m", cv = pt.50900149$sal)#SIG!
cvsKenb.no2.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$no2,block=pt.50900387$month, rep = "m", cv = pt.50900387$sal)
cvsKenb.no2.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$no2,block=pt.50950106$month, rep = "m", cv = pt.50950106$sal)
cvsKenb.no2.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$no2,block=pt.50950125$month, rep = "m", cv = pt.50950125$sal)
cvsKenb.no2.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$no2,block=pt.50950217$month, rep = "m", cv = pt.50950217$sal)##SIG
cvsKenb.no2.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$no2,block=pt.50950249$month, rep = "m", cv = pt.50950249$sal)
## ID trends in no2 WITH Serial correction & salinity covariable
cvssKenb.no2.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$no2,block=pt.50900149$month, rep = "m", cv = pt.50900149$sal)#SIG!
cvssKenb.no2.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$no2,block=pt.50900387$month, rep = "m", cv = pt.50900387$sal)
cvssKenb.no2.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$no2,block=pt.50950106$month, rep = "m", cv = pt.50950106$sal)
cvssKenb.no2.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$no2,block=pt.50950125$month, rep = "m", cv = pt.50950125$sal)
cvssKenb.no2.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$no2,block=pt.50950217$month, rep = "m", cv = pt.50950217$sal)
cvssKenb.no2.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$no2,block=pt.50950249$month, rep = "m", cv = pt.50950249$sal)
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = no2, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = expression(paste("NO"[2], " (mg/l)")))

a <- ggplot(data=mydat.trm, aes(x=date,y=log10(no2)))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
#a <- a + geom_line()
a <- a + theme_few()
a <- a + labs(x = "", y = expression(paste("log"[10],"(NO"[2], ")")))
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
#a <- a + ylim(0,0.23)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a
## plot significant
q <- ggplot(data = pt.50950217, aes(x= timestamp, y = no2))
q <- q + geom_point()
q <- q + geom_line()
q <- q + theme_few()
q <- q + labs(x = "", y = expression(paste("NO"[3], " (mg/l)")))
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.no2.217$tau,3),"\nslope = ",
             round(sKenb.no2.217$B,5), "\np = ",
             round(sKenb.no2.217$sl,5))
q <- q + annotate(geom="text", x=2013, y=0.175, label=lb1,
                  color="red", hjust = 0)

r <- ggplot(data = pt.50900149, aes(x= timestamp, y = no2))
r <- r + geom_point()
r <- r + geom_line()
r <- r + theme_few()
r <- r + labs(x = "", y = expression(paste("NO"[3], " (mg/l)")))
r <- r + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50900149","\ntau = ",
             round(sKenb.no2.149$tau,3),"\nslope = ",
             round(sKenb.no2.149$B,5), "\np = ",
             round(sKenb.no2.149$sl,5))
r <- r + annotate(geom="text", x=2013, y=0.175, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.NO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.NO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();

png(file = "output/figs/PT50950217.blseas.NO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off();

png(file = "output/figs/PT50900149.blseas.NO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(r)
dev.off();

rm(sKenb.no2.149,sKenb.no2.387,sKenb.no2.106,sKenb.no2.125,sKenb.no2.217,
   sKenb.no2.249,ssKenb.no2.149,ssKenb.no2.387,ssKenb.no2.106,
   ssKenb.no2.125,ssKenb.no2.217,ssKenb.no2.249)
rm(p,a,q,lb1,r)
################################################################
################################################################
#########
## NH3 ##
#########
## ID trends in nh3 WITHOUT Serial Correction
sKenb.nh3.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$nh3.fil.n, block=pt.50900149$month,rep = "m")
sKenb.nh3.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$nh3.fil.n, block=pt.50900387$month,rep = "m")##SIG
sKenb.nh3.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$nh3.fil.n, block=pt.50950106$month,rep = "m")
sKenb.nh3.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$nh3.fil.n, block=pt.50950125$month,rep = "m")
sKenb.nh3.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$nh3.fil.n, block=pt.50950217$month,rep = "m")##SIG
sKenb.nh3.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$nh3.fil.n, block=pt.50950249$month,rep = "m")##SIG!
## ID trends in nh3 WITH Serial Correction
ssKenb.nh3.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$nh3.fil.n, block=pt.50900149$month,rep = "m")
ssKenb.nh3.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$nh3.fil.n, block=pt.50900387$month,rep = "m")##SIG
ssKenb.nh3.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$nh3.fil.n, block=pt.50950106$month,rep = "m")
ssKenb.nh3.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$nh3.fil.n, block=pt.50950125$month,rep = "m")
ssKenb.nh3.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$nh3.fil.n, block=pt.50950217$month,rep = "m")##SIG
ssKenb.nh3.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$nh3.fil.n, block=pt.50950249$month,rep = "m")##SIG!
## ID trends in nh3 WITHOUT Serial Correction & salinity covariable
cvsKenb.nh3.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$nh3.fil.n, block=pt.50900149$month,rep = "m", cv = pt.50900149$sal)
cvsKenb.nh3.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$nh3.fil.n, block=pt.50900387$month,rep = "m", cv = pt.50900387$sal)##SIG
cvsKenb.nh3.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$nh3.fil.n, block=pt.50950106$month,rep = "m", cv = pt.50950106$sal)
cvsKenb.nh3.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$nh3.fil.n, block=pt.50950125$month,rep = "m", cv = pt.50950125$sal)
cvsKenb.nh3.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$nh3.fil.n, block=pt.50950217$month,rep = "m", cv = pt.50950217$sal)##SIG
cvsKenb.nh3.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$nh3.fil.n, block=pt.50950249$month,rep = "m", cv = pt.50950249$sal)##SIG!
## ID trends in nh3 WITH Serial Correction & salinity covariable
cvssKenb.nh3.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$nh3.fil.n, block=pt.50900149$month,rep = "m", cv = pt.50900149$sal)
cvssKenb.nh3.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$nh3.fil.n, block=pt.50900387$month,rep = "m", cv = pt.50900387$sal)##SIG
cvssKenb.nh3.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$nh3.fil.n, block=pt.50950106$month,rep = "m", cv = pt.50950106$sal)
cvssKenb.nh3.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$nh3.fil.n, block=pt.50950125$month,rep = "m", cv = pt.50950125$sal)
cvssKenb.nh3.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$nh3.fil.n, block=pt.50950217$month,rep = "m", cv = pt.50950217$sal)##SIG
cvssKenb.nh3.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$nh3.fil.n, block=pt.50950249$month,rep = "m", cv = pt.50950249$sal)##SIG!
## Plot all
p <- ggplot(data = mydat.trm, aes(x= date, y = nh3.fil.n, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = expression(paste("NH"[3], " (mg/l)")))

a <- ggplot(data=mydat.trm, aes(x=date,y=log10(nh3.fil.n)))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
#a <- a + geom_line()
a <- a + theme_few()
a <- a + labs(x = "", y = expression(paste("log"[10],"(NH"[3], ")")))
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
#a <- a + ylim(0,8)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a
## plot significant
q <- ggplot(data = pt.50900387, aes(x= timestamp, y = nh3.fil.n))
q <- q + geom_point()
q <- q + geom_line()
q <- q + theme_few()
q <- q + labs(x = "", y = expression(paste("NH"[3], " (mg/l)")))
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50900387","\ntau = ",
             round(sKenb.nh3.387$tau,3),"\nslope = ",
             round(sKenb.nh3.387$B,4), "\np = ",
             round(sKenb.nh3.387$sl,6))
q <- q + annotate(geom="text", x=2013, y=0.195, label=lb1,
                  color="red", hjust = 0)

## plot significant
r <- ggplot(data = pt.50950217, aes(x= timestamp, y = nh3.fil.n))
r <- r + geom_point()
r <- r + geom_line()
r <- r + theme_few()
r <- r + labs(x = "", y = expression(paste("NH"[3], " (mg/l)")))
r <- r + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.nh3.217$tau,3),"\nslope = ",
             round(sKenb.nh3.217$B,4), "\np = ",
             round(sKenb.nh3.217$sl,8))
r <- r + annotate(geom="text", x=2013, y=1.25, label=lb1,
                  color="red", hjust = 0)

## plot significant
s <- ggplot(data = pt.50950249, aes(x= timestamp, y = nh3.fil.n))
s <- s + geom_point()
s <- s + geom_line()
s <- s + theme_few()
s <- s + labs(x = "", y = expression(paste("NH"[3], " (mg/l)")))
s <- s + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950249","\ntau = ",
             round(sKenb.nh3.249$tau,3),"\nslope = ",
             round(sKenb.nh3.249$B,4), "\np = ",
             round(sKenb.nh3.249$sl,6))
s <- s + annotate(geom="text", x=2013, y=0.35, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.NH3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.NH3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();
png(file = "output/figs/PT50900387.blseas.NH3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off();
png(file = "output/figs/PT50950217.blseas.NH3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(r)
dev.off();
png(file = "output/figs/PT50950249.blseas.NH3.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(s)
dev.off();

rm(sKenb.nh3.149, sKenb.nh3.387,sKenb.nh3.106,sKenb.nh3.125,sKenb.nh3.217,
   sKenb.nh3.249,ssKenb.nh3.149,ssKenb.nh3.387,ssKenb.nh3.106,
   ssKenb.nh3.125,ssKenb.nh3.217,ssKenb.nh3.249)
rm(p,a,q,r,s,lb1)
################################################################

################################################################
#########
## PO4 ##
#########
## ID trends in po4 WITHOUT serial correction
sKenb.po4.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$po4,block=pt.50900149$month, rep = "m")
sKenb.po4.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$po4,block=pt.50900387$month, rep = "m")##SIG
sKenb.po4.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$po4,block=pt.50950106$month, rep = "m")
sKenb.po4.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$po4,block=pt.50950125$month, rep = "m")
sKenb.po4.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$po4,block=pt.50950217$month, rep = "m")##SIG
sKenb.po4.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$po4,block=pt.50950249$month, rep = "m")##SIG
## ID trends in po4 WITH serial correction
ssKenb.po4.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$po4,block=pt.50900149$month, rep = "m")
ssKenb.po4.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$po4,block=pt.50900387$month, rep = "m")##SIG
ssKenb.po4.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$po4,block=pt.50950106$month, rep = "m")
ssKenb.po4.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$po4,block=pt.50950125$month, rep = "m")
ssKenb.po4.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$po4,block=pt.50950217$month, rep = "m")##SIG
ssKenb.po4.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$po4,block=pt.50950249$month, rep = "m")
## ID trends in po4 WITHOUT serial correction & salinity covariable
cvsKenb.po4.149 <- rkt(correct=F,date=pt.50900149$timestamp, y=pt.50900149$po4,block=pt.50900149$month, rep = "m", cv=pt.50900149$sal)
cvsKenb.po4.387 <- rkt(correct=F,date=pt.50900387$timestamp, y=pt.50900387$po4,block=pt.50900387$month, rep = "m", cv=pt.50900387$sal)##SIG
cvsKenb.po4.106 <- rkt(correct=F,date=pt.50950106$timestamp, y=pt.50950106$po4,block=pt.50950106$month, rep = "m", cv=pt.50950106$sal)
cvsKenb.po4.125 <- rkt(correct=F,date=pt.50950125$timestamp, y=pt.50950125$po4,block=pt.50950125$month, rep = "m", cv=pt.50950125$sal)
cvsKenb.po4.217 <- rkt(correct=F,date=pt.50950217$timestamp, y=pt.50950217$po4,block=pt.50950217$month, rep = "m", cv=pt.50950217$sal)##SIG
cvsKenb.po4.249 <- rkt(correct=F,date=pt.50950249$timestamp, y=pt.50950249$po4,block=pt.50950249$month, rep = "m", cv=pt.50950249$sal)##SIG
## ID trends in po4 WITH serial correction & salinity covariable
cvssKenb.po4.149 <- rkt(correct=T,date=pt.50900149$timestamp, y=pt.50900149$po4,block=pt.50900149$month, rep = "m", cv=pt.50900149$sal)
cvssKenb.po4.387 <- rkt(correct=T,date=pt.50900387$timestamp, y=pt.50900387$po4,block=pt.50900387$month, rep = "m", cv=pt.50900387$sal)##SIG
cvssKenb.po4.106 <- rkt(correct=T,date=pt.50950106$timestamp, y=pt.50950106$po4,block=pt.50950106$month, rep = "m", cv=pt.50950106$sal)
cvssKenb.po4.125 <- rkt(correct=T,date=pt.50950125$timestamp, y=pt.50950125$po4,block=pt.50950125$month, rep = "m", cv=pt.50950125$sal)
cvssKenb.po4.217 <- rkt(correct=T,date=pt.50950217$timestamp, y=pt.50950217$po4,block=pt.50950217$month, rep = "m", cv=pt.50950217$sal)##SIG
cvssKenb.po4.249 <- rkt(correct=T,date=pt.50950249$timestamp, y=pt.50950249$po4,block=pt.50950249$month, rep = "m", cv=pt.50950249$sal)
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = po4, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
p <- p + theme_few()
p <- p + labs(x = "", y = expression(paste("PO"[4], " (mg/l)")))

a <- ggplot(data=mydat.trm, aes(x=date,y=po4))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
a <- a + theme_few()
a <- a + labs(x = "", y = expression(paste("PO"[4], " (mg/l)")))
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
a <- a + ylim(0,0.4)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a
## plot significant
r <- ggplot(data = pt.50900387, aes(x= timestamp, y = po4))
r <- r + geom_point()
r <- r + geom_line()
r <- r + theme_few()
r <- r + labs(x = "", y = expression(paste("PO"[4], " (mg/l)")))
r <- r + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50900387","\ntau = ",
             round(sKenb.po4.387$tau,3),"\nslope = ",
             round(sKenb.po4.387$B,4), "\np = ",
             round(sKenb.po4.387$sl,11))
r <- r + annotate(geom="text", x=2013, y=0.15, label=lb1,
                  color="red", hjust = 0)

s <- ggplot(data = pt.50950217, aes(x= timestamp, y = po4))
s <- s + geom_point()
s <- s + geom_line()
s <- s + theme_few()
s <- s + labs(x = "", y = expression(paste("PO"[4], " (mg/l)")))
s <- s + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.po4.217$tau,3),"\nslope = ",
             round(sKenb.po4.217$B,4), "\np = ",
             round(sKenb.po4.217$sl,8))
s <- s + annotate(geom="text", x=2013, y=0.3, label=lb1,
                  color="red", hjust = 0)

t <- ggplot(data = pt.50950249, aes(x= timestamp, y = po4))
t <- t + geom_point()
t <- t + geom_line()
t <- t + theme_few()
t <- t + labs(x = "", y = expression(paste("PO"[4], " (mg/l)")))
t <- t + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950249","\ntau = ",
             round(sKenb.po4.249$tau,3),"\nslope = ",
             round(sKenb.po4.249$B,4), "\np = ",
             round(sKenb.po4.249$sl,6))
t <- t + annotate(geom="text", x=2013, y=0.175, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.PO4.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.PO4.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();
png(file = "output/figs/PT50900387.blseas.PO4.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(r)
dev.off();
png(file = "output/figs/PT50950217.blseas.PO4.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(s)
dev.off();
png(file = "output/figs/PT50950249.blseas.PO4.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(t)
dev.off();

rm(sKenb.po4.149,sKenb.po4.387,sKenb.po4.106,sKenb.po4.125,sKenb.po4.217,
   sKenb.po4.249,ssKenb.po4.149,ssKenb.po4.387,ssKenb.po4.106,
   ssKenb.po4.125,ssKenb.po4.217,ssKenb.po4.249)
rm(p,q,r,s,t, lb1)
################################################################
################################################################
#############
##   TON   ##
#############
## ID trends in n.oxid WITHOUT serial correlation
sKenb.noxid.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$n.oxid,block=pt.50900149$month,rep = "m")
sKenb.noxid.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$n.oxid,block=pt.50900387$month,rep = "m")
sKenb.noxid.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$n.oxid,block=pt.50950106$month,rep = "m")
sKenb.noxid.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$n.oxid,block=pt.50950125$month,rep = "m")
sKenb.noxid.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$n.oxid,block=pt.50950217$month,rep = "m")##SIG
sKenb.noxid.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$n.oxid,block=pt.50950249$month,rep = "m")
## ID trends in n.oxid WITH serial correlation
ssKenb.noxid.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$n.oxid,block=pt.50900149$month,rep = "m")
ssKenb.noxid.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$n.oxid,block=pt.50900387$month,rep = "m")
ssKenb.noxid.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$n.oxid,block=pt.50950106$month,rep = "m")
ssKenb.noxid.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$n.oxid,block=pt.50950125$month,rep = "m")
ssKenb.noxid.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$n.oxid,block=pt.50950217$month,rep = "m")##SIG
ssKenb.noxid.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$n.oxid,block=pt.50950249$month,rep = "m")
## ID trends in n.oxid WITHOUT serial correlation & salinity covariable
cvsKenb.noxid.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$n.oxid,block=pt.50900149$month,rep = "m", cv=pt.50900149$sal)
cvsKenb.noxid.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$n.oxid,block=pt.50900387$month,rep = "m", cv=pt.50900387$sal)
cvsKenb.noxid.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$n.oxid,block=pt.50950106$month,rep = "m", cv=pt.50950106$sal)
cvsKenb.noxid.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$n.oxid,block=pt.50950125$month,rep = "m", cv=pt.50950125$sal)
cvsKenb.noxid.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$n.oxid,block=pt.50950217$month,rep = "m", cv=pt.50950217$sal)##SIG
cvsKenb.noxid.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$n.oxid,block=pt.50950249$month,rep = "m", cv=pt.50950249$sal)
## ID trends in n.oxid WITH serial correlation & salinity covariable
cvssKenb.noxid.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$n.oxid,block=pt.50900149$month,rep = "m", cv=pt.50900149$sal)
cvssKenb.noxid.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$n.oxid,block=pt.50900387$month,rep = "m", cv=pt.50900387$sal)
cvssKenb.noxid.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$n.oxid,block=pt.50950106$month,rep = "m", cv=pt.50950106$sal)
cvssKenb.noxid.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$n.oxid,block=pt.50950125$month,rep = "m", cv=pt.50950125$sal)
cvssKenb.noxid.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$n.oxid,block=pt.50950217$month,rep = "m", cv=pt.50950217$sal)##SIG
cvssKenb.noxid.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$n.oxid,block=pt.50950249$month,rep = "m", cv=pt.50950249$sal)
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = n.oxid, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = "TON", " (mg/l)")

a <- ggplot(data=mydat.trm, aes(x=date,y=n.oxid))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
a <- a + theme_few()
a <- a + labs(x = "", y = "TON", " (mg/l)")
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
a <- a + ylim(0,8)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a
## plot significant
q <- ggplot(data = pt.50950217, aes(x= timestamp, y = n.oxid))
q <- q + geom_point()
q <- q + geom_line()
#q <- q + ylim(0,8)
q <- q + theme_few()
q <- q + labs(x = "", y = "TON", " (mg/l)")
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.noxid.217$tau,3),"\nslope = ",
             round(sKenb.noxid.217$B,4), "\np = ",
             round(sKenb.noxid.217$sl,6))
q <- q + annotate(geom="text", x=2013, y=2.5, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.Nox.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.TON.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();
png(file = "output/figs/PT50950217.blseas.Nox.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off();
rm(sKenb.noxid.149,sKenb.noxid.387,sKenb.noxid.106,sKenb.noxid.125,sKenb.noxid.217,
   sKenb.noxid.249,ssKenb.noxid.149,ssKenb.noxid.387,ssKenb.noxid.106,
   ssKenb.noxid.125,ssKenb.noxid.217,ssKenb.noxid.249)
rm(p,a,q,lb1)
################################################################
################################################################
############
## SiO2 ##
############
## ID trends in si02 WITHOUT serial correlation
sKenb.sio2.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$sio2,block=pt.50900149$month,rep = "m")##SIG
sKenb.sio2.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$sio2,block=pt.50900387$month,rep = "m")
sKenb.sio2.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$sio2,block=pt.50950106$month,rep = "m")##SIG
sKenb.sio2.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$sio2,block=pt.50950125$month,rep = "m")##SIG
sKenb.sio2.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$sio2,block=pt.50950217$month,rep = "m")
sKenb.sio2.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$sio2,block=pt.50950249$month,rep = "m")
## ID trends in si02 WITH serial correlation
ssKenb.sio2.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$sio2,block=pt.50900149$month,rep = "m")##SIG
ssKenb.sio2.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$sio2,block=pt.50900387$month,rep = "m")
ssKenb.sio2.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$sio2,block=pt.50950106$month,rep = "m")
ssKenb.sio2.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$sio2,block=pt.50950125$month,rep = "m")
ssKenb.sio2.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$sio2,block=pt.50950217$month,rep = "m")
ssKenb.sio2.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$sio2,block=pt.50950249$month,rep = "m")
## ID trends in si02 WITHOUT serial correlation & salinity covariable
cvsKenb.sio2.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$sio2,block=pt.50900149$month,rep = "m",cv = pt.50900149$sal)##SIG
cvsKenb.sio2.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$sio2,block=pt.50900387$month,rep = "m",cv = pt.50900387$sal)
cvsKenb.sio2.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$sio2,block=pt.50950106$month,rep = "m",cv = pt.50950106$sal)##SIG
cvsKenb.sio2.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$sio2,block=pt.50950125$month,rep = "m",cv = pt.50950125$sal)##SIG
cvsKenb.sio2.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$sio2,block=pt.50950217$month,rep = "m",cv = pt.50950217$sal)
cvsKenb.sio2.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$sio2,block=pt.50950249$month,rep = "m",cv = pt.50950249$sal)
## ID trends in si02 WITH serial correlation & salinity covariable
cvssKenb.sio2.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$sio2,block=pt.50900149$month,rep = "m",,cv = pt.50900149$sal)##SIG
cvssKenb.sio2.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$sio2,block=pt.50900387$month,rep = "m",,cv = pt.50900387$sal)
cvssKenb.sio2.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$sio2,block=pt.50950106$month,rep = "m",,cv = pt.50950106$sal)
cvssKenb.sio2.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$sio2,block=pt.50950125$month,rep = "m",,cv = pt.50950125$sal)
cvssKenb.sio2.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$sio2,block=pt.50950217$month,rep = "m",,cv = pt.50950217$sal)
cvssKenb.sio2.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$sio2,block=pt.50950249$month,rep = "m",,cv = pt.50950249$sal)
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = sio2, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = expression(paste("SiO"[2], " (mg/l)")))

a <- ggplot(data=mydat.trm, aes(x=date,y=sio2))
a <- a + geom_point(pch = 21,colour=1,fill="darkgrey")
a <- a + theme_few()
a <- a + labs(x = "", y = expression(paste("SiO"[2], " (mg/l)")))
a <- a + geom_smooth(method = "loess", colour = "red", lwd = 1.25)
a <- a + ylim(0,8)
a <- a + facet_wrap(~ptcode, ncol=3)
a <- a + theme(legend.position = "none", strip.text.x = element_text(size=15))
a

## plot significant
q <- ggplot(data = pt.50900149, aes(x= timestamp, y = sio2))
q <- q + geom_point()
q <- q + geom_line()
q <- q + theme_few()
q <- q + labs(x = "", y = expression(paste("SiO"[2], " (mg/l)")))
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50900149","\ntau = ",
             round(sKenb.sio2.149$tau,3),"\nslope = ",
             round(sKenb.sio2.149$B,4), "\np = ",
             round(sKenb.sio2.149$sl,4))
q <- q + annotate(geom="text", x=2013.5, y=3, label=lb1,
                  color="red", hjust = 0)

s <- ggplot(data = pt.50950106, aes(x= timestamp, y = sio2))
s <- s + geom_point()
s <- s + geom_line()
s <- s + theme_few()
s <- s + labs(x = "", y = expression(paste("SiO"[2], " (mg/l)")))
s <- s + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950106","\ntau = ",
             round(sKenb.sio2.106$tau,3),"\nslope = ",
             round(sKenb.sio2.106$B,4), "\np = ",
             round(sKenb.sio2.106$sl,4))
s <- s + annotate(geom="text", x=2005, y=1.7, label=lb1,
                  color="red", hjust = 0)

t <- ggplot(data = pt.50950125, aes(x= timestamp, y = sio2))
t <- t + geom_point()
t <- t + geom_line()
t <- t + theme_few()
t <- t + labs(x = "", y = expression(paste("SiO"[2], " (mg/l)")))
t <- t + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950125","\ntau = ",
             round(sKenb.sio2.125$tau,3),"\nslope = ",
             round(sKenb.sio2.125$B,4), "\np = ",
             round(sKenb.sio2.125$sl,4))
t <- t + annotate(geom="text", x=1997, y=2, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.SiO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/pt.all.facet.SiO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(a)
dev.off();
png(file = "output/figs/PT50900149.blseas.SiO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off();
png(file = "output/figs/PT50950106.blseas.SiO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(s)
dev.off();
png(file = "output/figs/PT50950125.blseas.SiO2.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(t)
dev.off();

rm(sKenb.sio2.149,sKenb.sio2.387,sKenb.sio2.106,sKenb.sio2.125,sKenb.sio2.217,
   sKenb.sio2.249,ssKenb.sio2.149,ssKenb.sio2.387,ssKenb.sio2.106,
   ssKenb.sio2.125,ssKenb.sio2.217,ssKenb.sio2.249)
rm(p,q,s,t,lb1)
################################################################
################################################################
############
## Temp   ##
############
## ID trends in water temperature WITHOUT serial correction
sKenb.temp.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$wat.temp,block=pt.50900149$month,rep = "m")
sKenb.temp.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$wat.temp,block=pt.50900387$month,rep = "m")
sKenb.temp.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$wat.temp,block=pt.50950106$month,rep = "m")
sKenb.temp.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$wat.temp,block=pt.50950125$month,rep = "m")
sKenb.temp.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$wat.temp,block=pt.50950217$month,rep = "m")
sKenb.temp.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$wat.temp,block=pt.50950249$month,rep = "m")
## ID trends in water temperature WITH serial correction
ssKenb.temp.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$wat.temp,block=pt.50900149$month,rep = "m")
ssKenb.temp.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$wat.temp,block=pt.50900387$month,rep = "m")
ssKenb.temp.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$wat.temp,block=pt.50950106$month,rep = "m")
ssKenb.temp.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$wat.temp,block=pt.50950125$month,rep = "m")
ssKenb.temp.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$wat.temp,block=pt.50950217$month,rep = "m")
ssKenb.temp.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$wat.temp,block=pt.50950249$month,rep = "m")
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = wat.temp, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
p <- p + theme_few()
p <- p + labs(x = "", y = "Temperature (°C)")

png(file = "output/figs/pt.all.temp.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();

rm(sKenb.temp.149,sKenb.temp.387,sKenb.temp.106,sKenb.temp.125,
   sKenb.temp.217,sKenb.temp.249,ssKenb.temp.149,ssKenb.temp.387,
   ssKenb.temp.106,ssKenb.temp.125,ssKenb.temp.217,ssKenb.temp.249)
rm(p)
################################################################
################################################################
############
## Salinity ##
############
## ID trends in salinity WITHOUT Serial correction
sKenb.sal.149 <- rkt(correct=F,date=pt.50900149$timestamp,y=pt.50900149$sal,block=pt.50900149$month,rep = "m")
sKenb.sal.387 <- rkt(correct=F,date=pt.50900387$timestamp,y=pt.50900387$sal,block=pt.50900387$month,rep = "m")##SIG
sKenb.sal.106 <- rkt(correct=F,date=pt.50950106$timestamp,y=pt.50950106$sal,block=pt.50950106$month,rep = "m")
sKenb.sal.125 <- rkt(correct=F,date=pt.50950125$timestamp,y=pt.50950125$sal,block=pt.50950125$month,rep = "m")##SIG
sKenb.sal.217 <- rkt(correct=F,date=pt.50950217$timestamp,y=pt.50950217$sal,block=pt.50950217$month,rep = "m")##SIG
sKenb.sal.249 <- rkt(correct=F,date=pt.50950249$timestamp,y=pt.50950249$sal,block=pt.50950249$month,rep = "m")##SIG
## ID trends in salinity WITH Serial correction
ssKenb.sal.149 <- rkt(correct=T,date=pt.50900149$timestamp,y=pt.50900149$sal,block=pt.50900149$month,rep = "m")
ssKenb.sal.387 <- rkt(correct=T,date=pt.50900387$timestamp,y=pt.50900387$sal,block=pt.50900387$month,rep = "m")##SIG
ssKenb.sal.106 <- rkt(correct=T,date=pt.50950106$timestamp,y=pt.50950106$sal,block=pt.50950106$month,rep = "m")
ssKenb.sal.125 <- rkt(correct=T,date=pt.50950125$timestamp,y=pt.50950125$sal,block=pt.50950125$month,rep = "m")
ssKenb.sal.217 <- rkt(correct=T,date=pt.50950217$timestamp,y=pt.50950217$sal,block=pt.50950217$month,rep = "m")##SIG
ssKenb.sal.249 <- rkt(correct=T,date=pt.50950249$timestamp,y=pt.50950249$sal,block=pt.50950249$month,rep = "m")
## Plot
p <- ggplot(data = mydat.trm, aes(x= date, y = sal, group = ptcode,
                                  colour = ptcode))
p <- p + geom_point()
p <- p + geom_line()
#p <- p + ylim(0,8)
p <- p + theme_few()
p <- p + labs(x = "", y = "Salinity")

## plot significant
q <- ggplot(data = pt.50900387, aes(x= timestamp, y = sal))
q <- q + geom_point()
q <- q + geom_line()
q <- q + theme_few()
q <- q + labs(x = "", y = "Salinity")
q <- q + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50900387","\ntau = ",
             round(sKenb.sal.387$tau,3),"\nslope = ",
             round(sKenb.sal.387$B,4), "\np = ",
             round(sKenb.sal.387$sl,4))
q <- q + annotate(geom="text", x=2013.5, y=3, label=lb1,
                  color="red", hjust = 0)

s <- ggplot(data = pt.50950125, aes(x= timestamp, y = sal))
s <- s + geom_point()
s <- s + geom_line()
s <- s + theme_few()
s <- s + labs(x = "", y = "Salinity")
s <- s + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950125","\ntau = ",
             round(sKenb.sal.125$tau,3),"\nslope = ",
             round(sKenb.sal.125$B,4), "\np = ",
             round(sKenb.sal.125$sl,4))
s <- s + annotate(geom="text", x=1995, y=25, label=lb1,
                  color="red", hjust = 0)

t <- ggplot(data = pt.50950217, aes(x= timestamp, y = sal))
t <- t + geom_point()
t <- t + geom_line()
t <- t + theme_few()
t <- t + labs(x = "", y = "Salinity")
t <- t + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950217","\ntau = ",
             round(sKenb.sal.217$tau,3),"\nslope = ",
             round(sKenb.sal.217$B,4), "\np = ",
             round(sKenb.sal.217$sl,4))
t <- t + annotate(geom="text", x=1999, y=20, label=lb1,
                  color="red", hjust = 0)

u <- ggplot(data = pt.50950249, aes(x= timestamp, y = sal))
u <- u + geom_point()
u <- u + geom_line()
u <- u + theme_few()
u <- u + labs(x = "", y = "Salinity")
u <- u + geom_smooth(method = "loess", se = FALSE, lwd = 1.25)
lb1 <- paste("PT50950249","\ntau = ",
             round(sKenb.sal.249$tau,3),"\nslope = ",
             round(sKenb.sal.249$B,4), "\np = ",
             round(sKenb.sal.249$sl,4))
u <- u + annotate(geom="text", x=1997, y=13, label=lb1,
                  color="red", hjust = 0)

png(file = "output/figs/pt.all.sal.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(p)
dev.off();
png(file = "output/figs/PT50900387.blseas.sal.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(q)
dev.off();
png(file = "output/figs/PT50950125.blseas.sal.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(s)
dev.off();
png(file = "output/figs/PT50950217.blseas.sal.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(t)
dev.off();
png(file = "output/figs/PT50950249.blseas.sal.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(u)
dev.off();

rm(sKenb.sal.149,sKenb.sal.387,sKenb.sal.106,sKenb.sal.125,
   sKenb.sal.217,sKenb.sal.249,ssKenb.sal.149,ssKenb.sal.387,
   ssKenb.sal.106,ssKenb.sal.125,ssKenb.sal.217,ssKenb.sal.249)
rm(p,a,q,s,t,u,lb1)
################################################################

### tidy up
rm(pt.50900149,pt.50900387,pt.50950106,pt.50950125,pt.50950217,pt.50950249,
   mydat.trm, ppi)
detach("package:rkt", unload=TRUE)
detach("package:ggthemes", unload=TRUE)
detach("package:ggplot2", unload=TRUE)

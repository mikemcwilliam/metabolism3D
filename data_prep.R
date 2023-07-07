 
library("reshape2")
library("ggplot2")
library("cowplot")
library("png")
library("grid")

#################################### Sensor data

load_data <- function(path, cols) { 
  files <- dir(path=path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(x){cbind(read.csv(x, skip=1, col.names=c(1:ncol(read.csv(x, skip=1))))[,cols], filename=paste(x))})
  do.call(rbind, tables)
  }

O2 <- load_data("data/O2", c(2:4))
names(O2)[1]<-"Time"
names(O2)[2]<-"O2mgL"
names(O2)[3]<-"TempC"
O2$Day <- substr(O2$Time, 1,8)
O2$Day <- as.POSIXct(O2$Day, format="%m/%d/%y", tz="HST") 
O2$Time<-as.POSIXct(O2$Time, format="%m/%d/%y %I:%M:%S %p", tz="HST") 
O2$O2mgL[O2$O2mgL<0]<-NA#subset(O2, O2mgL >0)
O2$TempC[O2$TempC<0]<-NA
head(O2)

PAR <- load_data("data/light", c(2:4))
names(PAR)[1]<-"Time"
names(PAR)[2]<-"TempC"
names(PAR)[3]<-"PAR"
PAR$Day <- substr(PAR$Time, 1,8)
PAR$Day <- as.POSIXct(PAR$Day, format="%m/%d/%y", tz="HST") 
PAR$Time<-as.POSIXct(PAR$Time, format="%m/%d/%y %I:%M:%S %p", tz="HST")
head(PAR)

#################################### Unit mistakes in logger
# Lumens per ft2 to lux
# Temperature F to C

cols_data <- function(path) { 
  files <- dir(path=path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(x){cbind(c3 = substr(colnames(read.csv(x, skip=1)[3]), 1, 15), c4 = substr(colnames(read.csv(x, skip=1)[4]), 1, 15),filename=paste(x))})
  do.call(rbind, tables)
  }

cols_light <- data.frame(cols_data("data/light"))
cols_O2 <- data.frame(cols_data("data/O2"))

PAR$TempC <- ifelse(PAR$filename %in% cols_light$filename[cols_light$c3=="Temp...F..LGR.S"], (PAR$TempC-32)*(5/9), PAR$TempC )

PAR$PAR <- ifelse(PAR$filename %in% cols_light$filename[cols_light$c4=="Intensity..lum."], PAR$PAR*10.7639104167, PAR$PAR )

O2$TempC <- ifelse(O2$filename %in% cols_O2$filename[cols_O2$c4=="Temp...F..LGR.S"], (O2$TempC-32)*(5/9), O2$TempC )


#################################### Timings

times <- read.csv("data/chamber_data.csv")
head(times)
times[,c("Pstart", "Pend", "Rend","Rstart")]<-apply(times[,c("Pstart", "Pend", "Rend","Rstart")], 2, function(x){paste(times$day, x)})
times$Pstart<-as.POSIXct(times$Pstart, format="%m/%d/%Y %H:%M", tz="HST") 
times$Pend<-as.POSIXct(times$Pend, format="%m/%d/%Y %H:%M", tz="HST") 
times$Rstart<-as.POSIXct(times$Rstart, format="%m/%d/%Y %H:%M", tz="HST") 
times$Rend<-as.POSIXct(times$Rend, format="%m/%d/%Y %H:%M", tz="HST") 

dat <- NULL
par <- NULL
for(site in unique(times$site)){
#site <- "site001"
m <- 5
t <- times[times$site==site,] 
new1 <- cbind(site, subset(O2, Time > t$Pstart-m*60 & Time < t$Pend+m*60))
new1$type <- ifelse(new1$Time > t$Pstart & new1$Time < t$Pend, "PS", NA)
new2 <- cbind(site, subset(O2, Time > t$Rstart-m*60 & Time < t$Rend+m*60))
new2$type <- ifelse(new2$Time > t$Rstart & new2$Time < t$Rend, "R", NA)
dat <- rbind(dat, rbind(new1, new2))
new3 <- cbind(site, subset(PAR, Time > t$Pstart-m*60 & Time < t$Pend+m*60))
new3$type <- ifelse(new3$Time > t$Pstart & new3$Time < t$Pend, "PS", NA)
new4 <- cbind(site, subset(PAR, Time > t$Rstart-m*60 & Time < t$Rend+m*60))
new4$type <- ifelse(new4$Time > t$Rstart & new4$Time < t$Rend, "R", NA)
par <- rbind(par, rbind(new3, new4))
}
head(dat)
head(par)


####################################  SLOPES 

params <- NULL
p.lines <- NULL
r.lines <- NULL
# find slopes
for(site in unique(times$site)){
	#site <- "site025c"
p.sub <- dat[dat$type=="PS" & dat$site==site & !is.na(dat$type),]
p.sub$mins<-as.numeric(seq(1:nrow(p.sub)))
p.mod<-lm(O2mgL~mins, p.sub)
p.se <- coef(summary(p.mod))["mins","Std. Error"]*1000
p.slp<-round(coef(p.mod)["mins"]*1000,1)
p.rsq<-round(summary(p.mod)$r.squared,3)
p.slp # production in ug/L/min ?
p.new<-data.frame(mins=seq(1:nrow(p.sub)), Time=p.sub$Time)
p.new$pred<-predict(p.mod, p.new, type="response")
r.sub <- dat[dat$type=="R" & dat$site==site & !is.na(dat$type),]
r.sub$mins<-as.numeric(seq(1:nrow(r.sub)))
r.mod<-lm(O2mgL~mins, r.sub)
r.se <- coef(summary(r.mod))["mins","Std. Error"]*1000
r.slp<-round(coef(r.mod)["mins"]*1000,1)
r.rsq<-round(summary(r.mod)$r.squared,3)
r.slp # production in ug/L/min ?
r.new<-data.frame(mins=seq(1:nrow(r.sub)), Time=r.sub$Time)
r.new$pred<-predict(r.mod, r.new, type="response")
tempO2_PS <- mean(p.sub$TempC)
tempO2_R <- mean(r.sub$TempC)
par.temp.p <- par[par$type=="PS" & par$site==site & !is.na(par$type),]
parPS <- mean(par.temp.p$PAR)
par.temp.r <- par[par$type=="R" & par$site==site & !is.na(par$type),]
parR <- mean(par.temp.r$PAR)
tempHOBO_PS <- mean(par.temp.p$TempC)
tempHOBO_R <- mean(par.temp.r$TempC)
successful <- times$successful[times$site==site]
params <- rbind(params, data.frame(site, p.slp, p.rsq, p.se, r.slp, r.rsq,  r.se, tempO2_PS, tempO2_R, parPS, parR, tempHOBO_PS, tempHOBO_R, successful))
p.lines <- rbind(p.lines, cbind(site=site, p.new))
r.lines <- rbind(r.lines, cbind(site=site, r.new))
}


head(params)

plot_grid(ggplot()+geom_bar(data=params, aes(y=site, x=p.slp), stat="identity", fill="slategrey")+
geom_bar(data=params, aes(y=site, x=r.slp), stat="identity", fill="grey"), 
ggplot()+geom_point(data=params, aes(y=site, x=tempHOBO_PS)))


#####################################
#################################### Plots

#for(site in unique(times$site)){
site <- "site025c"
df.time <- times[times$site==site,]
par.df <- par[par$site==site,]
o2.df <- dat[dat$site==site,]
day <- as.POSIXct(times$day[times$site==site], format="%m/%d/%Y", tz="HST") 
day.o2 <- O2[O2$Day == day ,]
day.par <- PAR[PAR$Day == day ,]
pars <- params[params$site==site,]
p.line <- p.lines[p.lines$site==site,]
r.line <- r.lines[r.lines$site==site,]
img.path <- paste("sitepics/", site, ".png", sep="")

pO2day<-ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.o2, aes(Time, O2mgL))+
geom_point()+
ggtitle("Oxygen (day)")+theme(plot.title=element_text(size=8))

pPARday<-ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.par, aes(Time, PAR))+
geom_point()+
ggtitle("Light in lux (day)")+theme(plot.title=element_text(size=8))

pTEMPday <- ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.o2, aes(Time, TempC))+
geom_line(data=day.par, aes(Time, TempC), col="slategrey")+
#ylim(c(22,30))+
ggtitle("Temperature \nHOBOlogger - grey | O2probe - black")+theme(plot.title=element_text(size=8))

pPARsite<-ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=par.df, aes(Time, PAR), col="grey")+
geom_point(data=par.df[!is.na(par.df$type),], aes(Time, PAR))+
geom_segment(data=pars, aes(y=parPS, yend=parPS, x=df.time$Pstart, xend=df.time$Pend), col="red")+
geom_segment(data=pars, aes(y=parR, yend=parR, x=df.time$Rstart, xend=df.time$Rend), col="red")+
ggtitle("Light in lux (site)")+theme(plot.title=element_text(size=8))

pO2site<-ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=o2.df, aes(Time, O2mgL), col="grey")+
geom_point(data=o2.df[!is.na(o2.df$type),], aes(Time, O2mgL))+
geom_line(data=p.line, aes(Time, pred), col="red")+
geom_line(data=r.line, aes(Time, pred), col="red")+
ggtitle("Oxygen (site)")+theme(plot.title=element_text(size=8))

pTEMPsite <- ggplot()+
geom_rect(data=df.time, aes(xmin=Pstart,xmax=Pend, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=Rstart,xmax=Rend, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=o2.df, aes(Time, TempC), size=0.1)+
geom_point(data=par.df, aes(Time, TempC), size=0.1)+
geom_point(data=o2.df[!is.na(o2.df$type),], aes(Time, TempC))+
geom_point(data=par.df[!is.na(par.df$type),], aes(Time, TempC), col="slategrey")+
geom_segment(data=pars, aes(y=tempO2_PS, yend=tempO2_PS, x=df.time$Pstart, xend=df.time$Pend), col="red")+
geom_segment(data=pars, aes(y=tempO2_R, yend=tempO2_R, x=df.time$Rstart, xend=df.time$Rend), col="red")+
geom_segment(data=pars, aes(y=tempHOBO_PS, yend=tempHOBO_PS, x=df.time$Pstart, xend=df.time$Pend), col="red")+
geom_segment(data=pars, aes(y=tempHOBO_R, yend=tempHOBO_R, x=df.time$Rstart, xend=df.time$Rend), col="red")+
#ylim(c(22,30))+
ggtitle("Temperature \nHOBOlogger - grey | O2probe - black")+theme(plot.title=element_text(size=8))

title <- ggdraw() + draw_label(site, fontface='bold')

#img<-readPNG(img.path)
#img<-rasterGrob(img, interpolate=TRUE)
#imgplot <- ggplot()+annotation_custom(img, xmin=0, xmax=100, ymin=0, ymax=100)+xlim(c(1,100))+ylim(c(1,100))+theme_void()

plot <- plot_grid(title,plot_grid(plot_grid(pO2day, pPARday, pTEMPday, ncol=1),plot_grid(pO2site,pPARsite, pTEMPsite, ncol=1)), ncol=1, rel_heights=c(0.1, 1))
plot

#ggsave(paste("data/plots/", site, ".png", sep=""), plot, width=11, height=15, units="cm", dpi = 300)
#}



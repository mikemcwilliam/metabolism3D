 
library("reshape2")
library("ggplot2")
library("cowplot")
library("png")
library("grid")

#################################### Sensor data (lizard)

load_data <- function(path, cols) { 
  files <- dir(path=path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(x){cbind(read.csv(x, skip=1, col.names=c(1:ncol(read.csv(x, skip=1))))[,cols], filename=paste(x))})
  do.call(rbind, tables)
  }


O2 <- load_data("data/probe_data/O2_gbr", c(2:4))
O2_files <- read.csv("data/probe_data/O2_files_gbr.csv")

names(O2)[1]<-"Time"
names(O2)[2]<-"O2mgL"
names(O2)[3]<-"TempC"
O2$Day <- substr(O2$Time, 1,8)
O2$Day <- as.POSIXct(O2$Day, format="%m/%d/%y") 
O2$Time<-as.POSIXct(O2$Time, format="%m/%d/%y %I:%M:%S %p") - 60*60 # accounts for NSW time on laptop
O2$O2mgL[O2$O2mgL<0]<-NA #subset(O2, O2mgL >0)
O2$TempC[O2$TempC<0]<-NA
O2$filename <- gsub("data/probe_data/O2_gbr/","", O2$filename)
O2$probe <- O2_files$probe[match(O2$filename, O2_files$filename)]
O2$ampm <- O2_files$ampm[match(O2$filename, O2_files$filename)]
O2$site <- O2_files$site[match(O2$filename, O2_files$filename)]
head(O2)


PAR <- load_data("data/probe_data/light_gbr", c(2:4))
PARfiles <-  read.csv("data/probe_data/light_files_gbr.csv")
names(PAR)[1]<-"Time"
names(PAR)[2]<-"TempC"
names(PAR)[3]<-"PAR"
PAR$Day <- substr(PAR$Time, 1,8)
PAR$Day <- as.POSIXct(PAR$Day, format="%m/%d/%y") 
PAR$Time<-as.POSIXct(PAR$Time, format="%m/%d/%y %I:%M:%S %p") - 60*60
PAR$filename <- gsub("data/probe_data/light_gbr/","", PAR$filename)
PAR$probe <- PARfiles$probe[match(PAR$filename, PARfiles$filename)]
PAR$ampm <- PARfiles$ampm[match(PAR$filename, PARfiles$filename)]
PAR$site <- PARfiles$site[match(PAR$filename, PARfiles$filename)]
head(PAR)


#################################### Sensor data (hawaii)


O2hi <- load_data("data/probe_data/O2_hi", c(2:4))
O2_fileshi <- read.csv("data/probe_data/O2_files_hi.csv")

names(O2hi)[1]<-"Time"
names(O2hi)[2]<-"O2mgL"
names(O2hi)[3]<-"TempC"
O2hi$Day <- substr(O2hi$Time, 1,8)
O2hi$Day <- as.POSIXct(O2hi$Day, format="%m/%d/%y", tz="HST") 
O2hi$Time<-as.POSIXct(O2hi$Time, format="%m/%d/%y %I:%M:%S %p", tz="HST") 
O2hi$O2mgL[O2hi$O2mgL<0]<-NA
O2hi$TempC[O2hi$TempC<0]<-NA
O2hi$filename <- gsub("data/probe_data/O2_hi/","", O2hi$filename)
O2hi$probe <- "P0"
O2hi$ampm <- NA
O2hi$site <- O2_fileshi$site[match(O2hi$filename, O2_fileshi$filename)]
head(O2hi)


PARhi <- load_data("data/probe_data/light_hi", c(2:4))
PAR_fileshi <- read.csv("data/probe_data/light_files_hi.csv")

names(PARhi)[1]<-"Time"
names(PARhi)[2]<-"TempC"
names(PARhi)[3]<-"PAR"
PARhi$Day <- substr(PARhi$Time, 1,8)
PARhi$Day <- as.POSIXct(PARhi$Day, format="%m/%d/%y", tz="HST") 
PARhi$Time<-as.POSIXct(PARhi$Time, format="%m/%d/%y %I:%M:%S %p", tz="HST")
PARhi$filename <- gsub("data/probe_data/light_hi/","", PARhi$filename)
head(PARhi)
PARhi$probe <- PAR_fileshi$probe[match(PARhi$filename, PAR_fileshi$filename)]
PARhi$ampm <- NA
PARhi$site <- PAR_fileshi$site[match(PARhi$filename, PAR_fileshi$filename)]
head(PARhi)


#################################### Unit mistakes in logger
# Lumens per ft2 to lux
# Temperature F to C

cols_data <- function(path) { 
  files <- dir(path=path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(x){cbind(c3 = substr(colnames(read.csv(x, skip=1)[3]), 1, 15), c4 = substr(colnames(read.csv(x, skip=1)[4]), 1, 15),filename=paste(x))})
  do.call(rbind, tables)
  }

cols_light <- data.frame(cols_data("data/probe_data/light_hi"))
cols_light$filename <- gsub("data/probe_data/light_hi/","", cols_light$filename)
cols_O2 <- data.frame(cols_data("data/probe_data/O2_hi"))
cols_O2$filename <- gsub("data/probe_data/O2_hi/","", cols_O2$filename)

PARhi$TempC <- ifelse(PARhi$filename %in% cols_light$filename[cols_light$c3=="Temp...F..LGR.S"], (PARhi$TempC-32)*(5/9), PARhi$TempC )

PARhi$PAR <- ifelse(PARhi$filename %in% cols_light$filename[cols_light$c4=="Intensity..lum."], PARhi$PAR*10.7639104167, PARhi$PAR )

O2hi$TempC <- ifelse(O2hi$filename %in% cols_O2$filename[cols_O2$c4=="Temp...F..LGR.S"], (O2hi$TempC-32)*(5/9), O2hi$TempC )

#head(O2)
#O2$O2mgL[O2$Time > "2024-11-01 11:26:44" & O2$Time < "2024-11-01 11:38:44" & O2$site=="site36"] <- 
#O2$O2mgL[O2$Time > "2024-11-01 11:26:44" & O2$Time < "2024-11-01 11:38:44" & O2$site=="site36"] -0.07

#################################### Timings

times <- read.csv("data/probe_data/chambers_gbr.csv")
head(times)
#times[,c("Pstart", "Pend", "Rend","Rstart")]<-apply(times[,c("PS.start", "PS.end", "R.end","R.start")], 2, function(x){paste(times$day, x)})
times$PS.start<-as.POSIXct(paste(times$date,times$PS.start), format="%d/%m/%Y %H:%M") 
times$PS.end<-as.POSIXct(paste(times$date,times$PS.end), format="%d/%m/%Y %H:%M") 
times$R.start<-as.POSIXct(paste(times$date,times$R.start), format="%d/%m/%Y %H:%M") 
times$R.end<-as.POSIXct(paste(times$date,times$R.end), format="%d/%m/%Y %H:%M") 

#times <- times[!times$site %in% c(swaps, pumps),]

timehi <- read.csv("data/probe_data/chambers_hi.csv")
#timehi[,c("PS.start", "PS.end", "R.end","R.start")]<-apply(timehi[,c("PS.start", "PS.end", "R.end","R.start")], 2, function(x){paste(timehi$date, x)})
timehi$PS.start<-as.POSIXct(paste(timehi$date,timehi$PS.start), format="%m/%d/%Y %H:%M", tz="HST") 
timehi$PS.end<-as.POSIXct(paste(timehi$date,timehi$PS.end), format="%m/%d/%Y %H:%M", tz="HST") 
timehi$R.start<-as.POSIXct(paste(timehi$date,timehi$R.start), format="%m/%d/%Y %H:%M", tz="HST") 
timehi$R.end<-as.POSIXct(paste(timehi$date,timehi$R.end), format="%m/%d/%Y %H:%M", tz="HST") 

timehi <- timehi[timehi$successful %in% c('y'),]
times <- times[times$successful %in% c('y'),]

#################################### Remove faulty data

# site23 Probe4 - buried in sand? 
# site29 probe3 - buried in sand? 

O2 <- O2[!(O2$site=="site23" & O2$probe=="P4"),]
O2 <- O2[!(O2$site=="site29" & O2$probe=="P3"),]

# site10 probe3 - buried in sand? 

#################################### combine gbr/hi

head(times)
head(timehi)

cols <- c( "site", "date" , "successful","location" ,"description","dominant" , "chamber" ,       "oxy.probe.A" ,   "oxy.probe.B" ,   "light.probe.A", "light.probe.B", "PS.start"  , "PS.end" , "R.start" ,       "R.end" ,    "lat" ,  "long"  )

times <- rbind(cbind(times[,cols], region="GBR"), cbind(timehi[,cols], region="Hawaii"))

head(O2)
head(O2hi)
O2 <- rbind(cbind(O2, region="GBR"), cbind(O2hi, region="Hawaii"))

head(PAR)
head(PARhi)
PAR <- rbind(cbind(PAR, region="GBR"), cbind(PARhi, region="Hawaii"))

unique(O2$site)
unique(PAR$site)




#################################### crop

dat <- NULL
par <- NULL
for(site in unique(times$site)){
#site <- "site50_pump"
m <- 5
t <- times[times$site==site,] 
sub <- O2[O2$site==site,]
subpar <- PAR[PAR$site==site,]
new1 <-  subset(sub, Time > t$PS.start-m*60 & Time < t$PS.end+m*60)
new1$type <- ifelse(new1$Time > t$PS.start & new1$Time < t$PS.end, "PS", NA)
new2 <-  subset(sub, Time > t$R.start-m*60 & Time < t$R.end+m*60)
new2$type <- ifelse(new2$Time > t$R.start & new2$Time < t$R.end, "R", NA)
dat <- rbind(dat, rbind(new1, new2))
new3 <- subset(subpar, Time > t$PS.start-m*60 & Time < t$PS.end+m*60)
new3$type <- ifelse(new3$Time > t$PS.start & new3$Time < t$PS.end, "PS", NA)
new4 <- subset(subpar, Time > t$R.start-m*60 & Time < t$R.end+m*60)
new4$type <- ifelse(new4$Time > t$R.start & new4$Time < t$R.end, "R", NA)
par <- rbind(par, rbind(new3, new4))
}
head(dat)
head(par)
unique(dat$site)


####################################  SLOPES 

swaps <- c("site45_swap", "site46_swap", "site47_swap","site54_swap", "site55_swap", "site56_swap")
pumps <- c("site48_pump", "site49_pump", "site50_pump", "site52_pump", "site64_pump", "site66_pump", "site70_pump", "site68_pump") # no resp


params <- NULL
p.lines <- NULL
r.lines <- NULL
# find slopes
for(site in unique(times$site)){
	#site <- "site45_swap"
p.sub <- dat[dat$type=="PS" & dat$site==site  & !is.na(dat$type),]
r.sub <- dat[dat$type=="R" & dat$site==site & !is.na(dat$type),]
par.temp.p <- par[par$type=="PS" & par$site==site & !is.na(par$type),]
par.temp.r <- par[par$type=="R" & par$site==site & !is.na(par$type),]

for(probe in unique(c(p.sub$probe, r.sub$probe))){
#	probe <- "P3"
p.sub2 <- p.sub[p.sub$probe==probe,]
r.sub2 <- r.sub[r.sub$probe==probe,]

p.sub2$mins<-as.numeric(seq(1:nrow(p.sub2)))
p.mod<-lm(O2mgL~mins, p.sub2)
p.se <- coef(summary(p.mod))["mins","Std. Error"]*1000
p.slp<-round(coef(p.mod)["mins"]*1000,1)
p.rsq<-round(summary(p.mod)$r.squared,3)
p.slp # production in ug/L/min ?
p.new<-data.frame(mins=seq(1:nrow(p.sub2)), Time=p.sub2$Time)
p.new$pred<-predict(p.mod, p.new, type="response")
tempO2_PS <- mean(p.sub2$TempC)
parPS <- mean(par.temp.p$PAR)
tempHOBO_PS <- mean(par.temp.p$TempC)

if ( site %in% c(pumps, swaps)){
r.slp <- NA
r.rsq <- NA
r.se <- NA
tempO2_R <- NA
parR <- NA
tempHOBO_R <- NA	
	
}else{
r.sub2$mins<-as.numeric(seq(1:nrow(r.sub2)))
r.mod<-lm(O2mgL~mins, r.sub2)
r.se <- coef(summary(r.mod))["mins","Std. Error"]*1000
r.slp<-round(coef(r.mod)["mins"]*1000,1)
r.rsq<-round(summary(r.mod)$r.squared,3)
r.slp # production in ug/L/min ?
r.new<-data.frame(mins=seq(1:nrow(r.sub2)), Time=r.sub2$Time)
r.new$pred<-predict(r.mod, r.new, type="response")
tempO2_R <- mean(r.sub2$TempC)
parR <- mean(par.temp.r$PAR)
tempHOBO_R <- mean(par.temp.r$TempC)}

#successful <- times$successful[times$site==site]
params <- rbind(params, data.frame(site, region=unique(p.sub$region), probe, p.slp, p.rsq, p.se, r.slp, r.rsq,  r.se, tempO2_PS, tempO2_R, parPS, parR, tempHOBO_PS, tempHOBO_R))
p.lines <- rbind(p.lines, cbind(site=site, probe=probe, p.new))
r.lines <- rbind(r.lines, cbind(site=site, probe=probe,  r.new))
}}

head(times)
add_cols <- c("successful", "location", "description", "dominant", "chamber")
params[,add_cols] <- times[match(params$site, times$site), add_cols]
head(params)



#  write.csv(params, "data/probe_data/params.csv")

head(params)
plot_grid(
ggplot()+geom_bar(data=params, aes(y=site, x=r.slp, fill=probe), stat="identity", position = position_dodge2(width = 0.9, preserve = "single"))
,
ggplot()+geom_bar(data=params, aes(y=site, x=p.slp, fill=probe), stat="identity", position = position_dodge2(width = 0.9, preserve = "single"))
,
ggplot()+geom_point(data=params, aes(y=site, x=tempHOBO_PS)),ggplot()+geom_point(data=params, aes(y=site, x=parPS))+geom_point(data=params, aes(y=site, x=parR), col="blue"), nrow=1, rel_widths=c(1, 1, 0.7, 0.8))

#####################################
#################################### Plots

for(site in unique(times$site)){
	site <- "site21"
df.time <- times[times$site %in% site,]
par.df <- par[par$site %in% site,]
o2.df <- dat[dat$site %in% site,]


head(o2.df)
unique(o2.df$filename)


day <- as.POSIXct(times$date[times$site==site], format="%d/%m/%Y") 
day.o2 <- O2[O2$Day == day ,] 
day.par <- PAR[PAR$Day == day ,]
pars <- params[params$site==site,]
p.line <- p.lines[p.lines$site==site,]
r.line <- r.lines[r.lines$site==site,]
img.path <- paste("sitepics/", site, ".png", sep="")

head(par)

o2.df[o2.df$type=="R",]

ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=o2.df, aes(Time, O2mgL), col="grey")+
geom_point(data=o2.df[!is.na(o2.df$type),], aes(Time, O2mgL))+
geom_line(data=p.line, aes(Time, pred, group=probe), col="red")+
geom_line(data=r.line, aes(Time, pred, group=probe), col="red")+
ggtitle("Oxygen (site)")+theme(plot.title=element_text(size=8))


pO2day<-ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.o2, aes(Time, O2mgL, group=probe))+
geom_point()+
ggtitle("Oxygen (day)")+theme(plot.title=element_text(size=8))

pPARday<-ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.par, aes(Time, PAR, group=probe))+
geom_point()+
ggtitle("Light in lux (day)")+theme(plot.title=element_text(size=8))

pTEMPday <- ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_line(data=day.o2, aes(Time, TempC, group=probe))+
geom_line(data=day.par, aes(Time, TempC, group=probe), col="slategrey")+
#ylim(c(22,30))+
ggtitle("Temperature \nHOBOlogger - grey | O2probe - black")+theme(plot.title=element_text(size=8))

pPARsite<-ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=par.df, aes(Time, PAR), col="grey")+
geom_point(data=par.df[!is.na(par.df$type),], aes(Time, PAR))+
scale_y_sqrt()+
geom_segment(data=pars, aes(y=parPS, yend=parPS, x=df.time$PS.start, xend=df.time$PS.end), col="red")+
geom_segment(data=pars, aes(y=parR, yend=parR, x=df.time$R.start, xend=df.time$R.end), col="red")+
ggtitle("Light in lux (site)")+theme(plot.title=element_text(size=8))

pO2site<-ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=o2.df, aes(Time, O2mgL), col="grey")+
geom_point(data=o2.df[!is.na(o2.df$type),], aes(Time, O2mgL))+
geom_line(data=p.line, aes(Time, pred, group=probe), col="red")+
geom_line(data=r.line, aes(Time, pred, group=probe), col="red")+
ggtitle("Oxygen (site)")+theme(plot.title=element_text(size=8))
pO2site

pTEMPsite <- ggplot()+
geom_rect(data=df.time, aes(xmin=PS.start,xmax=PS.end, ymin=-Inf, ymax=Inf, group=site), fill="green", alpha=0.5)+
geom_rect(data=df.time, aes(xmin=R.start,xmax=R.end, ymin=-Inf, ymax=Inf, group=site), fill="blue", alpha=0.5)+
geom_point(data=o2.df, aes(Time, TempC), size=0.1)+
geom_point(data=par.df, aes(Time, TempC), size=0.1)+
geom_point(data=o2.df[!is.na(o2.df$type),], aes(Time, TempC))+
geom_point(data=par.df[!is.na(par.df$type),], aes(Time, TempC), col="slategrey")+
geom_segment(data=pars, aes(y=tempO2_PS, yend=tempO2_PS, x=df.time$PS.start, xend=df.time$PS.end), col="red")+
geom_segment(data=pars, aes(y=tempO2_R, yend=tempO2_R, x=df.time$R.start, xend=df.time$R.end), col="red")+
geom_segment(data=pars, aes(y=tempHOBO_PS, yend=tempHOBO_PS, x=df.time$PS.start, xend=df.time$PS.end), col="red")+
geom_segment(data=pars, aes(y=tempHOBO_R, yend=tempHOBO_R, x=df.time$R.start, xend=df.time$R.end), col="red")+
#ylim(c(22,30))+
ggtitle("Temperature \nHOBOlogger - grey | O2probe - black")+theme(plot.title=element_text(size=8))

title <- ggdraw() + draw_label(site, fontface='bold')

#img<-readPNG(img.path)
#img<-rasterGrob(img, interpolate=TRUE)
#imgplot <- ggplot()+annotation_custom(img, xmin=0, xmax=100, ymin=0, ymax=100)+xlim(c(1,100))+ylim(c(1,100))+theme_void()

plot <- plot_grid(title,plot_grid(plot_grid(pO2day, pPARday, pTEMPday, ncol=1),plot_grid(pO2site,pPARsite, pTEMPsite, ncol=1)), ncol=1, rel_heights=c(0.1, 1))
plot

ggsave(paste("probe_data/plots/", site, ".png", sep=""), plot, width=11, height=15, units="cm", dpi = 300)
}



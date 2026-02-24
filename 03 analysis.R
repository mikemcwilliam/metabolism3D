
rm(list = ls())

library("ggplot2")
library("cowplot")
library("ggrepel")
library("reshape2")
library("viridis")
se <- function(x){sd(x)/sqrt(length(x))}

#################################
################################# 
# Probe data

df <- read.csv("data/data.csv")
head(df)

chamb_planar <- pi*((0.6/2)^2 )  # planar area of 0.6m diam chamber (m3)

#################################### 
#################################### 
# COMPOSITION 

compHI <- read.csv("data/probe_data/compositionHI.csv")
compGBR <- read.csv("data/probe_data/compositionGBR.csv")
compHI <- compHI[compHI$metric %in% "planar",]
compHI <- compHI[compHI$use%in%"y",]
compHI <- melt(compHI, id=c("site","siteOLD", "metric", "use"))
compHI$area_m2<-compHI$value/10^6
compHI$area_m2[is.na(compHI$area_m2)]<-0
compHI$cover <- (compHI$area_m2/chamb_planar)*100
compGBR$cover <- (compGBR$area_m2/chamb_planar)*100
head(compGBR)
head(compHI)

plot_grid(
ggplot(compHI, aes(site, cover, fill=variable))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45, hjust=1)),
ggplot(compGBR, aes(Site, cover, fill=Group))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45, hjust=1)),
rel_widths=c(0.5,1))

 source("figs/supplement/FigS3_comp.R")
# FigS3

#################################### 
#################################### 
#### 2D COVER

head(compHI)
head(compGBR)

covHI <- aggregate(cover~site, compHI, sum)
covGBR <- aggregate(cover~Site, compGBR, sum)
colnames(covGBR)<-c("site", "cover")
cov2D <- rbind(covHI,covGBR )
df$cover <- cov2D$cover[match(df$site, cov2D$site)]
df$cover[df$site=="sand"]<-0

#################################### 
#################################### 
# METABOLISM

chamb_diam <- 0.55 #m #0.6
chamb_height1 <- 0.15 #0.3
chamb_height2 <- 0.2 #0.3
chamb_planar <- pi*((chamb_diam/2)^2 )
chamb_cyl <- chamb_planar * chamb_height1
chamb_cone <- (chamb_height2/3) * chamb_planar
chamb_vol_m3 <- chamb_cyl + chamb_cone
chamb_vol_m3

chamb_vol_gbr <- 0.08 #90000 * 0.000001 # m3 calculated
chamb_vol_hi <- chamb_vol_m3 
df$chamb_vol <- ifelse(df$region=="GBR", chamb_vol_gbr, ifelse(df$region=="Hawaii", chamb_vol_hi, NA))

# calculate water vol after displacement
df$watervol <- df$chamb_vol - df$Volm3
range(df$watervol, na.rm=T) # 76-111 Liters water
df$watervol[df$dominant=="Sand"] <- chamb_vol_gbr

# normalise metabolic rates by watervolume
df$NPP <- ((df$p.slp*df$watervol)/chamb_planar) # mg o2 m-2 min
df$R <- abs(((df$r.slp*df$watervol)/chamb_planar))

# gross production and se
df$GPP <- df$NPP + df$R
df$gpp.se <- df$p.se + abs(df$r.se)


############################################################
############################################################
#### TWO PROBES !  (check probe variability in same chamber)

swaps <- df[grepl("swap",df$siteRep),]
pumps <- df[grepl("pump",df$siteRep),]
vars <- df[!df$siteRep %in% c(swaps$siteRep, pumps$siteRep),]
vars <- vars[!vars$site %in% "sand",]
head(vars)
nrow(vars)

Nprobes <- data.frame(N=rowSums(table(vars[,c("site", "probe")])))
dups <- rownames(Nprobes)[Nprobes$N>1]
dups

vars <- vars[vars$site %in% dups,]

#vars <- vars[!vars$site %in% "site10",]

probes <- NULL
for(i in unique(vars$site)){
	#i <- unique(vars$site)[1]
	sub <- vars[vars$site %in% i,]
	probe1 <- unique(sub$probe)[1]
	probe2 <- unique(sub$probe)[2]
	newdf <- data.frame(site=i, Rug=sub$Rug[1], p1 = probe1, p2=probe2, npp1 = sub$NPP[sub$probe %in% probe1 ], npp2 = sub$NPP[sub$probe %in% probe2 ], r1 = sub$R[sub$probe %in% probe1 ], r2=sub$R[sub$probe %in% probe2 ], p.rsq1 = sub$p.rsq[sub$probe %in% probe1 ], p.rsq2 = sub$p.rsq[sub$probe %in% probe2 ], r.rsq1 = sub$r.rsq[sub$probe %in% probe1 ],r.rsq2 = sub$r.rsq[sub$probe %in% probe2 ])
probes <- rbind(probes, newdf)	
}
probes

probes$rdiff <- probes$r1 - probes$r2
probes$pdiff <- probes$npp1 - probes$npp2

plot_grid(ggplot(probes, aes(npp1, npp2))+geom_point(shape=21)+geom_abline(),
ggplot(probes, aes(r1, r2))+geom_point(shape=21)+geom_abline())

source("figs/supplement/FigS9_dups.R")
# FigS9 


############################################################
############################################################
#### Merge duplicate Probes 

df2 <- df
df2 <- df2[!df2$siteRep %in% c(swaps$siteRep, pumps$siteRep),]
head(df2)
nrow(df2)

df2[df2$site=="sand",]
nrow(df2)

df2 <- aggregate(.~site+region+dominant+location+description+chamber+successful+dom+siteRep, subset(df2, select=-c(probe, X, X.1)), mean)
unique(df2$site)
nrow(df2)
head(df2)

df2[df2$site=="sand",]
nrow(df2)

hist(scale(log(df2$Rug)))

#################################################################
#################################################################
# pump effects

pumps

# add no pump measurements
pumps$Rno <- df2$R[match(pumps$site, df2$site)]
pumps$NPPno <- df2$NPP[match(pumps$site, df2$site)]
pumps$PARno <- df2$parPS[match(pumps$site, df2$site)]

pumps$light_change <- log(pumps$parPS)-log(pumps$PARno)  #pumps$parPS-pumps$PARno
# how much more light in chambers with pumps? 

pumps$GPP <- pumps$NPP + pumps$Rno
pumps$GPPno <- pumps$NPPno + pumps$Rno

ggplot(pumps, aes(NPPno, NPP,col=light_change))+
geom_point()+geom_abline()+
scale_x_sqrt()+scale_y_sqrt()+
#geom_text(aes(label=site),size=2.5, col="red")+
geom_smooth(method="lm")+
scale_colour_viridis()

summary(lm(NPP~NPPno, data=pumps))
summary(lm(NPP-NPPno~light_change, data=pumps)) # effect of light change

plong <- melt(pumps[,c("NPPno", "NPP", "site", "Rug", "SAcm2", "light_change", "PARno", "parPS")], id.var=c("site", "Rug", "SAcm2", "light_change", "PARno", "parPS"))
plong$PAR <- ifelse(plong$variable=="NPPno", plong$PARno, plong$parPS)
plong$variable2 <- ifelse(plong$variable=="NPPno", "No Pump", "With Pump")
plong

ggplot(plong, aes(variable2, value))+
geom_line(aes(group=site), col="grey")+
geom_boxplot(outlier.size=0)

# check effect of pumps

summary(lm(log(value)~variable+log(Rug)+sqrt(PAR), data=plong))

pumps$diff <- pumps$NPPno - pumps$NPP
mean(pumps$diff)
(mean(pumps$diff)/max(df2$NPP))*100
length(unique(pumps$site))
nrow(pumps)

source("figs/supplement/FigS8_pumps.R")
# FigS8

############################################################
############################################################
#### Site effects

head(df2)
summary(aov(GPP~location, data=df2))
summary(aov(NPP~location, data=df2))
summary(aov(R~location, data=df2))

############################################################
############################################################

# Scaling of GPP/NPP/R with rugosity (Fig 1)

plot_grid(
ggplot(df2, aes(log(Rug), log(GPP), shape=region, col=region))+geom_point()+geom_smooth(method="lm", se=F),
ggplot(df2, aes(log(Rug), log(R), shape=region, col=region))+geom_point()+geom_smooth(method="lm", se=F),
ggplot(df2, aes(log(Rug), log(NPP), shape=region, col=region))+geom_point()+geom_smooth(method="lm", se=F),
nrow=1)

lm1a <- lm(log(GPP)~log(Rug), data=df2[df2$region=="GBR",])
lm1b <- lm(log(GPP)~log(Rug), data=df2[df2$region=="Hawaii",])
lm2a <- lm(log(R)~log(Rug), data=df2[df2$region=="GBR",])
lm2b <- lm(log(R)~log(Rug), data=df2[df2$region=="Hawaii",])
lm3a <- lm(log(NPP)~log(Rug), data=df2[df2$region=="GBR",])
lm3b <- lm(log(NPP)~log(Rug), data=df2[df2$region=="Hawaii",])
summary(lm1a)

source("figs/Fig1.R")
FIG1 
# ggsave( "figs/fig1.jpg",FIG1, height=5, width=8.2)

source("figs/supplement/FigS4_rank.R")
FigS4 

############################################################
############################################################
# Benthic composition

plot_grid(
ggplot(df2, aes(log(Rug), log(GPP), fill=dom, col=dom))+geom_point()+geom_smooth(method="lm", se=F),
ggplot(df2, aes(log(Rug), log(R), fill=dom, col=dom))+geom_point()+geom_smooth(method="lm", se=F),
ggplot(df2, aes(log(Rug), log(NPP), fill=dom, col=dom))+geom_point()+geom_smooth(method="lm", se=F),
nrow=1)


table(df2$dom)
samps <- data.frame(table(df2$dom))
samps$dom2 <- paste(samps$Var1, " (", samps$Freq, ")", sep="")
samps
df2$dom2 <- samps$dom2[match(df2$dom, samps$Var1)]

df2$hab <- ifelse(df2$dom=="Seagrass", "Seagrass", ifelse(df2$dom=="Sand","Sand", ifelse(df2$dom=="Algal turf", "Algal turf", ifelse(df2$dom=="Soft coral", "Soft coral", "Hard coral" ))))

#################################################################################
#################################################################################
#################################################################################
# rank benthic taxa by rates per cm2 

df2$GPPcm2 <- (df2$GPP / df2$SAcm2)*1000
df2$r <- (df2$GPP / df2$SAcm2)*1000
avs <- aggregate(r ~ dom,  df2, mean)
avs$se <- aggregate(r ~ dom,  df2, se)$r

df2$Rcm2 <- (df2$R / df2$SAcm2)*1000
df2$r <- (df2$R / df2$SAcm2)*1000
avs2 <- aggregate(r ~ dom,  df2, mean)
avs2$se <- aggregate(r ~ dom,  df2, se)$r

df2$NPPcm2 <- (df2$NPP / df2$SAcm2)*1000
df2$r <- (df2$NPP / df2$SAcm2)*1000
avs3 <- aggregate(r ~ dom,  df2, mean)
avs3$se <- aggregate(r ~ dom,  df2, se)$r

clong <- melt(df2[,c("dom", "Rcm2", "GPPcm2")], id.var="dom")
head(clong)

avz <- rbind(cbind(avs, t="GPP"),cbind(avs2, t="R"),cbind(avs3, t="NCP"))
avz$t <- factor(avz$t, levels=c("GPP", "R", "NCP")) 
head(avz)

ggplot(avz[!avz$t %in% c("NCP"),], aes(x=r, y=reorder(dom, -r), shape=t, group=dom, col=dom))+
geom_line(size=2, alpha=0.5)+geom_point()+
geom_segment(data=avz[!(avz$t %in% c("NCP") | avz$dom %in% c("Sand")),], aes(x=r-se, xend=r+se, y=dom, yend=dom))

############################################################
############################################################
# log-linear slopes for each benthic taxon

Edom <- NULL
for(i in unique(df2$dom)){
	for(y in c("GPP", "NPP", "R")){
#	i <- "Acropora"
dat <- df2[df2$dom %in% i,]	
dat$y <-  dat[,y]
dmod <- lm(log(y)~log(Rug), dat)	
slp <- coef(dmod)[2][1]
upper <- confint(dmod)[2,2]
lower <- confint(dmod)[2,1]
Edom <- rbind(Edom, data.frame(dom=i, slp, y, upper, lower))	
}}
Edom

ggplot(Edom[!Edom$dom %in% c("Sand", "Seagrass"),], aes(x=slp, y=reorder(dom, -slp), col=dom, fill=dom, group=y))+
geom_errorbar(aes(xmin=lower, xmax=upper, y=dom),  position = position_dodge(width=0.75), orientation = "y", width=0)+
geom_point(aes(shape=y),  position = position_dodge(width=0.75), stroke=0.2)

##############################################################################
#################################################### fig 2

source("figs/Fig2.R")
FIG2
# ggsave( "figs/fig2.jpg",FIG2, height=4.75, width=8)

source("figs/supplement/FigS5_benthic.R")
FigS5 

############################################################
############################################################
### LIGHT / TEMPERATURE

ggplot(df2, aes(x=parPS))+geom_density()+facet_wrap(~region, ncol=1, scales="free_y")+scale_x_log10()

light <- melt(df2[,c("site","parPS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "parPS", "region", "SAcm2"))
head(light)

ggplot(light, aes(parPS, (value/SAcm2)*1000, col=variable))+
geom_point()+scale_x_log10()+geom_smooth( method="lm")

ggplot(df2, aes(x=tempO2_PS))+geom_density()+facet_wrap(~region, ncol=1, scales="free_y")

df2$temp <- rowMeans(df2[,c("tempO2_PS", "tempHOBO_PS")])
ggplot(df2, aes(x=temp))+geom_density()+facet_wrap(~region, ncol=1, scales="free_y")

temp <- melt(df2[,c("site","tempO2_PS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "tempO2_PS", "region", "SAcm2"))
temp2 <- melt(df2[,c("site","tempHOBO_PS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "tempHOBO_PS", "region", "SAcm2"))
temp$id <- paste(temp$site, temp$variable)
temp2$id <- paste(temp2$site, temp2$variable)
temp$hobo <- temp2$tempHOBO_PS[match(temp$id, temp2$id)]
temp$temp <- rowMeans(temp[,c("hobo", "tempO2_PS")])
temp$variable <- ifelse(temp$variable =="NPP", "NCP",as.character(temp$variable))
temp$variable <- factor(temp$variable, levels=c("GPP", "R", "NCP"))
head(temp)

ggplot(temp, aes(temp, (value/SAcm2)*1000, col=variable))+geom_point()+
geom_smooth(method="lm", se=F, formula=y~poly(x, 2))

#################################################################
#################################################################
# effect sizes of light/temp/rug - cohens f2
# normalised and non-normalised

esizes3 <- NULL
	for(j in c("NPP", "GPP", "R", "NPPcm2", "Rcm2", "GPPcm2")){
	#j <- "GPP"
df2$y <- log(df2[,j])
full <- lm(y~scale(log(Rug))+scale(log(parPS))+poly(scale(temp),2), df2)
#summary(full)
modR <- lm(y~scale(log(parPS))+poly(scale(temp),2), df2)
modL <- lm(y~scale(log(Rug))+poly(scale(temp),2), df2)
modT <- lm(y~scale(log(Rug))+scale(log(parPS)), df2)
pR <- coef(summary(full))["scale(log(Rug))","Pr(>|t|)"]
pT <- coef(summary(full))["poly(scale(temp), 2)2","Pr(>|t|)"]
pL <- coef(summary(full))["scale(log(parPS))","Pr(>|t|)"]
rsqF <- summary(full)$r.squared
rsqR <- summary(modR)$r.squared
rsqL <- summary(modL)$r.squared
rsqT <- summary(modT)$r.squared
esizes3 <- rbind(esizes3, data.frame(j, pred =c("Habitat rugosity", "Light intensity", "Temperature"), rsq=c(rsqR, rsqL, rsqT), pval = c(pR, pL, pT), rsqF))
}

esizes3$AB_A <- esizes3$rsqF - esizes3$rsq
esizes3$unexpl <- 1 - esizes3$rsqF
esizes3$f2 <- esizes3$AB_A / esizes3$unexpl
esizes3$norm <- ifelse(esizes3$j %in% c("GPP", "NPP", "R"), "Total", "Normalised")
esizes3

ggplot(esizes3, aes(pred, f2))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j), width=0.65, col="black", size=0.1)+
facet_wrap(~norm, ncol=1)


#################################################################
#################################################################
# FIG 3 

source("figs/Fig3.R")
FIG3

# ggsave( "figs/fig3.jpg",FIG3, height=3.5, width=7.5)

#################################################################
#################################################################
# Model AICs (table 1)

library("MuMIn")

df2$cover[df2$dominant=="Sand"]<-NA

df2[,c("dominant", "cover", "Rug")]

head(df2)

modelAICs2 <- NULL
	for(j in c("NPP", "GPP", "R")){
	#j <- "NPP"
df3 <- df2 # [!df2$dominant %in% c("Sand"),]
df3$y <- log(df3[,j])
#mod.1 <-lm(y~log(reef_planar), data=df)
mod.C <-lm(y~log(cover), data=df3)
mod.R <-lm(y~log(Rug), data=df3)
#mod.3 <-lm(y~log(reef_planar)+dominant, data=df)
mod.RD <-lm(y~log(Rug)+dominant, data=df3)
#mod.5 <-lm(y~log(reef_planar)+parPS+poly(tempO2, 2), data=df)
mod.RL <-lm(y~log(Rug)+sqrt(parPS), data=df3)
mod.RT <-lm(y~log(Rug)+poly(temp, 2), data=df3)
mod.RLT <-lm(y~log(Rug)+sqrt(parPS)+poly(temp, 2), data=df3)
#mod.7 <- lm(y~log(reef_planar)+dominant+parPS+poly(tempO2, 2), data=df3)
mod.RDLT <- lm(y~log(Rug)+dominant+sqrt(parPS)+poly(temp, 2), data=df3)
#mod.9 <- lm(y~log(Rug)+dominant+log(parPS), data=df3)
mod.list <- list(mod.C, mod.R,  mod.RD,mod.RL,mod.RT,  mod.RDLT)
AICs <- AICc(mod.C, mod.R,  mod.RD,  mod.RL, mod.RT,   mod.RDLT)
AICs$dAIC <-  AICs$AICc - min(AICs$AICc)
AICs$w <- Weights(AICs$AICc)  
r.sq <- NULL
for(i in 1:length(mod.list)){
r.sq <- c(r.sq, summary(mod.list[[i]])$r.squared)
}
 modelAICs2 <- rbind(modelAICs2, cbind(type=j, round(AICs,2), r2 = round(r.sq,2)))
}

result.table<-cbind(modelAICs2[ modelAICs2$type=="GPP",], modelAICs2[ modelAICs2$type=="R",], modelAICs2[ modelAICs2$type=="NPP",])
result.table

#write.csv(mod.results, "data/modelAICs.csv")


#################################################################
#################################################################
# allometry

scaling <- NULL
mods <- c("Rug")
type <- c("NPP","GPP","R")
for(i in mods){
	for(j in type){
		dat <- df #2[!df2$dom %in% c("Sand", "Seagrass"),]
		#i <- "Rug"
		#j <- "GPP"
	dat$x <- dat[,i]
	dat$y <- dat[,j]
mod<-lm(y~x, dat)
l.mod <- lm(log(y)~log(x), dat)
full <- data.frame(i,j, slp=coef(mod)[2], int=coef(mod)[1], ci1=confint(mod)[2,2], ci2=confint(mod)[2,1], log.slp=coef(l.mod)[2], log.int = coef(l.mod)[1], log.ci1 =confint(l.mod)[2,2], log.ci2 = confint(l.mod)[2,1], lint.ci1=confint(l.mod)[1,2], lint.ci2=confint(l.mod)[1,1], loc="Total")
mod.hi<-lm(y~x, dat[dat$region=="Hawaii",])
l.mod.hi <- lm(log(y)~log(x), dat[dat$region=="Hawaii",])
hi <- data.frame(i,j, slp=coef(mod.hi)[2], int=coef(mod.hi)[1], ci1=confint(mod.hi)[2,2], ci2=confint(mod.hi)[2,1], log.slp=coef(l.mod.hi)[2], log.int = coef(l.mod.hi)[1], log.ci1 =confint(l.mod.hi)[2,2], log.ci2 = confint(l.mod.hi)[2,1], lint.ci1=confint(l.mod.hi)[1,2], lint.ci2=confint(l.mod.hi)[1,1], loc="Hawaii")
mod.gbr<-lm(y~x, dat[dat$region=="GBR",])
l.mod.gbr <- lm(log(y)~log(x), dat[dat$region=="GBR",])
gbr <- data.frame(i,j, slp=coef(mod.gbr)[2], int=coef(mod.gbr)[1], ci1=confint(mod.gbr)[2,2], ci2=confint(mod.gbr)[2,1], log.slp=coef(l.mod.gbr)[2], log.int = coef(l.mod.gbr)[1], log.ci1 =confint(l.mod.gbr)[2,2], log.ci2 = confint(l.mod.gbr)[2,1], lint.ci1=confint(l.mod.gbr)[1,2], lint.ci2=confint(l.mod.gbr)[1,1], loc="GBR")
scaling <- rbind(scaling, rbind(full, gbr, hi))
}}
scaling

ggplot(scaling, aes(x=loc, y=log.slp, col=j))+geom_point(position=position_dodge(width=0.5))+
geom_linerange(aes(xmin=loc, xmax=loc, ymin=log.ci1, ymax=log.ci2, col=j), position=position_dodge(width=0.5))

preds <- NULL
for(x in unique(scaling$loc)){
#x <- "Total"
sim.x <- log(seq(1, 5, length=20))
PS <- (scaling[scaling$i=="Rug" & scaling$j=="GPP" & scaling$loc==x,"log.slp"]*sim.x)+scaling[scaling$i=="Rug" & scaling$j=="GPP" & scaling$loc==x,"log.int"]
R <- (scaling[scaling$i=="Rug" & scaling$j=="R"& scaling$loc==x,"log.slp"]*sim.x)+scaling[scaling$i=="Rug" & scaling$j=="R"& scaling$loc==x,"log.int"]
Net <- log(exp(PS) - exp(R))
slp.n <- scaling[scaling$i=="Rug" & scaling$j=="NPP"& scaling$loc==x,"log.slp"] #10.74
int.n <-  scaling[scaling$i=="Rug" & scaling$j=="NPP"& scaling$loc==x,"log.int"] #8.612
Mod.Net <- (slp.n*sim.x)+int.n
preds <- rbind(preds, data.frame(loc=x, Rug=sim.x, GPP=PS, R=R, NCP=Mod.Net))
}
preds

lpreds <- melt(preds, id.var=c("Rug", "loc"))

ggplot(lpreds[!lpreds$loc=="Total",], aes(x=exp(Rug), y=exp(value), col=variable))+
geom_line()+facet_wrap(~loc,  scales="free")

#################################################################
#################################################################
# CUE

df$CUE <- df$NPP / df$GPP
df2$CUE <- df2$NPP / df2$GPP


plot_grid(
ggplot(df2[!df2$dom %in% c("Sand"),], aes(Rug, CUE, col=region))+geom_point()+geom_smooth( method="lm", se=F),
ggplot(df2[!df2$dom %in% c("Sand"),], aes(Rug, CUE, col=dom))+geom_point()+geom_smooth( method="lm", se=F), 
ncol=1)

summary(lm(CUE~log(Rug), data=df2))


#################################################################
#################################################################
# Fig4

source("figs/Fig4.R")
FIG4
#ggsave("figs/Fig4.jpg", FIG4, height=3.5, width=7)

source("figs/supplement/FigS6_region.R")
FigS6

source("figs/supplement/FigS7_cue.R")
FigS7




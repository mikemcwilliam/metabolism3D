 
 rm(list = ls())

library("ggplot2")
library("cowplot")
library("ggrepel")
library("reshape2")

 
#################################
################################# 
#################################

# O2 params
df <- read.csv("data/data.csv")
head(df)

df <- df[!(df$site=="site10" & df$probe == "P3"),]
df <- df[!(df$site=="site40" & df$probe == "P3"),]


df$site[df$site=="sand01"] <- "sand"# merge sand?
df$site[df$site=="sand02"] <- "sand"# merge sand?
df$location[df$site=="sand"] <- "sand"
df$chamber[df$site=="sand"] <- "sand"
df[df$site=="sand",]

# why are the NAs in temperature?
df$tempO2_PS[is.na(df$tempO2_PS)] <- df$tempHOBO_PS[is.na(df$tempO2_PS)]


chamb_planar <- pi*((0.6/2)^2 )  # planar area of 0.6m diam chamber (m3)


unique(df$dominant)
df$dom <- df$dominant
df$dom <- ifelse(df$dom %in% c("Pocillopora_fish", "P. damicornis"), "Pocillopora", df$dom)
df$dom <- ifelse(df$dom %in% c("P. compressa"), "Porites", df$dom)
df$dom <- ifelse(df$dom %in% c("Turf_dead", "Rock/Turf"), "Algal turf", df$dom)
df$dom <- ifelse(df$dom %in% c("M. capitata"), "Montipora", df$dom)
df$dom <- ifelse(df$dom %in% c("Mound_other","Branching_other","Foliose", "Lobactis"), "Other scleractinia", df$dom)
#df$dom <- ifelse(df$dom %in% c(), "Other branching", df$dom)
unique(df$dom)


#colsc <- c("#AD2831", "#E88EED",  "#F6511D", "#6A5B6E","#A833B9", "#E36397", "#F3C969", "#ECBA82", "#2E933C", "#96939B","#252627" )
colsc <- c("#AD2831","#2E933C",  "#F6511D", "#252627", "#A833B9","#ECBA82",  "yellow", "green","grey")
names(colsc) <- unique(df$dom)[order(unique(df$dom))]
colsc



ggplot(df, aes(SAcm2, Volcm3))+geom_point(aes(col=dom, shape=region, size=Rug))+
scale_y_log10()+
#geom_text_repel(aes( label=site), size=2,min.segment.length = unit(0, 'lines'))+
scale_x_log10()+
#geom_abline(slope=3/2)+
scale_radius(limits=c(1, 5))+
#geom_vline(xintercept=chamb_planar*100*100)+
scale_colour_manual(values=colsc)+
#geom_smooth(method="lm")+
labs(x=expression("3D"~surface~area~"("~cm^2~")"), y=expression(Habitat~volume~"("~cm^3~")"))+
theme_classic()+theme(legend.title=element_blank(), legend.key.size=unit(1, "mm"))


head(df)
df[is.na(df$Rug),]


#hi3D$Volm3 <- hi3D$Volcm3 * 0.000001 
#chamb_planar_hi <- pi*((0.6/2)^2 )  # planar area of 0.6m diam chamber (m3)
# df[,c("SAcm2", "SAm2", "Volcm3", "Volm3", "Rug")] <- hi3D[match(df$site, hi3D$site), c("SAcm2", "SAm2", "Volcm3", "Volm3", "Rug")]
df$Rug


ggplot(df, aes(SAcm2, SAcm2/Volcm3))+geom_point()+
scale_x_log10()+scale_y_log10()

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

# site 23? porites or mcap? OG says fluffy_por labelled as mcap dom
compHI[compHI$site=="hi23" & compHI$variable=="pcom","cover"]<-0
compHI[compHI$site=="hi23" & compHI$variable=="mcap","cover"]<-21.19519

compHI$Group <-ifelse(compHI$variable =="mcap", "Montipora", ifelse(compHI$variable =="pcom", "Porites", ifelse(compHI$variable =="pdam", "Pocillopora", ifelse(compHI$variable =="rock", "Turf", ifelse(compHI$variable =="fungid", "Other scleractinia", NA))))) 

compGBR$Group2 <- compGBR$Group
compGBR$Group2[compGBR$Group2=="Bare rock"]<-"Rock/rubble"
compGBR$Group2[compGBR$Group2=="Rubble"]<-"Rock/rubble"

colsc3 <- c("#89374FFF","#6CA167FF", "#635C72FF", "grey", "#145A76FF", "#E27B0CFF" ,"#B79E4BFF","aquamarine3","lightsalmon1")
colsc3

colnames <- c(unique(compGBR$Group2), "Other scleractinia")
colnames
tcols <- c("#E27B0CFF" , "slategrey", "grey30", "#6CA167FF", "pink", "#89374FFF", "grey", "green", "lightsalmon1", "#145A76FF", "#635C72FF", "aquamarine3", "grey")
names(tcols)<-colnames

compGBR$cover[compGBR$Group %in% "Seagrass"]<-30

planarGBR <- aggregate(cover~Site, compGBR, sum)
planarHI <- aggregate(cover~site, compHI, sum)
compHI$total <- planarHI$cover[match(compHI$site, planarHI$site)]
compGBR$total <- planarGBR$cover[match(compGBR$Site, planarGBR$Site)]


unique(df[,c("site", "dom")])


compHI$dom <- df$dom[match(compHI$site, df$site)]
compHI$dom[compHI$dom=="Algal turf"] <- "Turf"
compHI[compHI$site=="hi23",]
domHI <- compHI[compHI$Group==compHI$dom,]
domHI
length(unique(compHI$site))
length(unique(domHI$site))


plot_grid(
ggplot(domHI, aes(site, cover/total, fill=Group))+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
labs(y="Rel. abundance of\ndominant taxon")+
scale_fill_manual(values=tcols)+
geom_bar(stat="identity")+
scale_y_continuous(expand=c(0,0))
,
ggplot(compHI, aes(site, cover/total*100, fill=Group))+geom_bar(stat="identity")+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
scale_fill_manual(values=tcols)+
labs(y="% cover")+
scale_y_continuous(expand=c(0,0))#+theme(axis.text.x=element_blank())
,
ncol=1, align="hv", rel_heights=c(0.5, 1))


compGBR$dom <- df$dom[match(compGBR$Site, df$site)]
domGBR <- compGBR[compGBR$Group==compGBR$dom,]
domGBR
length(unique(compGBR$Site))
length(unique(domGBR$Site))

compHI$dom <- ifelse(compHI$dom=="Other scleractinia", "Other scler.", compHI$dom)
compGBR$dom <- ifelse(compGBR$dom=="Other scleractinia", "Other scler.", compGBR$dom)
compGBR$dom <- ifelse(compGBR$dom=="Algal turf", "Turf", compGBR$dom)

df2[df2$site=="site70",]

HIcompplot <- ggplot(compHI, aes(site, cover/total*100, fill=Group))+geom_bar(stat="identity")+
geom_text(aes(site, 102, label=dom), angle=70, hjust=0, size=2)+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
ylim(c(0, 150))+
scale_fill_manual(values=tcols)+
labs(y="% cover")+ggtitle("Hawai'i")+
scale_y_continuous(expand=c(0,0), limits=c(0, 120), breaks=c(0, 50, 100))+
theme(axis.text.x=element_blank(), legend.key.size=unit(2, "mm"))
HIcompplot

GBRcompplot <- ggplot(compGBR, aes(Site, cover/total*100, fill=Group2))+geom_bar(stat="identity")+
geom_text(aes(Site, 102, label=dom), angle=70, hjust=0, size=2)+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
scale_fill_manual(values=tcols)+
labs(y="% cover")+ggtitle("GBR")+
scale_y_continuous(expand=c(0,0), limits=c(0, 120), breaks=c(0, 50, 100))+
theme(axis.text.x=element_blank(), legend.key.size=unit(2, "mm"))
GBRcompplot 


plot_grid(plot_grid(HIcompplot, NULL), GBRcompplot , ncol=1)




############################################################
############################################################
#### 2D COVER

head(compHI)
head(compGBR)

covHI <- aggregate(cover~site, compHI, sum)
covGBR <- aggregate(cover~Site, compGBR, sum)
colnames(covGBR)<-c("site", "cover")
cov2D <- rbind(covHI,covGBR )
df$cover <- cov2D$cover[match(df$site, cov2D$site)]
df$cover[df$site=="sand"]<-0


ggplot(df, aes(cover, Rug))+geom_point()

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

# 0.1130973 # ORIGINAL HI

chamb_vol_gbr <- 0.08 #90000 * 0.000001 # m3 calculated
chamb_vol_hi <- chamb_vol_m3#0.045
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

ggplot(data=df[!df$dom%in% c("Sand"),], aes(NPP, R))+geom_point(aes(col=dom))+
scale_y_log10()+scale_x_log10()+
geom_smooth(method="lm")


ggplot(df, aes(x=(GPP*60)/1000))+geom_histogram(bins=12) # g/hr

# write.csv(data.frame(GPP = (df$GPP*60)/1000 ), "cesab.csv") # for cesab

head(df)

doms <- c("sand", "Soft coral")

head(df)

ggplot(df, aes(chamber, R))+geom_boxplot()




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


ggplot(probes, aes(npp1, npp2))+geom_point(shape=21)+geom_abline()

ggplot(probes, aes(r1, r2))+geom_point(shape=21)+geom_abline()



probes1 <- melt(probes[,c("site", "npp1", "npp2", "Rug")], id.var=c("site", "Rug"))
probes1$n <- substr(probes1$variable, 4,4)
rsq1 <- melt(probes[,c("site", "p.rsq1", "p.rsq2")], id.var="site")
rsq1$n <- substr(rsq1$variable, 6,6)
colnames(rsq1) <- c("site", "variable2", "value2", "n")
probes1x <- merge(probes1, rsq1)
probes1x$var <- ifelse(probes1x$variable=="npp1", "sensor 1", "sensor 2")
probes1x
nrow(probes1x)

r3a <- ggplot(probes1x)+
geom_line(aes(value, reorder(site, -value)))+
geom_point(aes(value, reorder(site, -value), col=var))+
#scale_radius(range=c(0.1,3))+
labs(y="Site",  x=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
theme_classic()+theme(axis.text.y=element_blank(), legend.title=element_blank())

r3b <- ggplot()+
#scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17, 22))+
#scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_y_continuous(limits=c(0.8,25))+
geom_point(data=df, aes(Rug, NPP), col="grey")+
geom_line(data=probes1x, aes(Rug, value, group=site))+
#geom_smooth(data=df, aes(Rug, NPP), col="grey", method="lm", formula=y~poly(x,2))+
geom_point(data=probes1x, aes(Rug, value, col=var))+
#scale_radius(range=c(0.1,3))+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
theme_classic()

plot_grid(r3a+guides(col="none"), r3b+guides(col="none"), get_legend(r3a),
nrow=1, labels=c("a", "b"), rel_widths=c(1,1.2,0.5))

mean(abs(probes$pdiff))
mean(abs(probes$pdiff))/diff(range(df$NPP))*100


probes2 <- melt(probes[,c("site", "r1", "r2", "Rug")], id.var=c("site", "Rug"))
probes2$n <- substr(probes2$variable, 2,2)
rsq2 <- melt(probes[,c("site", "r.rsq1", "r.rsq2")], id.var="site")
rsq2$n <- substr(rsq2$variable, 6,6)
colnames(rsq2) <- c("site", "variable2", "value2", "n")
probes2x <- merge(probes2, rsq2)
probes2x


ggplot(probes2x)+
geom_point(aes(value, reorder(site, -value), col=variable, size=value2))+
geom_line(aes(value, reorder(site, -value)))+
scale_radius(range=c(0.1,3))

ggplot()+
#scale_y_log10(limits=c(1,25), breaks=c(2, 7, 12, 17))+
#scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
geom_point(data=df, aes(Rug, R), col="grey")+
geom_smooth(data=df, aes(Rug, R), col="grey", method="lm")+
geom_point(data=probes2x, aes(Rug, value, col=variable, size=value2))+
scale_radius(range=c(0.1,3))+
geom_line(data=probes2x, aes(Rug, value, group=site))


mean(abs(probes$rdiff))
mean(abs(probes$rdiff))/diff(range(df$R, na.rm=T))*100


ggplot(df, aes(x=NPP, y=R))+geom_point(shape=21)+geom_text(aes(label=site))



mean(abs(probes$pdiff))


head(vars)
table(vars$site)



dcast(vars, site~probe, value.var="NPP")




hist(df$r.rsq)
hist(df$p.rsq)
mean(df$p.rsq, na.rm=T)
mean(df$r.rsq, na.rm=T)


############################################################
############################################################
#### DUPLICATES

head(df)
aggregate(Volm3~region, df, max)

df2 <- df

swaps <- df2[grepl("swap",df2$siteRep),]
pumps <- df2[grepl("pump",df2$siteRep),]
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


#################################################################
#################################################################

# chamber effects/pump effects
library("viridis")

head(pumps)
pumps
swaps

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
geom_text(aes(label=site),size=2.5, col="red")+
geom_smooth(method="lm")+
scale_colour_viridis()


summary(lm(NPP~NPPno, data=pumps))


plong <- melt(pumps[,c("NPPno", "NPP", "site", "Rug", "SAcm2", "light_change", "PARno", "parPS")], id.var=c("site", "Rug", "SAcm2", "light_change", "PARno", "parPS"))
plong$PAR <- ifelse(plong$variable=="NPPno", plong$PARno, plong$parPS)
plong$variable2 <- ifelse(plong$variable=="NPPno", "No Pump", "With Pump")
plong

summary(lm(log(value)~variable+log(Rug)+sqrt(PAR), data=plong))

ggplot(plong, aes(PAR, value/SAcm2, col=variable))+
scale_x_log10()+scale_y_log10()+
geom_point()

R3a <- ggplot(plong, aes(Rug, value, col=variable2))+
geom_point(data=df2, aes(x=Rug, y=NPP), col="grey")+
geom_smooth(data=df2, aes(x=Rug, y=NPP), col="grey", method="lm")+
geom_point()+
geom_smooth(method="lm")+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_radius()+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
theme_classic()+theme(legend.title=element_blank(), legend.position=c(0.8, 0.1))



R3b <- ggplot(plong, aes(variable2, value))+
geom_line(aes(group=site), col="grey")+
geom_violin(data=df2, aes(x="all", y=NPP), col="grey")+
geom_point(data=df2, aes(x="all", y=NPP), col="grey")+
geom_boxplot(outlier.size=0)+
geom_point()+
#geom_text(data=plong[plong$variable=="NPPno",],aes(x="No Pump", y=value, label=site), hjust=1, size=2.5, col="red")+
#geom_text(data=plong[plong$variable=="NPP",],aes(x="With Pump", y=value, label=site), hjust=0, size=2.5, col="red")+
labs(x="Treatment",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
#lims(y=c(min(df2$NPP), max(df2$NPP)))+
scale_y_log10()+
theme_classic()


R3c <-ggplot(pumps, aes(light_change, NPP-NPPno))+
geom_vline(xintercept=0, col="grey", size=0.3)+
geom_hline(yintercept=0, col="grey", size=0.3)+
geom_point()+
labs(x="Change in light intensity\nbetween treatments (log scale)", y="Change in NCP with the\naddition of pumps")+
geom_smooth(method="lm")+
theme_classic()


R3c2 <-ggplot()+
geom_histogram(data=pumps, aes(NPP-NPPno), bins=6, col="black", fill="grey", size=0.3)+
labs(x="Change in NCP with the\naddition of pumps", y="N chambers")+
scale_y_continuous(expand=c(0,0), limits=c(0, 10))+
geom_segment(data=NULL, aes(x=mean(pumps$NPP - pumps$NPPno), xend=mean(pumps$NPP - pumps$NPPno), y=Inf, yend=-Inf), col="red")+
xlim(c(-8, 6))+
theme_classic()


summary(lm(NPP-NPPno~light_change, data=pumps))

plot_grid(R3a, plot_grid(R3b, R3c2, ncol=1, labels=c("b", "c"), hjust=1, rel_heights=c(1,0.8)), rel_widths=c(1, 0.8), labels=c("a", ""))

ggplot(plong, aes(variable2, value))+
geom_line(aes(group=site), col="grey")+
geom_boxplot(outlier.size=0)+
geom_point()+
geom_text(data=plong[plong$variable=="NPPno",],aes(x="No Pump", y=value, label=site), hjust=1, size=2.5, col="red")+
geom_text(data=plong[plong$variable=="NPP",],aes(x="With Pump", y=value, label=site), hjust=0, size=2.5, col="red")+
labs(x="Treatment",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
#lims(y=c(min(df2$NPP), max(df2$NPP)))+
scale_y_log10()+
theme_classic()


aggregate(value~variable, plong, mean)


pumps$diff <- pumps$NPPno - pumps$NPP
mean(pumps$diff)
(mean(pumps$diff)/max(df2$NPP))*100
length(unique(pumps$site))
nrow(pumps)

swaps$NPPuse <- df2$NPP[match(swaps$site, df2$site)]
swaps$PARuse <- df2$parPS[match(swaps$site, df2$site)]

slong <- melt(swaps[,c("NPPuse", "NPP", "site", "Rug", "SAcm2")], id.var=c("site", "Rug", "SAcm2"))

ggplot(slong, aes(variable, value))+
geom_point(data=df2, aes(x="NPP", y=NPP), col="grey")+
geom_boxplot(outlier.size=0)+
geom_point()+
geom_text(data=slong[slong$variable=="NPPuse",],aes(x="NPPuse", y=value, label=site), hjust=1, size=2.5, col="red")+
geom_text(data=slong[slong$variable=="NPP",],aes(x="NPP", y=value, label=site), hjust=0, size=2.5, col="red")+
geom_line(aes(group=site))+
#lims(y=c(min(df2$NPP), max(df2$NPP)))+
theme_classic()


############################################################
############################################################
#### SIte

head(df2)

summary(aov(GPP~location, data=df2))
summary(aov(NPP~location, data=df2))
summary(aov(R~location, data=df2))



############################################################
############################################################

lm1a <- lm(log(GPP)~log(Rug), data=df2[df2$region=="GBR",])
lm1b <- lm(log(GPP)~log(Rug), data=df2[df2$region=="Hawaii",])
lm2a <- lm(log(R)~log(Rug), data=df2[df2$region=="GBR",])
lm2b <- lm(log(R)~log(Rug), data=df2[df2$region=="Hawaii",])
lm3a <- lm(log(NPP)~log(Rug), data=df2[df2$region=="GBR",])
lm3b <- lm(log(NPP)~log(Rug), data=df2[df2$region=="Hawaii",])


lm_eqn <- function(m){
    #m <- lm(y ~ x, df);
    eq <- substitute(italic(b) == slp %+-% std*","~~italic(r)^2~"="~r2, 
         list(a = format(unname(coef(m)[1]), digits = 2),
              slp = format(unname(coef(m)[2]), digits = 2),
               std = format(unname(coef(summary(m))[1,2]), digits = 2),
             r2 = format(summary(m)$r.squared, digits = 2)))
    as.character(as.expression(eq));
}

lm_eqn(lm3a)

coef(summary(lm3a))[1,2]

xeq <- 2

gpp_plot <- ggplot(df2, aes(Rug, GPP))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm", se=F, col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", se=F, linetype="longdash", size=0.5, col="red")+
geom_text(data=data.frame(1), aes(x=xeq, y=2.5, label=lm_eqn(lm1a)), parse = TRUE, size=2.5, hjust=0)+
geom_text(data=data.frame(1), aes(x=xeq, y=2, label=lm_eqn(lm1b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y=expression(GPP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(2,45), breaks=c(4, 13, 22, 31, 40))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Gross primary\nproduction (GPP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
gpp_plot


resp_plot <- ggplot(df2, aes(Rug, R))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm", se=F, col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", se=F,  linetype="longdash", size=0.5, col="red")+
geom_text(data=data.frame(1), aes(x=xeq, y=1.2, label=lm_eqn(lm2a)), parse = TRUE, size=2.5, hjust=0)+
geom_text(data=data.frame(1), aes(x=xeq, y=1, label=lm_eqn(lm2b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y=expression(R~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(1,25), breaks=c(2, 7, 12, 17, 22))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Respiration (R)")+
#lims(x=c(0.8,6))+
#scale_x_log10(breaks=c(1, 1.5, 3, 6))+scale_y_log10(breaks=c(1, 2, 4, 8, 16, 32))+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
resp_plot


npp_plot <-ggplot(df2, aes(Rug, NPP))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm", se=F, col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", se=F, linetype="longdash", size=0.5, col="red")+
geom_text(data=data.frame(1), aes(x=xeq, y=1, label=lm_eqn(lm3a)), parse = TRUE, size=2.5, hjust=0)+
geom_text(data=data.frame(1), aes(x=xeq, y=0.8, label=lm_eqn(lm3b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17, 22))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Net community\nproduction (NCP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
npp_plot


fig1abc <- plot_grid(
gpp_plot, resp_plot, npp_plot,
get_legend(ggplot(df2, aes(Rug, NPP, shape=region, col=region))+geom_point()+scale_shape_manual(values=c(16, 10))+theme_classic()+scale_colour_manual(values=c("black", "darkred"))+theme(legend.title=element_blank(), legend.key.height=unit(1, "mm"))),
nrow=1,  align="hv", axis="lr", rel_widths=c(1,1,1,0.4), labels=c("b", "c", "d", ""), label_size=9)
fig1abc

unique(df$dominant)




#################################### 
#################################### 
# FIG1

library("png")
library("grid")


img1<-readPNG("figs/img/chamber.png")
img1<-rasterGrob(img1, interpolate=TRUE)
img2<-readPNG("figs/img/img1b.png")
img2<-rasterGrob(img2, interpolate=TRUE)
img3<-readPNG("figs/img/img7b.png")
img3<-rasterGrob(img3, interpolate=TRUE)


images <- plot_grid(NULL, img1, NULL, img2, NULL, img3, NULL, nrow=1, rel_widths=c(0.3, 1.15, 0.05, 0.62, 0.1, 1, 0.5), labels=c("","a"), label_size=9)

FIG1 <- plot_grid(NULL, images, fig1abc, ncol=1, rel_heights=c(0.1, 0.6, 1))+
draw_text("Benthic metabolic chambers", 0.5, 0.95, size=8, fontface="bold")
FIG1 

# ggsave( "figs/fig1.jpg",FIG1, height=5, width=8.2)



#################################### 
#################################### 
# COLOURS

table(df2$dom)

samps <- data.frame(table(df2$dom))
samps$dom2 <- paste(samps$Var1, " (", samps$Freq, ")", sep="")
samps
df2$dom2 <- samps$dom2[match(df2$dom, samps$Var1)]

colsc1 <- c("#AD2831", "#E88EED",  "#F6511D", "#6A5B6E","#A833B9", "#E36397", "#F3C969", "#ECBA82", "#2E933C", "#96939B","#252627" )
colsc <- c("#AD2831","#2E933C",  "#F6511D", "#252627", "#A833B9","#ECBA82",  "yellow", "green","grey")
names(colsc) <- unique(df$dom)[order(unique(df$dom))]
colsc
colsc2 <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "black", "black", "#a6761d")
names(colsc2) <- unique(df$dom)[order(unique(df$dom))]
colsc2



library("fishualize")

fish<-fish_palettes()
fishualize( option = fish[14], n= 5)
fish( option = fish[14], n= 5)

fishualize( option = fish[18], n= 5)
fish( option = fish[18], n= 5)


colsc3 <- c("#89374FFF","#6CA167FF", "#635C72FF", "grey", "#145A76FF", "#E27B0CFF" ,"#B79E4BFF","aquamarine3","lightsalmon1")
colsc3
unique(df$dom)[order(unique(df$dom))]
names(colsc3) <- unique(df$dom)[order(unique(df$dom))]

colsc4 <- c("#fa9fb5", "#6CA167FF", "#8c96c6", "#bfd3e6", "#ae017e", "#8c6bb1","#B79E4BFF","aquamarine3", "grey")
colsc4
unique(df2$dom2)[order(unique(df2$dom2))]
names(colsc4) <- unique(df2$dom2)[order(unique(df2$dom2))]

colsc4b <- c("#fa9fb5", "#6CA167FF", "#8c96c6", "#bfd3e6", "#ae017e", "#8c6bb1","#B79E4BFF","aquamarine3", "grey")
colsc4b
unique(df2$dom)[order(unique(df2$dom))]
names(colsc4b) <- unique(df2$dom)[order(unique(df2$dom))]


ggplot(df, aes(Rug, fill=dom))+
geom_density(bw=0.5)+
facet_wrap(~dom, ncol=1, scales="free_y")+
scale_fill_manual(values=colsc3)+
theme(strip.text=element_blank(), axis.text.y=element_blank())


df2$hab <- ifelse(df2$dom=="Seagrass", "Seagrass", ifelse(df2$dom=="Sand","Sand", ifelse(df2$dom=="Algal turf", "Algal turf", ifelse(df2$dom=="Soft coral", "Soft coral", "Hard coral" ))))

df[df$site=="site02",]

ggplot(df, aes(x=Rug, fill=dom))+
geom_histogram(position="stack", bins=7)+
scale_fill_manual(values=colsc3)+
scale_x_log10()

############################################################
############################################################
# volume? 

head(df2)

ggplot(df2[!df2$dom %in% "Sand",], aes(Volcm3+1, GPP))+geom_point(aes(col=dom))+
geom_smooth(method="lm")+
scale_y_log10()+
scale_x_log10()


summary(lm(log(GPP)~log(Volcm3), df2[!df2$dom %in% "Sand",]))

summary(lm(log(R)~log(Volcm3), df2[!df2$dom %in% "Sand",]))




############################################################
############################################################

### COMP


legplot <-ggplot(df2, aes(Rug, GPP, col=dom2))+geom_point(size=0.75)+scale_colour_manual(values=colsc4, name="Dominant\ntaxon")+theme_classic()+
theme(legend.title=element_text(size=7, face="bold"), legend.text=element_text(size=7), legend.key.height=unit(1, "mm"))


# df2[!df2$dom %in% c("Seagrass", "Sand"),]


gpp_comp <- ggplot(df2, aes(Rug, GPP, fill=dom2))+
geom_smooth(data=df2[!df2$dom %in% c("Seagrass"),], aes(col=dom2), method="lm", se=F, size=0.6, show.legend=FALSE)+
geom_point( aes(shape=dom2, size=dom2), stroke=0.1)+ # size=1
scale_fill_manual(values=colsc4, name="Dominant\ntaxon")+scale_colour_manual(values=colsc4, name="Dominant\ntaxon")+
#geom_text_repel(data=df2, aes(label=site), size=2,min.segment.length = unit(0, 'lines'))+
labs(x="Habitat rugosity",  y=expression(GPP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(2,45), breaks=c(4, 13, 22, 31))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
#scale_shape_manual(values=c(21, 25))+guides(shape="none")+
scale_shape_manual(values=c(21, 21, 21, 21, 21, 21, 8, 24, 25), name="Dominant\ntaxon")+
scale_size_manual(values=c(1 ,1 ,1 ,1 ,1,1,2,1.5,1.5), name="Dominant\ntaxon")+
guides(size="none")+
ggtitle("Gross primary\nproduction (GPP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8), legend.title=element_text(size=7, face="bold"), legend.text=element_text(size=7), legend.key.height=unit(1, "mm"))
gpp_comp

habcols <- c("grey", "darkgreen","yellow","black", "red")
names(habcols)<-  unique(df2$hab)
habcols



resp_comp <- ggplot(df2, aes(Rug, R, fill=dom2))+
geom_smooth(data=df2[!df2$dom %in% c("Seagrass"),], aes(col=dom2), method="lm", se=F, size=0.6, show.legend=FALSE)+
geom_point( aes(shape=dom2, size=dom2), stroke=0.1)+ # 
scale_fill_manual(values=colsc4)+scale_colour_manual(values=colsc4)+ 
#geom_text_repel(data=df2, aes(label=site), size=2,min.segment.length = unit(0, 'lines'))+
labs(x="Habitat rugosity",  y=expression(R~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(1,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
#scale_shape_manual(values=c(21, 25))+guides(shape="none")+
scale_shape_manual(values=c(21, 21, 21, 21, 21, 21, 8, 24, 25))+
scale_size_manual(values=c(1 ,1 ,1 ,1 ,1,1,2,1.5,1.5))+
guides(col="none", fill="none", shape="none", size="none")+
ggtitle("Respiration (R)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
resp_comp

npp_comp <- ggplot(df2, aes(Rug, NPP, fill=dom2))+
geom_smooth(data=df2[!df2$dom %in% c("Seagrass"),], aes(col=dom2), method="lm", se=F, size=0.6, show.legend=FALSE)+
geom_point( aes(shape=dom2, size=dom2), stroke=0.1)+ # 
scale_fill_manual(values=colsc4)+scale_colour_manual(values=colsc4)+
#geom_text_repel(data=df2, aes(label=site), size=2,min.segment.length = unit(0, 'lines'))+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", fill="none", shape="none", size="none")+
#scale_shape_manual(values=c(21, 25))+guides(shape="none")+
scale_shape_manual(values=c(21, 21, 21, 21, 21, 21, 8, 24, 25))+
scale_size_manual(values=c(1 ,1 ,1 ,1 ,1,1,2,1.5,1.5))+
ggtitle("Net community\nproduction (NCP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
npp_comp

fig2abc <- plot_grid(gpp_comp+guides(col="none", fill="none", shape="none", size="none"), resp_comp, npp_comp, NULL, plot_grid(get_legend(gpp_comp), NULL, ncol=1, rel_heights=c(1, 0.3)),NULL,nrow=1, align="h", axis="lr", rel_widths=c(1,1,1,0.2, 0.3, 0.2), labels=c("a", "b", "c","", ""), label_size=9)
fig2abc


#################################################################################
#################################################################################
#################################################################################



se <- function(x){sd(x)/sqrt(length(x))}

themeX <- theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))

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


avz <- rbind(cbind(avs, t="GPP"),cbind(avs2, t="R"),cbind(avs3, t="NCP"))
avz$t <- factor(avz$t, levels=c("GPP", "R", "NCP")) 

head(avz)

ranks <- ggplot(avz, aes(x=r, y=reorder(dom, - r), fill=dom))+
#geom_bar(stat="identity", col="black", size=0.1)+
geom_segment(aes(x=r-se, xend=r+se, y=dom, yend=dom), size=0.25)+
geom_point(size=2.3, shape=21, stroke=0.25)+
labs( x=expression(μg~O[2]~cm^-2~min^-1))+
#coord_cartesian(xlim=c(0.75, 3))+
#facet_wrap(~t, scales="free_x")+
facet_wrap(~t, scales="free_x", ncol=1)+
scale_fill_manual(values=colsc3)+guides(fill="none")+
theme_classic()+themeX +theme(axis.title.y=element_blank(), axis.text.y=element_text(size=8), plot.title=element_text(size=8, hjust=0.5, face="bold"))
ranks

##############################################################################
####################################################

clong <- melt(df2[,c("dom", "Rcm2", "GPPcm2")], id.var="dom")
head(clong)

ggplot(clong, aes(x=value, y=dom, shape=variable))+geom_point()+geom_path()


avz <- rbind(cbind(avs, t="GPP"),cbind(avs2, t="R"),cbind(avs3, t="NCP"))
avz$t <- factor(avz$t, levels=c("GPP", "R", "NCP")) 

head(avz)


netplot <- ggplot(avz[!avz$t %in% c("NCP"),], aes(x=r, y=reorder(dom, -r), shape=t, group=dom, col=dom))+
geom_line(size=2, alpha=0.5)+
scale_colour_manual(values=colsc4b)+
scale_fill_manual(values=colsc4b)+guides(fill="none", col="none")+
geom_segment(data=avz[!(avz$t %in% c("NCP") | avz$dom %in% c("Sand")),], aes(x=r-se, xend=r+se, y=dom, yend=dom), size=0.25)+
geom_point(aes(fill=dom), col="black", stroke=0.2)+
ggtitle("Area-normalised rates")+
labs( x=expression(μg~O[2]~cm^-2~min^-1))+
scale_x_continuous(breaks=c(1, 2, 3))+
scale_shape_manual(values=c(24,25))+
theme_classic()+theme(axis.title.y=element_blank(), legend.text=element_text(size=7), axis.text=element_text(size=8),plot.title=element_text(size=8, hjust=0.5, face="bold"), 
legend.title=element_blank(), legend.background=element_blank(), legend.key.height=unit(1, "mm"), legend.position=c(0.9, 0.85), axis.title.x=element_text(size=8), axis.line.y=element_blank(), axis.line.x=element_line(size=0.2))
netplot


############################################################
############################################################
##########################

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

Edom$y <- ifelse(Edom$y=="NPP", "NCP", Edom$y)

Edom$y <- factor(Edom$y, levels=c("GPP", "R", "NCP"))

slopeplot1S <- ggplot(Edom[!Edom$dom %in% c("Sand", "Seagrass"),], aes(x=slp, y=reorder(dom, -slp), fill=dom))+
geom_vline(xintercept=0, col="grey", linetype="dotted")+
#geom_vline(xintercept=0, col="grey")+
geom_segment(aes(x=lower, xend=upper, y=dom, yend=dom), size=0.1)+
geom_point(shape=21, col="black", size=2.5)+
scale_fill_manual(values=colsc4)+guides(fill="none")+
facet_wrap(~y)+
xlab("log-log slope of rate against habitat rugosity")+
coord_cartesian(xlim=c(-1, 3))+
theme_classic()+theme(strip.background=element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank(),axis.line.x=element_line(size=0.2), axis.text.y=element_blank(), panel.spacing = unit(2, "lines"))
slopeplot1S

slopeplot1 <- ggplot(Edom[!Edom$dom %in% c("Sand", "Seagrass"),], aes(x=slp, y=reorder(dom, -slp), fill=dom, group=y))+
geom_vline(xintercept=0, col="grey", linetype="dotted")+
#geom_vline(xintercept=0, col="grey")+
geom_errorbar(aes(xmin=lower, xmax=upper, y=dom, col=dom), size=0.2, position = position_dodge(width=0.75), orientation = "y", width=0)+
geom_point(aes(shape=y), col="black", size=1.5, position = position_dodge(width=0.75), stroke=0.2)+
scale_fill_manual(values=colsc4b)+scale_colour_manual(values=colsc4b)+guides(fill="none", col="none")+
#facet_wrap(~y, ncol=1)+
scale_shape_manual(values=c(24, 25, 21))+
xlab("log-log slope (rate vs rugosity)")+
coord_cartesian(xlim=c(-1, 3))+
ggtitle("Scaling exponents")+
theme_classic()+theme(axis.title.y=element_blank(), legend.text=element_text(size=7), axis.text=element_text(size=8),plot.title=element_text(size=8, hjust=0.5, face="bold"), 
legend.title=element_blank(), legend.background=element_blank(), legend.key.height=unit(1, "mm"), legend.position=c(1.1, 0.9), axis.title.x=element_text(size=8), axis.line.y=element_blank(), axis.line.x=element_line(size=0.2), plot.background=element_blank())
slopeplot1


##############################################################################
####################################################



Fig2blank <- plot_grid(
plot_grid(gpp_comp, resp_comp, npp_comp, NULL, plot_grid(get_legend(legplot), NULL, ncol=1, rel_heights=c(1, 0.3)),NULL,nrow=1, align="h", axis="lr", rel_widths=c(1,1,1,0.2, 0.3, 0.2), labels=c("a", "b", "c","", ""), label_size=9),
plot_grid(NULL, netplot, NULL, rel_widths=c(0.5, 1, 0.8), nrow=1, labels=c("", "d", ""),label_size=9),
ncol=1, rel_heights=c(1,0.8))
Fig2blank



Fig2blank2 <- plot_grid(
fig2abc,
plot_grid(NULL, netplot, slopeplot1, NULL, rel_widths=c(0.65, 1,1, 1.1), nrow=1, labels=c("", "d", "e",""), hjust=-8,label_size=9),
ncol=1, rel_heights=c(1,0.8))
Fig2blank2

poc<-readPNG("figs/img/site10b.png")
acr <- readPNG("figs/img/site60.png")
oth <- readPNG("figs/img/site16.png")
por <- readPNG("figs/img/site36.png")
turf <- readPNG("figs/img/site31b.png")
soft <- readPNG("figs/img/site26b.png")

FIG2 <- Fig2blank+
draw_image(poc,  x = -0.015, y = 0.12,  height=0.2, width=0.15)+
draw_line(x=c(0.12, 0.26), y=c(0.245, 0.245), col="grey", size=0.3)+
draw_image(acr,  x = 0.08, y = 0.05,  height=0.19, width=0.2)+
draw_line(x=c(0.255, 0.28), y=c(0.12, 0.12), col="grey", size=0.3)+
draw_image(oth,  x = 0.06, y = 0.23,  height=0.19, width=0.2)+
draw_line(x=c(0.21, 0.225), y=c(0.275, 0.275), col="grey", size=0.3)+
draw_image(por,  x = 0.62, y = 0.035,  height=0.2, width=0.23)+
draw_line(x=c(0.6, 0.67), y=c(0.15, 0.15), col="grey", size=0.3)+
draw_image(turf,  x = 0.75, y = 0.14,  height=0.19, width=0.23)+
draw_line(x=c(0.6, 0.8), y=c(0.22, 0.22), col="grey", size=0.3)+
draw_image(soft,  x = 0.62, y = 0.25,  height=0.18, width=0.2)+
draw_line(x=c(0.565, 0.65), y=c(0.31, 0.31), col="grey", size=0.3)
FIG2 

FIG22 <- Fig2blank2+
draw_image(turf,  x = 0.01, y = 0.12,  height=0.2, width=0.1)+
draw_line(x=c(0.12, 0.23), y=c(0.225, 0.21), col="grey", size=0.3)+
draw_image(acr,  x = 0.09, y = 0.01,  height=0.19, width=0.1)+
draw_line(x=c(0.2, 0.23), y=c(0.1, 0.13), col="grey", size=0.3)+
draw_image(oth,  x = 0.05, y = 0.27,  height=0.19, width=0.1)+
draw_line(x=c(0.15, 0.18), y=c(0.33, 0.3), col="grey", size=0.3)+
draw_image(por,  x = 0.75, y = 0.005,  height=0.2, width=0.1)+
draw_line(x=c(0.69, 0.75), y=c(0.15, 0.12), col="grey", size=0.3)+
draw_image(poc,  x = 0.83, y = 0.13,  height=0.2, width=0.1)+
draw_line(x=c(0.69, 0.83), y=c(0.19, 0.22), col="grey", size=0.3)+
draw_image(soft,  x = 0.8, y = 0.28,  height=0.18, width=0.1)+
draw_line(x=c(0.71, 0.79), y=c(0.28, 0.33), col="grey", size=0.3)
FIG22 


# ggsave( "figs/fig2.jpg",FIG22, height=4.75, width=8)


##############################################################################
####################################################




gpp_comp2 <- ggplot(df2[!df2$dom %in% c("Seagrass", "Sand"),], aes(Rug, GPP))+
geom_point( size=1, aes(shape=region, fill=dom), stroke=0.3)+
geom_smooth(aes(fill=dom), method="lm", size=0.3, col="black")+
labs(x="Habitat rugosity",  y=expression(mg~O[2]~m^-2~min^-1))+
scale_y_log10(limits=c(2,55), breaks=c(4, 13, 22, 31))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
facet_wrap(~dom, nrow=1)+
guides(fill="none")+
scale_fill_manual(values=colsc4b)+scale_colour_manual(values=colsc4b)+
ggtitle("Gross primary production (GPP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
gpp_comp2

resp_comp2 <- ggplot(df2[!df2$dom %in% c("Seagrass", "Sand"),], aes(Rug, R))+
geom_point( size=1, aes(shape=region, fill=dom), stroke=0.3)+
geom_smooth(aes(fill=dom), method="lm", size=0.3, col="black")+
labs(x="Habitat rugosity",  y=expression(mg~O[2]~m^-2~min^-1))+
scale_y_log10(limits=c(1,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
facet_wrap(~dom, nrow=1)+
ggtitle("Respiration (R)")+
guides(fill="none")+
scale_fill_manual(values=colsc4b)+scale_colour_manual(values=colsc4b)+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
resp_comp2


npp_comp2 <- ggplot(df2[!df2$dom %in% c("Seagrass", "Sand"),], aes(Rug, NPP))+
geom_point( size=1, aes(shape=region, fill=dom), stroke=0.3)+
geom_smooth( aes(fill=dom), method="lm", size=0.3, col="black")+
labs(x="Habitat rugosity",  y=expression(mg~O[2]~m^-2~min^-1))+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
facet_wrap(~dom, nrow=1)+
guides(fill="none")+
scale_fill_manual(values=colsc4b)+scale_colour_manual(values=colsc4b)+
ggtitle("Net community production (NCP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
npp_comp2


plot_grid(gpp_comp2, resp_comp2, npp_comp2, ncol=1)


############################################################
############################################################

library(RColorBrewer)
pcols <- brewer.pal(n = 3, name = "Dark2")

df2$site[df2$site=="sand"] <- c("sand1", "sand2")

dfx <- melt(df2[, c("NPP", "R", "Rug", "site", "region")], id.var=c("Rug", "site", "region"))
dfx$GPP <- df2$GPP[match(dfx$site, df2$site)]

plot_grid(
ggplot(dfx, aes(reorder(site, Rug), value, fill=variable))+geom_bar(stat="identity", width=0.7)+
theme_classic()+
theme(axis.text.x=element_blank(), strip.background=element_blank(), legend.title=element_blank(), 
legend.position=c(0.1, 0.8))+
scale_y_continuous(expand=c(0,0))+
labs(x="Site ranked by rugosity", y="Overall rate")+
scale_fill_manual(values=pcols[c(1,2)])+
#scale_fill_manual(values=c("darkgreen", "red"))+
facet_grid(.~region, scales="free_x", space="free")
,
ggplot(dfx, aes(reorder(site, Rug), value/GPP, fill=variable))+geom_bar(stat="identity", width=0.7)+
theme_classic()+
guides(fill="none")+
scale_y_continuous(expand=c(0,0))+
labs(x="Site ranked by rugosity", y="Rate as proportion of GPP")+
scale_fill_manual(values=pcols[c(1,2)])+
theme(axis.text.x=element_blank(), strip.background=element_blank())+
facet_grid(.~region, scales="free_x", space="free")
,
 ncol=1)


############################################################
############################################################
##########################

Edom2 <- NULL
dom.use <- unique(df2$dom)[!unique(df2$dom)%in%c("Sand", "Seagrass")]
for(i in dom.use){
	for(y in c("GPP", "NPP", "R")){
#	i <- "Acropora"
dat <- df2[df2$dom %in% i,]	
dat$y <-  dat[,y]
dmod <- lm(log(y)~log(Volcm3), dat)	
slp <- coef(dmod)[2][1]
upper <- confint(dmod)[2,2]
lower <- confint(dmod)[2,1]
Edom2 <- rbind(Edom2, data.frame(dom=i, slp, y, upper, lower))	
}}
Edom2

Edom2$y <- ifelse(Edom2$y=="NPP", "NCP", Edom2$y)

slopeplot2 <- ggplot(Edom2[!Edom2$dom %in% c("Sand", "Seagrass"),], aes(x=slp, y=reorder(dom, -slp), fill=dom))+
geom_vline(xintercept=1, col="grey", linetype="dotted")+
geom_vline(xintercept=0, col="grey")+
geom_segment(aes(x=lower, xend=upper, y=dom, yend=dom))+
geom_point(shape=21, col="black", size=2.5)+
scale_fill_manual(values=colsc)+guides(fill="none")+
facet_wrap(~y)+
xlab("log-log slope of rate against habitat volume")+
coord_cartesian(xlim=c(-2, 3))+
theme_classic()+theme(strip.background=element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())
slopeplot2 

plot_grid(slopeplot1, slopeplot2, ncol=1, labels=c("a", "b"))

ggplot(df2, aes(Rug, Volcm3))+geom_point()+geom_smooth()

summary(lm(log(Volcm3)~log(Rug), df2[!df2$dom %in% c("Sand"),]))

ggplot(NULL, aes(c(1:100), c(1:100)^0.5))+geom_point()

#[!Edom$dom %in% "Seagrass",]


############################################################
############################################################
##########################

Edom3 <- NULL
dom.use <- unique(df2$dom)[!unique(df2$dom)%in%c("Sand", "Seagrass")]
for(i in dom.use){
	for(y in c("Volcm3")){
#	i <- "Acropora"
dat <- df2[df2$dom %in% i,]	
dat$y <-  dat[,y]
dmod <- lm(log(y)~log(Rug), dat)	
slp <- coef(dmod)[2][1]
upper <- confint(dmod)[2,2]
lower <- confint(dmod)[2,1]
Edom3 <- rbind(Edom3, data.frame(dom=i, slp, y, upper, lower))	
}}
Edom3

Edom3$y <- ifelse(Edom3$y=="NPP", "NCP", Edom3$y)

slopeplot3 <- ggplot(Edom3[!Edom3$dom %in% c("Sand", "Seagrass"),], aes(x=slp, y=reorder(dom, -slp), fill=dom))+
geom_vline(xintercept=1, col="grey", linetype="dotted")+
geom_vline(xintercept=0, col="grey")+
geom_segment(aes(x=lower, xend=upper, y=dom, yend=dom))+
geom_point(shape=21, col="black", size=2.5)+
scale_fill_manual(values=colsc)+guides(fill="none")+
facet_wrap(~y)+
xlab("log-log slope of volume against habitat rugosity")+
#coord_cartesian(xlim=c(-2, 3))+
theme_classic()+theme(strip.background=element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y=element_blank())
slopeplot3

plot_grid(slopeplot1, slopeplot2, slopeplot3, ncol=1, labels=c("a", "b", "c"))

############################################################
############################################################

### LIGHT TEMP


ggplot(df2, aes(parPS, NPP/SAcm2))+
geom_point( aes(shape=region))+
geom_smooth(data=df[!df$dom %in% c("Seagrass"),], method="lm", se=F, size=0.7)+
#geom_point(data=hi, aes(x=site_rugosity, y=GPP), shape=21)+
#stat_ellipse(aes(col=dom))+
#scale_colour_manual(values=colsc)+
#geom_text_repel(data=df2, aes(label=site), size=2,min.segment.length = unit(0, 'lines'))+
scale_y_log10()+
scale_x_log10()+
#facet_wrap(~dom, nrow=1)+
theme_classic()+theme(strip.background=element_blank())

library("reshape2")

light <- melt(df2[,c("site","parPS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "parPS", "region", "SAcm2"))
head(light)
light$variable <- ifelse(light$variable =="NPP", "NCP",as.character(light$variable))
light$variable <- factor(light$variable, levels=c("GPP", "R", "NCP"))

lbox <- ggplot(df2, aes(y=region, x=parPS))+geom_boxplot(outlier.size=0.1, size=0.3, fill="grey90")+
geom_jitter(shape=21, height=0.2, width=0, size=0.2, col="grey")+
theme_classic()+theme(axis.line=element_blank(), axis.title=element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank())+scale_x_log10()

ggplot(df2, aes( x=parPS))+
geom_density(outlier.size=0.1, size=0.3, fill="grey90")+
facet_wrap(~region, ncol=1, scales="free_y")+
theme_classic()+theme(axis.line=element_blank(), axis.title=element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank())+scale_x_log10()


lplot <- ggplot(light, aes(parPS, (value/SAcm2)*1000, col=variable))+
geom_point(shape=21, size=0.5)+
labs(x="Light intensity (lux)",  y=expression(Rate~"("*μg~O[2]~cm^-2~min^-1*")"))+
#scale_y_sqrt()+
scale_x_log10()+
#scale_shape_manual(values=c(16, 4))+
geom_smooth( method="lm", se=F,size=0.5)+
#scale_colour_manual(values=pcols[c(1,2,3)])+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
#facet_wrap(~region)+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_text(size=9), axis.title.y=element_text(size=9, lineheight = 0.1), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2))
lplot

lplot2 <- plot_grid(lbox, lplot+guides(col="none"), ncol=1, rel_heights=c(0.2, 1), align="v", axis="lr")
lplot2

temp <- melt(df2[,c("site","tempO2_PS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "tempO2_PS", "region", "SAcm2"))
temp2 <- melt(df2[,c("site","tempHOBO_PS", "region", "R", "GPP", "NPP", "SAcm2")], id.var=c("site", "tempHOBO_PS", "region", "SAcm2"))
temp$id <- paste(temp$site, temp$variable)
temp2$id <- paste(temp2$site, temp2$variable)
temp$hobo <- temp2$tempHOBO_PS[match(temp$id, temp2$id)]
temp$temp <- rowMeans(temp[,c("hobo", "tempO2_PS")])
temp$variable <- ifelse(temp$variable =="NPP", "NCP",as.character(temp$variable))
temp$variable <- factor(temp$variable, levels=c("GPP", "R", "NCP"))


df2$temp <- rowMeans(df2[,c("tempO2_PS", "tempHOBO_PS")])
tbox <- ggplot(df2, aes(y=region, x=temp))+geom_boxplot(outlier.size=0.1, size=0.3, fill="grey90")+
geom_jitter(shape=21, height=0.2, width=0, size=0.2, col="grey")+
theme_classic()+theme(axis.line=element_blank(), axis.title=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank())


tplot <- ggplot(temp, aes(temp, (value/SAcm2)*1000, col=variable))+
geom_point(shape=21, size=0.5)+
#scale_y_sqrt()+
#scale_x_log10()+
geom_smooth(method="lm", se=F, formula=y~poly(x, 2), size=0.5)+
labs(x="Temperature (°C)",  y=expression(Rate~"("*μg~O[2]~cm^-2~min^-1*")"))+
#scale_shape_manual(values=c(16, 4))+
#facet_wrap(~region)+
#scale_colour_manual(values=c("darkgreen", "red", "black"))+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_text(size=9), axis.title.y=element_text(size=9, lineheight = 0.1), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2))
tplot

tplot2 <- plot_grid(tbox, tplot+guides(col="none"), ncol=1, rel_heights=c(0.2, 1), align="v", axis="lr")
tplot2

plot_grid(lplot2, tplot2, get_legend(tplot), nrow=1, rel_widths=c(1,1,0.3))

#bquote(atop("Area-normalised rate",  "(" ~ μg~O[2]~cm^-2~min^-1~")"))



#################################################################
#################################################################

# effect sizes

hist(scale(log(df2$Rug)))

esizes <- NULL
	for(j in c("NPP", "GPP", "R", "NPPcm2", "Rcm2", "GPPcm2")){
	#j <- "NPP"
df2$y <- log(df2[,j])
mod <- lm(y~scale(log(Rug))+scale(log(parPS))+scale(temp+ I(temp^2)), df2)
summary(mod)
#coef(mod)
upper <- confint(mod)[,2]
lower <- confint(mod)[,1]
esizes <- rbind(esizes, data.frame(j, pred =c("int", "Habitat rugosity", "Light intensity", "Temperature"),slp=coef(mod), upper, lower))
}
esizes
esizes$norm <- ifelse(esizes$j %in% c("GPP", "NPP", "R"), "Total", "Normalised")
esizes$j2 <- gsub("cm2", "",esizes$j)
esizes$j2 <- factor(esizes$j2 , levels=c("GPP", "R", "NPP"))
esizes$pred <- factor(esizes$pred , levels=c("Habitat rugosity", "Light intensity", "Temperature", "int"))

e1 <- ggplot(esizes[!esizes$pred=="int",], aes(pred, slp))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65)+
#geom_point(position=position_dodge)+
#scale_fill_manual(values=c("darkgreen", "red", "grey20"))+
scale_fill_manual(values=c("black", pcols[c(2,1)]))+
geom_linerange(aes(xmin=pred, xmax=pred, ymin=upper, ymax=lower, group=j2), position=position_dodge(width=0.65))+
theme_classic()+
facet_wrap(~norm)+
theme(axis.text.x=element_text(angle=45, hjust=1))
e1


esizes2 <- NULL
	for(j in c("NPP", "GPP", "R", "NPPcm2", "Rcm2", "GPPcm2")){
	#j <- "NPP"
df2$y <- log(df2[,j])
modR <- lm(y~scale(log(Rug)), df2)
slpR <- coef(modR)[2]
upperR <- confint(modR)[2,2]
lowerR <- confint(modR)[2,1]
modL <- lm(y~scale(log(parPS)), df2)
slpL <- coef(modL)[2]
upperL <- confint(modL)[2,2]
lowerL <- confint(modL)[2,1]
modT <- lm(y~scale(temp+I(temp^2)), df2)
slpT <- coef(modT)[2]
upperT <- confint(modT)[2,2]
lowerT <- confint(modT)[2,1]
esizes2 <- rbind(esizes2, data.frame(j, pred =c("Habitat rugosity", "Light intensity", "Temperature"),slp=c(slpR, slpL, slpT), upper=c(upperR, upperL, upperT), lower=c(lowerR, lowerL, lowerT)))
}
esizes2
esizes2$norm <- ifelse(esizes2$j %in% c("GPP", "NPP", "R"), "Total", "Normalised")
esizes2$j2 <- gsub("cm2", "",esizes2$j)
esizes2$j2 <- factor(esizes2$j2 , levels=c("GPP", "R", "NPP"))
esizes2$pred <- factor(esizes2$pred , levels=c("Habitat rugosity", "Light intensity", "Temperature"))

e2 <- ggplot(esizes2, aes(pred, slp))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65)+
#geom_point(position=position_dodge)+
scale_fill_manual(values=c("darkgreen", "red", "grey20"))+
geom_linerange(aes(xmin=pred, xmax=pred, ymin=upper, ymax=lower, group=j2), position=position_dodge(width=0.65))+
theme_classic()+
facet_wrap(~norm)+
theme(axis.text.x=element_text(angle=45, hjust=1))
e2

plot_grid(e1, e2)


e3 <- ggplot(esizes[ esizes$pred %in% c("Habitat rugosity", "Light intensity", "Temperature"),], aes(pred, slp))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65, col="black", size=0.1)+
#geom_point(position=position_dodge)+
scale_fill_manual(values=c("darkgreen", "red", "grey20"))+
geom_linerange(aes(xmin=pred, xmax=pred, ymin=upper, ymax=lower, group=j2), position=position_dodge(width=0.65))+
theme_classic()+
facet_wrap(~norm)+
labs(x="Predictor", y= "Standardised effect size")+
theme(axis.text.x=element_text(angle=45, hjust=1), legend.title=element_blank(), strip.background=element_blank())
e3

e3+coord_flip()

plot_grid(get_legend(tplot), lplot2, tplot2, e3+guides(fill="none"), nrow=1, rel_widths=c(0.2, 1,1,0.7))

plot_grid(plot_grid(lplot2, tplot2, get_legend(tplot), nrow=1, rel_widths=c( 1,1,0.2)), 
e3+coord_flip()+guides(fill="none"), ncol=1, rel_heights=c(1,0.6))


e4 <- ggplot(esizes[esizes$norm %in% "Total" & esizes$pred %in% c("Habitat rugosity", "Light intensity", "Temperature"),], aes(pred, slp))+
geom_hline(yintercept=0)+
#geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65, col="black", size=0.1)+
scale_fill_manual(values=c("darkgreen", "red", "black"))+
geom_linerange(aes(xmin=pred, xmax=pred, ymin=upper, ymax=lower, group=j2), position=position_dodge(width=0.65), size=0.2)+
geom_point(position=position_dodge(width=0.65), shape=21, aes(fill=j2), stroke=0.2, size=2)+
theme_classic()+
#facet_wrap(~norm)+
labs(x="Predictor", y= "Standardised\neffect size")+
theme(axis.title=element_text(size=8), axis.line=element_line(size=0.2))
e4

summary(lm(log(Rcm2)~poly(temp,2), df2))
summary(lm(log(NPPcm2)~poly(temp,2), df2))
summary(lm(log(GPPcm2)~poly(temp,2), df2))

summary(lm(log(Rcm2)~log(parPS), df2))
summary(lm(log(NPPcm2)~log(parPS), df2))
summary(lm(log(GPPcm2)~log(parPS), df2))


ggplot(temp, aes(temp, (value/SAcm2)*1000, col=variable))+
geom_point(shape=21, size=0.5)+
geom_smooth(method="lm", se=F, formula=y~poly(x, 1), size=0.5)

plot_grid(NULL, get_legend(tplot),NULL, lplot2, tplot2, e4+guides(fill="none")+theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8, angle=25, hjust=1)), nrow=1, rel_widths=c(0.1, 0.2, 0.05,1,1,0.8))

plot_grid(lplot2, tplot2, plot_grid(get_legend(tplot),  e4+coord_flip()+guides(fill="none")+theme(axis.title.y=element_blank()), ncol=1, rel_heights=c(0.4,1)), nrow=1, rel_widths=c( 1,1,1))

plot_grid(plot_grid(lplot2, tplot2,  nrow=1, rel_widths=c( 1,1)), 
plot_grid(e4+coord_flip()+guides(fill="none"),get_legend(tplot),rel_widths=c(1,0.3)), ncol=1, rel_heights=c(1,0.5))

test <- data.frame(x=c(-100:100))
test$y <- test$x + -test$x^2  + 100^2
test$y2 <- test$x + (-2*test$x^2) + 100^2
test$y3 <-( -10*test$x) + (-test$x^2) + 100^2
ggplot(data=test)+
geom_point(aes(x,y))+
geom_point(aes(x,y2), col="red")+
geom_point(aes(x,y3), col="green")



# exclusion / cohens f2
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
esizes3
esizes3$sig <- ifelse(esizes3$pval>0.1, "", ifelse(esizes3$pval<=0.05 & esizes3$pval>0.01, "*", ifelse(esizes3$pval<=0.01 & esizes3$pval>0.001, "**", ifelse(esizes3$pval<=0.001, "***", ifelse(esizes3$pval <=0.1 & esizes3$pval>0.05, ".", NA)))))
esizes3$AB_A <- esizes3$rsqF - esizes3$rsq
esizes3$unexpl <- 1 - esizes3$rsqF
esizes3$f2 <- esizes3$AB_A / esizes3$unexpl
esizes3
esizes3$norm <- ifelse(esizes3$j %in% c("GPP", "NPP", "R"), "Total", "Normalised")
esizes3$j2 <- gsub("cm2", "",esizes3$j)
esizes3$j2 <- factor(esizes3$j2 , levels=c("GPP", "R", "NPP"))
esizes3$pred <- factor(esizes3$pred , levels=c("Habitat rugosity", "Light intensity", "Temperature"))

esizes3[esizes3$pred=="Light intensity",]

ggplot(esizes3, aes(pred, f2))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65, col="black", size=0.1)+
scale_fill_manual(values=c("darkgreen", "red", "black"))+
facet_wrap(~norm, ncol=1)

e5 <- ggplot(esizes3[esizes3$norm=="Total",], aes(pred, f2))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65, col="black", size=0.1)+
geom_text(data=esizes3[esizes3$norm=="Total" & esizes3$pred=="Temperature",], aes(label=sig, x=pred, y=f2+0.1, group=j2), position=position_dodge(width=0.65), size=4)+
geom_text(data=esizes3[esizes3$norm=="Total" & !esizes3$pred=="Temperature",], aes(label=sig, x=pred, y=f2+0.1, group=j2), position=position_dodge(width=0.65), angle=90, hjust=0.4, vjust=0.82, size=3)+
#scale_fill_manual(values=c("darkgreen", "red", "black"))+
scale_fill_manual(values=c("black", pcols[c(2,1)]))+
#facet_wrap(~norm, ncol=1)+
labs(x="Predictor", y= expression("Cohen's"~f~""^2))+
scale_y_continuous(expand=c(0,0), limits=c(0, 1.8))+
theme_classic()+
ggtitle("Standardised\neffect sizes")+
theme(axis.title=element_text(size=8), axis.line=element_line(size=0.2),axis.title.x=element_blank(), axis.text.x=element_text(size=8, angle=25, hjust=1), plot.title=element_text(size=8, hjust=0.5))
e5

#################################################################
#################################################################
# FIG 3 


 plot_grid(NULL, get_legend(tplot),NULL, lplot2, tplot2, e5+guides(fill="none"), nrow=1, rel_widths=c(0.05, 0.2, 0.05,1,1,0.7), labels=c("","","","a", "b", "c"), label_size=9, vjust=c(0,0,0,7,7,5))


FIG3 <- plot_grid( lplot2, tplot2, e5+guides(fill="none"),NULL, get_legend(tplot),NULL, nrow=1, 
rel_widths=c(1,1,0.7,0.05, 0.2, 0.05), labels=c("a","b","c","", "", ""), label_size=9, vjust=c(7,7,5,0,0,0))
FIG3

# ggsave( "figs/fig3.jpg",FIG3, height=3.5, width=7.5)



#################################################################
#################################################################
# models

library("MuMIn")

df2[,c("dominant", "cover", "Rug")]

# previous
# no sand in 2Dcover

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


#mod1 <- lm(log(R)~log(Rug)+dominant+log(parPS)+poly(temp,2), data=df2)
#summary(mod1)



mod1 <- lm(log(GPP)~log(Rug)+dominant+log(parPS)+poly(temp,2)+as.numeric(depth), data=df2)
summary(mod1)


mod1 <- lm(log(NPP)~log(Rug)+dominant+log(parPS)+poly(temp,2)+as.numeric(depth), data=df2)
summary(mod1)


#################################################################
#################################################################
# allometry model ... probably 1

summary(lm(log(SAcm2)~log(Rug), df2))

head(df2)
nrow(df2)


circ <- pi * ((100/2)^2)
test <- data.frame(diam = c(1:100))
test$PA <- test$diam^2
test$SA <- test$PA * 5
test$SAtot <- test$SA + (circ - test$PA)
test$RugSite <- test$SAtot/circ
test$RugReef <- test$SA/test$PA

summary(lm(log(SAtot)~log(RugSite), test))
ggplot(test, aes(RugSite, SAtot))+geom_point()+
scale_y_log10()+scale_x_log10() # isometric is 1 when whole (same size) site included, more reef, more rugosity

summary(lm(log(SA)~log(RugReef), test))
ggplot(test, aes(RugReef, SAtot))+geom_point()+ 
scale_y_log10()+scale_x_log10()


# 1, 2 3, 4, 5 cubes... 
circ <- pi * ((100/2)^2)
cubediam <- 10
cubePA <- cubediam^2
cubeSA <- cubePA*5
test <- data.frame(ncubes =c(1:10))
test$reefPA <- cubePA * test$ncubes
test$reefSA <- cubeSA * test$ncubes
test$siteSA <- (circ - test$reefPA) + test$reefSA
test$siteRug <-  test$siteSA / circ
test$reefRug <-  test$reefSA / test$reefPA

summary(lm(log(siteSA)~log(siteRug), test))
ggplot(test, aes(siteRug, siteSA))+geom_point()+ 
scale_y_log10()+scale_x_log10()

summary(lm(log(reefSA)~log(reefRug), test))
ggplot(test, aes(reefRug, reefSA))+geom_point()+ 
scale_y_log10()+scale_x_log10()

# increaseing site PA captures more cubes

test <- data.frame(sitediam = c(1:100))
test$sitePA <- test$sitediam^2
test$ncubes <- (test$sitePA ) / 2 # 1 cube per 2m2
cubediam <- 1.1
cubePA <- cubediam^2
cubeSA <- cubePA*5
test$reefPA <- cubePA * test$ncubes
test$reefSA <- cubeSA * test$ncubes
test$siteSA <- (test$sitePA - test$reefPA) + test$reefSA
test$siteRug <-  test$siteSA / test$sitePA
test$reefRug <-  test$reefSA / test$reefPA

# what if metabolism scales with volume? 

test <- data.frame(sitediam = c(1:100))
test$PA <- test$sitediam^2
test$SA <- test$PA * 6
test$Vol <- test$sitediam^3
#test$Rug <- test$SA/test$PA

summary(lm(log(Vol)~log(SA), test))
ggplot(test, aes(SA, Vol))+geom_point()+ 
scale_y_log10()+scale_x_log10()


summary(lm(log(Rcm2)~log(Rug), df2))



#################################################################
#################################################################
# allometry

df2$vol1 <- df2$Volcm3 + 1

# site 46 vol off. 

voldf <- df2[!df2$dom %in% c("Sand", "Seagrass"),]
voldf <- voldf[!voldf$site %in% c("site46"),]



ggplot(voldf, aes(vol1, GPP))+
geom_point(aes( col=dom))+
geom_smooth(aes(fill=region), method="lm", formula=y~poly(x,1))+
#geom_text(aes(label=site))+
scale_y_log10()+scale_x_log10()


ggplot(voldf, aes(vol1, GPP))+
geom_point(aes( col=dom))+
geom_smooth(aes(fill=region), method="lm")+
#geom_text(aes(label=site))+
scale_y_log10()+scale_x_log10()


ggplot(voldf, aes(vol1, R))+
geom_point(aes( col=dom))+
geom_smooth(aes(fill=region), method="lm")+
scale_y_log10()+scale_x_log10()

ggplot(voldf, aes(vol1, NPP))+
geom_point(aes( col=dom))+
geom_smooth(aes(fill=region), method="lm")+
scale_y_log10()+scale_x_log10()


summary(lm(log(GPP)~log(Volcm3+1), data=voldf))

summary(lm(log(R)~log(Volcm3), data=voldf))

summary(lm(log(NPP)~log(Volcm3+1), data=voldf))



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
scaling <- rbind(scaling, rbind(full, gbr, hi))}}

scaling
scaling$j2 <- ifelse(scaling$j=="NPP", "NCP", as.character(scaling$j))
scaling$j2 <- factor(scaling$j2, levels=c("GPP", "R", "NCP"))


df2$Volcm3



expo <- ggplot(scaling, aes(x=loc, y=log.slp, col=j2))+
geom_hline(yintercept=1, linetype="dotted", col="grey")+
geom_point(position=position_dodge(width=0.5))+
geom_linerange(aes(xmin=loc, xmax=loc, ymin=log.ci1, ymax=log.ci2, col=j2), position=position_dodge(width=0.5))+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
#ggtitle("scaling exponents")+
#facet_wrap(~i)+
labs(y="log-log slope\nagainst rugosity")+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_blank(), axis.title.y=element_text(size=9), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2), legend.key.width=unit(1,"mm"),)
expo

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

long2 <- melt(df2[,c("NPP", "R", "GPP", "Rug", "region")], id.var=c("Rug", "region"))
long2$variable <- ifelse(long2$variable=="NPP", "NCP", as.character(long2$variable ))
long2$loc <- long2$region

lpreds <- lpreds[!(lpreds$loc=="Hawaii" & lpreds$Rug > log(4)), ]

scaleplot <- ggplot(lpreds[!lpreds$loc=="Total",], aes(x=exp(Rug), y=exp(value), col=variable))+
geom_point(data=long2, aes(Rug, value, col=variable), size=0.1, shape=21, alpha=0.35)+
geom_line()+
facet_wrap(~loc, ncol=2, scales="free")+
scale_x_continuous(limits=c(0.85, 5.7), breaks=c(1, 2, 3, 4, 5))+
#scale_colour_manual(values=c("darkgreen", "red", "black"))+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
#ggtitle("scaling\nrelationships")+
labs(x="Habitat rugosity", y=expression(Rate~"("*mg~O[2]~m^-2~min^-1*")"))+
theme_classic()+theme(strip.background=element_blank(),axis.title.x=element_text(size=9), axis.title.y=element_text(size=9), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"), plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2), strip.text=element_blank())
scaleplot

# strip.text=element_text(size=8,hjust=0.5), 

max(df2$Rug)

#################################################################
#################################################################
# CUE

############################################################

df$CUE <- df$NPP / df$GPP
df2$CUE <- df2$NPP / df2$GPP
cueplot <- ggplot(df2[!df2$dom %in% c("Sand"),], aes(Rug, CUE))+
geom_point( aes(shape=region, col=region), size=1)+
geom_smooth(data=df2, method="lm",  col="black", size=0.5, se=F)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm",  linetype="solid", size=0.5, col="red", se=F)+
labs(x="Habitat rugosity",  y="CUE")+
scale_y_continuous(limits=c(0.15, 0.95))+
#scale_x_sqrt(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
#guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
#facet_wrap(~region)+
#ggtitle("Carbon use efficiency (CUE)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8),  legend.text=element_text(size=7), 
legend.title=element_blank(), legend.background=element_blank(), legend.key.height=unit(1, "mm"))
cueplot



df2$hab <- ifelse(df2$dom=="Seagrass", "Seagrass", ifelse(df2$dom=="Sand", "Sand", ifelse(df2$dom=="Algal turf", "Algal Turf", ifelse(df2$dom=="Soft coral", "Soft Coral", "Hard Coral"))))

check <- df[df$region=="GBR", ]
check <- check[!check$dom %in% c("Sand"), ]


# branching /massive/turf/seagreass
df2$simp <- ifelse(
df2$dom=="Seagrass", "Seagrass", ifelse(
df2$dom=="Algal turf", "Algal turf", ifelse(
df2$dom=="Sand", "Sand", ifelse(
df2$dom=="Soft coral", "Soft coral", ifelse(
df2$dom=="Acropora", "Branching coral", ifelse(
df2$dom=="Pocillopora", "Branching coral", ifelse(
df2$dom=="Other scleractinia", "Non-branching coral", ifelse(
df2$dom=="Porites" & df2$region=="GBR", "Non-branching coral", ifelse(
df2$dom=="Porites" & df2$region=="Hawaii", "Branching coral", ifelse(
df2$dom=="Montipora" & df2$region=="GBR", "Non-branching coral", ifelse(
df2$dom=="Montipora" & df2$region=="Hawaii", "Branching coral", NA)))))))))))

df2[,c("dom", "simp")]

df2$simp2 <- ifelse(df2$simp %in% c("Branching coral", "Algal turf", "Sand"), df2$simp, "Other")

simpcol <- c("#6CA167FF", "#ae017e", "grey", "#B79E4BFF", "aquamarine", "white")
names(simpcol)<- unique(df2$simp)[order(unique(df2$simp))]

colsc4

df$simp <- df2$simp[match(df$site, df2$site)]

df.use <- df2

CUEcomp <- ggplot(df.use, aes(Rug, CUE))+
geom_smooth(data=df.use[!(df.use$simp %in% c("Soft coral", "Seagrass", "Sand")),], aes(col=simp, linetype=region), method="lm",  size=0.4, se=F, formula=y~poly(x, 1), show.legend=F)+
geom_point( aes( fill=simp, shape=region), stroke=0.1, size=1.4)+
geom_point( aes( col=simp, shape=region), stroke=0.1, size=1.3)+
#geom_point( aes(shape=region), stroke=0.1, size=1.5, col="black", fill=NA)+
scale_shape_manual(values=c(21, 24))+
#geom_text(aes(label=site), size=2)+
scale_colour_manual(values=simpcol)+
scale_fill_manual(values=simpcol)+
#scale_colour_manual(values=colsc4b)+
#scale_colour_viridis()+
#facet_wrap(~region)+
#scale_x_sqrt()+
labs(x="Habitat rugosity",  y="CUE")+
scale_y_continuous(limits=c(0.15, 0.95))+
#scale_x_sqrt(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_x_log10(limits=c(0.9, 7), breaks=c(1, 2, 3, 4, 5))+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"),  
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8), 
 legend.text=element_text(size=7), 
legend.title=element_blank(), legend.background=element_blank(), legend.key.height=unit(1, "mm"))
CUEcomp


summary(lm(NPP/GPP~log(Rug), data=df2))

summary(lm(NPP/GPP~log(Rug), data=df2[df2$simp=="Branching coral",]))



summary(lm(NPP/GPP~sqrt(Rug), data=df[df$region=="Hawaii",]))




#df2$br <- df2$dom
#df2$br <- ifelse(df2$br %in% c("Acropora","Pocillopora"), "br", )

cuemods <- NULL
for(j in unique(df2$region)){
	sub <- df2#[df2$region==j,]
for(i in unique(sub$simp)[!unique(sub$simp) %in% c("Sand", "Seagrass", "Algal turf")]){
	#i <- "Acropora"
	dat <- sub[sub$simp %in% i, ]
	mod <- lm(CUE~sqrt(Rug), dat)
	conf <- confint(mod)
	slp <- coef(mod)[2]
cuemods <- rbind(cuemods, data.frame(i,j, slp, upp=conf[2,2], low=conf[2,1]))	
}}
cuemods

ggplot(cuemods, aes(x=slp))+
geom_density()+
geom_jitter(aes(x=slp, y=0, col=j), width=0, height=0.1)

ggplot(cuemods, aes(i, slp))+geom_point()+
geom_errorbar(aes(x=i, ymin=low, ymax=upp), width=0.1)+
facet_wrap(~j)+
coord_flip()

plot_grid(
plot_grid(expo+guides(col="none"), NULL, cueplot, ncol=1, rel_heights=c(0.5, 0.1, 1), labels=c("a", "c"), label_size=9),
plot_grid(plot_grid(NULL, get_legend(expo+theme(legend.direction="horizontal")), rel_widths=c(0.2,1)), scaleplot+guides(col="none"), ncol=1, rel_heights=c(0.2, 1), labels=c("", "b"), label_size=9, vjust=4, hjust=-5), 
rel_widths=c(1, 0.65))




plot_grid(
plot_grid(expo+guides(col="none"),  scaleplot+guides(col="none")+ggtitle(""),  ncol=1, rel_heights=c(1,  2)),
plot_grid(
plot_grid(cueplot+ggtitle(""), get_legend(CUEcomp), rel_widths=c(1, 0.5)),
plot_grid(CUEcomp+ggtitle("")+guides(fill="none"), get_legend(CUEcomp), rel_widths=c(1, 0.5)), ncol=1), 
nrow=1, rel_widths=c(1, 2))


plot_grid(
plot_grid(expo+guides(col="none"), scaleplot+guides(col="none")+ggtitle(""), rel_widths=c(1, 1.2)),
plot_grid(cueplot+guides(col="none"), CUEcomp+guides(fill="none")+ggtitle("")),
nrow=2
, rel_heights=c(1, 1.5))


plot_grid(NULL, 
plot_grid(
plot_grid(expo, NULL, scaleplot+guides(col="none")+theme(strip.text=element_blank()), ncol=1, rel_heights=c(1,0.,  1.7)),
plot_grid(cueplot+guides(col="none", shape="none"), CUEcomp+guides(fill="none", shape="none")),
plot_grid(get_legend(CUEcomp)),
nrow=1, rel_widths=c(1, 1.8, 0.5)), 
ncol=1, rel_heights=c(0.1, 1))


ggplot(df2, aes(Rug, CUE, fill=simp))+geom_point(shape=21)+theme(legend.position="bottom", legend.key.size=unit(0, "mm"), 
legend.spacing.y = unit(0, 'mm'), legend.background=element_blank())+guides(fill=guide_legend(ncol=2))


CUEcomp+theme(legend.position="bottom", legend.key.size=unit(0, "mm"), 
legend.spacing.x = unit(1, 'mm'), legend.background=element_blank())+guides(fill=guide_legend(ncol=2))


plot_grid(NULL, 
plot_grid(
plot_grid(expo, NULL, scaleplot+guides(col="none")+theme(strip.text=element_blank()), ncol=1, rel_heights=c(1,0.,  1.7)),
plot_grid(cueplot+theme(legend.position="bottom"), 
CUEcomp+theme(legend.position="bottom", legend.key.size=unit(0, "mm"), 
legend.spacing.x = unit(1, 'mm'), legend.background=element_blank())+guides(fill=guide_legend(ncol=2))),
nrow=1, rel_widths=c(1, 1.8, 0.5)), 
ncol=1, rel_heights=c(0.1, 1))

FIG3 <- plot_grid(NULL,
plot_grid(
get_legend(expo+theme()),
plot_grid(expo+guides(col="none"), scaleplot+guides(col="none"), ncol=1, align="hv", axis="lr", labels=c("a", "b"), label_size=9, vjust=c(0,-2)),
NULL,
CUEcomp+guides(col="none", fill="none", shape="none"), 
get_legend(CUEcomp),
NULL,
labels=c("", "", "c", ""), label_size=9,
nrow=1, rel_widths=c(0.3, 1, 0.05, 1.2, 0.4, 0.1)),
ncol=1, rel_heights=c(0.1, 1))+
draw_text("Scaling exponents", x=0.3, y=0.95, size=8, fontface="bold")+
draw_text("Carbon use efficiency (CUE)", x=0.67, y=0.95, size=8, fontface="bold")
FIG3

ggsave("figs/Fig3.jpg", FIG3, height=3.5, width=7)




cue_compSupX <- ggplot(df2, aes(Rug, NPP/GPP))+
geom_point(size=1, aes(shape=region, fill=dom), stroke=0.2)+
geom_point(size=1, aes(shape=region, col=dom), stroke=0.2)+
geom_smooth(aes(col=dom), method="lm" ,se=F, size=0.5, show.legend=F)+
#facet_wrap(~dom, nrow=1)+
#ggtitle("Carbon use efficency (CUE)")+
theme_classic()+
#guides(col="none")+
scale_fill_manual(values=colsc4b)+scale_colour_manual(values=colsc4b)+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
labs(x="Habitat rugosity",  y="CUE")+
theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8), legend.title=element_blank(), legend.key.size=unit(1, "mm"))
cue_compSupX


cue_compSup <- ggplot(df2[!df2$dom %in% c("Seagrass", "Sand"),], aes(Rug, NPP/GPP))+
geom_point(size=1, aes(shape=region, fill=dom), stroke=0.3)+
geom_smooth(aes(fill=dom), method="lm" , size=0.3, col="black")+
facet_wrap(~dom, nrow=1)+
ggtitle("Carbon use efficency (CUE)")+
theme_classic()+
guides(fill="none")+
scale_fill_manual(values=colsc4b)+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
labs(x="Habitat rugosity",  y="CUE")+
theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
cue_compSup

plot_grid(cue_compSup, plot_grid(cue_compSupX, NULL, rel_widths=c(1, 0.5)), ncol=1, rel_heights=c(1, 1.5))


ggplot(df2[!df2$dom %in% c("Seagrass", "Sand"),], aes(Rug, NPP/GPP))+
geom_point(size=1, aes(shape=region), stroke=0.3)+
geom_smooth(method="lm" , size=0.5, col="black")+
ggtitle("Carbon use efficency (CUE)")+
theme_classic()+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
scale_shape_manual(values=c(21, 25))+guides(shape="none")+
labs(x="Habitat rugosity",  y="NPP to GPP ratio")+
theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=7, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))







# SUPPLEENT




gpp_plotX <- ggplot(df2, aes(Rug, GPP))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm",  col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", linetype="longdash", size=0.5, col="red")+
#geom_text(data=data.frame(1), aes(x=xeq, y=2.5, label=lm_eqn(lm1a)), parse = TRUE, size=2.5, hjust=0)+
#geom_text(data=data.frame(1), aes(x=xeq, y=2, label=lm_eqn(lm1b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
facet_wrap(~region)+
labs(x="Habitat rugosity",  y=expression(GPP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(2,45), breaks=c(4, 13, 22, 31, 40))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Gross primary\nproduction (GPP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
gpp_plotX


resp_plotX <- ggplot(df2, aes(Rug, R))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm",  col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm",  linetype="longdash", size=0.5, col="red")+
#geom_text(data=data.frame(1), aes(x=xeq, y=1.2, label=lm_eqn(lm2a)), parse = TRUE, size=2.5, hjust=0)+
facet_wrap(~region)+
#geom_text(data=data.frame(1), aes(x=xeq, y=1, label=lm_eqn(lm2b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y=expression(R~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(1,25), breaks=c(2, 7, 12, 17, 22))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Respiration (R)")+
#lims(x=c(0.8,6))+
#scale_x_log10(breaks=c(1, 1.5, 3, 6))+scale_y_log10(breaks=c(1, 2, 4, 8, 16, 32))+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
resp_plotX


npp_plotX <-ggplot(df2, aes(Rug, NPP))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=df2[!df2$dom=="Seagrass",], method="lm",  col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", linetype="longdash", size=0.5, col="red")+
#geom_text(data=data.frame(1), aes(x=xeq, y=1, label=lm_eqn(lm3a)), parse = TRUE, size=2.5, hjust=0)+
facet_wrap(~region)+
#geom_text(data=data.frame(1), aes(x=xeq, y=0.8, label=lm_eqn(lm3b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17, 22))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Net community\nproduction (NCP)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
npp_plotX


cue_plotX <-ggplot(df2[!df2$dom %in% c("Sand"),], aes(Rug, CUE))+
geom_point( aes(shape=region, col=region))+
geom_smooth(data=, method="lm",  col="black", size=0.5)+
geom_smooth(data=df2[df2$region=="Hawaii",], method="lm", linetype="longdash", size=0.5, col="red")+
#geom_text(data=data.frame(1), aes(x=xeq, y=1, label=lm_eqn(lm3a)), parse = TRUE, size=2.5, hjust=0)+
facet_wrap(~region)+
#geom_text(data=data.frame(1), aes(x=xeq, y=0.8, label=lm_eqn(lm3b)), parse = TRUE, size=2.5, col="darkred", hjust=0)+
labs(x="Habitat rugosity",  y="CUE")+
#scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17, 22))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
guides(col="none", shape="none")+
scale_colour_manual(values=c("black", "darkred"))+
scale_shape_manual(values=c(16, 10))+
ggtitle("Carbon use\nefficiency (CUE)")+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
cue_plotX

plot_grid(gpp_plotX , resp_plotX, npp_plotX, cue_plotX , labels=c("a", "b", "c", "d"), label_size=9)


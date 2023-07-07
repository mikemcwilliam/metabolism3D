
rm(list = ls())
source("data_prep.R")

df <- merge(times, params)

# missing data
df <- df[!is.na(df$site_area),] # no scans
df <- df[!df$site=="site010",]
#  possibly remove site023 (no flow)
nrow(df) # n sites

#################################### 
# GEOMETRY

# (1) whole site

chamb_planar <- pi*((0.6/2)^2 )  # planar area of 0.6m diam chamber (m3)
df$site_area_m2<-df$site_area / 10^6
df$site_vol_m3<-df$site_vol / 10^9
df$site_rugosity<- (df$site_area / 10^6) / chamb_planar

f.dim <- read.csv("data/stl_files/corals_fd_Nina.csv")
f.dim <- f.dim[!f.dim$site %in% c("site009", "site008"),] # nina says wrong?
df$f.dim <- f.dim$fd[match(df$site, f.dim$site)]

# (2) reef only

comp <- read.csv("data/composition.csv")
comp <- comp[!comp$site=="site010",]
include <- c("fungid", "mcap", "pdam", "pcom", "rock")
comp$total <- rowSums(comp[,include], na.rm=T)
comp <- comp[!comp$total==0,] # no 3D data
vol <- subset(comp, metric=="vol")
area <- subset(comp, metric=="area")
planar <- subset(comp, metric=="planar")

df$reef_planar <- planar$total[match(df$site, planar$site)] /10^6
df$reef_cover <- (df$reef_planar/chamb_planar)*100
df$reef_area <-  area$total[match(df$site, area$site)] /10^6
df$reef_vol <-  vol$total[match(df$site, vol$site)] /10^9
df$reef_rugosity <- df$reef_area / df$reef_planar

# site rugosity and reef area are very similar
plot_grid(
ggplot(df, aes(reef_cover, reef_area))+geom_point()+geom_smooth(method="lm"),
ggplot(df, aes(site_rugosity, reef_area))+geom_point()+geom_smooth(method="lm"),
ggplot(df, aes(reef_cover, reef_rugosity))+geom_point(), 
ggplot(df, aes(f.dim, dominant))+geom_boxplot(),
ggplot(df, aes(reef_rugosity, dominant))+geom_boxplot(),
ggplot(df, aes(reef_rugosity, f.dim))+
geom_point(aes(shape=dominant))+guides(shape="none"))

#################################### 
# METABOLISM

chamb_vol_m3 <- 0.11309 # m3 calculated

# calculate water vol after displacement
df$watervol <- chamb_vol_m3 - df$site_vol_m3
range(df$watervol, na.rm=T) # 76-111 Liters water
df[,c("p.slp","watervol","dominant", "reef_cover")]

# normalise metabolic rates by watervolume
df$NPP <- ((df$p.slp*df$watervol)/chamb_planar) # mg o2 m-2 min
df$R <- abs(((df$r.slp*df$watervol)/chamb_planar))

# gross production and se
df$GPP <- df$NPP + abs(df$R)
df$gpp.se <- df$p.se + abs(df$r.se)

ggplot(data=df)+geom_point(aes(NPP, R)) 
ggplot(df, aes(x=(GPP*60)/1000))+geom_histogram(bins=12) # g/hr

#################################### 
# COMPOSITION

planar2 <- melt(subset(planar, select=-c(total)), id.var=c("site", "metric"))
planar2$rugosity <- df$site_rugosity[match(planar2$site, df$site)]
planar2$cover <- (planar2$value/10^6)/chamb_planar*100

ggplot(planar2, aes(reorder(site, -rugosity), cover))+
geom_bar(stat="identity", aes(fill=variable), col="black", size=0.05)+
theme(axis.text.x=element_text(angle=45, hjust=1))

#################################### 
# FIG1 

source("figs/Fig1.R")
fig1
#ggsave("figs/Fig1.jpg", fig1, width=15, height=9, units="cm", dpi = 300)

#################################### 
# LONG DATA

ids <- c("site", "dominant", "site_rugosity", "reef_area", "tempO2_PS", "tempO2_R", "parPS",      "tempHOBO_PS", "tempHOBO_R")
dflong <- melt(df[,c(ids, "GPP", "NPP", "R")], id.vars=ids, variable.name="type", value.name="rate")
dflong[,c("err", "se")] <- melt(df[,c(ids, "gpp.se", "p.se", "r.se")], id.vars=ids)[,c("variable", "value")]
head(dflong)

###################################
# RUGOSITY VS PRODUCTIVITY

library("MuMIn")

modelAICs <- NULL
for(i in c("site_rugosity", "reef_area", "reef_planar")){
	for(j in c("NPP", "GPP", "R")){
df$X <- df[,i]
df$y <- df[,j]
mod.lin<-lm(y~X, data=df) #  linear 
mod.pwr<-nls(y~alpha*X^beta, data=df, start=list(alpha=3, beta=0.5)) 
#mod.poly <- lm(y~poly(X, 2, raw=T), data=df) #polynomial y = b + bx + bx2 
#mod.as<-nls(y~SSasymp(x,a,b,c), data=df) # try ASYMPTOTIC
#mod.log<-nls(y~SSlogis(x, a,b,c), data=df) #LOGISTIC, sigmoidal s shape curve
AICs <- AICc(mod.lin, mod.pwr)
modelAICs <- rbind(modelAICs, data.frame(response = c(j,j), predictor = c(i,i), type=c("linear","power"), aic = c(AICs[1,2],  AICs[2,2])))
}}
modelAICs[modelAICs$response=="R",]
modelAICs[modelAICs$response=="GPP",]
modelAICs[modelAICs$response=="NPP",]

slopeLinear <- function(x_var, y_var){
df$X <- df[,x_var]
df$y <- df[,y_var]
mod.lin <- lm(y~X, df)
newdata<-with(df, data.frame(X=seq(min(X, na.rm=T), max(X, na.rm=T), len=100)))
fit<-predict(mod.lin,  newdata=newdata, se.fit=T)
q<-qt(0.95, df.residual(mod.pwr))
newdata<-data.frame(newdata, fit=fit$fit, lower=fit$fit-(q*fit$se.fit), upper=fit$fit+(q*fit$se.fit))}

slopePower <- function(x_var, y_var){
df$X <- df[,x_var]
df$y <- df[,y_var]
mod.pwr<-nls(y~alpha*X^beta, data=df, start=list(alpha=3, beta=0.5)) 
newdata<-with(df, data.frame(X=seq(min(X, na.rm=T), max(X, na.rm=T), len=100)))
fit<-predict(mod.pwr,  newdata=newdata)
coef(mod.pwr)
# SEs = https://www.flutterbys.com.au/stats/tut/tut12.2a.html
grad <- deriv3(~alpha*X^beta, c("alpha","beta"), function(alpha,beta,X) NULL)
mod.pwr2<-nls(y~grad(alpha,beta,X), data=df,start=list(alpha=3,beta=0.5))
fit<-predict(mod.pwr2, newdata=newdata)
se <- sqrt(apply(attr(predict(mod.pwr2,list(X = newdata$X)),"gradient"),1,function(x) sum(vcov(mod.pwr2)*outer(x,x))))
q<-qt(0.95, df.residual(mod.pwr))
newdata<-data.frame(newdata, fit=fit, lower=fit-(q*se), upper=fit+(q*se))
newdata}

modelLines <- rbind(cbind(slopePower("site_rugosity","GPP"), type="GPP"), cbind(slopePower("site_rugosity","R"), type="R"), cbind(slopePower("site_rugosity","NPP"), type="NPP"))

plot_grid(
ggplot()+ geom_line(data=modelLines, aes(x=X, y=fit, col=type)),
ggplot()+geom_line(data=modelLines, aes(x=X, y=fit))+
geom_point(data=dflong, aes(x=site_rugosity, y=rate))+
geom_segment(data=dflong, aes(x=site_rugosity, xend=site_rugosity, y=rate-se, yend=rate+se), size=0.25)+
facet_wrap(~type, scale="free"))

###################################
# ALLOMETRY

df$site_area_cm2 <- df$site_area_m2*10000
scaling <- NULL
mods <- c("site_rugosity", "reef_area", "reef_planar", "site_area_m2", "reef_vol", "reef_rugosity", "site_vol_m3", "site_area_cm2", "reef_cover")
type <- c("NPP","GPP","R")
for(i in mods){
	for(j in type){
	df$x <- df[,i]
	df$y <- df[,j]
mod<-lm(y~x, df)
l.mod <- lm(log(y)~log(x), df)
scaling <- rbind(scaling, data.frame(i,j, slp=coef(mod)[2], int=coef(mod)[1], ci1=confint(mod)[2,2], ci2=confint(mod)[2,1], log.slp=coef(l.mod)[2], log.int = coef(l.mod)[1], log.ci1 =confint(l.mod)[2,2], log.ci2 = confint(l.mod)[2,1], lint.ci1=confint(l.mod)[1,2], lint.ci2=confint(l.mod)[1,1]))}}
scaling

plot_grid(
ggplot(scaling, aes(j, log.slp))+geom_point()+ggtitle("log")+
geom_segment(aes(x=j, xend=j, y=log.ci1, yend=log.ci2))+facet_wrap(~i, scales="free_y"),
ggplot(scaling, aes(j, slp))+geom_point()+ggtitle("no log")+
geom_segment(aes(x=j, xend=j, y=ci1, yend=ci2))+facet_wrap(~i, scales="free_y"))

sim.x <- seq(1.3, 3.5, length=20)
PS <- (scaling[scaling$i=="site_rugosity" & scaling$j=="GPP","slp"]*sim.x)+scaling[scaling$i=="site_rugosity" & scaling$j=="GPP","int"]
R <- (scaling[scaling$i=="site_rugosity" & scaling$j=="R","slp"]*sim.x)+scaling[scaling$i=="site_rugosity" & scaling$j=="R","int"]
Net <- PS - R
slp.n <- scaling[scaling$i=="site_rugosity" & scaling$j=="NPP","slp"] #10.74
int.n <-  scaling[scaling$i=="site_rugosity" & scaling$j=="NPP","int"] #8.612
Mod.Net <- (slp.n*sim.x)+int.n
par(mfrow=c(1,2))
plot(sim.x,PS, ylim=c(0,100))
points(sim.x, R, col="red")
points(sim.x, Net, col="green")
points(sim.x, Mod.Net, col="darkgreen", cex=0.5)
plot(sim.x,Net/PS, col="green", ylim=c(0, 1))

sim.x <- log(seq(1.3, 4, length=20))
PS <- (scaling[scaling$i=="site_rugosity" & scaling$j=="GPP","log.slp"]*sim.x)+scaling[scaling$i=="site_rugosity" & scaling$j=="GPP","log.int"]
R <- (scaling[scaling$i=="site_rugosity" & scaling$j=="R","log.slp"]*sim.x)+scaling[scaling$i=="site_rugosity" & scaling$j=="R","log.int"]
Net <- exp(PS) - exp(R)
slp.n <- scaling[scaling$i=="site_rugosity" & scaling$j=="NPP","log.slp"] #10.74
int.n <-  scaling[scaling$i=="site_rugosity" & scaling$j=="NPP","log.int"] #8.612
Mod.Net <- (slp.n*sim.x)+int.n
par(mfrow=c(1,2))
plot(exp(sim.x),exp(PS), ylim=c(0,100))
points(exp(sim.x), exp(R), col="red")
points(exp(sim.x), Net, col="green")
points(exp(sim.x), exp(Mod.Net), col="darkgreen", cex=0.5)
plot(exp(sim.x),Net/exp(PS), col="green", ylim=c(0, 1))


# total photosynthesis increases faster than respiration, leading to an increasing energy surplus. But respiration increases faster than the surplus, leading to reduced portion of photosynthetic energy left after resp. 

###################################
# CARBON USE EFFICIENCY

df$CUE <- df$NPP/df$GPP
df$ratio <- abs(df$R)/df$GPP

plot_grid(
ggplot(data=df, aes(site_rugosity, ratio))+
geom_smooth(method="lm", formula=y~poly(x,1))+geom_point(aes(col=dominant))+
geom_text(aes(label=site, col=dominant), size=2)+
labs(y="R per GPP"), 
ggplot(data=df, aes(site_rugosity, CUE))+
geom_smooth(method="lm", formula=y~poly(x,1))+geom_point(aes(col=dominant))+
geom_text(aes(label=site, col=dominant), size=2))


modelAICs3 <- NULL
for(i in c("site_rugosity", "reef_area")){
	for(j in c("CUE")){
df$X <- df[,i]
df$y <- df[,j]
mod.lin<-lm(y~X, data=df) #  linear 
mod.pwr<-nls(y~alpha*X^beta, data=df, start=list(alpha=3, beta=0.5)) 
AICs <- AICc(mod.lin, mod.pwr)
modelAICs3 <- rbind(modelAICs3, data.frame(response = c(j,j), predictor = c(i,i), type=c("linear","power"), aic = c(AICs[1,2],  AICs[2,2])))
}}
modelAICs3

mod.fish <- lm(ratio~site_rugosity, df[!df$site=="site010",])
summary(mod.fish)

mod.fish <- lm(CUE~site_rugosity, df[!df$site=="site010",])
summary(mod.fish)


#################################### 
# FIG2

source("figs/Fig2.R")
fig2
#ggsave("figs/Fig2.jpg", fig2, width=13, height=11, units="cm", dpi = 300)


###################################
# OTHER PLOTS

p1 <- ggplot(df, aes(reef_area/reef_planar, GPP))+
geom_text(aes(label=site), size=2)+
geom_smooth(method="lm")+ggtitle("reef rugosity")

p2 <- ggplot(df, aes(reef_area, GPP/reef_area))+geom_point(aes(col=dominant))+geom_smooth(method="lm", formula=y~poly(x,1), aes(col=dominant))+
scale_y_log10()+scale_x_log10()+ggtitle("per area")

plot_grid(p1, p2)
summary(lm(log(GPP/reef_area)~log(reef_area)+dominant, df))

###################################
# DOMINANT TAXON 

ggplot(data=dflong, aes(log10(site_rugosity),log10(rate), col=dominant))+
geom_smooth(method="lm", se=F, size=0.5, show.legend=F)+
geom_point()+facet_wrap(~type, scales="free")

###################################
# LIGHT

head(df)
ggplot(df, aes(parPS, weather))+geom_boxplot(width=0.5)
# 1000=overcast day / 10000-25000=daylight 
# https://en.wikipedia.org/wiki/Lux

ggplot(data=dflong, aes(log10(parPS),log10(rate)))+
geom_smooth(method="lm", se=F, size=0.5, show.legend=F)+
geom_point()+facet_wrap(~type, scales="free")

###################################
# TEMPERATURE

df$tempO2 <- rowMeans(df[,c("tempO2_PS","tempO2_R")],na.rm=TRUE) # from o2 probe
df$month<-format(df$Pstart, "%m/%Y")

# starts to drop in temp in Sept.
# Hobo consistently wrong
plot_grid(ggplot(df, aes(tempO2, month))+geom_boxplot(width=0.5), ggplot(df, aes(tempO2, tempHOBO_R))+geom_point()+geom_smooth(method="lm")+geom_abline(slope=1),
ggplot(df, aes(tempO2_R, tempO2_PS))+geom_point()+geom_smooth(method="lm")+geom_abline(slope=1))

dflong$tempO2 <- rowMeans(dflong[,c("tempO2_PS","tempO2_R")],na.rm=TRUE)
ggplot(data=dflong, aes(tempO2,log10(rate)))+
geom_smooth(method="lm", se=F, size=0.5, show.legend=F, formula=y~poly(x,2))+
geom_point()+facet_wrap(~type, scales="free")

# dflong$rate2D <-  (dflong$rate*chamb_planar) / dflong$reef_area

#################################### 
# FIG3

source("figs/Fig3.R")
fig3
#ggsave("figs/Fig3.jpg", fig3, width=15, height=15, units="cm", dpi = 300)

###################################
# MODELLING PRODUCTIVITY
 
modelAICs2 <- NULL
	for(j in c("NPP", "GPP", "R")){
#		j <- "NPP"
df$y <- log(df[,j])
mod.1 <-lm(y~log(reef_planar), data=df)
mod.2 <-lm(y~log(site_rugosity), data=df)
mod.3 <-lm(y~log(reef_planar)+dominant, data=df)
mod.4 <-lm(y~log(site_rugosity)+dominant, data=df)
mod.5 <-lm(y~log(reef_planar)+parPS+poly(tempO2, 2), data=df)
mod.6 <-lm(y~log(site_rugosity)+parPS+poly(tempO2, 2), data=df)
mod.7 <- lm(y~log(reef_planar)+dominant+parPS+poly(tempO2, 2), data=df)
mod.8 <- lm(y~log(site_rugosity)+dominant+parPS+poly(tempO2, 2), data=df)
mod.list <- list(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8)
AICs <- AICc(mod.1, mod.2, mod.3, mod.4, mod.5, mod.6, mod.7, mod.8)
r.sq <- NULL
for(i in 1:length(mod.list)){
r.sq <- c(r.sq, summary(mod.list[[i]])$r.squared)
}
 modelAICs2 <- rbind(modelAICs2, cbind(type=j, round(AICs,2), r2 = round(r.sq,2)))
}
modelAICs2[ modelAICs2$type=="GPP",]

result.table<-cbind(modelAICs2[ modelAICs2$type=="GPP",], modelAICs2[ modelAICs2$type=="R",], modelAICs2[ modelAICs2$type=="NPP",])
result.table

#write.csv(mod.results, "data/modelAICs.csv")



#########################################
#########################################
# Two sensors in different locations

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

FigS9 <- plot_grid(r3a+guides(col="none"), r3b+guides(col="none"), get_legend(r3a),
nrow=1, labels=c("a", "b"), rel_widths=c(1,1.2,0.5))
FigS9

mean(abs(probes$pdiff))
mean(abs(probes$pdiff))/diff(range(df$NPP))*100

probes2 <- melt(probes[,c("site", "r1", "r2", "Rug")], id.var=c("site", "Rug"))
probes2$n <- substr(probes2$variable, 2,2)
rsq2 <- melt(probes[,c("site", "r.rsq1", "r.rsq2")], id.var="site")
rsq2$n <- substr(rsq2$variable, 6,6)
colnames(rsq2) <- c("site", "variable2", "value2", "n")
probes2x <- merge(probes2, rsq2)
probes2x

mean(abs(probes$rdiff))
mean(abs(probes$rdiff))/diff(range(df$R, na.rm=T))*100

#########################################
#########################################



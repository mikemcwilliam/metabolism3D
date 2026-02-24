
# slopes

scaling$j2 <- ifelse(scaling$j=="NPP", "NCP", as.character(scaling$j))
scaling$j2 <- factor(scaling$j2, levels=c("GPP", "R", "NCP"))


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


# predict


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



# CUE 

df2$hab <- ifelse(df2$dom=="Seagrass", "Seagrass", ifelse(df2$dom=="Sand", "Sand", ifelse(df2$dom=="Algal turf", "Algal Turf", ifelse(df2$dom=="Soft coral", "Soft Coral", "Hard Coral"))))


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






# plot fig 4

FIG4 <- plot_grid(NULL,
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
FIG4



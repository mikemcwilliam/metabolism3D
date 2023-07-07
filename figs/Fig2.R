

#################################### 
# colours

library("wesanderson")
spCols <- wes_palette("GrandBudapest1", n = 3)

#################################### 


dflong$type <- factor(dflong$type, levels=c("GPP", "R", "NPP"))
modelLines$type <- factor(modelLines$type, levels=c("GPP", "R", "NPP"))

metabolism1 <- ggplot()+
geom_point(data=dflong, aes(x=site_rugosity, y=rate, col=type), size=1.2)+
geom_line(data=modelLines, aes(x=X, y=fit),size=0.4)+
geom_segment(data=dflong, aes(x=site_rugosity, xend=site_rugosity, y=rate-se, yend=rate+se, col=type), size=0.1)+
facet_wrap(~type, scales="free")+
guides(colour="none")+
labs(x="Site rugosity",  y=expression(Rate~(mg~O[2]~m^-2~min^-1)))+
scale_colour_manual(values=c("darkgreen", "darkred", "slategrey"))+
theme_classic()+ theme(axis.line=element_line(size=0.3), plot.title=element_text(size=9, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))
metabolism1

all <- ggplot()+ geom_line(data=modelLines, aes(x=X, y=fit, col=type))+
scale_colour_manual(values=c("darkgreen", "darkred", "slategrey"))+
labs(x="Site rugosity",  y=expression(Rate~(mg~O[2]~m^-2~min^-1)))+
theme_classic()+ theme(axis.line=element_line(size=0.3), plot.title=element_text(size=9, hjust=0.5, face="bold"), strip.background=element_blank(), axis.title=element_text(size=8), legend.title=element_blank(), legend.key.height=unit(1, "mm"), legend.position=c(0.2, 0.85), legend.background=element_blank())

CUEplot <- ggplot(df, aes(site_rugosity, NPP/GPP))+
geom_point(aes(shape=dominant), size=1.5)+
theme_classic()+theme(axis.line=element_line(size=0.3), plot.title=element_text(size=9, hjust=0.5, face="bold"), strip.background=element_blank(), axis.title=element_text(size=8), legend.title=element_blank(),  legend.key.height=unit(1,"mm"), legend.key.width=unit(1,"mm"), legend.text=element_text(size=7, face="italic"), legend.position=c(0.83,0.9), 
legend.margin=margin(-4,1,1,1),
legend.background=element_blank(),
legend.box.background=element_rect(colour="grey"))+
xlim(c(1,3.8))+
geom_smooth(method="lm", se=F, col="grey60", size=0.5)+
guides(shape=guide_legend(override.aes=list(size=0.9)))+
labs(x="Site rugosity", y="Carbon use\nefficiency (CUE)")
CUEplot

fig2 <- plot_grid(metabolism1, plot_grid(all, NULL, CUEplot, labels=c("B","", "C"), label_size=10, nrow=1, rel_widths=c(1,0.1, 1)), nrow=2, labels=c("A", ""), label_size=10)
fig2

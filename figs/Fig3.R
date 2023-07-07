

mgO2 <- expression(log~(mg~O[2]~m^-2~min^-1))


COLS <- c("slategrey",spCols[3],spCols[1],spCols[2],"red")
names(COLS)<- c("Lobactis", "M. capitata", "P. compressa", "P. damicornis", "Rock/Turf")


dflong$type <- factor(dflong$type, levels=c("GPP","R","NPP"))

F2a <- ggplot()+
geom_smooth(data=dflong, aes(log10(site_rugosity),log10(rate)), method="lm", se=F, size=0.5, show.legend=F, col="black")+
geom_smooth(data=dflong[!dflong$dominant=="Rock/Turf",], aes(log10(site_rugosity),log10(rate), colour=dominant), method="lm", se=F, linetype="dashed", size=0.5, show.legend=FALSE)+
geom_point(data=dflong, aes(log10(site_rugosity), log10(rate), fill=dominant, shape=dominant))+
geom_point(data=dflong[dflong$dominant%in% c("Rock/Turf"),], aes(log10(site_rugosity),log10(rate)), shape=8, stroke=0.7)+
facet_wrap(~type, nrow=1, scale="free")+
scale_fill_manual(values=COLS)+
scale_colour_manual(values=COLS)+
scale_shape_manual(values=c(7, 21, 21, 21, 8))+
labs(x="log(rugosity)", y=mgO2)+
theme_classic()+
theme(strip.background=element_blank(), strip.text=element_text(size=8, hjust=0.5),axis.title=element_text(size=9), legend.background=element_blank(), legend.key.height=unit(1,"mm"), legend.title=element_text(size=8, face="bold"),
legend.key.width=unit(0,"mm"), plot.title=element_text(size=8, face="bold", hjust=0.5))
F2a

# linetype=type
F2b <-ggplot()+
geom_smooth(data=dflong, aes(parPS/1000, log10(rate)), method="lm", col="black", se=F, size=0.5)+
geom_point(data=dflong, aes(parPS/1000,log10(rate)), shape=21, fill="grey")+
facet_wrap(~type, nrow=1, scales="free")+
labs(x="Light intensity (lux x 1000)", y=mgO2)+
scale_x_log10()+
#scale_shape_manual(values=c(21, 16), name="per unit:")+
#scale_linetype_manual(values=c("dashed","solid"), name="per unit:")+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_text(size=9), axis.title.y=element_text(size=9), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5))

F2c <-ggplot()+
geom_smooth(data=dflong, aes(tempO2, log10(rate)), method="lm", col="black", se=F, size=0.5, formula=y~poly(x,2))+
geom_point(data=dflong, aes(tempO2,log10(rate)), shape=21, fill="grey")+
facet_wrap(~type, nrow=1, scales="free")+
labs(x="Temperature (°C)", y=mgO2)+
#scale_shape_manual(rates=c(21, 16), name="per unit:")+
#scale_linetype_manual(rates=c("dashed","solid"), name="per unit:")+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8, hjust=0.5),legend.title=element_blank(), legend.key.height=unit(1,"mm"), axis.title=element_text(size=9), legend.background=element_blank(), plot.background=element_blank(),panel.background=element_blank(),plot.title=element_text(size=9, face="bold", hjust=0.5))



fig3 <- plot_grid(ggplot+theme_void(),####
F2a+xlim(c(0,0.7))+theme(legend.position=c(0.07,1.01), legend.text=element_text(size=6, face="italic"),strip.text=element_blank(), legend.title=element_blank()), #####
F2b+scale_x_log10(limits=c(0.8, 70))+guides(linetype="none")+theme(legend.position=c(0.05,1.01), legend.text=element_text(size=6), legend.key.width=unit(2, "mm"), strip.text=element_blank(), 
legend.background=element_blank(),
legend.title=element_text(size=6, face="bold")), ##### 
F2c+xlim(c(25.5, 30))+guides(linetype="none")+theme(legend.position=c(0.05,1.01), legend.text=element_text(size=6), legend.key.width=unit(2, "mm"), strip.text=element_blank(), legend.title=element_text(size=6, face="bold")), #####
 ncol=1, align="hv", axis="lr", labels=c("", "A", "B", "C"), label_size=9, rel_heights=c(0.25, 1,1,1), vjust=-1)+
 draw_label("Gross primary \nproduction", 0.23,0.965, size=8, fontface="bold")+
  draw_label("Respiration", 0.55,0.965, size=8, fontface="bold")+
   draw_label("Net community \nproduction", 0.86,0.965, size=8, fontface="bold")

fig3

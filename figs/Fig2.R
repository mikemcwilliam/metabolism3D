

themeX <- theme(axis.line=element_line(size=0.3), plot.title=element_text(size=8, hjust=0.5, face="bold"), 
strip.text=element_text(size=9, hjust=0.5, face="bold"), 
strip.background=element_blank(), axis.title=element_text(size=8))

# colour scheme

#library("fishualize")
#fish<-fish_palettes()
#fishualize( option = fish[14], n= 5)
#fish( option = fish[14], n= 5)
#fishualize( option = fish[18], n= 5)
#fish( option = fish[18], n= 5)

colsc4 <- c("#fa9fb5", "#6CA167FF", "#8c96c6", "#bfd3e6", "#ae017e", "#8c6bb1","#B79E4BFF","aquamarine3", "grey")
colsc4
unique(df2$dom2)[order(unique(df2$dom2))]
names(colsc4) <- unique(df2$dom2)[order(unique(df2$dom2))]

legplot <-ggplot(df2, aes(Rug, GPP, col=dom2))+geom_point(size=0.75)+scale_colour_manual(values=colsc4, name="Dominant\ntaxon")+theme_classic()+
theme(legend.title=element_text(size=7, face="bold"), legend.text=element_text(size=7), legend.key.height=unit(1, "mm"))


####### scatterplots

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


# rank benthic taxa by rates per cm2 

# colours with no samples size in name.. 
colsc4b <- c("#fa9fb5", "#6CA167FF", "#8c96c6", "#bfd3e6", "#ae017e", "#8c6bb1","#B79E4BFF","aquamarine3", "grey")
colsc4b
unique(df2$dom)[order(unique(df2$dom))]
names(colsc4b) <- unique(df2$dom)[order(unique(df2$dom))]
colsc4b

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


# log-linear slopes for each taxon

Edom$y <- ifelse(Edom$y=="NPP", "NCP", Edom$y)

Edom$y <- factor(Edom$y, levels=c("GPP", "R", "NCP"))


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



################### PLOT FIG2




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


FIG2 <- Fig2blank2+
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
FIG2








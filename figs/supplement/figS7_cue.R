




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

FigS7 <- plot_grid(cue_compSup, plot_grid(cue_compSupX, NULL, rel_widths=c(1, 0.5)), ncol=1, rel_heights=c(1, 1.5))
FigS7 


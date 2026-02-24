


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


FigS5 <- plot_grid(gpp_comp2, resp_comp2, npp_comp2, ncol=1)
FigS5 





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

FigS6 <- plot_grid(gpp_plotX , resp_plotX, npp_plotX, cue_plotX , labels=c("a", "b", "c", "d"), label_size=9)
FigS6 


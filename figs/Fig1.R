


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

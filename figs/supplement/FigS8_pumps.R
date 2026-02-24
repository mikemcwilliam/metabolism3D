

R3a <- ggplot(plong, aes(Rug, value, col=variable2))+
geom_point(data=df2, aes(x=Rug, y=NPP), col="grey")+
geom_smooth(data=df2, aes(x=Rug, y=NPP), col="grey", method="lm")+
geom_point()+
geom_smooth(method="lm")+
labs(x="Habitat rugosity",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
scale_radius()+
scale_y_log10(limits=c(0.8,25), breaks=c(2, 7, 12, 17))+
scale_x_log10(limits=c(0.85, 7), breaks=c(1, 2, 3, 4, 5))+
theme_classic()+theme(legend.title=element_blank(), legend.position=c(0.8, 0.1))


R3b <- ggplot(plong, aes(variable2, value))+
geom_line(aes(group=site), col="grey")+
geom_violin(data=df2, aes(x="all", y=NPP), col="grey")+
geom_point(data=df2, aes(x="all", y=NPP), col="grey")+
geom_boxplot(outlier.size=0)+
geom_point()+
#geom_text(data=plong[plong$variable=="NPPno",],aes(x="No Pump", y=value, label=site), hjust=1, size=2.5, col="red")+
#geom_text(data=plong[plong$variable=="NPP",],aes(x="With Pump", y=value, label=site), hjust=0, size=2.5, col="red")+
labs(x="Treatment",  y=expression(NCP~"("*mg~O[2]~m^-2~min^-1*")"))+
#lims(y=c(min(df2$NPP), max(df2$NPP)))+
scale_y_log10()+
theme_classic()

R3c2 <-ggplot()+
geom_histogram(data=pumps, aes(NPP-NPPno), bins=6, col="black", fill="grey", size=0.3)+
labs(x="Change in NCP with the\naddition of pumps", y="N chambers")+
scale_y_continuous(expand=c(0,0), limits=c(0, 10))+
geom_segment(data=NULL, aes(x=mean(pumps$NPP - pumps$NPPno), xend=mean(pumps$NPP - pumps$NPPno), y=Inf, yend=-Inf), col="red")+
xlim(c(-8, 6))+
theme_classic()


FigS8 <- plot_grid(R3a, plot_grid(R3b, R3c2, ncol=1, labels=c("b", "c"), hjust=1, rel_heights=c(1,0.8)), rel_widths=c(1, 0.8), labels=c("a", ""))
FigS8


light_effect <-ggplot(pumps, aes(light_change, NPP-NPPno))+
geom_vline(xintercept=0, col="grey", size=0.3)+
geom_hline(yintercept=0, col="grey", size=0.3)+
geom_point()+
labs(x="Change in light intensity\nbetween treatments (log scale)", y="Change in NCP with the\naddition of pumps")+
geom_smooth(method="lm")+
theme_classic()
light_effect


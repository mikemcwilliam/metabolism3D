

light$variable <- ifelse(light$variable =="NPP", "NCP",as.character(light$variable))
light$variable <- factor(light$variable, levels=c("GPP", "R", "NCP"))


lbox <- ggplot(df2, aes(y=region, x=parPS))+geom_boxplot(outlier.size=0.1, size=0.3, fill="grey90")+
geom_jitter(shape=21, height=0.2, width=0, size=0.2, col="grey")+
theme_classic()+theme(axis.line=element_blank(), axis.title=element_blank(), axis.text.x=element_blank(),axis.ticks=element_blank())+scale_x_log10()


lplot <- ggplot(light, aes(parPS, (value/SAcm2)*1000, col=variable))+
geom_point(shape=21, size=0.5)+
labs(x="Light intensity (lux)",  y=expression(Rate~"("*μg~O[2]~cm^-2~min^-1*")"))+
#scale_y_sqrt()+
scale_x_log10()+
#scale_shape_manual(values=c(16, 4))+
geom_smooth( method="lm", se=F,size=0.5)+
#scale_colour_manual(values=pcols[c(1,2,3)])+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
#facet_wrap(~region)+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_text(size=9), axis.title.y=element_text(size=9, lineheight = 0.1), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2))
lplot

lplot2 <- plot_grid(lbox, lplot+guides(col="none"), ncol=1, rel_heights=c(0.2, 1), align="v", axis="lr")
lplot2


tbox <- ggplot(df2, aes(y=region, x=temp))+geom_boxplot(outlier.size=0.1, size=0.3, fill="grey90")+
geom_jitter(shape=21, height=0.2, width=0, size=0.2, col="grey")+
theme_classic()+theme(axis.line=element_blank(), axis.title=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank())


tplot <- ggplot(temp, aes(temp, (value/SAcm2)*1000, col=variable))+
geom_point(shape=21, size=0.5)+
#scale_y_sqrt()+
#scale_x_log10()+
geom_smooth(method="lm", se=F, formula=y~poly(x, 2), size=0.5)+
labs(x="Temperature (°C)",  y=expression(Rate~"("*μg~O[2]~cm^-2~min^-1*")"))+
#scale_shape_manual(values=c(16, 4))+
#facet_wrap(~region)+
#scale_colour_manual(values=c("darkgreen", "red", "black"))+
scale_colour_manual(values=c("black", pcols[c(2,1)]))+
theme_classic()+theme(strip.background=element_blank(), strip.text=element_text(size=8,hjust=0.5), axis.title.x=element_text(size=9), axis.title.y=element_text(size=9, lineheight = 0.1), plot.background=element_blank(),panel.background=element_blank(),legend.title=element_blank(),legend.key.height=unit(1,"mm"),plot.title=element_text(size=8, face="bold", hjust=0.5), axis.line=element_line(size=0.2))
tplot



tplot2 <- plot_grid(tbox, tplot+guides(col="none"), ncol=1, rel_heights=c(0.2, 1), align="v", axis="lr")
tplot2


# cohens f2

esizes3$sig <- ifelse(esizes3$pval>0.1, "", ifelse(esizes3$pval<=0.05 & esizes3$pval>0.01, "*", ifelse(esizes3$pval<=0.01 & esizes3$pval>0.001, "**", ifelse(esizes3$pval<=0.001, "***", ifelse(esizes3$pval <=0.1 & esizes3$pval>0.05, ".", NA)))))
esizes3$j2 <- gsub("cm2", "",esizes3$j)
esizes3$j2 <- factor(esizes3$j2 , levels=c("GPP", "R", "NPP"))
esizes3$pred <- factor(esizes3$pred , levels=c("Habitat rugosity", "Light intensity", "Temperature"))
esizes3


eplot <- ggplot(esizes3[esizes3$norm=="Total",], aes(pred, f2))+
geom_hline(yintercept=0)+
geom_bar(stat="identity", position="dodge", aes(fill=j2), width=0.65, col="black", size=0.1)+
geom_text(data=esizes3[esizes3$norm=="Total" & esizes3$pred=="Temperature",], aes(label=sig, x=pred, y=f2+0.1, group=j2), position=position_dodge(width=0.65), size=4)+
geom_text(data=esizes3[esizes3$norm=="Total" & !esizes3$pred=="Temperature",], aes(label=sig, x=pred, y=f2+0.1, group=j2), position=position_dodge(width=0.65), angle=90, hjust=0.4, vjust=0.82, size=3)+
#scale_fill_manual(values=c("darkgreen", "red", "black"))+
scale_fill_manual(values=c("black", pcols[c(2,1)]))+
#facet_wrap(~norm, ncol=1)+
labs(x="Predictor", y= expression("Cohen's"~f~""^2))+
scale_y_continuous(expand=c(0,0), limits=c(0, 1.8))+
theme_classic()+
ggtitle("Standardised\neffect sizes")+
theme(axis.title=element_text(size=8), axis.line=element_line(size=0.2),axis.title.x=element_blank(), axis.text.x=element_text(size=8, angle=25, hjust=1), plot.title=element_text(size=8, hjust=0.5))
eplot


FIG3 <- plot_grid( lplot2, tplot2, eplot+guides(fill="none"),NULL, get_legend(tplot),NULL, nrow=1, 
rel_widths=c(1,1,0.7,0.05, 0.2, 0.05), labels=c("a","b","c","", "", ""), label_size=9, vjust=c(7,7,5,0,0,0))
FIG3





# plot rate ranked by rugosity

library(RColorBrewer)
pcols <- brewer.pal(n = 3, name = "Dark2")

df2$site[df2$site=="sand"] <- c("sand1", "sand2")

dfx <- melt(df2[, c("NPP", "R", "Rug", "site", "region")], id.var=c("Rug", "site", "region"))
dfx$GPP <- df2$GPP[match(dfx$site, df2$site)]

FigS4 <- plot_grid(
ggplot(dfx, aes(reorder(site, Rug), value, fill=variable))+geom_bar(stat="identity", width=0.7)+
theme_classic()+
theme(axis.text.x=element_blank(), strip.background=element_blank(), legend.title=element_blank(), 
legend.position=c(0.1, 0.8))+
scale_y_continuous(expand=c(0,0))+
labs(x="Site ranked by rugosity", y="Overall rate")+
scale_fill_manual(values=pcols[c(1,2)])+
#scale_fill_manual(values=c("darkgreen", "red"))+
facet_grid(.~region, scales="free_x", space="free")
,
ggplot(dfx, aes(reorder(site, Rug), value/GPP, fill=variable))+geom_bar(stat="identity", width=0.7)+
theme_classic()+
guides(fill="none")+
scale_y_continuous(expand=c(0,0))+
labs(x="Site ranked by rugosity", y="Rate as proportion of GPP")+
scale_fill_manual(values=pcols[c(1,2)])+
theme(axis.text.x=element_blank(), strip.background=element_blank())+
facet_grid(.~region, scales="free_x", space="free")
,
 ncol=1)
FigS4
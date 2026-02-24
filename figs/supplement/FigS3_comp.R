


compHI$Group <-ifelse(compHI$variable =="mcap", "Montipora", ifelse(compHI$variable =="pcom", "Porites", ifelse(compHI$variable =="pdam", "Pocillopora", ifelse(compHI$variable =="rock", "Turf", ifelse(compHI$variable =="fungid", "Other scleractinia", NA))))) 

compGBR$Group2 <- compGBR$Group
compGBR$Group2[compGBR$Group2=="Bare rock"]<-"Rock/rubble"
compGBR$Group2[compGBR$Group2=="Rubble"]<-"Rock/rubble"


colnames <- c(unique(compGBR$Group2), "Other scleractinia")
colnames
tcols <- c("#E27B0CFF" , "slategrey", "grey30", "#6CA167FF", "pink", "#89374FFF", "grey", "green", "lightsalmon1", "#145A76FF", "#635C72FF", "aquamarine3", "grey")
names(tcols)<-colnames

compGBR$cover[compGBR$Group %in% "Seagrass"]<-30

planarGBR <- aggregate(cover~Site, compGBR, sum)
planarHI <- aggregate(cover~site, compHI, sum)
compHI$total <- planarHI$cover[match(compHI$site, planarHI$site)]
compGBR$total <- planarGBR$cover[match(compGBR$Site, planarGBR$Site)]

compHI$dom <- df$dom[match(compHI$site, df$site)]
compHI$dom[compHI$dom=="Algal turf"] <- "Turf"
compHI[compHI$site=="hi23",]
domHI <- compHI[compHI$Group==compHI$dom,]

compGBR$dom <- df$dom[match(compGBR$Site, df$site)]
domGBR <- compGBR[compGBR$Group==compGBR$dom,]


compHI$dom <- ifelse(compHI$dom=="Other scleractinia", "Other scler.", compHI$dom)
compGBR$dom <- ifelse(compGBR$dom=="Other scleractinia", "Other scler.", compGBR$dom)
compGBR$dom <- ifelse(compGBR$dom=="Algal turf", "Turf", compGBR$dom)

HIcompplot <- ggplot(compHI, aes(site, cover/total*100, fill=Group))+geom_bar(stat="identity")+
geom_text(aes(site, 102, label=dom), angle=70, hjust=0, size=2)+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
#ylim(c(0, 150))+
scale_fill_manual(values=tcols)+
labs(y="% cover")+ggtitle("Hawai'i")+
scale_y_continuous(expand=c(0,0), limits=c(0, 120), breaks=c(0, 50, 100))+
theme(axis.text.x=element_blank(), legend.key.size=unit(2, "mm"))
HIcompplot

GBRcompplot <- ggplot(compGBR, aes(Site, cover/total*100, fill=Group2))+geom_bar(stat="identity")+
geom_text(aes(Site, 102, label=dom), angle=70, hjust=0, size=2)+
theme_classic()+theme(axis.text.x=element_text(size=5, hjust=1, angle=45), legend.title=element_blank())+
scale_fill_manual(values=tcols)+
labs(y="% cover")+ggtitle("GBR")+
scale_y_continuous(expand=c(0,0), limits=c(0, 120), breaks=c(0, 50, 100))+
theme(axis.text.x=element_blank(), legend.key.size=unit(2, "mm"))
GBRcompplot 


FigS3 <- plot_grid(plot_grid(HIcompplot, NULL), GBRcompplot , ncol=1)


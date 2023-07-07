

#################################### 
# colours

library("wesanderson")
spCols <- wes_palette("GrandBudapest1", n = 3)

#################################### 
# Load chamber diagram

library("png")

chamb <- readPNG("figs/chamber.png")
chamb <- rasterGrob(chamb, interpolate=TRUE)

#################################### 
# 3D plots

site001 <- readPNG("figs/3Dshots/site001L.png")
site001 <- rasterGrob(site001, interpolate=TRUE)
site012 <- readPNG("figs/3Dshots/site012L.png")
site012 <- rasterGrob(site012, interpolate=TRUE)
site011 <- readPNG("figs/3Dshots/site1100.png")
site011 <- rasterGrob(site011, interpolate=TRUE)
site004 <- readPNG("figs/3Dshots/site004L.png")
site004 <- rasterGrob(site004, interpolate=TRUE)
site006 <- readPNG("figs/3Dshots/site006L.png")
site006 <- rasterGrob(site006, interpolate=TRUE)
site007L <- readPNG("figs/3Dshots/site007L.png")
site007L <- rasterGrob(site007L, interpolate=TRUE)
site008L <- readPNG("figs/3Dshots/site008L.png")
site008L <- rasterGrob(site008L, interpolate=TRUE)
site013L <- readPNG("figs/3Dshots/site013L.png")
site013L <- rasterGrob(site013L, interpolate=TRUE)

#################################### 

planar2$taxon <- ifelse(planar2$variable=="mcap", "M. capitata", ifelse(planar2$variable=="pdam", "P. damicornis", ifelse(planar2$variable=="pcom", "P. compressa", ifelse(planar2$variable=="fungid", "Lobactis spp.", ifelse(planar2$variable=="rock", "Rock/Turf", NA)))))

compplot <- ggplot(planar2, aes(reorder(site, -rugosity), cover))+
geom_bar(stat="identity", aes(fill=taxon), col="black", size=0.05)+
theme_classic()+
theme(axis.text.x=element_blank(), 
axis.title=element_text(size=8),
legend.title=element_blank(), 
axis.line.x=element_blank(), legend.text=element_text(face="italic", size=8), legend.key.size=unit(2,"mm"), legend.direction="vertical", panel.background=element_blank(), plot.background=element_blank())+
labs(x="Rugosity rank", y="% reef cover")+
scale_y_continuous(expand=c(0,0))+
scale_fill_manual(values=c("slategrey",spCols[3],spCols[1],spCols[2],"grey"))
compplot

#################################### 



linedat <- df[df$site%in%c("site012", "site001", "site004", "site006", "site007","site013", "site008"),]
linedat$x <- c(65, 22, 16, 40, 60, 67, 45)
linedat$y <- c(3.5, 1.4, 2.8, 3.5, 2.2, 2.9, 1.8)
linedat$site
#"site001" "site004" "site006" "site007" "site008" "site012" "site013"

geometry <- ggplot(df, aes(reef_cover, site_rugosity))+
labs(x="% reef cover", y="Site rugosity")+geom_point()
geometry

geomFULL <- plot_grid(NA, plot_grid(geometry+
geom_smooth(data=df, aes(reef_cover, site_rugosity),method="lm", se=F, col="black", size=0.3)+
geom_point(data=df, aes(reef_cover, site_rugosity, fill=dominant), shape=21, stroke=0.2, size=1.7)+
geom_segment(data=linedat, aes(x=reef_cover, y=site_rugosity, xend=x, yend=y), size=0.15, col="grey")+
guides(fill="none")+
scale_fill_manual(values=c("slategrey",spCols[3],spCols[1],spCols[2],"grey"))+
#geom_text(data=df, aes(x=reef_cover, y=site_rugosity, label=site), size=2)+
theme_classic(), NA, rel_widths=c(1,0.3)), 
rel_heights=c(0.25,1), ncol=1)+
draw_plot(site004, 0.37, 0.1, 0.2, 0.3)+
draw_plot(site013L, 0.58, 0.16, 0.23, 0.33)+
draw_plot(site008L, 0.68, 0.32, 0.2, 0.3)+
draw_plot(site012, 0.73, 0.48, 0.2, 0.3)+
draw_plot(site001, 0.62, 0.65, 0.23, 0.33)+
draw_plot(site007L, 0.38, 0.68, 0.2, 0.3)+
draw_plot(site006, 0.2, 0.51, 0.2, 0.3)
geomFULL

#################################### 

fig1 <- plot_grid(NA, plot_grid(plot_grid(chamb, NA, plot_grid(NA, compplot+theme(legend.position=c(0.48, 1.35), legend.background=element_rect(colour="grey", size=0.1), legend.margin=margin(-4, 1, 1, 1), legend.text=element_text(size=6.5))+
guides(fill=guide_legend(ncol=2)), rel_widths=c(0.1, 1)), ncol=1, rel_heights=c(1,0.25,0.8), labels=c("A","","C"), vjust=-0.1, label_size=9), geomFULL, rel_widths=c(0.8,1), labels=c("","B"), label_size=9, vjust=7), ncol=1, rel_heights=c(0.05, 1))+
draw_label("Study sites", 0.5, 0.97, fontface="bold", size=9)
fig1



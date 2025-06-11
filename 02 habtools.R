# New theory test


library("Rvcg")
library("rgl")
# devtools::install_github("jmadinlab/habtools")
library("habtools")

files <- dir("3D/site_CUTS/")#site_CUTS
files
#files <- "Xsite63_Vol.ply"
store <- data.frame()

for (i in files[1:5]) {
  ply <- vcgPlyRead(paste0("3D/site_CUTS/", i))
  ply <- vcgClean(ply, sel=0:6,iterate=T)
  ply <- vcgIsolated(ply)

  #cb <- cubes(ply, 2)
  #FD <- cb$fd
  FD <- fd_cubes(ply, 2)
  R_PA <- rg(ply, L0=2, method="mesh")
  SA <- vcgArea(ply)
  #L <- cb$cubes$L0[length(cb$cubes$L0)]
  #R_L <- SA / L^2
  Vol <- vcgVolume(ply)
  PA <- planar(ply, L0=2)
  
  store <- rbind(store, data.frame(i, SA=SA, Vol=Vol, PA=PA,FD=FD, R_PA=R_PA)) # L=L, , R_L=R_L
  
}

store

# write.csv(store, "3D/site_habtools/output1to5.csv", row.names = FALSE)
# transfored to z?
# 1-5 - problem with 03?
# 6-10 - site06 small?
# 11 - 20, 12?
# 49 small?






plot(log(SA) ~ log(Vol), store)
points(log(SA) ~ log(Vol), store[store$i=="mcap_2021_01_nursery_garrett.ply",], col="red")
mod <- lm(log(SA) ~ log(Vol), store)
summary(mod)

plot(log(L) ~ log(Vol), store)
mod <- lm(log(L) ~ log(Vol), store)
summary(mod)

##################################################################
##################################################################

# EXPORT DATA

df <- read.csv("data/probe_data/params.csv")

chamb_planar <- pi*((0.6/2)^2 )  # planar area of 0.6m diam chamber (m3)


hi3D <- read.csv("data/probe_data/chambers_hi.csv")
hi3D$SA <- hi3D$site_area * 0.01 # to cm2
hi3D$Vol <- hi3D$site_volume * 0.001 # to cm3
hi3D <- data.frame(i = hi3D$site, SA=hi3D$SA, Vol=hi3D$Vol, PA= pi*((0.55/2)^2 )/0.0001, FD=NA, R_PA=NA)

# 3D files 
d1 <- read.csv("data/3D/site_habtools/output1to5.csv")
d2 <- read.csv("data/3D/site_habtools/output6to10.csv")
d3 <- read.csv("data/3D/site_habtools/output11to15.csv")
d4 <- read.csv("data/3D/site_habtools/output16to20.csv")
d5 <- read.csv("data/3D/site_habtools/output21to25.csv")
d6 <- read.csv("data/3D/site_habtools/output26to30.csv")
d7 <- read.csv("data/3D/site_habtools/output31to35.csv")
d8 <- read.csv("data/3D/site_habtools/output36to40.csv")
d9 <- read.csv("data/3D/site_habtools/output41to45.csv")
d10 <- read.csv("data/3D/site_habtools/output46to50.csv")
d11 <- read.csv("data/3D/site_habtools/output51to55.csv")
d12 <- read.csv("data/3D/site_habtools/output56to60.csv")
d13 <- read.csv("data/3D/site_habtools/output61to65.csv")
d14 <- read.csv("data/3D/site_habtools/output66to70.csv")
d <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, hi3D)
head(d)
d$site <- gsub(".ply", "",d$i)
d$site <- ifelse(d$site=="site11", "site12", ifelse(d$site=="site12", "site11", d$site)) #?????


# corrections - vol 
d$Vol[d$site=="site03"] <- d$Vol[d$site=="site44"] #1131.021 #Xsite03_Vol.ply
d$Vol[d$site=="site06"] <- 1668.325 #Xsite06_Vol.ply
d$Vol[d$site=="site12"] <- 12841.8 #Xsite12_Vol.ply
d$Vol[d$site=="site46"] <- 226
d$Vol[d$site=="site36"] <- 28000
d$Vol[d$site=="site63"] <- 3357.806


df$siteRep <- df$site
df$site <- gsub("_pump", "", df$site )
df$site <- gsub("_swap", "", df$site )
df$site

df$SAcm2 <- d$SA[match(df$site, d$site)]
df$SAcm2[df$dominant=="Sand"] <- chamb_planar/0.0001
df$SAm2 <- df$SAcm2 * 0.0001
df$Volcm3 <- d$Vol[match(df$site, d$site)]
df$Volcm3[df$dominant=="Sand"] <- 0
df$Volm3 <- df$Volcm3 * 0.000001 
df$PA <- (d$PA[match(df$site, d$site)]) * 0.0001 
df$PA[df$dominant=="Sand"] <- chamb_planar
df$Rug <- df$SAm2  /  df$PA  #chamb_planar


head(df)

# write.csv(df, "data/data.csv")



##### MAP OF SITES
library('sf')
library('rnaturalearth')
library('rnaturalearthdata')
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")

library('rnaturalearthhires')
library(rgeos)
library(ggspatial)
library(ggrepel)



worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]




world <- ne_states(country='canada',returnclass = 'sf')
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

NL_map<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-61.00, -52.00), ylim = c(46.00, 52.00), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.3,text_cex=1) +
  theme_bw(base_size=15)+theme(axis.text=element_blank(),
                               axis.ticks=element_blank())

Lat<-c(47.803947,47.212302,47.722456)
Long<-c(-52.787087,-52.844915,-52.835011)
Site<-("Biscayan Cove", 'Tors Cove', 'Bauline')
SiteLab<-c("A", 'B', 'C')
Locs<-tibble(Lat,Long,Site,SiteLab)

Avalon<-ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-54.50, -52.50), ylim = c(46.50, 48.30), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5,text_cex=1) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw(base_size=15)+geom_point(aes(x=B1,y=B2),colour='darkcyan',size=4,shape=25,fill='darkcyan')+
  geom_point(data=Locs,aes(x=Long,y=Lat),colour='darkcyan',size=4,shape=25,fill='darkcyan')+
  geom_text_repel(data=Locs,aes(x=Long,y=Lat,label=SiteLab),size=5)

pdf(width = 7, useDingbats=TRUE,height = 7, bg="white", file="Map")

Avalon + annotation_custom(ggplotGrob(NL_map), xmin = -54.45, xmax = -53.4,
                           ymin = 47.3, ymax = 48.5)

dev.off()




pos.dat <- cod.dat %>% 
  group_by(HLogId) %>% 
  summarise(Lat= mean(as.numeric(Lat)), 
            Lon=mean(abs(as.numeric(Lon))*-1))  %>%
  filter(Lon > -100 & Lon < -40) %>%
  filter(Lat < 100 & Lat >40)



library(ggOceanMaps)
locs <- read.csv("CodQualityCommunity_latlon.csv")

basemap(limits = c (-60, -50, 53, 46), rotate = F, bathymetry = F) + xlab("Longitude") + ylab("Latitude") + 
  geom_spatial_point(data= locs, aes(x=Long, y=Lat), inherit.aes = F, shape=24, color="red", fill="red") + 
  geom_spatial_text_repel(data= locs, aes(x=Long, y=Lat, label=Community), size=2, max.overlaps=84) 
  





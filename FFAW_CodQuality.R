library(tidyverse)
library(readxl)
library(ggOceanMaps)
library(scales)
library(ggrepel)
library(lubridate)

cod.dat <- read_xlsx("Cod Quality 18-20 MI Data_ec.xlsx", sheet=1)

table(cod.dat$Commercial)

table(cod.dat$StewardShip)

table(cod.dat$Grade)

length(unique(cod.dat$HLogId))


class(cod.dat$Lat )

range(pos.dat$Lon, na.rm= T)


# there is a lot of missing position data...about 25% is either "NULL" or erroneous entry

pos.dat <- cod.dat %>% 
  group_by(HLogId) %>% 
  summarise(Lat= mean(as.numeric(Lat)), 
            Lon=mean(abs(as.numeric(Lon))*-1))  %>%
             filter(Lon > -100 & Lon < -40) %>%
              filter(Lat < 100 & Lat >40)

basemap(limits = c (-60, -40, 60, 40), rotate = T, bathymetry = T) +
  geom_spatial_point(data= pos.dat, aes(x=Lon, y=Lat), inherit.aes = F, shape=24, color="red", fill="red") 



#sort by gear type

gear.tbl <- table(cod.dat$GTName) %>% melt()
colnames(gear.tbl)[1] <- "Gear"
gear.tbl <- gear.tbl %>% arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) 

ggplot(gear.tbl, aes(x = "", y = value, fill=fct_inorder(Gear))) + 
  geom_bar(width=1, stat = "identity", alpha=0.8) + 
  geom_label_repel(aes(label = prop), size=6, show.legend = F, nudge_x = 0.1) +
  #geom_text(aes(x = 1.5, y = midpoint, label = label), angle=45) +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Paired") +
   theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(hjust=0.5), 
        legend.direction = "vertical",
        legend.position = "right",
        legend.background = element_blank(),
        legend.key=element_blank(),
        legend.key.size = unit(1, "line"),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        text = element_text(size=13,colour = "black", face = "bold"))



#time x gear x effort

ggplot(cod.dat, aes(x=Date, fill=GTName)) + geom_histogram(bins= 32, color="grey42") +
scale_x_datetime(labels = date_format("%b-%d-%Y"), date_breaks = "1 month") +
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                            axis.title = element_blank())




#condition data prep

colnames(cod.dat)[75:88] # names of grading columns

cond.dat <- cod.dat[,c(6,23,25,56,64,75:88)]
cond.dat <- cond.dat[-which(cond.dat$Grade == "NULL"), ] # drop "NULL" values from df

cond.dat$month <- month(cond.dat$Date)
cond.dat$year  <- year(cond.dat$Date) 
cond.dat$day   <- day(cond.dat$Date)

cols <- colnames(cond.dat)[7:19]
cond.dat[cols] <- lapply(cond.dat[cols], factor) 

ggplot(cond.dat, aes(x=month, y=Grade)) + geom_point()



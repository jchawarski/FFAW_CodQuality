library(tidyverse)
library(readxl)
library(ggOceanMaps)
library(scales)
library(ggrepel)
library(lubridate)

cod.dat <- read_xlsx2("Cod Quality 18-20 MI Data_ec.xlsx", sheet=1)

cod.dat <- read.csv("FFAWCodQuality18-20.csv")


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

basemap(limits = c (-60, -40, 60, 40), rotate = T, bathymetry = F) +
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

cond.melt <- reshape2::melt(cond.dat, id=c("Date", "DateSet", "DateHaul", "DelDate", "ProcDate", "SampleNo", "year", "month", "day")) 

cond.melt %>% filter(value %in% "B") %>% 
  filter(!variable %in% "Grade") %>%
  ggplot(., aes(x=variable)) +
  geom_bar(fill="navyblue") + 
  xlab("Condition") + ylim(0,3000) +
  annotate("text", y=3000, x=5, label= "B Grading") + 
  theme_minimal(base_size=14)


time.dat <- cod.dat %>% select(Date, DateSet, TimeSet, DateHaul, TimeHaul, StowStart, StowEnd, LandTime, UnloadStart, UnloadEnd, PUDate, PUTime, DelDate, DelTime, ProcDate, ProcStart, ProcEnd)

time.dat$Set_DT <- mdy_hms(paste(time.dat$DateSet, time.dat$TimeSet), tz="Canada/Newfoundland")
time.dat$Haul_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$TimeHaul), tz="Canada/Newfoundland")
time.dat$Stowstart_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$StowStart), tz="Canada/Newfoundland")
time.dat$Stowend_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$StowEnd), tz="Canada/Newfoundland")
time.dat$LandTime_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$LandTime), tz="Canada/Newfoundland")
time.dat$UnloadStart_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$UnloadStart), tz="Canada/Newfoundland")
time.dat$UnloadEnd_DT <- mdy_hms(paste(time.dat$DateHaul, time.dat$UnloadEnd), tz="Canada/Newfoundland")
time.dat$PU_DT <- mdy_hms(paste(time.dat$PUDate, time.dat$PUTime), tz="Canada/Newfoundland")
time.dat$Del_DT <- mdy_hms(paste(time.dat$DelDate, time.dat$DelTime), tz="Canada/Newfoundland")
time.dat$ProcStart_DT <- mdy_hms(paste(time.dat$ProcDate, time.dat$ProcStart), tz="Canada/Newfoundland")
time.dat$ProcEnd_DT <- mdy_hms(paste(time.dat$ProcDate, time.dat$ProcEnd), tz="Canada/Newfoundland")



time.dat <- time.dat[complete.cases(time.dat), ] # Store the complete cases subset in a new data frame

time.dat$Index <- c(1:length(time.dat$Date))

time.dat  <- time.dat[,c(18:29)]
time.melt <- melt(time.dat, id="Index")

dislocations <- runif(11,-1.5,1.5)

time.melt %>% filter(Index %in% 1200) %>% ggplot(., aes(x=value, y=0, color=variable, label=variable)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_point(size=4) + 
  geom_segment(  aes(x = value, y=dislocations, xend=value, yend=0, alpha=.7 )) +
  geom_text(aes(x = value, y = dislocations, label=variable), angle = 45, position="jitter") + ylim(-2,2) + 
  
  theme_minimal(base_size=14) +
 theme(legend.position="none", 
       axis.text.y = element_blank(),
       axis.ticks.y = element_blank(),
       axis.title.y = element_blank())
       



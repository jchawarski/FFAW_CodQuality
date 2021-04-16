

cod.dat <- read_xlsx("Cod Quality 18-20 MI Data_V1.3.xlsx", sheet=1)

cod.dat <- read_excel("Cod Quality 18-20 MI Data_V1.3.xlsx", sheet=1)

cod.dat <- read.csv("Cod Quality 18-20 MI Data_V3.csv")

cod.dat <- read.csv("GillNetSoakTime_corrected.csv")

cod.dat2 <- read.csv("Soak time corrected_Other gear.csv")

cod.dat <- cod.dat %>% bind_rows(cod.dat2)

cod.dat$GTName <- trimws(cod.dat$GTName)



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
  #geom_label_repel(aes(label = prop), size=6, show.legend = F, nudge_x = 0.1) +
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

time.dat$Index <- c(1:length(time.dat$Date)) #create index column 
cod.dat$Index <- c(1:length(cod.dat$Date))

time.dat <- time.dat[complete.cases(time.dat), ] # Store the complete cases subset in a new data frame

cod.sub <- cod.dat %>% 
  select(Index, 
         Site,
         HLogId, 
         UNQID_HARVESTER,
         UNQID_GRADERS,
         GTName, 
         Reefer, 
         IceType,
         ProdWgt, 
         Grade, 
         GradeBP,
         MaxDepth) %>%
           mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% 
            filter(!Grade %in% c("NULL", "R"))


time.sub <- time.dat %>% left_join(., cod.sub, by="Index")
time.sub$GTName <- trimws(time.sub$GTName)
time.sub <- time.sub %>% mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% 
                  filter(!Grade %in% c("NULL", "R"))

time.sub$date <- as.POSIXct(as.character(time.sub$Date), format='%m/%d/%Y')
time.sub$year <- year(time.sub$date)


#Example timeline
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

# look at variability of timeline in CoC

#Soak time - how long the gear was in the water
time.sub$Soak <- as.numeric(time.sub$Haul_DT - time.sub$Set_DT)/60 # calculated in minutes
time.sub %>% filter(between(Soak, 0, 10000)) %>%
  ggplot(., aes(x=Grade, y=Soak/60)) + 
  geom_boxplot() +  ylab("Soak Time [hours]") + 
  theme_minimal(base_size=14) + facet_grid(~year)

#Soak time distribution across years

time.sub$Year <- year(as.POSIXct(as.character(time.sub$Date), format='%d/%m/%Y'))
time.sub %>% filter(between(Soak, 0, 10000)) %>% 
  filter(GTName == "NETS") %>%
  ggplot(., aes(x=as.factor(Year), y=Soak/60)) + ylim(0,40) +
  geom_boxplot(outlier.shape = NA) +  ylab("Soak Time [hours]") + 
  theme_minimal(base_size=14)

#soak time - UPDATED 
cod.sub$date <- as.POSIXct(as.character(cod.sub$Date), format='%m/%d/%Y')
cod.sub$year <- year(cod.sub$date)
cod.sub %>% 
  ggplot(., aes(x=Grade, y=Soak/60)) + 
  geom_boxplot() +  ylab("Soak Time [hours]") + 
  theme_minimal(base_size=14) + facet_grid(~year)



#Dock Sitting time - time from Unload to Pickup
time.sub$Dock <- as.numeric(time.sub$PU_DT - time.sub$UnloadEnd_DT)/60 # calculated in minutes
time.sub %>% filter(between(Dock, 0, 10000)) %>%
  ggplot(., aes(x=Grade, y=Dock/60)) + 
  geom_boxplot() +  ylab("Dock Wait [hours]") +
  theme_minimal(base_size=14) + facet_grid(~GTName)

#Transport time - from Pickup to Delivery
time.sub$Transport <-  as.numeric(time.sub$Del_DT - time.sub$PU_DT)/60
time.sub %>% filter(between(Transport, 0, 10000)) %>%
ggplot(., aes(x=Grade, y=Transport/60)) + 
  geom_boxplot() +  ylab("Transport [hours]") +
  theme_minimal(base_size=14) + facet_grid(~GTName)

#Process wait time - from Delivery to Processing
time.sub$Factory <- as.numeric(time.sub$ProcStart_DT - time.sub$Del_DT)/60 # calculated in minutes
time.sub %>% filter(between(Factory, 0, 10000)) %>%
ggplot(., aes(x=Grade, y=Factory/60)) + 
  geom_boxplot() +  ylab("Factory Wait [hours]") +
  theme_minimal(base_size=14) + facet_grid(~GTName)

#Total time from haul to processing
time.sub$Total <- as.numeric(time.sub$ProcStart_DT - time.sub$Haul_DT)/60 
time.sub %>% filter(between(Total, 0, 10000)) %>%
  drop_na(Grade) %>% filter(!GTName =="NULL") %>%
  ggplot(., aes(x=Grade, y=Total/60)) + 
  geom_boxplot() +  ylab("Haul to Processing Time [hours]") +
  theme_minimal(base_size=14) + facet_grid(~GTName)


# summarize the percentage of each grade
cod.sum <- cod.sub %>% 
group_by(HLogId, Grade) %>% 
  filter(!Grade %in% "NULL") %>%
  summarise(n = n(), 
            wwt = unique(ProdWgt)) %>%
  mutate(freq = n / sum(n))

# plot the percentage of grade B against the Total Weight of Catch
cod.sum %>% 
  filter(Grade %in% "B") %>% 
  ggplot(., aes(x=freq, y=as.numeric(wwt))) + 
  geom_point()



filter(Grade %in% "A") %>%
  summarize(dwngrd = 1 - mean(freq))




#distributions
hist(cod.sum$freq, breaks= 10) 
hist(as.numeric(cod.sum$wwt), breaks =100)


# quick look at the effects of harvesters on grade
cod.sum <- cod.sub %>% 
  filter(GTName == "NETS") %>%
  group_by(HLogId, Grade) %>% 
 filter(!Grade %in% "NULL" ) %>%
  summarise(n = n(), 
            wwt = unique(ProdWgt), 
            site = unique(Site),
            harv = unique(UNQID_HARVESTER)) %>%
              mutate(freq = n / sum(n))

H_102 <- cod.sum %>% filter(Grade %in% "A") %>% 
  filter(harv %in% "H-102") %>% filter(freq < 1)
H_204 <- cod.sum %>% filter(harv %in% "H-204")
H_104 <- cod.sum %>% filter(harv %in% "H-104")


# site by harvester and their ranking by grade - this is likely an incorrect calculation of their stats
  cod.sum %>% filter(Grade %in% "A") %>% filter(freq < 1) %>%
  ggplot(., aes(x=harv, y=site, fill=freq)) + geom_tile(stat="identity") +
  scale_fill_gradient(low = "red", high = "yellow", na.value = NA) +
  theme_minimal(base_size=8) + theme(axis.text.x = element_text(angle=90)) 

  
# create a new df with each harvesters downgraded score as the mean  
 harv.sum <- cod.sum %>% 
   group_by(harv, site) %>%
    filter(Grade %in% "A") %>%
   summarize(dwngrd = 1 - mean(freq))
                                                      
 ggplot(harv.sum, aes(x=harv, y=site, fill=dwngrd*100)) + geom_tile(stat="identity") +
   scale_fill_viridis_c(name = "Percent Downgraded")  +
   theme_minimal(base_size=8) + theme(axis.text.x = element_text(angle=90),
                                      legend.position = "top")

# the effect of total catch weight on downgrading
 cod.sum %>% 
   filter(Grade %in% "A") %>% 
   ggplot(., aes(y=(1-freq)*100, x=log(as.numeric(wwt)))) + 
   xlab("Log Total Weight of Catch") + 
   ylab("Percent Downgraded") +
   geom_jitter() + theme_minimal(base_size=12)
 
  
#total downgrades by region
 

site.grd <- cod.sub %>% 
  filter(GTName == "COD POTS") %>%
  group_by(Site, Grade) %>% 
  filter(!Grade %in% "NULL" ) %>% 
  summarise(n = n()) %>% 
   mutate(freq = n / sum(n)) %>%
    drop_na(freq) %>%
    filter(Grade %in% "A") 
 
ggplot(site.grd, aes(x=reorder(Site, -freq), y=(1-freq)*100)) + 
  geom_bar(stat="identity", fill="navyblue")  + coord_flip() + ylab("Percent Downgraded") + xlab("Site") +
  theme_minimal(base_size=10) 


ggplot(, aes(x=Site, ))

# grader effects 
# there appears to be more than one grader per haul
 
grd.sum <- cod.sub %>% 
   group_by(UNQID_GRADERS, Grade) %>% 
  filter(!Grade %in% "NULL" ) %>% 
    summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  drop_na(freq) %>%
  filter(Grade %in% "A") 

ggplot(grd.sum, aes(x=reorder(UNQID_GRADERS, -n), y=n)) + 
  geom_bar(stat="identity", fill="navyblue")  +
  coord_flip() + 
  ylab("Number of Graded Fish") + 
  xlab("Grader") +
  theme_minimal(base_size=10) 


ggplot(grd.sum, aes(y=log(n), x=(1-freq)*100)) + 
  geom_point()+ 
  ylab("Number of Graded Fish") + 
  xlab("Percent Downgraded") +
  theme_minimal(base_size=10) 


grd.sum <- cod.sub %>% 
  group_by(UNQID_GRADERS, Site, Grade) %>% 
  filter(!Grade %in% "NULL" ) %>% 
  summarize(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  drop_na(freq) %>%
  filter(Grade %in% "A") 



ggplot(grd.sum, aes(x=UNQID_GRADERS,
                    y=Site, fill=(1-freq)*100)) +
  geom_tile(stat="identity") +
  scale_fill_viridis_c(name = "Percent Downgraded")  +
  theme_minimal(base_size=8) + theme(axis.text.x = element_text(angle=90),
                                     legend.position = "top")

#additional factors that appear to have little effect on grade
ice <- cod.sub %>%   filter(!Grade %in% "NULL" ) %>% 
 group_by(Ice, Grade) %>% 
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))
  
icetype  <-  cod.sub %>%   
  group_by(IceType, Grade) %>% 
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

reefer <- cod.sub %>%   filter(!Grade %in% "NULL" ) %>% 
  group_by(Reefer, Grade) %>% 
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

depth <-  cod.sub %>%   filter(!Grade %in% "NULL" ) %>%  
  filter(GTName == "NETS") %>%
  group_by(MaxDepth, Grade) %>% 
  summarize(n=n()) %>%
  mutate(freq = n / sum(n))

depth %>% filter(Grade == "A") %>%
   ggplot(., aes(x=as.numeric(MaxDepth), y=freq)) + geom_point()




#temperature variability within the major dataset. 


temp.sub <- cod.dat %>% 
  select(Index, 
         HLogId,
         Site,
         GTName,
         WaterTemp,
         FishTemp,
         FishTemp.1,
         FishTemp.2,
         FishTemp.3,
         FishTemp.4,
         Grade) %>%
  mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% 
  filter(!Grade %in% c("NULL", "R"))

# of the 45,000 observations, 15,000 are NULL

temp.sub[temp.sub == "NULL"] <- NA
temp.sub <- temp.sub[complete.cases(temp.sub), ] # Store the complete cases subset in a new data frame

# of the 45,000 observations 5,000 have temperature recorded for entire chain of custody

colnames(temp.sub)[6] <- "Temp_catch" 
colnames(temp.sub)[7] <- "Temp_land"
colnames(temp.sub)[8] <- "Temp_trans"
colnames(temp.sub)[9] <- "Temp_proc" 
colnames(temp.sub)[10] <- "Temp_insp" 

temp.sub$WaterTemp <- as.numeric(as.character(temp.sub$WaterTemp))
temp.sub$Temp_catch <- as.numeric(as.character(temp.sub$Temp_catch))
temp.sub$Temp_land <- as.numeric(as.character(temp.sub$Temp_land))
temp.sub$Temp_trans <- as.numeric(as.character(temp.sub$Temp_trans))
temp.sub$Temp_proc <- as.numeric(as.character(temp.sub$Temp_proc))
temp.sub$Temp_insp <- as.numeric(as.character(temp.sub$Temp_insp))


watertemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  filter(WaterTemp > -2) %>%
  ggplot(., aes(x=Grade, y=WaterTemp)) + geom_boxplot() + theme_minimal()

catchtemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  ggplot(., aes(x=Grade, y=Temp_catch)) + geom_boxplot() + theme_minimal()

landtemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  ggplot(., aes(x=Grade, y=Temp_land)) + geom_boxplot() + theme_minimal()

transtemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  ggplot(., aes(x=Grade, y=Temp_trans)) + geom_boxplot() + theme_minimal()

proctemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  ggplot(., aes(x=Grade, y=Temp_proc)) + geom_boxplot() + theme_minimal()

insptemp <- temp.sub %>% filter(GTName %in% "NETS") %>% 
  ggplot(., aes(x=Grade, y=Temp_insp)) + geom_boxplot() + theme_minimal()

cowplot::plot_grid(watertemp, catchtemp, landtemp, transtemp, proctemp, insptemp)

#load temperature logger data. 
temp.dat <- read_xlsx("Temperature logger files/1247_34-22-08-20-1.xlsx", sheet=1)
temp.dat <- read_xlsx("Temperature logger files/1303_51-23-08-20-1.xlsx", sheet=1)
temp.dat <- read_xlsx("Temperature logger files/1526_36-13-09-20-1.xlsx", sheet=1)

temp.dat <- temp.dat[,c(1:3)]
colnames(temp.dat) <- c("Index","datetime", "tempC")

ggplot(temp.dat, aes(x=datetime, y=tempC)) + geom_path() +
  theme_minimal()

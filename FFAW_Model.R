#FFAW Cod Quality Project - Modelling the effects of fishery on Cod Quality

# Quality is defined as score indicating the proportion of fish in a given catch which recieved a perfect score

# Data prep for the model. 
library(tidyverse)
library(lubridate)

#set up a standard theme for all plots 
theme <- theme_bw(base_size = 14)

#modelling tools
library(lme4)
library(DHARMa)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

### DATA PREPARATION ###

cod.dat$Index <- c(1:length(cod.dat$Date)) # add an index for each measurement 

#subset the relevant information for the model
cod.sub <- cod.dat %>% 
  select(Index,
         Date,
         Site,
         HLogId, 
         UNQID_HARVESTER,
         UNQID_GRADERS,
         GTName, 
         Grade, 
         Texture,   #included for the secondary models
         Bruising,  #included for the secondary models
         ProdWgt,
         WaterTemp,
         FishTemp.2, 
         DateSet,
         TimeSet,
         ProcDate, 
         ProcStart,
         DateHaul,
         TimeHaul) %>%
  mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% # in case this wasnt done before
  filter(!Grade %in% c("NULL"))
  
# compute the handling and transport time as a single variable "Total"
cod.sub$Haul_DT <- mdy_hms(paste(cod.sub$DateHaul, cod.sub$TimeHaul), tz="Canada/Newfoundland")
cod.sub$ProcStart_DT <- mdy_hms(paste(cod.sub$ProcDate, cod.sub$ProcStart), tz="Canada/Newfoundland")
cod.sub$Total <- as.numeric(cod.sub$ProcStart_DT - cod.sub$Haul_DT)/60  # total time between haul and process in minutes

# compute the soak time - "Soak"
cod.sub$Set_DT <- mdy_hms(paste(cod.sub$DateSet, cod.sub$TimeSet), tz="Canada/Newfoundland")
cod.sub$Haul_DT <- mdy_hms(paste(cod.sub$DateHaul, cod.sub$TimeHaul), tz="Canada/Newfoundland")
cod.sub$Soak <- as.numeric(cod.sub$Haul_DT - cod.sub$Set_DT)/60 

colnames(cod.sub)[13] <- "Trans_Temp" # rename Fish.Temp3 to "Trans_temp"

# calculate summary by haul and condition for model input
# before doing this, make sure to replace Grade/Bruising/Texture with the desired value in three places in the pipeline below
cod.in <- cod.sub %>%
          filter(GTName %in% "NETS") %>%
        group_by(HLogId,Bruising) %>%                 # REPLACE Grade with bruising, or texture - depending on the model
          dplyr::summarise(gear = unique(GTName), 
                    date = unique(Date),
                    n = n(), 
                    wwt = unique(ProdWgt),
                    site = unique(Site),
                    time = unique(Total),
                    soak = unique(Soak),
                    water_temp = unique(WaterTemp),
                    trans_temp = unique(Trans_Temp),
                    harvester = unique(UNQID_HARVESTER),
                    grader = paste(unique(UNQID_GRADERS), collapse=",")) %>%
                          ungroup() %>%
                            tidyr::complete(Bruising,                 #REPLACE Grade with bruising, or texture - depending on the model
                                            nesting(HLogId, 
                                                    wwt, gear, 
                                                    date, site, time,
                                                    soak, water_temp, 
                                                    trans_temp, harvester)) %>% # completes all possible combinations of grades
                             dplyr::arrange(., HLogId) %>% 
                              dplyr::mutate(n = tidyr::replace_na(n, 0)) %>% # replaces all NAs from complete() with 0.  
                              group_by(HLogId) %>%  
                                mutate(freq = n / sum(n),       # calculates the frequency of A scores within the haul
                                            sum = sum(n))  %>%     # #gives us the total number of graded fish in each haul
                                      filter(Bruising %in% "A") %>%     #rREPLACE Grade with bruising, or texture - depending on the model
                                       filter(between(time, 0, 10000)) %>% # removes extraneous values
                                        filter(soak > 0)                  # soak time cannot be negative

#pull out year
cod.in$date <- as.POSIXct(as.character(cod.in$date), format='%m/%d/%Y')
cod.in$year <- year(cod.in$date)

#convert variables to numeric due to poor formatting in dataset
cod.in$water_temp <- as.numeric(as.character(cod.in$water_temp))
cod.in$trans_temp <- as.numeric(as.character(cod.in$trans_temp))
cod.in$wwt <- as.numeric(as.character(cod.in$wwt))
cod.in$time <- as.numeric(as.character(cod.in$time))
# make sure random effects are coded as factors
cod.in$harvester <- as.factor(cod.in$harvester)
cod.in$site <- as.factor(cod.in$site)
cod.in$grader <- as.factor(cod.in$grader)

#convert from frequency of A to frequency of non-"A"
cod.in$freq <- 1- cod.in$freq

# create a binary index for quality - as we discovered only good fits using a binomial model
cod.in <- cod.in %>% mutate(quality = case_when(freq < 0.1 ~ 1,
                                                freq >= 0.1 ~ 0))

#create a binary temperature variable to deal with bimodal distribution
#cod.in <- cod.in %>% mutate(bi.temp = case_when(water_temp <= 5 ~ 0, 
#                                               water_temp > 5 ~ 1))

### DATA EXPLORATION AND PLOTS ###
#quick and dirty data explorations done here
plot(cod.in$time, cod.in$temp)
plot(cod.in$trans_temp, cod.in$wwt)
plot(cod.in$wwt, cod.in$freq)
plot(cod.in$time, cod.in$freq)
plot(cod.in$temp, cod.in$freq)
hist(cod.in$freq, breaks =25)

#quality as frequency on non-A
raw <- cod.in %>%
ggplot(., aes(x=freq)) + geom_histogram(bins=10, color="black") + theme

#quality as binary
binary <- cod.in %>%
ggplot(., aes(x=quality, fill= quality)) + geom_bar(color="black") + 
  scale_fill_manual(values= c("grey42", "grey70")) + 
  theme

library(cowplot)
plot_grid(raw, binary, ncol=2, align="hv")

#histogram of soak time
soak <- cod.in %>%
  ggplot(., aes(x=soak/60)) + geom_histogram(bins=20, color="black", alpha=0.8) + 
  xlab("Soak Time [hrs]") + theme
#histogram of total transport time
total <- cod.in %>%
  ggplot(., aes(x=time/60)) + geom_histogram(bins=20, color="black", alpha = 0.8) + 
  xlab("Transport Time [hrs]") + theme

plot_grid(soak, total, ncol=2, align="hv")


tt <- cod.in %>%
  ggplot(., aes(x=trans_temp)) + geom_histogram(bins=20, color="black", alpha = 0.8) + 
  xlab("Transport Temp") + theme

wt <- cod.in %>%
  ggplot(., aes(x=water_temp)) + geom_histogram(bins=20, color="black", alpha = 0.8) + 
  xlab("Water Temp") + theme


plot_grid(tt, wt, ncol=2, align="hv")


### MODELLING ###

# Setting up the model
library(glmmTMB)  #tried for 'other' types of distributions like quasi-poisson - not useful in the ednd
tmb.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
                 data=cod.in, family = nbinom1())

first.glm <- glmer.nb(freq ~ wwt + time + temp + (1 | harvester) + (1 | grader),  
                     data=cod.in)

first.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader),  
                      data=cod.in, family="binomial")

qqnorm(residuals(first.glm)) # qnorm plot, but DHARMa is better

#predicted vs. residual plot - DHARMa is better
ggplot(data.frame(eta=predict(first.glm,type="link"),pearson=residuals(first.glm,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


ggplot(data.frame(lev=hatvalues(first.glm),pearson=residuals(first.glm,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

#poisson distribution - full dataset
pois.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader) + (1 | site),  
      data=cod.in, family="poisson")


E2 <- resid(pois.glm, type = "pearson")
N  <- nrow(cod.in)
p  <- length(coef(pois.glm))   
sum(E2^2) / (N - p)

testDispersion(pois.glm)
simulationOutput <- simulateResiduals(fittedModel = pois.glm, plot = T)

#poisson distribution - zeros removed
poisnz.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(trans_temp) + (1 | harvester) + (1 | grader) + (1 | site),  
                  data=subset(cod.in, freq > 0), family="poisson")

testDispersion(poisnz.glm)
simulationOutput <- simulateResiduals(fittedModel = poisnz.glm, plot = T)


#Gaussian distribution, zeros removed
gausnz.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader) + (1 | site),  
                   data=subset(cod.in, freq > 0))

testDispersion(gausnz.glm)
simulationOutput <- simulateResiduals(fittedModel = gausnz.glm, plot = T)
plot(gausnz.glm)
#Gaussian distribution, full dataset
gaus.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader) + (1 | site),  
                  data=cod.in)

testDispersion(gaus.glm)
simulationOutput <- simulateResiduals(fittedModel = gaus.glm, plot = T)
plot(gaus.glm)

#check for overdispersion
E2 <- resid(gaus.glm, type = "pearson")
N  <- nrow(cod.in)
p  <- length(coef(gaus.glm))   
sum(E2^2) / (N - p)

plot(gaus.glm)

testDispersion(gaus.glm)
simulationOutput <- simulateResiduals(fittedModel = gaus.glm, plot = T)

# negative binomial model
nb1.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
                     data=cod.in, family = nbinom1())
testDispersion(nb1.glm)
simulationOutput <- simulateResiduals(fittedModel = nb1.glm, plot = T)

# negative binomial model without zeros
nb1nz.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
                   data=subset(cod.in, freq > 0), family = nbinom1())
testDispersion(nb1nz.glm)
simulationOutput <- simulateResiduals(fittedModel = nb1nz.glm, plot = T)

testZeroInflation(simulationOutput)

# zero inflated negative binomial model - full dataset

zinb.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
        ziformula = ~1,
        data=cod.in, family = nbinom1())


testDispersion(zinb.glm)
simulationOutput <- simulateResiduals(fittedModel = zinb.glm, plot = T)

testZeroInflation(simulationOutput)

# zero inflated poisson model - full dataset

zip.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
                    ziformula = ~1,
                    data=cod.in, family = poisson())

testDispersion(zip.glm)
simulationOutput <- simulateResiduals(fittedModel = zip.glm, plot = T)

# binomial model

binom.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(trans_temp) + (1 | harvester) + (1 | grader) + (1 | site),  
                  data=cod.in, family="binomial")

testDispersion(binom.glm)
simulationOutput <- simulateResiduals(fittedModel = binom.glm, plot = T)

#quasibinomial 

# Here is where the working models begin...
# convert data to binomial - seems to be supported by group. But need to decide on a final threshold
# in case it wasn't calculated above -->
#cod.in <- cod.in %>% mutate(quality = case_when(freq < 0.1 ~ 1,
#                                                 freq >= 0.1 ~ 0))

true.binom <- glmer(quality ~ scale(wwt) + scale(time) +scale(trans_temp) + (1 | harvester) + (1 | site) + (1|grader),  
      data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)
summary(true.binom)

#add soak time to the binomial model

true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) +  (1 | harvester)+ (1 | site),  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)
summary(true.binom)

#add water temperature to the binomial model

true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) + scale(water_temp) +  (1 | harvester) + (1 | site),  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)
summary(true.binom)

#convert water temp to binary form  0 = cold, 1= warm using a 5 deg C threshold
true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) + bi.temp +  (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")


#drop transport temp and total time, because it seems to be useless
true.binom <- glmer(quality ~ scale(wwt) + scale(soak) + scale(water_temp) +   (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")
#same but use binary watertemp
true.binom <- glmer(quality ~ scale(wwt) + scale(soak) + scale(bi.temp) +   (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")

# drop just transport temp
true.binom <- glmer(quality ~ scale(wwt) + scale(soak) + scale(water_temp) +  scale(time) +  (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")

#same but use binary water temp
true.binom <- glmer(quality ~ scale(wwt) + scale(soak) + scale(bi.temp) +  scale(time) +  (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)
summary(true.binom)


# model output, in html table format. 
library(sjPlot)  # also an awesome package for coefficent plots
library(sjmisc)
library(sjlabelled)

tab_model(gaus.glm)
tab_model(gausnz.glm)
tab_model(binom.glm)
tab_model(zinb.glm)
tab_model(true.binom)

#BEST models - re-run the sampling for cod.in for net vs. all gear, adding soak time, and texture and bruising both with soak time. 

binom1.ag <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + (1 | harvester) + (1 | grader) + (1 | site) ,  
                    data=cod.in, family="binomial")

binom2.ag <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) +  (1 | harvester) + (1 | grader) + (1 | site) ,  
                    data=cod.in, family="binomial")


binom1.nets <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + (1 | harvester) +  (1 | site) ,  
                   data=cod.in, family="binomial")

binom2.nets <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) +  (1 | harvester) + (1 | site) ,  
                   data=cod.in, family="binomial")


texture.ag <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) + (1 | harvester) + (1 | grader) + (1 | site) ,  
                    data=cod.in, family="binomial")
  
  
bruise.ag  <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) + (1 | harvester)  + (1 | site) ,  
                    data=cod.in, family="binomial")
  

# do this to test for gear effect 
cod.in <- cod.in %>% ungroup() %>% mutate(gear_code = case_when(gear=="NETS" ~ "A",
                                                  gear=="LONGLINE" ~ "B",
                                                  gear=="COD POTS" ~ "C",
                                                  gear=="HANDLINE" ~ "D"))
cod.in$gear_code <- as.factor(cod.in$gear_code)

#gear model
gear.mod <- glmer(quality ~ scale(wwt) + scale(time) + gear_code + (1 | harvester) + (1 | grader) + (1 | site),
                  data=cod.in, family="binomial")

testDispersion(gear.mod)
simulationOutput <- simulateResiduals(fittedModel = gear.mod, plot = T)
summary(gear.mod)


tab_model(binom1.ag, binom2.ag, binom1.nets, binom2.nets, texture.ag, bruise.ag)

bi.ag1 <- plot_model(binom1.ag, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
  xlab("") + ylab("") + labs(title="Quality [All Gear]") + theme 

bi.net1 <- plot_model(binom1.nets, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
  xlab("") +  ylab("") + labs(title="Quality [Gillnets]") + theme 

bi.gear <- plot_model(gear.mod, show.values = T, sort.est = T, vline.color = "red") + 
  scale_x_discrete(labels=rev(c("Longline", "Handline", "Cod Pots", "time", "wwt"))) + # ylim(0.5, 1.5) +
 xlab("Explanatory Variables") + ylab("") + labs(title="Quality [Compare Gear]") + theme 


#bi.ag2 <- plot_model(binom2.ag, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
 # xlab("Explanatory Variables") + ylab("") + labs(title="Quality [All Gear]") + theme 

bi.net2 <- plot_model(binom2.nets, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
  xlab("") + ylab("")  + labs(title="Quality [Gillnets]") + theme 

bi.text <- plot_model(texture.ag, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
  xlab("") + labs(title="Texture [Gillnets]") + theme 

bi.bruise <- plot_model(bruise.ag, show.values = T, sort.est = T, vline.color = "red") + ylim(0.5, 1.5) +
  xlab("") + labs(title="Bruising [Gillnets]") + theme 

# combine all the best models and put them in a single coefficient plot
cowplot::plot_grid(bi.ag1, bi.net1, bi.gear,bi.net2, bi.text, bi.bruise, ncol=2)

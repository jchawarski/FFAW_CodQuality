#FFAW Cod Quality Project - Modelling the effects of fishery on Cod Quality

# Quality is defined as score indicating the proportion of fish in a given catch which recieved a perfect score

# Data prep for the model. 
library(tidyverse)
library(lubridate)

cod.dat$Index <- c(1:length(cod.dat$Date))

cod.sub <- cod.dat %>% 
  select(Index,
         Date,
         Site,
         HLogId, 
         UNQID_HARVESTER,
         UNQID_GRADERS,
         GTName, 
         Grade, 
         ProdWgt,
         WaterTemp,
         FishTemp.2, 
         DateSet,
         TimeSet,
         ProcDate, 
         ProcStart,
         DateHaul,
         TimeHaul) %>%
  #mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% 
  filter(!Grade %in% c("NULL", "R"))

cod.sub$Haul_DT <- mdy_hms(paste(cod.sub$DateHaul, cod.sub$TimeHaul), tz="Canada/Newfoundland")
cod.sub$ProcStart_DT <- mdy_hms(paste(cod.sub$ProcDate, cod.sub$ProcStart), tz="Canada/Newfoundland")
cod.sub$Total <- as.numeric(cod.sub$ProcStart_DT - cod.sub$Haul_DT)/60  # total time between haul and process in minutes


cod.sub$Set_DT <- mdy_hms(paste(cod.sub$DateSet, cod.sub$TimeSet), tz="Canada/Newfoundland")
cod.sub$Haul_DT <- mdy_hms(paste(cod.sub$DateHaul, cod.sub$TimeHaul), tz="Canada/Newfoundland")
cod.sub$Soak <- as.numeric(cod.sub$Haul_DT - cod.sub$Set_DT)/60 


colnames(cod.sub)[11] <- "Trans_Temp"

cod.in <- cod.sub %>%
           filter(GTName %in% "NETS") %>%
        group_by(HLogId, Grade) %>% 
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
                            tidyr::complete(Grade, nesting(HLogId, wwt, gear, date, site, time, soak, water_temp, trans_temp, harvester)) %>% # completes all possible combinations of grades
                             dplyr::arrange(., HLogId) %>% 
                              dplyr::mutate(n = tidyr::replace_na(n, 0)) %>% # replaces all NAs from complete() with 0.  
                              group_by(HLogId) %>%  
                                mutate(freq = n / sum(n),       # calculates the frequency of A scores within the haul
                                            sum = sum(n))  %>%     # #gives us the total number of graded fish in each haul
                                      filter(Grade %in% "A") %>%
                                       filter(between(time, 0, 10000)) %>%
                                        filter(soak > 0)

cod.in$date <- as.POSIXct(as.character(cod.in$date), format='%m/%d/%Y')
cod.in$year <- year(cod.in$date)

cod.in$water_temp <- as.numeric(as.character(cod.in$water_temp))
cod.in$trans_temp <- as.numeric(as.character(cod.in$trans_temp))
cod.in$wwt <- as.numeric(as.character(cod.in$wwt))
cod.in$time <- as.numeric(as.character(cod.in$time))

cod.in <- cod.in[complete.cases(cod.in), ] # removes roughly 100 observations in the dataset

cod.in$freq <- 1- cod.in$freq


# Setting up the model

library(glmmTMB)

plot(cod.in$time, cod.in$temp)

plot(cod.in$temp, cod.in$wwt)

tmb.glm <- glmmTMB(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader), 
                 data=cod.in, family = nbinom1())


first.glm <- glmer.nb(freq ~ wwt + time + temp + (1 | harvester) + (1 | grader),  
                     data=cod.in)

first.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader),  
                      data=cod.in, family="binomial")


plot(cod.in$wwt, cod.in$freq)
plot(cod.in$time, cod.in$freq)
plot(cod.in$temp, cod.in$freq)

hist(cod.in$freq, breaks =25)

qqnorm(residuals(first.glm))

ggplot(data.frame(eta=predict(first.glm,type="link"),pearson=residuals(first.glm,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()


ggplot(data.frame(lev=hatvalues(first.glm),pearson=residuals(first.glm,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()


library(DHARMa)

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
poisnz.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader) + (1 | site),  
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

binom.glm <- glmer(freq ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader),  
                  data=cod.in, family="binomial")

testDispersion(binom.glm)
simulationOutput <- simulateResiduals(fittedModel = binom.glm, plot = T)


#quasibinomial 


# convert data to binomial - seems to be supported by group. But need to decide on a final threshold

cod.in <- cod.in %>% mutate(quality = case_when(freq < 0.1 ~ 1,
                                                  freq >= 0.1 ~ 0))

true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(temp) + (1 | harvester) + (1 | grader) + (1 | site) ,  
      data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)

#add soak time to the binomial model

true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(temp) + scale(soak) +  (1 | harvester) + (1 | grader) + (1 | site) ,  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)

#add water temperature to the binomial model


true.binom <- glmer(quality ~ scale(wwt) + scale(time) + scale(trans_temp) + scale(soak) + scale(water_temp) +  (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)

#drop transport temp and total time, because it seems to be useless
true.binom <- glmer(quality ~ scale(wwt) + scale(soak) + scale(water_temp) +  (1 | harvester) + (1 | grader),  
                    data=cod.in, family="binomial")

testDispersion(true.binom)
simulationOutput <- simulateResiduals(fittedModel = true.binom, plot = T)




library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(gaus.glm)
tab_model(gausnz.glm)
tab_model(binom.glm)
tab_model(zinb.glm)
tab_model(true.binom)


#FFAW Cod Quality Project - Modelling the effects of fishery on Cod Quality

# Quality is defined as score indicating the proportion of fish in a given catch which recieved a perfect score

# Data prep for the model. 

cod.sub <- cod.dat %>% 
  select(Index, 
         Site,
         HLogId, 
         UNQID_HARVESTER,
         UNQID_GRADERS,
         GTName, 
         Grade, 
         ProdWgt, 
         FishTemp.2, 
         ProcDate, 
         ProcStart,
         DateHaul,
         TimeHaul) %>%
  mutate(GTName = recode(GTName, "HOOK AND LINE" = "HANDLINE")) %>% 
  filter(!Grade %in% c("NULL", "R"))

cod.sub$Haul_DT <- mdy_hms(paste(cod.sub$DateHaul, cod.sub$TimeHaul), tz="Canada/Newfoundland")
cod.sub$ProcStart_DT <- mdy_hms(paste(cod.sub$ProcDate, cod.sub$ProcStart), tz="Canada/Newfoundland")
cod.sub$Total <- as.numeric(cod.sub$ProcStart_DT - cod.sub$Haul_DT)/60  # total time between haul and process in minutes

colnames(cod.sub)[9] <- "Trans_Temp"

cod.in <- cod.sub %>%
         filter(GTName == "NETS") %>%
        group_by(HLogId, Grade) %>% 
          summarise(n = n(), 
                    wwt = unique(ProdWgt),
                    site = unique(Site),
                    time = unique(Total),
                    temp = unique(Trans_Temp),
                    harvester = unique(UNQID_HARVESTER),
                    grader = unique(UNQID_GRADERS))

cod.in <- cod.in %>% group_by(HLogId) %>% mutate(freq = n / sum(n)) %>% filter(Grade %in% "A") %>% filter(between(time, 0, 10000))


cod.in$temp <- as.numeric(as.character(cod.in$temp))
cod.in$wwt <- as.numeric(as.character(cod.in$wwt))
cod.in$time <- as.numeric(as.character(cod.in$time))

cod.in <- cod.in[complete.cases(cod.in), ] # removes roughly 100 observations in the dataset




# Setting up the model

library(glmmTMB)

plot(cod.in$time, cod.in$temp)

plot(cod.in$temp, cod.in$wwt)

first.glm <- glmmTMB(freq ~ wwt + time + temp + (1 | harvester) + (1 | grader), 
                 data=cod.in, family = poisson)


first.glm <- glmm(freq ~ wwt + time + temp,  
                     data=cod.in, family = "n)


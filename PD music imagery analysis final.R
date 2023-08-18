#library(ggridges)
library(readr)

DataFile = paste(c("PDMusic_cleanfinal",".csv"), collapse = "")
df <- read.csv(DataFile)

#Colourblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Music-evoked motor imagery questions and musical imagery
#Dataframe of Music evoked vividness

dfMI <- df[, c("Participant", "MusicMIVis", "MusicMIK")]
dfMI <- dfMI %>%
  pivot_longer(cols = c(MusicMIVis, MusicMIK), 
               names_to = "Type", 
               values_to = "Strength")

#removes those who with NAs
dfMI = dfMI %>%
  filter(dfMI$Strength != "NA")
#Sets imagery type as a factor
dfMI$Type <- as.factor(dfMI$Type)
levels(dfMI$Type) <- c("Kinaesthetic", "Visual")

#Plots group bar chart of motor imagery strength
ggplot(dfMI, aes(x=Strength, fill=Type)) + 
  theme_classic(base_size = 9)  +
  labs( y="Number of people with Parkinson's", x = "Imagery vividness") +
  geom_bar(stat="count", width=.5, position = "dodge")
 ggsave("MotorIm.png")

 #dataframe - as df but removed those who did not do the MI questions
dfMI_subset <- df
dfMI_subset = dfMI_subset %>%
  filter(dfMI_subset$MusicMITot != "NA")

#Report age and gender of those who did the MI questions
dfMI_subset%>%
  group_by(dfMI_subset$Gender) %>%
  summarise(number= n())

dfMI_subset %>%
  summarise(Av_age = mean(Age), SD_age = sd(Age), Max_age = max(Age), 
            Min_age = min (Age))

#Count number at each stage
dfMI_subset %>% group_by(dfMI_subset$DisStage) %>% summarise(number= n())

#PDQ_8 total
dfMI_subset %>% summarise(AV_PDQ = mean(PDQ_8Tot), SD_PDQ = sd(PDQ_8Tot), 
                 Max_PDQ = max(PDQ_8Tot), Min_PDQ = min (PDQ_8Tot))

dfMI_subset %>% 
summarise(mean = mean(TimeSinceDiagnosisYrs, na.rm=T),
          sd = sd (TimeSinceDiagnosisYrs, na.rm=T),
          min = min (TimeSinceDiagnosisYrs, na.rm=T),
          max = max (TimeSinceDiagnosisYrs, na.rm=T))

#Recalculate MusicMITot as mean vividness
dfMI_subset$MusicMITot <- (dfMI_subset$MusicMITot /2)

#Counts numbers of participants at each level of average music-
#evoked motor imagery
dfMI_subset %>%
  group_by(dfMI_subset$MusicMITot) %>%
  summarise(number= n())

#Report mean and sd of MI vividness
dfMI_subset$MusicMIVis <- as.numeric(dfMI_subset$MusicMIVis)
dfMI_subset$MusicMIK <- as.numeric(dfMI_subset$MusicMIK)
dfMI_subset %>%
  summarise(VisIm = mean(MusicMIVis), KIm = mean(MusicMIK))
dfMI_subset %>%
  summarise(VisIm = sd(MusicMIVis), KIm = sd(MusicMIK))

#Vividness reported by those that report using Music-evoked MI
dfMI_subset %>% 
  summarise_at(c("UseMIVis"), mean, na.rm = TRUE) 
dfMI_subset %>% 
  summarise_at(c("UseMIVis"), sd, na.rm = TRUE) 
dfMI_subset %>% 
  summarise_at(c("UseMIK"), mean, na.rm = TRUE) 
dfMI_subset %>% 
  summarise_at(c("UseMIK"), sd, na.rm = TRUE) 

shapiro.test(df$MusicMIVis) #not normal
shapiro.test(df$MusicMIK) #not normal
shapiro.test(df$MusicMITot) #not normal
#Comparion of vividness of Music evoked visual and kinaesthetic MI
wilcox.test(df$MusicMIVis, df$MusicMIK, paired=TRUE)

#Count different levels of music-evoked MI
dfMI_subset %>%
  group_by(dfMI_subset$UseMusicMI) %>%
  summarise(number= n())

#Table of Music MI by disease stage
dfMI_subset %>%
  group_by(DisStage) %>%
  summarise(meanMMI = mean(MusicMITot, na.rm=T), 
            SDMMI = sd(MusicMITot, na.rm=T),
            MaxMMI = max(MusicMITot, na.rm=T), 
            MinMMI = min(MusicMITot, na.rm=T))

#Rating of agreement of using music-evoke MI by disease stage
dfMI_subset$UseMusicMI <- as.numeric(dfMI_subset$UseMusicMI)
dfMI_subset %>%
  group_by(DisStage) %>%
  summarise(meanMMI = mean(UseMusicMI, na.rm=T), 
            SDMMI = sd(UseMusicMI, na.rm=T),
            MaxMMI = max(UseMusicMI, na.rm=T), 
            MinMMI = min(UseMusicMI, na.rm=T))

#Counts those who use music evoked MI at the different disease stages
dfMI_subset %>%
  group_by(DisStage, UseMusicMI_bin) %>%
  summarise(Number = n())


#Correlations with MusicMItotal
#Spearman's
cor.test(df$DSI_Training, df$MusicMITot, method = "spearman", exact = FALSE)
cor.test(df$DSI_Urge, df$MusicMITot, method = "spearman", exact = FALSE)
cor.test(df$Gold_Training, df$MusicMITot, method = "spearman", exact = FALSE)
cor.test(df$Gold_Total, df$MusicMITot, method = "spearman", exact = FALSE)

#Comparison between those who do/ do not use musical MI
#GSI Training sub-scale
shapiro.test(df$Gold_Training) #Not normally distributed
wilcox.test(Gold_Training ~ UseMusicMI_bin, data=dfMI_subset)

df$Gold_Training <- as.numeric(df$Gold_Training)
dfMI_subset %>%
  group_by(UseMusicMI_bin) %>%
  filter(Gold_Training != "NA") %>%
  summarise(mean = mean(Gold_Training), SD = sd(Gold_Training))

#DSI urge to dance sub-scale
shapiro.test(df$DSI_Urge) #Not normally distributed
wilcox.test(DSI_Urge ~ UseMusicMI_bin, data=dfMI_subset)
df$DSI_Urge <- as.numeric(df$DSI_Urge)

dfMI_subset %>%
  group_by(UseMusicMI_bin) %>%
  filter(DSI_Urge != "NA") %>%
  summarise(mean = mean(DSI_Urge), SD = sd(DSI_Urge))

#DSI training subscale
shapiro.test(df$DSI_Training) #Not normally distributed
wilcox.test(DSI_Training ~ UseMusicMI_bin, data=dfMI_subset)

df$DSI_Training <- as.numeric(df$DSI_Training)

dfMI_subset %>%
  group_by(UseMusicMI_bin) %>%
  filter(DSI_Training != "NA") %>%
  summarise(mean = mean(DSI_Training), SD = sd(DSI_Training))

#multiple plot figure for vividenss
dfMIcorr <- dfMI_subset[, c("Participant", "MusicMITot", "Gold_Training", 
                            "Gold_Total","DSI_Urge","DSI_Training")]
#change to % of total
dfMIcorr$Gold_Training <- (dfMIcorr$Gold_Training/49)*100
dfMIcorr$Gold_Total <- (dfMIcorr$Gold_Total/126)*100
dfMIcorr$DSI_Urge <- (dfMIcorr$DSI_Urge/35)*100
dfMIcorr$DSI_Training <- (dfMIcorr$DSI_Training/21)*100

dfMIcorr <- dfMIcorr %>%
  pivot_longer(cols = c(Gold_Training, Gold_Total,
                        DSI_Urge, DSI_Training), 
               names_to = "Type", 
               values_to = "Score")
#Remove missing values
dfMIcorr = dfMIcorr %>%
  filter(dfMIcorr$Score != "NA")
dfMIcorr$Type <- as.factor(dfMIcorr $Type)
dfMIcorr$Type <- factor(dfMIcorr$Type, labels= c("Gold_DSI Dance Training", "Gold_DSI Urge to Dance",
                                                 "Gold_MSI Music Sophistication",
                                                 "Gold_MSI Musical Training"))
ggplot(dfMIcorr, aes(x=Score, y=MusicMITot)) +
  geom_point(size = 0.5) + 
  theme_minimal(base_size = 7)  +  
  labs(x="Score (percentage of maximum)", y = "Music-Evoked Motor Imagery vividness")+
  geom_smooth(formula = y ~ x, method= "lm", se=TRUE, fullrange=FALSE, level=0.95)+
  facet_wrap(vars(Type))
ggsave("MIVivcorr2.png")

#Subset - as df but removed those who did not do the auditory imagery questions
dfBAIS_subset <- dfMI_subset
dfBAIS_subset = dfBAIS_subset %>%
  filter(dfBAIS_subset$BAIS_Tot != "NA")

dfBAIS_subset  %>% summarise(number= n())

#Demographics of those in auditory imagery analysis
dfBAIS_subset%>%
  group_by(dfBAIS_subset$Gender) %>%
  summarise(number= n())

dfBAIS_subset %>%
  summarise(Av_age = mean(Age), SD_age = sd(Age), Max_age = max(Age), 
            Min_age = min (Age))

#Count number at each stage
dfBAIS_subset  %>% group_by(dfBAIS_subset$DisStage) %>% summarise(number= n())

#PDQ_8 total
dfBAIS_subset %>% summarise(AV_PDQ = mean(PDQ_8Tot), SD_PDQ = sd(PDQ_8Tot), 
                          Max_PDQ = max(PDQ_8Tot), Min_PDQ = min (PDQ_8Tot))

dfBAIS_subset %>% 
  summarise(mean = mean(TimeSinceDiagnosisYrs, na.rm=T),
            sd = sd (TimeSinceDiagnosisYrs, na.rm=T),
            min = min (TimeSinceDiagnosisYrs, na.rm=T),
            max = max (TimeSinceDiagnosisYrs, na.rm=T))

#Report mean and sd of Auditory imagery vividness
dfBAIS_subset %>%
  summarise_at(c("BAIS_Tot_mean", "BAISM_Tot_mean", "BAISNM_Tot_mean"), mean)
dfBAIS_subset %>%  
  summarise_at(c("BAIS_Tot_mean", "BAISM_Tot_mean", "BAISNM_Tot_mean"), sd)
dfBAIS_subset %>%  
  summarise_at(c("BAIS_Tot_mean", "BAISM_Tot_mean", "BAISNM_Tot_mean"), max)
dfBAIS_subset %>%  
  summarise_at(c("BAIS_Tot_mean", "BAISM_Tot_mean", "BAISNM_Tot_mean"), min)

#% of participants scoring 1 across all questions in BAIS
dfBAIS_subset %>% group_by(dfBAIS_subset$BAIS_Tot_mean) %>% 
  summarise(numper = (n()/192)*100)

#Uses of musical imagery
dfBAIS_subset %>%
  summarise(Av = mean(TotUses_MusIm, na.rm=T), SD = sd(TotUses_MusIm,na.rm=T), 
            Max = max(TotUses_MusIm, na.rm=T), 
            Min = min (TotUses_MusIm, na.rm=T))

# percentage with each number of uses of musical imagery
dfBAIS_subset %>% group_by(dfBAIS_subset$UseMusI) %>% 
summarise(numberper = ((n()/192)*100))

#Count number for each level of usage (N=2 did not complete usage)
dfBAIS_subset %>% group_by(dfBAIS_subset$TotUses_MusIm) %>% 
  summarise(numberper = ((n()/190)*100))

# % for each use of musical imagery
dfBAISuses <- dfBAIS_subset[, c("Participant","MusIm_Tasks", "MusIm_Walking",
                     "MusIm_Energise","MusIm_Anthem", "MusIm_Exercise", "MusIm_Stuck", 
                     "MusIm_Relax","MusIm_Distract", "MusIm_Motivate", "MusIm_Concentrate", 
                     "MusIm_Companion","MusIm_Driving", "MusIm_Syncrhonise", 
                     "MusIm_Feelings")]
dfBAISuses <- dfBAISuses %>%
  pivot_longer(cols = c(MusIm_Tasks, MusIm_Walking,MusIm_Energise, MusIm_Anthem, 
                        MusIm_Exercise, MusIm_Stuck, MusIm_Relax,MusIm_Distract, 
                        MusIm_Motivate, MusIm_Concentrate, MusIm_Companion, 
                        MusIm_Driving, MusIm_Syncrhonise, MusIm_Feelings), 
               names_to = "UseType", 
               values_to = "Use")
dfBAISuses$UseType <- as.factor(dfBAISuses$UseType)
dfBAISuses = dfBAISuses %>%
  filter(dfBAISuses$Use != "NA")

dfBAISuses %>%
  group_by(UseType) %>%
  summarise(UseN = (sum(Use)/n() *100)) %>%
  arrange(desc(UseN))

#Correlations for BAIS
#BAIS and Disease duration
cor.test(dfBAIS_subset$TimeSinceDiagnosisYrs, dfBAIS_subset$BAIS_Tot_mean, 
         method = "spearman", exact=FALSE)
#BAIS and Music evoked motor imagery
cor.test(dfBAIS_subset$MusicMITot, dfBAIS_subset$BAIS_Tot_mean, 
         method = "spearman", exact=FALSE)
#BAIS and Cognition on PDQ-8
cor.test(dfBAIS_subset$PDQ_5, dfBAIS_subset$BAIS_Tot_mean, 
         method = "spearman", exact=FALSE)

#multiple plot figure for vividness
dfBAIScorr <- dfBAIS_subset[, c("Participant", "BAIS_Tot_mean", "TimeSinceDiagnosisYrs", 
                                "PDQ_5","MusicMITot")]
dfBAIScorr  <- dfBAIScorr  %>%
  pivot_longer(cols = c(TimeSinceDiagnosisYrs, 
                        PDQ_5,MusicMITot), 
               names_to = "Type", 
               values_to = "Score")

dfBAIScorr$Type <- factor(dfBAIScorr$Type, 
                          labels= c("Music-evoked motor imagery",
                                    "Cognition (PDQ8 score)", 
                                    "Time since diagnosis (years)"))

dfBAIScorr = dfBAIScorr %>%
  filter(dfBAIScorr$Score != "NA")
ggplot(dfBAIScorr, aes(x=Score, y=BAIS_Tot_mean)) +
  geom_point(size = 0.5) + 
  theme_minimal(base_size = 7)  +  
  labs(x="", y = "Auditory Imagery vividness")+
  geom_smooth(formula = y ~ x, method= "lm", se=TRUE, fullrange=FALSE, level=0.95)+
  facet_wrap(vars(Type),scales = "free", strip.position = "bottom") +
  theme (strip.placement = "outside")
ggsave("BAIS.png")

#Number of uses of musical imagery and BAIS musical imagery
cor.test(dfBAIS_subset$TotUses_MusIm, dfBAIS_subset$BAIS_MTot, 
         method = "spearman", exact=FALSE)
#Number of uses of musical imagery and GSI singing sub-scale
cor.test(dfBAIS_subset$TotUses_MusIm, dfBAIS_subset$Gold_Singing, 
         method = "spearman", exact=FALSE)

#multiple plot figure for musical imagery uses
dfBAIScorr2 <- dfBAIS_subset[, c("Participant", "TotUses_MusIm", 
                                 "BAIS_MTot", "Gold_Singing")]
dfBAIScorr2  <- dfBAIScorr2  %>%
  pivot_longer(cols = c(BAIS_MTot, Gold_Singing), 
               names_to = "Type", 
               values_to = "Score")

dfBAIScorr2 = dfBAIScorr2 %>%
  filter(dfBAIScorr2$Score != "NA")
dfBAIScorr2 = dfBAIScorr2 %>%
  filter(dfBAIScorr2$TotUses_MusIm != "NA")
dfBAIScorr2$Type <- factor(dfBAIScorr2$Type, 
                          labels= c("Musical imagery vividness",
                                    "Gold_MSI Singing"))

ggplot(dfBAIScorr2, aes(x=Score, y=TotUses_MusIm)) +
  geom_point(size = 0.5) + 
  theme_minimal(base_size = 7)  +  
  labs(x="", y = "Uses of musical imagery (number)")+
  geom_smooth(formula = y ~ x, method= "lm", se=TRUE, fullrange=FALSE, level=0.95)+
  facet_wrap(vars(Type),scales = "free", strip.position = "bottom") +
  theme (strip.placement = "outside")
ggsave("MusicalImUses.png")

#Report age and gender of those who did the DSI questions
dfMI_subset = dfMI_subset %>%
  filter(dfMI_subset$DSI_Total != "NA")

dfMI_subset%>%
  group_by(dfMI_subset$Gender) %>%
  summarise(number= n())

dfMI_subset %>%
  summarise(Av_age = mean(Age), SD_age = sd(Age), Max_age = max(Age), 
            Min_age = min (Age))

#Count number at each stage
dfMI_subset %>% group_by(dfMI_subset$DisStage) %>% summarise(number= n())

#PDQ_8 total
dfMI_subset %>% summarise(AV_PDQ = mean(PDQ_8Tot), SD_PDQ = sd(PDQ_8Tot), 
                          Max_PDQ = max(PDQ_8Tot), Min_PDQ = min (PDQ_8Tot))
#Time since diagnosis
dfMI_subset %>% 
  summarise(mean = mean(TimeSinceDiagnosisYrs, na.rm=T),
            sd = sd (TimeSinceDiagnosisYrs, na.rm=T),
            min = min (TimeSinceDiagnosisYrs, na.rm=T),
            max = max (TimeSinceDiagnosisYrs, na.rm=T))
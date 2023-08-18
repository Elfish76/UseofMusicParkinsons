library(tidyverse)#includes GGplot
library(ggridges)

library(readr)
DataFile = paste(c("PDMusic_cleanfinal",".csv"), collapse = "")
df <- read.csv(DataFile)

#Colourblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####Demographics
#Number wishing for future contact
#Event
df %>%
  group_by(df$Event) %>%
  summarise(number= n())
#Results
df %>%
  group_by(df$Results) %>%
  summarise(number= n())
#Future research
df %>%
  group_by(df$FutureRes) %>%
  summarise(number= n())

##General Demographics
#Gender
df %>%
  group_by(df$Gender) %>%
  summarise(number= n())

#Age
df %>%
  summarise(Av_age = mean(Age), SD_age = sd(Age), Max_age = max(Age), 
            Min_age = min (Age))

#Nationality
df_National <- count(df, Nationality)
view(df_National)

#Ethnicity
df %>% group_by(df$Ethnicity) %>% summarise(number= n()) 

#Count number of each education level
df %>% group_by(df$Education) %>% summarise(number= n())

#Count number of each employment status
df %>% group_by(df$Employment) %>% summarise(number= n())
df %>% group_by(df$Employ_other_coded) %>% summarise(number= n())

##Parkinson's specific Demographics
#Count number at each stage
df %>% group_by(df$DisStage) %>% summarise(number= n())

#PDQ_8 total
df %>% summarise(AV_PDQ = mean(PDQ_8Tot), SD_PDQ = sd(PDQ_8Tot), 
                 Max_PDQ = max(PDQ_8Tot), Min_PDQ = min (PDQ_8Tot))

#Figure for PDQ-8 by disease stage (Figure S1)
ggplot(df, aes(x=DisStage, y = PDQ_8Tot, fill=DisStage)) + theme_classic()  +  
  scale_fill_manual(values=cbbPalette) +
  labs( y="Quality of life (PDQ-8)", x = "Disease stage") +
  theme(legend.position= "right") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_violin()
  ggsave("QoL.png")

#TimeSinceDiagnosis set up separate dataframe to filter out NAs
dfTime <- df[, c("Participant", "TimeSinceDiagnosisYrs")]
dfTime = dfTime %>% filter(TimeSinceDiagnosisYrs != "NA")

dfTime %>% summarise(AV_Dur = mean(TimeSinceDiagnosisYrs), 
                     SD_Dur  = sd(TimeSinceDiagnosisYrs), 
                 Max_Dur  = max(TimeSinceDiagnosisYrs), 
                 Min_Dur = min (TimeSinceDiagnosisYrs))


####Visualing and describing data

#### Listening etc
#Count number for each listening type
df %>% group_by(ListenType) %>% summarise(number= n(), perc = (n()/217)*100)%>% 
  arrange(desc(number))

#Count each type of technology for listening
Techcoded = c('Tech_Smart','Tech_Radio','Tech_CD','Tech_Stream','Tech_Personal',
              'Tech_People','Tech_Activity','Tech_Other')
#df %>%
#  summarise(across(all_of(Techcoded), sum))
#df %>%
#  summarise(SumOther = sum(Tech_Other))
df %>%
  summarise(AV = mean(Tech_Tot), SD = sd(Tech_Tot), Max = max(Tech_Tot), 
            Min= min (Tech_Tot))

df$How_other_coded <- as.factor(df$How_other_coded)
df %>% group_by(df$How_other_coded) %>% summarise(number= n())

#Dataframe for ways of listening

dfTech <- df[, c("Participant","Tech_Smart","Tech_Radio","Tech_CD",
                 "Tech_Stream","Tech_Personal",
                 "Tech_People","Tech_Activity")]

dfTech <- dfTech %>%
  pivot_longer(cols = c(Tech_Smart,Tech_Radio,Tech_CD,Tech_Stream,Tech_Personal,
                        Tech_People,Tech_Activity), 
               names_to = "How", 
               values_to = "N")

dfTech$How <- as.factor(dfTech$How)

dfTech %>%
  group_by(How) %>%
  summarise(Number = sum(N), Per = (sum(N)/n()) *100) %>%
  arrange(desc(Number))

dfTech$N <- as.factor(dfTech$N)
dfTech$N <- factor(dfTech$N, levels = c(0,1), 
                     labels = c("No", "Yes"))
#Relabel and reorders factor to be in frequency order
dfTech$How<- factor(dfTech$How, c("Tech_People","Tech_Activity", "Tech_Smart",
                                  "Tech_CD","Tech_Stream","Tech_Personal","Tech_Radio"))

dfTech$How <- factor(dfTech$How, labels = c("Others choose", "During activity", 
                                            "Smart Device", "CD", 
                                            "Streaming", "Personal Device", "Radio"))

ggplot(dfTech, aes(y=How, fill=N)) + theme_classic()  +  
  scale_fill_manual(values=cbbPalette) +
  labs( x="Number of people with Parkinson's", y = "How/when listen to music") +
  theme(legend.position= "right", legend.title =element_blank()) +
  geom_bar(stat="count")
#Figure S2
ggsave("HowListen.png")

##Uses of Music
#Uses top level
dfUses <- df[, c("Participant","Motivation", "Walking","Music",
                 "Relaxation", "Concentration", "Distraction", "Company", "Feeling",
                 "Anthem")]
#Recode anthem into Yes and no
dfUses = dfUses %>%
  mutate(Anthem = case_when(grepl("Yes", Anthem)~ '1', 
                            grepl("Perhaps", Anthem)~ '1',
                            TRUE ~ '0'))
dfUses$Anthem <- as.numeric(dfUses$Anthem)

dfUses <- dfUses %>%
  pivot_longer(cols = c(Motivation, Walking, Music, Relaxation, Concentration,
                        Distraction, Company, Feeling, Anthem), 
               names_to = "UseType", 
               values_to = "Use")
#removes those who with NAs or error
dfUses = dfUses %>%
  filter(dfUses$Use != "NA")

dfUses$UseType <- as.factor(dfUses$UseType)
dfUses$Use <- as.factor(dfUses$Use)

dfUses$UseType<- factor(dfUses$UseType, c("Walking", "Concentration", "Anthem", "Feeling",
                                          "Distraction","Company", "Relaxation",
                                          "Motivation", "Music"))

#Counts frequencies of each case
dfUses%>% count(UseType, Use)
dfUsesCount <- dfUses%>% count(UseType, Use)
dfUsesCount %>% group_by(UseType)%>% 
  mutate(per= prop.table(n) * 100) %>% arrange (desc(n))

dfUses$Use <- factor(dfUses$Use , levels = c(0,1), 
                           labels = c("Never", "Sometimes"))

#Plots group bar chart on main uses of music
ggplot(dfUses, aes(y=UseType, fill=Use)) + 
  theme_classic(base_size = 8)  +  
  scale_fill_manual(values=cbbPalette) +
  labs( x="Number of people with Parkinson's", y = "Use") +
  theme(legend.position= "right", legend.title = element_blank()) + 
  geom_bar(stat="count")
#Figure 4
ggsave("Uses.png")

#Dataframe for Motivation

dfMUses <- df[, c("Participant","Motivation", "Energise","Chores",
                 "Exercise", "Motiv_other" )]
dfMUses = dfMUses %>%
  filter(dfMUses$Motivation != "0")
dfMUses <- dfMUses[, c("Participant","Energise","Chores",
                  "Exercise", "Motiv_other" )]
dfMUses <- dfMUses %>%
  pivot_longer(cols = c(Energise, Chores, Exercise, Motiv_other), 
               names_to = "UseType", 
               values_to = "Use")
dfMUses$UseType <- as.factor(dfMUses$UseType)
dfMUses$UseType<- factor(dfMUses$UseType, c("Motiv_other", "Chores",
                                            "Exercise","Energise"))

dfMUses$Use <- as.factor(dfMUses$Use)

dfMUses$Use <- factor(dfMUses$Use , levels = c(0,1), 
                     labels = c("Never", "Sometimes"))

#Dataframe for Walking
dfWUses <- df[, c("Participant","Walking", "Faster","Control",
                  "Synchronise", "Walk_other" )]
#removes those who with NAs or error
dfWUses = dfWUses %>%
  filter(dfWUses$Walking != "NA")
#removes those who said no to walking
dfWUses = dfWUses %>%
  filter(dfWUses$Walking != "0")
dfWUses <- dfWUses[, c("Participant","Faster","Control",
                       "Synchronise", "Walk_other" )]
dfWUses <- dfWUses %>%
  pivot_longer(cols = c(Faster, Control, Synchronise, Walk_other), 
               names_to = "UseType", 
               values_to = "Use")

dfWUses$UseType <- as.factor(dfWUses$UseType)
dfWUses$UseType<- factor(dfWUses$UseType, c("Walk_other", "Faster",
                                            "Control","Synchronise"))

dfWUses$Use <- as.factor(dfWUses$Use)

dfWUses$Use <- factor(dfWUses$Use , levels = c(0,1), 
                      labels = c("Never", "Sometimes"))

#Dataframe for "For the Music"
dfMusUses <- df[, c("Participant","Music", "Entertainment","Sing",
                  "Aesthetic", "Music_other" )]
dfMusUses = dfMusUses %>%
  filter(dfMusUses$Music != "0")
dfMusUses <- dfMusUses[, c("Participant", "Entertainment","Sing",
                           "Aesthetic", "Music_other" )]
dfMusUses <- dfMusUses %>%
  pivot_longer(cols = c(Entertainment, Sing, Aesthetic, Music_other), 
               names_to = "UseType", 
               values_to = "Use")
dfMusUses$UseType <- as.factor(dfMusUses$UseType)
dfMusUses$UseType<- factor(dfMusUses$UseType, c("Music_other", "Sing",
                                            "Entertainment","Aesthetic"))

dfMusUses$Use <- as.factor(dfMusUses$Use)

dfMusUses$Use <- factor(dfMusUses$Use , levels = c(0,1), 
                      labels = c("Never", "Sometimes"))

#Dataframe for Feelings
dfFeelUses <- df[, c("Participant","Feeling", "Anger","Cry", "Happy",
                    "Grieve", "Remember", "Feelings_other" )]
dfFeelUses = dfFeelUses %>%
  filter(dfFeelUses$Feeling != "0")
dfFeelUses <- dfFeelUses[, c("Participant", "Anger","Cry", "Happy",
                             "Grieve", "Remember", "Feelings_other" )]
dfFeelUses <- dfFeelUses %>%
  pivot_longer(cols = c(Anger, Cry, Happy, Grieve, Remember, Feelings_other), 
               names_to = "UseType", 
               values_to = "Use")
dfFeelUses$UseType <- as.factor(dfFeelUses$UseType)
dfFeelUses$UseType<- factor(dfFeelUses$UseType, c("Feelings_other", "Anger", "Grieve", 
                                                  "Cry","Remember","Happy"))
dfFeelUses$Use <- as.factor(dfFeelUses$Use)

dfFeelUses$Use <- factor(dfFeelUses$Use , levels = c(0,1), 
                        labels = c("Never", "Sometimes"))

#All uses together
dfUsesAll<-full_join(dfWUses, dfMUses)
dfUsesAll<-full_join(dfUsesAll, dfMusUses)
dfUsesAll<-full_join(dfUsesAll, dfFeelUses)

dfUsesAll = dfUsesAll %>%
  mutate(Level = case_when(grepl("Energise", UseType)~ 'Motivation',
                           grepl("Chores", UseType)~ 'Motivation',
                           grepl("Exercise", UseType)~ 'Motivation',
                           grepl("Motiv_other", UseType)~ 'Motivation',
                           grepl("Faster", UseType)~ 'Walking',
                           grepl("Control", UseType)~ 'Walking',
                           grepl("Synchronise", UseType)~ 'Walking',
                           grepl("Walk_other", UseType)~ 'Walking',
                           grepl("Aesthetic", UseType)~ 'For the Music',
                           grepl("Entertainment", UseType)~ 'For the Music',
                           grepl("Sing", UseType)~ 'For the Music',
                           grepl("Music_other", UseType)~ 'For the Music',
                           grepl("Anger", UseType)~ 'Feelings',
                           grepl("Happy", UseType)~ 'Feelings',
                           grepl("Grieve", UseType)~ 'Feelings',
                           grepl("Remember", UseType)~ 'Feelings',
                           grepl("Cry", UseType)~ 'Feelings',
                           grepl("Feelings_other", UseType)~ 'Feelings',
                           TRUE ~ 'error'))
#Change other names

dfUsesAll$UseType <- as.factor(dfUsesAll$UseType)
levels(dfUsesAll$UseType)[levels(dfUsesAll$UseType)=="Music_other"] <- "Other"
levels(dfUsesAll$UseType)[levels(dfUsesAll$UseType)=="Motiv_other"] <- "Other"
levels(dfUsesAll$UseType)[levels(dfUsesAll$UseType)=="Walk_other"] <- "Other"
levels(dfUsesAll$UseType)[levels(dfUsesAll$UseType)=="Feelings_other"] <- "Other"
dfUsesAll$Use <- as.factor(dfUsesAll$Use)


#Plots group bar chart of all uses and subthemes
#reorder(Currency, -Value, sum), Value)
ggplot(dfUsesAll, aes(y=UseType, fill=Use)) + 
  theme_classic(base_size = 6)  +  
  scale_fill_manual(values=cbbPalette) +
  labs( x="Number of people with Parkinson's", y = "Use") +
  theme(legend.position= "right",legend.title = element_blank()) + 
  geom_bar(stat="count") + facet_wrap(~Level, ncol =2,  scales = "free") +
theme(strip.background = element_blank(), strip.placement = "inside")
#Figure 5
ggsave("Uses_subthemes.png")

#Anthem
df %>%
  group_by(df$Anthem) %>%
  summarise(number= n())

##Musical Sophistication
#dataframe for GSI subscales
dfGSI <- df[, c("Participant", "Gold_Engage", "Gold_Perceptual", "Gold_Training",
                "Gold_Singing", "Gold_Emotions","Gold_Total")]

#Change to percentages of total score
dfGSI$Gold_Engage <- (dfGSI$Gold_Engage/63)*100
dfGSI$Gold_Perceptual <- (dfGSI$Gold_Perceptual/63)*100
dfGSI$Gold_Training <- (dfGSI$Gold_Training/49)*100
dfGSI$Gold_Singing <- (dfGSI$Gold_Singing/49)*100
dfGSI$Gold_Emotions <- (dfGSI$Gold_Emotions/42)*100
dfGSI$Gold_Total <- (dfGSI$Gold_Total/126)*100

dfGSI <- dfGSI %>%
  pivot_longer(cols = c(Gold_Engage, Gold_Perceptual, Gold_Training,
                        Gold_Singing, Gold_Emotions,Gold_Total), 
               names_to = "Subscale", 
               values_to = "Score")

#removes those who with NAs
dfGSI = dfGSI %>%
  filter(dfGSI$Score != "NA")

dfGSI$Subscale <- as.factor(dfGSI$Subscale)

dfGSI$Subscale<- factor(dfGSI$Subscale, levels = c("Gold_Emotions","Gold_Engage","Gold_Perceptual",
                                                   "Gold_Singing","Gold_Total", "Gold_Training"), 
                        labels = c("Emotional Engagement", "Active Engagement with Music",
                                   "Perceptual Abilities", "Singing Abilities", 
                                   "General Musical Sophistication", "Musical Training"))

#Reorders 
dfGSI$Subscale<- factor(dfGSI$Subscale, c("General Musical Sophistication",
                                          "Active Engagement with Music", 
                                          "Perceptual Abilities",
                                          "Musical Training", 
                                          "Singing Abilities","Emotional Engagement"))

ggplot(dfGSI, aes(x = Score, y = Subscale, fill = Subscale)) + 
  geom_density_ridges() + theme_minimal(base_size = 8) +
  theme(legend.position = "none") + 
  scale_x_continuous(limits = c(0, 100)) +
  labs(y = NULL, x="Score (% total)") +  
  scale_fill_manual(values=cbbPalette)
ggsave("GMSIRidge.png")

#Redo df to present means, sds, range
dfGSI <- df[, c("Participant", "Gold_Engage", "Gold_Perceptual", "Gold_Training",
                "Gold_Singing", "Gold_Emotions","Gold_Total")]

#removes those who with NAs
dfGSI = dfGSI %>%
  filter(dfGSI$Gold_Total != "NA")

dfGSI = dfGSI %>%
  mutate(Gold_Total_Med = ifelse(as.numeric(Gold_Total)>=median(as.numeric(Gold_Total)),
                                 "high", "low"))
#Adds GSI median split back into main dataframe
df<-full_join(df, dfGSI)

#Compare to normative data - count number > or < mean plus/minus 2 SD
df %>% 
  select(Participant, Gold_Total) %>% 
  filter(Gold_Total >= (81.53 + (2 * 20.62)))
df %>% 
  select(Participant, Gold_Total) %>% 
  filter(Gold_Total <= (81.53 - (2 * 20.62)))
df %>% 
  select(Participant, Gold_Engage) %>% 
  filter(Gold_Engage >= (41.52  + (2 * 10.36)))
df %>% 
  select(Participant, Gold_Engage) %>% 
  filter(Gold_Engage <= (41.52  - (2 * 10.36)))
df %>% 
  select(Participant, Gold_Perceptual) %>% 
  filter(Gold_Perceptual >= (50.20 + (2 * 7.86)))
df %>% 
  select(Participant, Gold_Perceptual) %>% 
  filter(Gold_Perceptual <= (50.20  - (2 * 7.86)))
df %>% 
  select(Participant, Gold_Training) %>% 
  filter(Gold_Training >= (26.52  + (2 * 11.44)))
df %>% 
  select(Participant, Gold_Training) %>% 
  filter(Gold_Training <= (26.52  - (2 * 11.44)))
df %>% 
  select(Participant, Gold_Singing) %>% 
  filter(Gold_Singing >= (31.67  + (2 * 8.72)))
df %>% 
  select(Participant, Gold_Singing) %>% 
  filter(Gold_Singing <= (31.67  - (2 * 8.72)))
df %>% 
  select(Participant, Gold_Emotions) %>% 
  filter(Gold_Emotions >= (34.66   + (2 * 5.04)))
df %>% 
  select(Participant, Gold_Emotions) %>% 
  filter(Gold_Emotions <= (34.66   - (2 * 5.04)))

dfGSI <- dfGSI %>%
  pivot_longer(cols = c(Gold_Engage, Gold_Perceptual, Gold_Training,
                        Gold_Singing, Gold_Emotions,Gold_Total), 
               names_to = "Subscale", 
               values_to = "Score")

#removes those who with NAs
dfGSI = dfGSI %>%
  filter(dfGSI$Score != "NA")

dfGSI$Subscale <- as.factor(dfGSI$Subscale)

dfGSI$Subscale<- factor(dfGSI$Subscale, levels = c("Gold_Emotions","Gold_Engage","Gold_Perceptual",
                                                   "Gold_Singing","Gold_Total", "Gold_Training"), 
                        labels = c("Emotions", "Engagement", "Perceptual", "Singing", "Sophistication", "Training"))

#Reorders 
dfGSI$Subscale<- factor(dfGSI$Subscale, c("Sophistication","Engagement", "Perceptual","Training", "Singing","Emotions"))

dfGSI %>%
  group_by(Subscale) %>%
  summarise(Av= mean(Score), SD = sd(Score), 
            Max = max(Score), Min = min (Score)) 


#Musical Genre preferences

#Dataframe for genre preferences

dfGenre <- df[, c("Participant","Gold_Total_Med", "Classical", "Rock", "Pop","Jazz",
                  "Folk", "Electronic")]
dfGenre$Classical <- as.character(dfGenre$Classical)
dfGenre <- dfGenre %>%
  pivot_longer(cols = c(Classical, Rock, Pop, Jazz, Folk, Electronic), 
               names_to = "Genre", 
               values_to = "Preference")
#removes those with NAs or error
dfGenre = dfGenre %>%
  filter(dfGenre$Preference != "NA")
dfGenre = dfGenre %>%
  filter(dfGenre$Preference != "error")

dfGenre$Genre <- as.factor(dfGenre$Genre)
dfGenre$Preference <- as.factor(dfGenre$Preference)

#Reorders factor
dfGenre$Genre<- factor(dfGenre$Genre, c("Classical", "Rock", "Pop", 
                                        "Folk", "Jazz","Electronic"))

#Plots group bar chart
ggplot(dfGenre, aes(x=Preference, fill=Genre)) + 
  theme_classic(base_size = 8)  +  
  scale_fill_manual(values=cbbPalette) +
  labs( y="Number of people with Parkinson's", x = "Enjoyment") +
  geom_bar(stat="count", position = "dodge", show.legend = FALSE)+ 
  facet_wrap(~Genre, ncol = 3) + 
  theme(strip.background = element_blank(), strip.placement = "inside")
#Figure 2
ggsave("Genre.png")

dfGenreSum <- dfGenre %>%
  group_by(Genre) %>%
  count(Preference)

dfGenreSum = dfGenreSum %>%
  pivot_wider(names_from = Genre, values_from = n)
write.csv(c(dfGenreSum), 
          file = "GenreSum.csv",row.names=FALSE) 

#Split by median on Goldsmiths Musical Sophistication
dfGenre_High <- dfGenre
dfGenre_High = dfGenre_High %>%
  filter(dfGenre_High$Gold_Total_Med == "high")

#Plots group bar chart for high GM
ggplot(dfGenre_High, aes(x=Preference, fill=Genre)) + 
  theme_classic(base_size = 8)  +  
  scale_fill_manual(values=cbbPalette) +
  labs( y="Number of people with Parkinson's", x = "Preference") +
  geom_bar(stat="count", position = "dodge", show.legend = FALSE)+ 
  facet_wrap(~Genre, ncol = 3) + 
  theme(strip.background = element_blank(), strip.placement = "inside")
ggsave("Genre_high.png")

dfGenre_Low <- dfGenre
dfGenre_Low = dfGenre_Low %>%
  filter(dfGenre_Low$Gold_Total_Med == "low")

#Plots group bar chart for low GM
ggplot(dfGenre_Low, aes(x=Preference, fill=Genre)) + 
  theme_classic(base_size = 8)  +  
  scale_fill_manual(values=cbbPalette) +
  labs( y="Number of people with Parkinson's", x = "Preference") +
  geom_bar(stat="count", position = "dodge", show.legend = FALSE)+ 
  facet_wrap(~Genre, ncol = 3) + 
  theme(strip.background = element_blank(), strip.placement = "inside")
ggsave("Genre_low.png")


#dataframe for DSI subscales

dfDSI <- df[, c("Participant", "DSI_Body", "DSI_Social", "DSI_Urge",
                "DSI_Training", "DSI_Total")]

#Change to percentages of total score
dfDSI$DSI_Body <- (dfDSI$DSI_Body/42)*100
dfDSI$DSI_Social <- (dfDSI$DSI_Social/42)*100
dfDSI$DSI_Urge <- (dfDSI$DSI_Urge/35)*100
dfDSI$DSI_Training <- (dfDSI$DSI_Training/21)*100
dfDSI$DSI_Total <- (dfDSI$DSI_Total/140)*100

#removes those who with NAs
dfDSI = dfDSI %>%
  filter(dfDSI$DSI_Body != "NA")

dfDSI <- dfDSI %>%
  pivot_longer(cols = c(DSI_Body, DSI_Social, DSI_Urge,
                        DSI_Training, DSI_Total), 
               names_to = "Subscale", 
               values_to = "Score")

#removes those who with NAs
dfDSI = dfDSI %>%
  filter(dfDSI$Score != "NA")

dfDSI$Subscale <- as.factor(dfDSI$Subscale)
dfDSI$Subscale<- factor(dfDSI$Subscale, levels = c("DSI_Body", "DSI_Social", 
                                                   "DSI_Total", "DSI_Urge", "DSI_Training"), 
                        labels = c("Body Awareness", "Social Dancing", 
                                   "Participatory Dance Experience",
                                   "Urge to Dance", "Dance Training"))

#Reorders 
dfDSI$Subscale<- factor(dfDSI$Subscale, c("Participatory Dance Experience", 
                                          "Dance Training", "Urge to Dance", 
                                          "Social Dancing","Body Awareness"))


ggplot(dfDSI, aes(x = Score, y = Subscale, fill = Subscale)) + 
  geom_density_ridges() + theme_minimal(base_size = 8) +
  theme(legend.position = "none") + #geom_joy(scale=3, rel_min_height=.01)+
  scale_x_continuous(limits = c(0, 100)) +
  labs( y=NULL, x="Score (% total)") +
  scale_fill_manual(values=cbbPalette)
ggsave("DSIRidge.png")

#Redo df for DSI for summary table (values rather than percentages)
dfDSI <- df[, c("Participant", "DSI_Body", "DSI_Social", "DSI_Urge",
                "DSI_Training", "DSI_Total")]

#removes those who with NAs
dfDSI = dfDSI %>%
  filter(dfDSI$DSI_Body != "NA")

dfDSI <- dfDSI %>%
  pivot_longer(cols = c(DSI_Body, DSI_Social, DSI_Urge,
                        DSI_Training, DSI_Total), 
               names_to = "Subscale", 
               values_to = "Score")

#removes those who with NAs
dfDSI = dfDSI %>%
  filter(dfDSI$Score != "NA")

dfDSI$Subscale <- as.factor(dfDSI$Subscale)
dfDSI$Subscale<- factor(dfDSI$Subscale, levels = c("DSI_Body", "DSI_Social", 
                                                   "DSI_Total", "DSI_Urge", "DSI_Training"), 
                        labels = c("Body", "Social", "Overall", "Urge", "Training"))

#Reorders 
dfDSI$Subscale<- factor(dfDSI$Subscale, c("Overall","Body", "Social", "Urge", "Training"))

dfDSI %>%
  group_by(Subscale) %>%
  summarise(Av= mean(Score), SD = sd(Score), Max = max(Score), 
            Min = min (Score))


#Pie chart for attentive listening (Gold_MSI_38)
dfListen <- df[, c("Participant","Gold_MSI_38")]
dfListen$Gold_MSI_38 <- as.factor(dfListen$Gold_MSI_38)
dfListen = dfListen %>%
  filter(dfListen$Gold_MSI_38 != "NA")

dfListen$Gold_MSI_38 <- factor(dfListen$Gold_MSI_38, levels = c(1,2,3,4,5,6,7), 
                               labels = c("0-15","15-30","30-60", "0-90", 
                                          "2hrs", "2-3hrs", "4hrs or more"))

ggplot(dfListen, aes(x=factor(1), fill=Gold_MSI_38))+ theme_classic(base_size = 7)  + 
  scale_fill_manual(values=cbbPalette) +
  geom_bar(width = 1)+ theme(axis.text.x=element_blank(), axis.line=element_blank())+
  theme(axis.text.y=element_blank())+ theme(panel.grid = element_blank())+
  labs(fill = "Time listening attentively", y="", x = "") +
  coord_polar("y")
#Figure 11
ggsave("AttentivePie.png")
dfListen %>%
  group_by(dfListen$Gold_MSI_38) %>%
  summarise(number= n())

#####Correlations
#Attentive listening

df$ListenType<- as.factor(df$ListenType)
df$ListenType <- factor(df$ListenType, labels = c("None", "Mixture", 
                                            "Mostly background", "Mostly attentive"))
df$ListenType<- factor(df$ListenType, c("None", "Mostly background", "Mixture", 
                                         "Mostly attentive"))

ggplot(df, aes(x=ListenType, y = Gold_MSI_38, fill=ListenType)) + 
  theme_classic(base_size = 8)  +  scale_fill_manual(values=cbbPalette) +
  labs( y="Time listening attentively", x = "How do you listen?") +
  theme(legend.position= "none") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_point(size = 1, position = "jitter")
#Figure 10
ggsave("AttentiveJitter.png")


ggplot(df, aes(x=Gold_Total, y=Gold_MSI_38)) + geom_point() + theme_classic()  +  
  labs( y="Attentive listening", x = "Musical Sophistication") +
  geom_smooth(formula = y ~ x, method="lm", se=TRUE, fullrange=FALSE, level=0.95)
#Figure 12
ggsave("Attentive.png")

shapiro.test(df$Gold_Total)
shapiro.test(df$Gold_MSI_38) #This is non-normal so non-parametric correlation
#Spearman's test
cor.test(df$Gold_Total, df$Gold_MSI_38, method = "spearman",
         exact=FALSE)
#Count sample size for Spearman's
sum(df$Gold_Total > 0 & df$Gold_MSI_38 > 0, na.rm=TRUE)

#Correlations with PDQ-8
shapiro.test(df$PDQ_8Tot) #Not normally distributed

cor.test(df$Gold_Engage, df$PDQ_8Tot, method = "spearman",
         exact=FALSE)
sum(df$Gold_Engage >= 0 & df$PDQ_8Tot >= 0, na.rm=TRUE)

cor.test(df$DSI_Total, df$PDQ_8Tot, method = "spearman",
         exact=FALSE)
sum(df$DSI_Total > 0 & df$PDQ_8Tot >= 0, na.rm=TRUE)

cor.test(df$DSI_Total, df$TimeSinceDiagnosisYrs, method = "spearman",
         exact=FALSE)
sum(df$DSI_Total > 0 & df$TimeSinceDiagnosisYrs >= 0, na.rm=TRUE)

#multiple plot figure for QoL
dfQoLCor <- df[, c("Participant", "PDQ_8Tot","Gold_Engage","DSI_Total")]

dfQoLCor  <- dfQoLCor  %>%
  pivot_longer(cols = c(Gold_Engage, DSI_Total), 
               names_to = "Type", 
               values_to = "Score")

dfQoLCor$Type <- factor(dfQoLCor$Type, 
                        labels= c("Participatory Dance Experience (General)",
                                  "Musical Engagement (subscale)"))

ggplot(dfQoLCor, aes(x=Score, y=PDQ_8Tot)) +
  geom_point(size = 0.5) + 
  theme_minimal(base_size = 7)  +  
  labs(x="", y = "PDQ-8 Score")+
  geom_smooth(formula = y ~ x, method="lm", se=TRUE, fullrange=FALSE, level=0.95)+
  facet_wrap(vars(Type),scales = "free", strip.position = "bottom") +
  theme (strip.placement = "outside")
#Figure 8
ggsave("QoLCorr2.png")

#Correlations uses of music
shapiro.test(df$TotUses) #Not normally distributed
cor.test(df$TotUses, df$PDQ_8Tot, method = "spearman",
         exact=FALSE)
sum(df$TotUses >= 0 & df$PDQ_8Tot >= 0, na.rm=TRUE)

cor.test(df$TotUses, df$TimeSinceDiagnosisYrs, method = "spearman",
         exact=FALSE)
sum(df$TotUses >= 0 & df$TimeSinceDiagnosisYrs >= 0, na.rm=TRUE)

cor.test(df$TotUses, df$Gold_Training, method = "spearman",
         exact=FALSE)
sum(df$TotUses >= 0 & df$Gold_Training >= 0, na.rm=TRUE)

cor.test(df$TotUses, df$DSI_Total, method = "spearman",
         exact=FALSE)
sum(df$TotUses >= 0 & df$DSI_Total >= 0, na.rm=TRUE)

#multiple plot figure for Uses of Music
dfUsesCor <- df[, c("Participant", "TotUses","PDQ_8Tot", "TimeSinceDiagnosisYrs",
                   "Gold_Training","DSI_Total")]

dfUsesCor  <- dfUsesCor  %>%
  pivot_longer(cols = c(PDQ_8Tot, TimeSinceDiagnosisYrs,
                        Gold_Training, DSI_Total), 
               names_to = "Type", 
               values_to = "Score")

dfUsesCor$Type <- factor(dfUsesCor$Type, c("Gold_Training", "DSI_Total",
                                           "PDQ_8Tot","TimeSinceDiagnosisYrs"))
dfUsesCor$Type <- factor(dfUsesCor$Type, 
                        labels= c("Musical Traning","Dance Sophistication",
                                  "Quality of Life (PDQ-8)", 
                                  "Years since diagnosis"))

ggplot(dfUsesCor, aes(x=Score, y=TotUses)) +
  geom_point(size = 0.5) + 
  theme_minimal(base_size = 6)  +  
  labs(x="", y = "Number of uses of music")+
  geom_smooth(formula = y ~ x, method="lm", se=TRUE, fullrange=FALSE, level=0.95)+
  facet_wrap(vars(Type),scales = "free", strip.position = "bottom", nrow = 1) +
  theme (strip.placement = "outside")
#Figure 9
ggsave("NumberUsesCorr2.png")


#Does musical training differ between those who use music for walking versus not
shapiro.test(df$Gold_Training) #Not normally distributed
wilcox.test(Gold_Training ~ Walking, data=df)

df$Gold_Training <- as.numeric(df$Gold_Training)
df %>%
  group_by(Walking) %>%
  filter(Gold_Training != "NA") %>%
  summarise(mean = median(Gold_Training))

#Compare attentive listening for those who do/do not use music for mood/feelings
wilcox.test(Gold_MSI_38 ~ Feeling, data=df)
df %>%
  group_by(Feeling) %>%
  filter(Gold_MSI_38 != "NA") %>%
  summarise(mean = mean(Gold_MSI_38), SD = sd(Gold_MSI_38))

#Compare attentive listening for those who do/do not use music for walking
wilcox.test(Gold_MSI_38 ~ Walking, data=df)
df %>%
  group_by(Walking) %>%
  filter(Gold_MSI_38 != "NA") %>%
  summarise(mean = mean(Gold_MSI_38), SD = sd(Gold_MSI_38))

#Compare DSI for those who do/do not use music for walking
shapiro.test(df$DSI_Total) #Normally distributed
t.test(df$DSI_Total ~ df$Walking, var.equal = TRUE) 

#Correlations between age and sub-scales of the GSI
#Not normally distributed
cor.test(df$Age, df$Gold_Training, method = "spearman", exact=FALSE)
shapiro.test(df$Gold_Perceptual) #Not normal
cor.test(df$Age, df$Gold_Perceptual, method = "spearman", exact=FALSE)
shapiro.test(df$Gold_Total) #Normal
cor.test(df$Age, df$Gold_Total, method = "pearson")
shapiro.test(df$Gold_Singing) #Not normal
cor.test(df$Age, df$Gold_Singing, method = "spearman", exact=FALSE)
shapiro.test(df$Gold_Emotions) #Not normal
cor.test(df$Age, df$Gold_Emotions, method = "spearman", exact=FALSE)
sum(df$Age >= 0 & df$Gold_Emotions >= 0, na.rm=TRUE)
shapiro.test(df$Gold_Engage) #Normal
cor.test(df$Age, df$Gold_Engage, method = "pearson")




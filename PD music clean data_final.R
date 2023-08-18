#June 2021 to clean Parkinson's and music survey
#Reworked September 2021 to be script for cleaning
#Finalised in June 2023 alongside articles
#Updated in August 2023 to work from CSV of data rather than xls

#####packages first time install all these packages 
library(tidyverse)
library(readr)

####ADD NAME OF DATAFILE (currently works from xls file)
#Note the same file is also saved as a .csv file for future reference
DataFile = paste(c("PDUoMQuantRevised",".csv"), collapse = "")
df <- read.csv(DataFile)

#Rename columns
names(df)[names(df) == 'ResponseId'] <- 'Participant'
names(df)[names(df) == 'Q1.1'] <- 'Gender'
df$Gender <- as.factor(df$Gender)

names(df)[names(df) == 'Q1.2'] <- 'Age'
df$Age <- as.numeric(df$Age)

#Uses manually coded column
names(df)[names(df) == 'Nationality_coded'] <- 'Nationality'
df$Nationality <- as.factor(df$Nationality)

names(df)[names(df) == 'Q1.4'] <- 'Ethnicity'
df$Ethnicity <- as.factor(df$Ethnicity)

names(df)[names(df) == 'Q6..Attention.Check'] <- 'Attn1'
df$Attn1 = recode(df$Attn1,"Woof" = 1, "Meow" = 0)

names(df)[names(df) == 'Q10.AC'] <- 'Attn2'
df$Attn2 = recode(df$Attn2,"1" = 0, "4" = 1, "8" = 0)

#Total number of correct attention check answers
df$AttnChk <- df$Attn1 + df$Attn2

##Disease stage (PADLS)
#Rename column
names(df)[names(df) == 'Q3..PD.ADL.Scale'] <- 'DisStage'
#Set as Factor
df$DisStage <- as.factor(df$DisStage)
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(DisStage = case_when(grepl("1", DisStage)~ 'None',
                              grepl("2", DisStage)~ 'Mild',
                              grepl("3", DisStage)~ 'Moderate',
                              grepl("4", DisStage)~ 'High',
                              grepl("5", DisStage)~ 'Extreme',
                              TRUE ~ "error"))
df$DisStage<- factor(df$DisStage, c("None", "Mild", "Moderate", "High", "Extreme"))

##Employment
#Rename column
names(df)[names(df) == 'Q1.5.From.Gold.MSI'] <- 'Employment'
#Set as Factor
df$Employment <- as.factor(df$Employment)
df$Employ_other_coded <- as.factor(df$Employ_other_coded)

##Educational level
#Rename column
names(df)[names(df) == 'Q1.6.From.Gold.DSI'] <- 'Education'
#Set as Factor
df$Education <- as.factor(df$Education)

#GSAP
names(df)[names(df) == 'Q4..G.SAP_1'] <- 'GSAP_1'
names(df)[names(df) == 'Q4..G.SAP_2'] <- 'GSAP_2'
names(df)[names(df) == 'Q4..G.SAP_3'] <- 'GSAP_3'
names(df)[names(df) == 'Q4..G.SAP_4'] <- 'GSAP_4'
names(df)[names(df) == 'Q4..G.SAP_5'] <- 'GSAP_5'
names(df)[names(df) == 'Q4..G.SAP_6'] <- 'GSAP_6'
names(df)[names(df) == 'Q4..G.SAP_7'] <- 'GSAP_7'
names(df)[names(df) == 'Q4..G.SAP_8'] <- 'GSAP_8'
names(df)[names(df) == 'Q4..G.SAP_9'] <- 'GSAP_9'
names(df)[names(df) == 'Q4..G.SAP_10'] <- 'GSAP_10'
names(df)[names(df) == 'Q4..G.SAP_11'] <- 'GSAP_11'

GSAPcoded = c('GSAP_1','GSAP_2','GSAP_3','GSAP_4','GSAP_5','GSAP_6','GSAP_7',
              'GSAP_8','GSAP_9','GSAP_10','GSAP_11')

df = df %>%
  mutate(across(all_of(GSAPcoded),
            function(x) case_when(x == "Not much at all" ~ 1,
                           x== "Not very much" ~ 2,
                           x== "Moderately so" ~ 3,
                           x== "Often" ~ 4,
                           x== "Very much so" ~ 5,
                           TRUE ~ NA_real_)))
df = df %>%
  mutate_at(vars(all_of(GSAPcoded)), as.numeric)

df$GSAP_Anxiety <- df$GSAP_1 + df$GSAP_2 + df$GSAP_10
df$GSAP_Conscious <- df$GSAP_7+ df$GSAP_8 + df$GSAP_9
df$GSAP_Rumination <- df$GSAP_3+ df$GSAP_4 + df$GSAP_6
df$GSAP_Process <- df$GSAP_5 + df$GSAP_11


#PDQ8
names(df)[names(df) == 'Q5..PDQ.8_1'] <- 'PDQ_1'
names(df)[names(df) == 'Q5..PDQ.8_2'] <- 'PDQ_2'
names(df)[names(df) == 'Q5..PDQ.8_3'] <- 'PDQ_3'
names(df)[names(df) == 'Q5..PDQ.8_4'] <- 'PDQ_4'
names(df)[names(df) == 'Q5..PDQ.8_5'] <- 'PDQ_5'
names(df)[names(df) == 'Q5..PDQ.8_6'] <- 'PDQ_6'
names(df)[names(df) == 'Q5..PDQ.8_7'] <- 'PDQ_7'
names(df)[names(df) == 'Q5..PDQ.8_8'] <- 'PDQ_8'

PDQcoded = c('PDQ_1','PDQ_2','PDQ_3','PDQ_4','PDQ_5','PDQ_6','PDQ_7','PDQ_8')

df = df %>%
  mutate(across(all_of(PDQcoded),
             function(x)case_when(x ==  "Never" ~ 0,
                           x== "Occasionally" ~ 1,
                           x== "Sometimes" ~ 2,
                           x== "Often" ~ 3,
                           x== "Always (or cannot do at all)" ~ 4,
                           TRUE ~ NA_real_)))

df = df %>% mutate_at(vars(all_of(PDQcoded)), as.numeric)
#Multiplied by 100
df = df %>% mutate_at(vars(all_of(PDQcoded)), funs(. * (100/4)))

df$PDQ_8Tot <- (df$PDQ_1 + df$PDQ_2 + df$PDQ_3 +df$PDQ_4 + df$PDQ_5 + df$PDQ_6 + 
  df$PDQ_7 + df$PDQ_8)/8

df$PDQ_8Tot<- as.numeric(df$PDQ_8Tot)


##How listen
names(df)[names(df) == 'Q8..DR1'] <- 'ListenType'
df$ListenType <- as.factor(df$ListenType)

#Technology for listening
#Rename column
names(df)[names(df) == 'Q9..DR2'] <- 'Tech'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Tech_Smart = case_when(grepl("Alexa", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_Radio = case_when(grepl("radio", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_CD = case_when(grepl("CD", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_Stream = case_when(grepl("stream", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_Personal = case_when(grepl("personal", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_People = case_when(grepl("people", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_Activity = case_when(grepl("doing", Tech)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Tech_Other = case_when(grepl("Other", Tech)~ '1', TRUE ~ '0'))

Techcoded = c('Tech_Smart','Tech_Radio','Tech_CD','Tech_Stream','Tech_Personal',
              'Tech_People','Tech_Activity','Tech_Other')

df = df %>%
  mutate_at(vars(all_of(Techcoded)), as.numeric)


df$Tech_Tot <- df$Tech_Smart + df$Tech_Radio + df$Tech_CD + df$Tech_Stream + 
  df$Tech_Personal + df$Tech_People + df$Tech_Activity + df$Tech_Other

df$Tech_Tot<- as.numeric(df$Tech_Tot)

##Genre of music preferences
#Rename column
names(df)[names(df) == 'Q7..AbiQ1_1'] <- 'Classical'
names(df)[names(df) == 'Q7..AbiQ1_2'] <- 'Rock'
names(df)[names(df) == 'Q7..AbiQ1_3'] <- 'Pop'
names(df)[names(df) == 'Q7..AbiQ1_4'] <- 'Jazz'
names(df)[names(df) == 'Q7..AbiQ1_5'] <- 'Folk'
names(df)[names(df) == 'Q7..AbiQ1_6'] <- 'Electronic'
names(df)[names(df) == 'Q7..AbiQ1_7'] <- 'Other'

Genrenames = c('Classical','Rock','Pop','Jazz','Folk','Electronic','Other')

df = df %>% mutate_at(vars(all_of(Genrenames)), as.factor)

df = df %>%
  mutate_at(vars(all_of(Genrenames)),
            funs(case_when(grepl("1", .)~ '1',
                           grepl("2", .)~ '2',
                           grepl("3", .)~ '3',
                           grepl("4", .)~ '4',
                           grepl("5", .)~ '5',
                           TRUE ~ "error")))

##Reasons for listening to music
names(df)[names(df) == 'Q10.1'] <- 'Motivation'
names(df)[names(df) == 'Q10.2'] <- 'Walking'
names(df)[names(df) == 'Q10.3'] <- 'Anthem'
names(df)[names(df) == 'Q10.4'] <- 'Music'
names(df)[names(df) == 'Q10.5'] <- 'Relaxation'
names(df)[names(df) == 'Q10.6'] <- 'Distraction'
names(df)[names(df) == 'Q10.7'] <- 'Concentration'
names(df)[names(df) == 'Q10.8'] <- 'Company'
names(df) [names(df) == 'Q10.9'] <- 'Feeling'

Reasonscoded = c('Motivation','Walking','Music','Relaxation','Distraction',
                 'Concentration','Company', 'Feeling')

df = df %>%
  mutate(across(all_of(Reasonscoded),
                function(x) case_when(x == "Never" ~ 0,
                           x== "Sometimes" ~ 1,
                           x== "About half the time" ~ 1,
                           x== "Most of the time" ~ 1,
                           x== "Always" ~ 1,
                           TRUE ~ NA_real_)))
df = df %>%
  mutate_at(vars(all_of(Reasonscoded)), as.numeric)

df$TotUses <- df$Motivation + df$Walking + df$Music + df$Relaxation + df$Distraction +
    df$Concentration + df$Company + df$Feeling

df$Anthem <- as.factor(df$Anthem )

#Motivation
names(df)[names(df) == 'Q10.1.FO'] <- 'Motiv_uses'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Energise = case_when(grepl("energize", Motiv_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Chores = case_when(grepl("tasks", Motiv_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Exercise = case_when(grepl("exercises", Motiv_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Motiv_other = case_when(grepl("Other", Motiv_uses)~ '1', TRUE ~ '0'))

#Walking
names(df)[names(df) == 'Q10.2.FO'] <- 'Walk_uses'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Faster = case_when(grepl("faster", Walk_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Control = case_when(grepl("control", Walk_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Synchronise = case_when(grepl("synchronize", Walk_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Walk_other = case_when(grepl("Other", Walk_uses)~ '1', TRUE ~ '0'))

#For the Music
names(df)[names(df) == 'Q10.4.FO'] <- 'M_uses'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Entertainment = case_when(grepl("entertainment", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Sing = case_when(grepl("hum", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Aesthetic = case_when(grepl("aesthetic", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Music_other = case_when(grepl("Other", M_uses)~ '1', TRUE ~ '0'))

#For the Music
names(df)[names(df) == 'Q10.4.FO'] <- 'M_uses'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Entertainment = case_when(grepl("entertainment", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Sing = case_when(grepl("hum", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Aesthetic = case_when(grepl("aesthetic", M_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Music_other = case_when(grepl("Other", M_uses)~ '1', TRUE ~ '0'))

#Feelings
names(df)[names(df) == 'Q10.9.FO'] <- 'Feelings_uses'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Anger = case_when(grepl("anger", Feelings_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Cry = case_when(grepl("cry",Feelings_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Happy = case_when(grepl("happy", Feelings_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Grieve = case_when(grepl("grieve", Feelings_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Remember = case_when(grepl("remember", Feelings_uses)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Feelings_other = case_when(grepl("Other", Feelings_uses)~ '1', TRUE ~ '0'))


##Reasons for using musical imagery
names(df)[names(df) == 'Q13.1.Music.Imagery'] <- 'UseMusI'
names(df)[names(df) == 'Q13.2FO.A_1'] <- 'MusIm_Tasks'
names(df)[names(df) == 'Q13.2FO.A_2'] <- 'MusIm_Walking'
names(df)[names(df) == 'Q13.2FO.A_3'] <- 'MusIm_Energise'
names(df)[names(df) == 'Q13.2FO.A_4'] <- 'MusIm_Anthem'
names(df)[names(df) == 'Q13.2FO.A_5'] <- 'MusIm_Exercise'
names(df)[names(df) == 'Q13.2FO.A_6'] <- 'MusIm_Stuck'
names(df)[names(df) == 'Q13.2FO.A_7'] <- 'MusIm_Relax'
names(df)[names(df) == 'Q13.2FO.A_8'] <- 'MusIm_Distract'
names(df)[names(df) == 'Q13.2FO.A_9'] <- 'MusIm_Motivate'
names(df)[names(df) == 'Q13.2FO.A_10'] <- 'MusIm_Concentrate'
names(df)[names(df) == 'Q13.2FO.A_11'] <- 'MusIm_Companion'
names(df)[names(df) == 'Q13.2FO.A_12'] <- 'MusIm_Driving'
names(df)[names(df) == 'Q13.2FO.A_13'] <- 'MusIm_Syncrhonise'
names(df)[names(df) == 'Q13.2FO.A_14'] <- 'MusIm_Feelings'
Reasonscoded2 = c('MusIm_Tasks', 'MusIm_Walking', 'MusIm_Energise', 
                  'MusIm_Anthem', 'MusIm_Exercise', 'MusIm_Stuck', 
                  'MusIm_Relax', 'MusIm_Distract', 'MusIm_Motivate', 
                  'MusIm_Concentrate', 'MusIm_Companion', 
                  'MusIm_Driving', 'MusIm_Syncrhonise', 
                  'MusIm_Feelings')

df = df %>%
  mutate(across(all_of(Reasonscoded2),
         function(x) case_when(x == "Never" ~ 0,
                           x == "Sometimes" ~ 1,
                           x == "About half the time" ~ 1,
                           x == "Most of the time" ~ 1,
                           x == "Always" ~ 1,
                           TRUE ~ NA_real_)))

df = df %>%
  mutate_at(vars(all_of(Reasonscoded2)), as.numeric)

df$TotUses_MusIm <- df$MusIm_Tasks + df$MusIm_Walking + df$MusIm_Energise + 
  df$MusIm_Anthem + df$MusIm_Exercise + df$MusIm_Stuck + df$MusIm_Relax + 
  df$MusIm_Distract + df$MusIm_Motivate + df$MusIm_Concentrate + 
  df$MusIm_Companion + df$MusIm_Driving + df$MusIm_Syncrhonise + 
  df$MusIm_Feelings

##Dance sophistication questionnaire
#Rename columns
names(df)[names(df) == 'Q17_1'] <- 'DSI_1.1'
names(df)[names(df) == 'Q17_2'] <- 'DSI_1.2'
names(df)[names(df) == 'Q17_3'] <- 'DSI_1.3'
names(df)[names(df) == 'Q17_4'] <- 'DSI_1.4'
names(df)[names(df) == 'Q17_5'] <- 'DSI_1.5'
names(df)[names(df) == 'Q17_6'] <- 'DSI_1.6'
names(df)[names(df) == 'Q17_7'] <- 'DSI_2.1'
names(df)[names(df) == 'Q17_8'] <- 'DSI_2.2'
names(df)[names(df) == 'Q17.DSI.PDE_1'] <- 'DSI_2.3'
names(df)[names(df) == 'Q17.DSI.PDE_2'] <- 'DSI_2.4'
names(df)[names(df) == 'Q17.DSI.PDE_3'] <- 'DSI_2.5'
names(df)[names(df) == 'Q17.DSI.PDE_4'] <- 'DSI_2.6'
names(df)[names(df) == 'Q17.DSI.PDE_5'] <- 'DSI_3.1'
names(df)[names(df) == 'Q17.DSI.PDE_6'] <- 'DSI_3.2'
names(df)[names(df) == 'Q17.DSI.PDE_7'] <- 'DSI_3.3'
names(df)[names(df) == 'Q17.DSI.PDE_8'] <- 'DSI_3.4'
names(df)[names(df) == 'Q17.DSI.PDE_9'] <- 'DSI_3.5'
#Check this as the names for these columns were created during import
names(df)[names(df) == 'DSI.DT'] <- 'DSI_4.1'
names(df)[names(df) == 'DSI.DT.1'] <- 'DSI_4.2'
names(df)[names(df) == 'DSI.DT.2'] <- 'DSI_4.3'

#Positive questions
DSIPositive = c('DSI_1.1', 'DSI_1.3', 'DSI_1.5', 'DSI_1.6', 'DSI_2.1', 'DSI_2.3', 
                'DSI_2.5','DSI_3.1', 'DSI_3.3', 'DSI_3.4', 'DSI_3.5')
df = df %>%
  mutate(across(all_of(DSIPositive),
                function(x) case_when(x == "Completely agree"~ 7, 
                           x == "Completely Agree"~ 7, 
                           x == "Strongly Agree" ~6, 
                           x == "Strongly agree" ~ 6,
                           x == "Agree" ~ 5, 
                           x == "Neither agree nor disagree" ~ 4, 
                           x == "Disagree" ~ 3,
                           x == "Strongly Disagree" ~ 2, 
                           x == "Strongly disagree" ~ 2, 
                           x == "Completely Disagree" ~ 1, 
                           x == "Completely disagree" ~ 1,
                           TRUE ~ NA_real_)))

#Negative questions
DSINegative = c('DSI_1.2', 'DSI_1.4', 'DSI_2.2', 'DSI_2.4', 'DSI_2.6', 'DSI_3.2')
df = df %>%
  mutate(across(all_of(DSINegative),
                function(x) case_when(x == "Completely agree"~ 1, 
                           x == "Completely Agree"~ 1, 
                           x == "Strongly Agree" ~2, 
                           x == "Strongly agree" ~ 2,
                           x == "Agree" ~ 3, 
                           x == "Neither agree nor disagree" ~ 4, 
                           x == "Disagree" ~ 5,
                           x == "Strongly Disagree" ~ 6, 
                           x == "Srongly disagree" ~ 6, 
                           x == "Strongly disagree" ~ 6, 
                           x == "Completely Disagree" ~ 7, 
                           x == "Completely disagree" ~ 7,
                           TRUE ~ NA_real_)))

df$DSI_4.1 = recode(df$DSI_4.1,"0 years" = 1, "1 Year" = 2,
                    "2 Years" = 3, "3 Years" = 4, "4-5 Years" = 5,
                    "6-9 Years" = 6, "10 or more Years" = 7,.default = NA_real_)
df$DSI_4.2 = recode(df$DSI_4.2,"None at all" = 1, "Beginner" = 2, "Intermediate" = 4,
                    "Advanced" = 6, "Professional" = 7, .default = NA_real_)
df$DSI_4.3 = recode(df$DSI_4.3,"0 Years" = 1, "0.5 Years" = 2,
                    "1 Year" = 3, "2 Years" = 4, "3 Years" = 5,
                    "4-6 Years" = 6, "7 or more Years" = 7, 
                    .default = NA_real_)

df$DSI_Body <- df$DSI_1.1 + df$DSI_1.2 + df$DSI_1.3 + df$DSI_1.4 + df$DSI_1.5 +
  df$DSI_1.6
df$DSI_Social <- df$DSI_2.1 + df$DSI_2.2 + df$DSI_2.3 + df$DSI_2.4 + df$DSI_2.5 +
  df$DSI_2.6
df$DSI_Urge <- df$DSI_3.1 + df$DSI_3.2 + df$DSI_3.3 + df$DSI_3.4 + df$DSI_3.5
df$DSI_Training <- df$DSI_4.1 + df$DSI_4.2 + df$DSI_4.3
df$DSI_Total <- df$DSI_Body + df$DSI_Social + df$DSI_Urge + df$DSI_Training


##Music-evoked MI
#Rename columns
names(df)[names(df) == 'Q12.1.'] <- 'MusicMIVis'
names(df)[names(df) == 'Q12.3.FO.B'] <- 'UseMIVis'
names(df)[names(df) == 'Q12.2'] <- 'MusicMIK'
names(df)[names(df) == 'Q12.3.FO.C'] <- 'UseMIK'
#Recode as numbers
df = df %>%
  mutate_at(vars(MusicMIK, UseMIK),
            funs(case_when(grepl("No", .)~ '1',
                           grepl("Mildly", .)~ '2',
                           grepl("Moderately", .)~ '3',
                           grepl("Intense", .)~ '4',
                           grepl("executing", .)~ '5',
                           TRUE ~ "error")))
df = df %>%
  mutate_at(vars(MusicMIVis, UseMIVis),
            funs(case_when(grepl("No", .)~ '1',
                           grepl("Blurred", .)~ '2',
                           grepl("Moderately", .)~ '3',
                           grepl("Clear", .)~ '4',
                           grepl("real", .)~ '5',
                           TRUE ~ "error")))

#Create total MusicMI score from K and V
df$UseMIVis <-as.numeric(df$UseMIVis)
df$UseMIK <-as.numeric(df$UseMIK)
df$MusicMIK <- as.numeric(df$MusicMIK)
df$MusicMIVis <- as.numeric(df$MusicMIVis)
df$MusicMITot <- df$MusicMIK + df$MusicMIVis

##How often use Music-evoked MI
names(df)[names(df) == 'Q12.3'] <- 'UseMusicMI'
df = df %>%
  mutate_at(vars(UseMusicMI),
            funs(case_when(grepl("Strongly disagree", .)~ '1',
                           grepl("Disagree", .)~ '2',
                           grepl("Neither", .)~ '3',
                           grepl("Agree", .)~ '4',
                           grepl("Strongly", .)~ '5',
                           TRUE ~ "error")))
#Binary variable
df$UseMusicMI_bin <- df$UseMusicMI
df = df %>%
  mutate_at(vars(UseMusicMI_bin),
            funs(case_when(grepl("1", .)~ '0',
                           grepl("2", .)~ '0',
                           grepl("3", .)~ '0',
                           grepl("4", .)~ '1',
                           grepl("5", .)~ '1',
                           TRUE ~ "error")))

#Musical Imagery vividness
names(df)[names(df) == 'Q15..BAIS_1'] <- 'BAIS_1'
names(df)[names(df) == 'Q15..BAIS_2'] <- 'BAIS_2'
names(df)[names(df) == 'Q15..BAIS_3'] <- 'BAIS_3'
names(df)[names(df) == 'Q15..BAIS_4'] <- 'BAIS_4'
names(df)[names(df) == 'Q15..BAIS_5'] <- 'BAIS_5'
names(df)[names(df) == 'Q15..BAIS_6'] <- 'BAIS_6'
names(df)[names(df) == 'Q15..BAIS_7'] <- 'BAIS_7'
names(df)[names(df) == 'Q15..BAIS_8'] <- 'BAIS_8'
names(df)[names(df) == 'Q15..BAIS_9'] <- 'BAIS_9'
names(df)[names(df) == 'Q15..BAIS_10'] <- 'BAIS_10'
names(df)[names(df) == 'Q15..BAIS_11'] <- 'BAIS_11'
names(df)[names(df) == 'Q15..BAIS_12'] <- 'BAIS_12'
names(df)[names(df) == 'Q15..BAIS_13'] <- 'BAIS_13'
names(df)[names(df) == 'Q15..BAIS_14'] <- 'BAIS_14'

BAIS = c('BAIS_1','BAIS_2','BAIS_3','BAIS_4','BAIS_5','BAIS_6','BAIS_7', 'BAIS_8',
         'BAIS_9', 'BAIS_10','BAIS_11','BAIS_12','BAIS_13','BAIS_14')

df = df %>%
  mutate_at(vars(all_of(BAIS)),
            funs(case_when(grepl("1", .)~ '1',
                           grepl("2", .)~ '2',
                           grepl("3", .)~ '3',
                           grepl("4", .)~ '4',
                           grepl("5", .)~ '5',
                           grepl("6", .)~ '6',
                           grepl("7", .)~ '7',
                           TRUE ~ "error")))

df = df %>% mutate_at(vars(all_of(BAIS)), as.numeric)

df$BAIS_Tot <- df$BAIS_1 + df$BAIS_2  + df$BAIS_3  + df$BAIS_4 + df$BAIS_5  + 
  df$BAIS_6 + df$BAIS_7 + df$BAIS_8 + df$BAIS_9 + df$BAIS_10 + df$BAIS_11 +
  df$BAIS_12 + df$BAIS_13 + df$BAIS_14
df = df %>%
  mutate(BAIS_Tot_mean = BAIS_Tot / 14)

BAISM = c('BAIS_1','BAIS_5','BAIS_7', 'BAIS_8',
         'BAIS_11','BAIS_12','BAIS_14')
df = df %>%
  mutate(BAISM_Tot_mean = rowMeans(select(., all_of(BAISM)), na.rm = TRUE))

df$BAIS_MTot <- df$BAIS_1 +  df$BAIS_5  + df$BAIS_7 + df$BAIS_8 + 
  df$BAIS_11 + df$BAIS_12 +  df$BAIS_14

BAISNM = c('BAIS_2','BAIS_3','BAIS_4','BAIS_6',
         'BAIS_9', 'BAIS_10','BAIS_13')
df$BAIS_NMTot <-  df$BAIS_2  + df$BAIS_3  + df$BAIS_4 + 
  df$BAIS_6 + df$BAIS_9 + df$BAIS_10 + df$BAIS_13 

df = df %>%
  mutate(BAISNM_Tot_mean = rowMeans(select(., all_of(BAISNM)), na.rm = TRUE))

#Goldsmiths MSI
#Rename columns
names(df)[names(df) == 'Q11.1.A.Gold.MSI_1'] <- 'Gold_MSI_1'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_2'] <- 'Gold_MSI_2'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_3'] <- 'Gold_MSI_3'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_4'] <- 'Gold_MSI_4'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_5'] <- 'Gold_MSI_5'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_6'] <- 'Gold_MSI_6'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_7'] <- 'Gold_MSI_7'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_8'] <- 'Gold_MSI_8'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_9'] <- 'Gold_MSI_9'
names(df)[names(df) == 'Q11.1.A.Gold.MSI_10'] <- 'Gold_MSI_10'
names(df)[names(df) == 'Q11.1.B_1'] <- 'Gold_MSI_11'
names(df)[names(df) == 'Q11.1.B_2'] <- 'Gold_MSI_12'
names(df)[names(df) == 'Q11.1.B_3'] <- 'Gold_MSI_13'
names(df)[names(df) == 'Q11.1.B_4'] <- 'Gold_MSI_14'
names(df)[names(df) == 'Q11.1.B_5'] <- 'Gold_MSI_15'
names(df)[names(df) == 'Q11.1.B_6'] <- 'Gold_MSI_16'
names(df)[names(df) == 'Q11.1.B_7'] <- 'Gold_MSI_17'
names(df)[names(df) == 'Q11.1.B_8'] <- 'Gold_MSI_18'
names(df)[names(df) == 'Q11.1.B_9'] <- 'Gold_MSI_19'
names(df)[names(df) == 'Q11.1.B_10'] <- 'Gold_MSI_20'
names(df)[names(df) == 'Q11.1.B_11'] <- 'Gold_MSI_21'
names(df)[names(df) == 'Q11.1.C_1'] <- 'Gold_MSI_22'
names(df)[names(df) == 'Q11.1.C_2'] <- 'Gold_MSI_23'
names(df)[names(df) == 'Q11.1.C_3'] <- 'Gold_MSI_24'
names(df)[names(df) == 'Q11.1.C_4'] <- 'Gold_MSI_25'
names(df)[names(df) == 'Q11.1.C_5'] <- 'Gold_MSI_26'
names(df)[names(df) == 'Q11.1.C_6'] <- 'Gold_MSI_27'
names(df)[names(df) == 'Q11.1.C_7'] <- 'Gold_MSI_28'
names(df)[names(df) == 'Q11.1.C_8'] <- 'Gold_MSI_29'
names(df)[names(df) == 'Q11.1.C_9'] <- 'Gold_MSI_30'
names(df)[names(df) == 'Q11.1.C_10'] <- 'Gold_MSI_31'
names(df)[names(df) == 'Q11.2.Gold.MSI_1'] <- 'Gold_MSI_32'
names(df)[names(df) == 'Q11.3.Gold.MSI_1'] <- 'Gold_MSI_33'
names(df)[names(df) == 'Q11.4.Gold.MSI_1'] <- 'Gold_MSI_34'
names(df)[names(df) == 'Q11.5.Gold.MSI_1'] <- 'Gold_MSI_35'
names(df)[names(df) == 'Q11.6.Gold.MSI_1'] <- 'Gold_MSI_36'
names(df)[names(df) == 'Q11.7.Gold.MSI_1'] <- 'Gold_MSI_37'
names(df)[names(df) == 'Q11.8.Gold.MSI_1'] <- 'Gold_MSI_38'

MSIPositive = c('Gold_MSI_1','Gold_MSI_2','Gold_MSI_3','Gold_MSI_4','Gold_MSI_5',
                'Gold_MSI_6','Gold_MSI_7','Gold_MSI_8','Gold_MSI_10',
                'Gold_MSI_12','Gold_MSI_15','Gold_MSI_16','Gold_MSI_18',
                'Gold_MSI_19','Gold_MSI_20','Gold_MSI_22','Gold_MSI_24',
                'Gold_MSI_26','Gold_MSI_28','Gold_MSI_29','Gold_MSI_30',
                'Gold_MSI_31')
MSINegative = c('Gold_MSI_9', 'Gold_MSI_11','Gold_MSI_13','Gold_MSI_14', 
                'Gold_MSI_17', 'Gold_MSI_21','Gold_MSI_23','Gold_MSI_25', 'Gold_MSI_27')

df = df %>%
  mutate(across(all_of(MSIPositive),
                function(x) case_when(x == "Completely Disagree" ~ 1,
                          x == "Strongly Disagree" ~ 2,
                          x == "Disagree" ~ 3,
                          x == "Neither Agree nor Disagree" ~ 4,
                          x == "Agree" ~ 5,
                          x == "Strongly Agree" ~ 6,
                          x == "Completely Agree" ~ 7,
                           TRUE ~ NA_real_)))

df = df %>%
  mutate(across(all_of(MSINegative),
         function(x) case_when(x == "Completely Disagree" ~ 7,
                           x == "Strongly Disagree" ~ 6,
                           x == "Disagree" ~ 5,
                           x == "Neither Agree nor Disagree" ~ 4,
                           x == "Agree" ~ 3,
                           x == "Strongly Agree" ~ 2,
                           x == "Completely Agree" ~ 1,
                           TRUE ~ NA_real_)))

df$Gold_MSI_32 = recode(df$Gold_MSI_32,"0 years" = 1, "1 year" = 2,
                        "2 years" = 3, "3 years" = 4, "4-5 years" = 5,
                        "6-9 years" = 6, "10 or more years" = 7, 
                        .default = NA_real_)
df$Gold_MSI_33 = recode(df$Gold_MSI_33,"0 hours" = 1, "0.5 hours" = 2,
                        "1 hour" = 3, "1.5 hours" = 4, "2 hours" = 5,
                        "3-4 hours" = 6, "5 or more hours" = 7, 
                        .default = NA_real_)
df$Gold_MSI_34 = recode(df$Gold_MSI_34,"0" = 1, "1" = 2,
                        "2" = 3, "3" = 4, "4-6" = 5,
                        "7-10" = 6, "11 or more" = 7,.default = NA_real_)

df$Gold_MSI_35 = recode(df$Gold_MSI_35,"0 years" = 1, "0.5 years" = 2,
                        "1 year" = 3, "2 years" = 4, "3 years" = 5,
                        "4-6 years" = 6, "7 or more years" = 7,
                        .default = NA_real_)
df$Gold_MSI_36 = recode(df$Gold_MSI_36,"0 years" = 1, "0.5 years" = 2,
                        "1 year" = 3, "2 years" = 4, "3-5 years" = 5,
                        "6-9 years" = 6, "10 or more years" = 7,
                        .default = NA_real_)
df$Gold_MSI_37 = recode(df$Gold_MSI_37, "0" = 1, "1" = 2,
                        "2" = 3, "3" = 4, "4" = 5,
                        "5" = 6, "6 or more" = 7, .default = NA_real_)
df$Gold_MSI_38 = recode(df$Gold_MSI_38,"0-15 minutes" = 1, "15-30 minutes" = 2,
                        "30-60 minutes" = 3, "60-90 minutes" = 4, "2 hours" = 5,
                        "2-3 hours" = 6, "4 or more hours" = 7, .default = NA_real_)

df = df %>%
  mutate_at(vars(all_of(MSINegative)), as.numeric)%>%
  mutate_at(vars(all_of(MSIPositive)), as.numeric)

#Set as numbers so can be totalled

df$Gold_MSI_32 <- as.numeric(df$Gold_MSI_32)
df$Gold_MSI_33 <- as.numeric(df$Gold_MSI_33)
df$Gold_MSI_34 <- as.numeric(df$Gold_MSI_34)
df$Gold_MSI_35 <- as.numeric(df$Gold_MSI_35)
df$Gold_MSI_36 <- as.numeric(df$Gold_MSI_36)
df$Gold_MSI_37 <- as.numeric(df$Gold_MSI_37)
df$Gold_MSI_38 <- as.numeric(df$Gold_MSI_38)

# Will only add values provided there is no NA in the scales so participant 
# must have completed the full scale
df$Gold_Total <- df$Gold_MSI_1 + df$Gold_MSI_3 + df$Gold_MSI_4 + df$Gold_MSI_7 + 
  df$Gold_MSI_10 + df$Gold_MSI_12 + df$Gold_MSI_14 + df$Gold_MSI_15 + df$Gold_MSI_17 +
  df$Gold_MSI_19 + df$Gold_MSI_23 + df$Gold_MSI_24 + df$Gold_MSI_25 + df$Gold_MSI_27 +
  df$Gold_MSI_29 + df$Gold_MSI_32 + df$Gold_MSI_33 + df$Gold_MSI_37

df$Gold_Engage <- df$Gold_MSI_1 + df$Gold_MSI_3 + 
  df$Gold_MSI_8 + df$Gold_MSI_15 + df$Gold_MSI_21 +
  df$Gold_MSI_24 + df$Gold_MSI_28 + df$Gold_MSI_34 + df$Gold_MSI_38

df$Gold_Perceptual <-   df$Gold_MSI_5 +
  df$Gold_MSI_6 + df$Gold_MSI_11 + df$Gold_MSI_12 + df$Gold_MSI_13 +
  df$Gold_MSI_18 + df$Gold_MSI_22 + df$Gold_MSI_23 + df$Gold_MSI_26

df$Gold_Training <- df$Gold_MSI_14 +
  df$Gold_MSI_27 + df$Gold_MSI_32 + df$Gold_MSI_33 + df$Gold_MSI_35 +
  df$Gold_MSI_36 + df$Gold_MSI_37

df$Gold_Singing <- df$Gold_MSI_4 + df$Gold_MSI_7 + 
  df$Gold_MSI_10 + df$Gold_MSI_17 +
  df$Gold_MSI_25 + df$Gold_MSI_29 + df$Gold_MSI_30

df$Gold_Emotions <- df$Gold_MSI_2 + df$Gold_MSI_9 + 
  df$Gold_MSI_16 + df$Gold_MSI_19 + df$Gold_MSI_20 + df$Gold_MSI_31

#Removing participants from analysis
#removes those who did not consent
df = df %>%
  filter(df$Consent == "Yes, I agree")

#removes those who did not answer any of the genre questions since they
#did not complete any questions related to music

df = df %>%
  filter(df$Classical != "error")

#Attention Check Questions and screening
df$Attn1 <- as.factor(df$Attn1)
#number who got first question right
df %>% group_by(df$Attn1) %>% summarise(number= n()) 

#removes people who got first question wrong
df = df %>% filter(df$Attn1 != "0")

df$AttnChk <- as.factor(df$AttnChk)

#number who got both right (total 2)
df %>% group_by(df$AttnChk) %>% summarise(number= n()) 

#Counts people interested in further contact
names(df)[names(df) == 'Contact.1'] <- 'ContactPref'
#Tidy up names. grep1 looks for that text in the variable name
df = df %>%
  mutate(Results = case_when(grepl("results", ContactPref)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(FutureRes = case_when(grepl("future", ContactPref)~ '1', TRUE ~ '0'))
df = df %>%
  mutate(Event = case_when(grepl("event", ContactPref)~ '1', TRUE ~ '0'))
#Save processed df as csv file
#Use write_csv as part of tidyverse
write.csv(c(df), 
          file = "PDMusic_cleanfinal.csv",row.names=FALSE)  

#Clear everything
rm(list = ls())




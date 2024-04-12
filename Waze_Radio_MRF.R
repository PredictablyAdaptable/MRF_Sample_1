#--------------------------------------------------------------------------------------
# PROJECT: Google Waze - Radio November 2021
# SECTION: MRF Analysis
# AUDIENCE:
# SCRIPT: MRF DiD Analysis
# NOTE: Please map your onedrive/oneworkplace drive before running this, otherwise you'll have to change paths
#--------------------------------------------------------------------------------------
rm(list=ls())
# Working directory
# once you open the R Project file, this should point to the directory for you
getwd()

#------------------------------------------------------
# Step 0: load libraries and Set up working directory
#------------------------------------------------------
# load libraries
library(readxl)
library(xlsx)
library(openxlsx)
library(car)
library(mefa)
library(foreign)
library(haven)
library(dplyr)
library(DT)
library(optimx)
library(tidyverse)
library(sjlabelled) # for removing labels from sav files




# load the script of MRF functions
user_name <- Sys.getenv('username')
source(paste0('C:\\Users\\', user_name, '\\OneWorkplace\\OMD-Audience Analytics OMDUK - General\\Products & Techniques\\R FUNCTIONS\\MRF_Functions.R'))

#-----------------------------------------------------------------------------------------------
# Step 1: Import Dataset, Radio Variable Names, and Radio Spots using 'Data Interface' file
#-----------------------------------------------------------------------------------------------

#--------------- Import the dataset ----------------------
filename <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "data", 
                      rows = 2:3, cols = 2, colNames = FALSE)

prewave_data <- read_sav(paste0("./1. Raw Data/", filename$X1[1]))
postwave_data <- read_sav('./1. Raw Data/topup/ORD-656067-T3L1_SPSS_Data.SAV')

# data overview
dim(prewave_data) # 2507
dim(postwave_data) # 1285

head(prewave_data)
head(postwave_data)


# Questbook (i.e. codebook): Extract the columns names and description for easy navigation 
questbook_prewave <- CreateQuestBook(prewave_data)
questbook_postwave <- CreateQuestBook(postwave_data)

datatable(questbook_prewave)
datatable(questbook_postwave)

# remove all labels
postwave_df <- remove_all_labels(postwave_data) %>% as_tibble()
prewave_df <- remove_all_labels(prewave_data) %>% as_tibble()

#--------------------------------------------
# load in the data interface vars
#--------------------------------------------
#------- import the variable names for stations---------
sta_df <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "stations", 
                      startRow = 2, cols = 1:2, colNames = TRUE)

head(sta_df)
tail(sta_df)

#-------- import the days listen for the stations ------------
dlis_df <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "days_listen", 
                     startRow = 2, cols = 1:2, colNames = TRUE)
 


head(dlis_df)
tail(dlis_df)

#-------- import the station-timeslot vars ------------
stim_df <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "station-timeslot", 
                     startRow = 3, cols = 1:3, colNames = TRUE)



head(stim_df)
tail(stim_df)

#-------- import the spots for the stations ------------
spot_df <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "spots", 
                       startRow = 2, cols = 1:4, colNames = TRUE) %>%
  select(-concat)
  

head(spot_df)
tail(spot_df)

#--------- import the KPI varnames ---------------
kpi <- read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "kpi", 
                 startRow = 2, cols = 1:4, colNames = TRUE)

kpi


#----------------------------------------------------------------------------------
# Step 2: Extract only the variables needed for the analysis
#----------------------------------------------------------------------------------
interim_df <- postwave_df %>%
  select(pid, sta_df$variable, dlis_df$vars, stim_df$var, kpi$variable, Q9)

dim(interim_df)
head(interim_df)

#-----------------------------------------------------------------------------------
# Step 3: Convert Print Reading to Proportion
#-----------------------------------------------------------------------------------
# Daily (7 days): 1 
# 6 days: 2 
# 5 days: 3 
# 4 days: 4 
# 3 days: 5 
# 2 days: 6 
# 1 day: 7

recode_listen_to_prop <- function(varname) {
  # function to recode frequency of readership into proportion
  result <- car::recode(as.integer(varname), 
                        "NA = 0; 1 = 7/7; 2 = 6/7; 3 = 5/7; 4 = 4/7; 
                        5 = 3/7; 6 = 2/7; 7 = 1/7")
  return(result)
}

# example of how the func works: recode_listen_to_prop
recode_listen_to_prop(interim_df$Q8br2 %>% head())

# function works well - now apply to all vars
interim_df[ ,dlis_df$vars] <- lapply(interim_df[ ,dlis_df$vars], 
                                    FUN = recode_listen_to_prop)


#turn into matrix for multiplying
stations <- interim_df[, sta_df$variable]
stations[is.na(stations)] <- 0

day_weight <- interim_df[ ,dlis_df$vars]

timeslot <- interim_df[, stim_df$var]
timeslot[is.na(timeslot)] <- 0

spot <- spot_df$spots
names(spot) <- spot_df$station

#Create big matrix for stations, day listen prop and timeslot
#--------------------------------------------------------------
num_timeslot <- c('6h-9h30', '9h30-12h', '12h-16h', '16h-17h30', 
                  '17h30-20h', '20h-23h', '23h-1h', '1h-6h') %>% length()

# column dimension should 552 in length i.e. 69 stations by number of timeslot (8) -> 69*8 = 552
stations_big <- as.matrix(stations[rep(names(stations), each = num_timeslot)]) 
day_weight_big <- as.matrix(day_weight[rep(names(day_weight), each = num_timeslot)])
timeslot_big <- as.matrix(timeslot)
spot_big <- mefa:::rep.data.frame(spot, each = nrow(interim_df)) %>% as.matrix()

stations_big[1:5, 1:10]
day_weight_big[1:5, 1:10]
timeslot[1:5, 1:10]
spot_big[1:5, 1:10]


#------------------------------------------------------------------------
#  Step 4 Contacts calculation and control vs exposed classification 
#------------------------------------------------------------------------
# Calculate ad contacts
contacts_big <- stations_big * day_weight_big * timeslot_big * spot_big
radio_contacts <- rowSums(contacts_big)

head(radio_contacts)
length(radio_contacts)

MRF_data <- as.data.frame(cbind(interim_df, radio_contacts))

# Threshold = net reach
net_reach = 58.45
reach_threshold <- net_reach/100


# R ranks cases by frequency of contact and creates index variable
MRF_data$index <- as.numeric(rownames(MRF_data[rank(MRF_data$radio_contacts, ties.method = "first"),]))

#Determine the case that splits the dataset 
threshold_cutoff <- round(nrow(MRF_data)*(1 - reach_threshold))

MRF_data$exposed <- NA

# Classifiy control/exposed
for (i in 1:nrow(MRF_data)){
  if (MRF_data$index[i] <= threshold_cutoff) MRF_data$exposed[i] = 0
  else if (MRF_data$index[i] > threshold_cutoff) MRF_data$exposed[i] = 1
}

MRF_data$period <- 1

table(MRF_data$exposed)
table(MRF_data$exposed) %>% prop.table()



#------------------------------------------------------------------
# T-test kpi for the postwave
#------------------------------------------------------------------

# Familiarity
# Never heard of it: 1 
# Heard of, but don't know what it’s for: 2 
# Know what it’s for, but don’t know it’s features or benefits: 3 
# Familiar with its features and benefits: 4

MRF_data <- MRF_data %>% 
  mutate(never_familiar = ifelse(Q1r1 == 1, 1, 0),
         heard_familiar = ifelse(Q1r1 == 2, 1, 0),
         know_familiar = ifelse(Q1r1 == 3, 1, 0),
         most_familiar = ifelse(Q1r1 == 4, 1, 0),
         likely_waze = ifelse(Q2r1 > 2, 1, 0),
         likely_google = ifelse(Q2r2 > 2, 1, 0),
         likely_apple = ifelse(Q2r3 > 2, 1, 0),
         likely_tomtom = ifelse(Q2r4 > 2, 1, 0),
         likely_waze_score3 = ifelse(Q2r1 == 3, 1, 0),
         likely_waze_score4 = ifelse(Q2r1 == 4, 1, 0))


summary(MRF_data %>% select(never_familiar:likely_tomtom))

# familiarity
t.test(never_familiar ~ exposed, data = MRF_data)
t.test(heard_familiar ~ exposed, data = MRF_data)
t.test(know_familiar ~ exposed, data = MRF_data)
t.test(most_familiar ~ exposed, data = MRF_data)

# Likely to download
t.test(likely_waze ~ exposed, data = MRF_data)

t.test(likely_waze_score3 ~ exposed, data = MRF_data)
t.test(likely_waze_score4 ~ exposed, data = MRF_data)

t.test(likely_google ~ exposed, data = MRF_data)
t.test(likely_apple ~ exposed, data = MRF_data)
t.test(likely_tomtom ~ exposed, data = MRF_data)


# Waze: as a table
MRF_data %>% 
  group_by(exposed) %>%
  summarise(Never_Familiar = mean(never_familiar),
            Heard_Familiar = mean(heard_familiar),
            Know_Familiar = mean(know_familiar),
            Most_Familiar = mean(most_familiar),
            Likely_Download = mean(likely_waze),
            Likely_Download_Score3 = mean(likely_waze_score3),
            Likely_Download_Score4 = mean(likely_waze_score4))


#-----------------------------------
# Investigate Ad recall
#----------------------------------
table(MRF_data$Q9) %>% prop.table()

t.test(ad_recall ~ exposed, 
       data = MRF_data %>% mutate(ad_recall = ifelse(Q9 == 1, 1, 0)))

#----------------------------------------
# save out the contacts and pid
#-----------------------------------------
write.csv(MRF_data %>% select(pid, radio_contacts:period),
          '2. Analysis/Waze_Exposed.csv', row.names = F)

#-----------------------------------------------
# DiD - data and result export
#-----------------------------------------------

did_df <- prewave_df %>% 
  select(pid, kpi$variable) %>%
  mutate(period = 0) %>%
  inner_join(select(MRF_data, pid, exposed), 
             by = 'pid') %>%
  rbind(select(MRF_data, pid, kpi$variable, period, exposed) %>%
          filter(pid %in% prewave_df$pid)) %>%
  mutate(never_familiar = ifelse(Q1r1 == 1, 1, 0),
         heard_familiar = ifelse(Q1r1 == 2, 1, 0),
         know_familiar = ifelse(Q1r1 == 3, 1, 0),
         most_familiar = ifelse(Q1r1 == 4, 1, 0),
         likely_waze = ifelse(Q2r1 > 2, 1, 0),
         likely_google = ifelse(Q2r2 > 2, 1, 0),
         likely_apple = ifelse(Q2r3 > 2, 1, 0),
         likely_tomtom = ifelse(Q2r4 > 2, 1, 0),
         likely_waze_score3 = ifelse(Q2r1 == 3, 1, 0),
         likely_waze_score4 = ifelse(Q2r1 == 4, 1, 0))


nrow(did_df) # 796*2 = 1592
head(did_df)
tail(did_df)

# did plotset for Waze
did_plotset <- did_df %>%
  group_by(period, exposed) %>%
  dplyr::summarise(Never_Familiar = mean(never_familiar),
                   Heard_Familiar = mean(heard_familiar),
                   Know_Familiar = mean(know_familiar),
                   Most_Familiar = mean(most_familiar),
                   Likely_Download = mean(likely_waze),
                   Likely_Download_Score3 = mean(likely_waze_score3),
                   Likely_Download_Score4 = mean(likely_waze_score4),
                   nresp = n()) %>% as.data.frame()

did_plotset

# plot know_familiar
ggplot(data = did_plotset, aes(x = factor(period), y = Know_Familiar, group = factor(exposed)))+
  geom_line(aes(color = factor(exposed)), size = 2)+
  ylim(0, 1)

# Most Familiar
ggplot(data = did_plotset, aes(x = factor(period), y = Most_Familiar, group = factor(exposed)))+
  geom_line(aes(color = factor(exposed)), size = 2)+
  ylim(0, 1)

# Likely to Download
ggplot(data = did_plotset, aes(x = factor(period), y = Likely_Download, group = factor(exposed)))+
  geom_line(aes(color = factor(exposed)), size = 2)+
  ylim(0, 1)

# Likely to Download (Scored 3)
ggplot(data = did_plotset, aes(x = factor(period), y = Likely_Download_Score3, group = factor(exposed)))+
  geom_line(aes(color = factor(exposed)), size = 2)+
  ylim(0, 1)

# Likely to Download (Scored 4)
ggplot(data = did_plotset, aes(x = factor(period), y = Likely_Download_Score4, group = factor(exposed)))+
  geom_line(aes(color = factor(exposed)), size = 2)+
  ylim(0, 1)

#-----------------------------------------------------------------------------
# Regression
#----------------------------------------------------------------------------
# Heard about Waze
heardfam_reg <- lm(heard_familiar ~ period * exposed, data = did_df) %>%
  summary()

heardfam_reg

heardfam_reg_df <- as.data.frame(heardfam_reg$coefficients) %>%
  mutate(varname = rownames(heardfam_reg$coefficients)) %>%
  select(varname, Estimate:`Pr(>|t|)`)

t.test(heard_familiar ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(heard_familiar ~ exposed, 
       data = did_df %>% filter(period == 1))

# Know about Waze
knowfam_reg <- lm(know_familiar ~ period * exposed, data = did_df) %>%
  summary()

knowfam_reg

knowfam_reg_df <- as.data.frame(knowfam_reg$coefficients) %>%
  mutate(varname = rownames(knowfam_reg$coefficients)) %>%
  select(varname, Estimate:`Pr(>|t|)`)

t.test(know_familiar ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(know_familiar ~ exposed, 
       data = did_df %>% filter(period == 1))

# Mostly familiar with Waze and its benefits
mostfam_reg <- lm(most_familiar ~ period * exposed, data = did_df) %>%
  summary()

mostfam_reg

mostfam_reg_df <- as.data.frame(mostfam_reg$coefficients) %>%
  mutate(varname = rownames(mostfam_reg$coefficients)) %>%
  select(varname, Estimate:`Pr(>|t|)`)


t.test(most_familiar ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(most_familiar ~ exposed, 
       data = did_df %>% filter(period == 1))

# likley to download waze
likely_reg <- lm(likely_waze ~ period * exposed, data = did_df) %>%
  summary()

likely_reg

lm(likely_apple ~ period * exposed, data = did_df) %>%
  summary()



likely_reg_df <- as.data.frame(likely_reg$coefficients) %>%
  mutate(varname = rownames(likely_reg$coefficients)) %>%
  select(varname, Estimate:`Pr(>|t|)`)


t.test(likely_waze ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(likely_waze ~ exposed, 
       data = did_df %>% filter(period == 1))

#----------------------------------------------------------
# Note: check individually for 3 or 4 in likely to download
#----------------------------------------------------------

# WAZE: Likely download (Scored 3)
likely_score3_reg <- lm(likely_waze_score3 ~ period * exposed, data = did_df) %>%
  summary()

likely_score3_reg


t.test(likely_waze_score3 ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(likely_waze_score3 ~ exposed, 
       data = did_df %>% filter(period == 1))


# WAZE: Likely download (Scored 4)
likely_score4_reg <- lm(likely_waze_score4 ~ period * exposed, data = did_df) %>%
  summary()

likely_score4_reg


t.test(likely_waze_score4 ~ exposed, 
       data = did_df %>% filter(period == 0))

t.test(likely_waze_score4 ~ exposed, 
       data = did_df %>% filter(period == 1))



#-------------------------------------
# First Choice
#--------------------------------------
# Waze: 1
# Google Maps: 2
# Apple Maps: 3
# TomTom Go: 4
# None of the above: 5

table(postwave_df$Q3) %>% prop.table()



rm(list = ls())

getwd()

library(dplyr)
library(readr)
library(haven)
library(MatchIt)
library(openxlsx)
library(lmtest)
library(sandwich)
library(xlsx)
library(readxl)

# load in the dataset
filename <- openxlsx::read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "data", 
                      rows = 2:3, cols = 2, colNames = FALSE)

prewave_data <- read_sav(paste0("./1. Raw Data/", filename$X1[1]))
prewave_data$pid <- as.numeric(prewave_data$pid)

postwave_data <- read_sav('./1. Raw Data/topup/ORD-656067-T3L1_SPSS_Data.SAV')
postwave_data$pid <- as.numeric(postwave_data$pid)


exposed_data <- read_csv('./2. Analysis/Waze_Exposed.csv')

kpi <- openxlsx::read.xlsx("1. Raw Data/DataInterface_Waze.xlsx", sheet = "kpi", 
                 startRow = 2, cols = 1:4, colNames = TRUE)

kpi

#---------------------------------
#----------------------------------------------
# select data to use for the matching modeling
#----------------------------------------------
class(postwave_data$pid)
class(exposed_data$pid)

model_data <- postwave_data %>%
  select(pid, S1:Q3, Q9) %>%
  inner_join(exposed_data, by = 'pid')


dim(model_data)

#----------------------
names(model_data)

post_unmatch <- model_data %>%
  filter(!pid %in% prewave_data$pid) %>%
  select(-index, -radio_contacts)

pre_unmatch <- prewave_data %>%
  filter(!pid %in% postwave_data$pid) %>%
  select(pid, S1:Q3, Q9) %>%
  mutate(exposed = NA,
         period = 0)

nrow(post_unmatch)
nrow(pre_unmatch)

names(post_unmatch)
names(pre_unmatch)

# append prewave data 
find_match_df <- pre_unmatch %>%
  rbind(post_unmatch)

table(find_match_df$period)

find_match_df

# No matching; constructing a pre-match matchit object
indepvar <- c('S1', 'A1', 'A2', 'A3', 'A4', 'A5', 'A6',
              'A7', 'A8', 'A9r1', 'A9r2', 'A9r3', 'A9r4')
treatvar <- 'period'

f <- as.formula(paste(treatvar, paste(indepvar, collapse = " + "), sep = " ~ "))

f

match_out_nearest <- matchit(f, data = find_match_df,
                             method = 'nearest', distance = 'glm')

summary(match_out_nearest, un = F)

# note: Std. Mean diff are all very close 0 and Var ratio is close to 1, which is good

#-----------------------------------------
# get the data
#-----------------------------------------
nearest_df <- match.data(match_out_nearest)

table(nearest_df$period)

nearest_df %>% select(pid, period, exposed, subclass) %>%
  filter(subclass == 480)

# use the subclass as the identifier to bring in the exposed to the 495 match in the prewave
found_match_prewave <- nearest_df %>%
  filter(period == 0) %>%
  select(-exposed)

interim_match_prewave <- nearest_df %>%
  filter(period == 1) %>%
  select(pid, exposed, subclass) %>%
  inner_join(found_match_prewave, by = 'subclass') %>%
  mutate(pid = pid.x + pid.y,
         matched = 1) %>%
  select(pid, S1:Q3, Q9, exposed, period, matched)

# checking that we have the exposed attached
interim_match_prewave %>% select(pid, exposed)

names(interim_match_prewave)
table(interim_match_prewave$period)
table(interim_match_prewave$exposed)

# get the 495 match to append to the 800 in both pre and post
interim_did_df <- prewave_data %>% 
  select(pid, S1:Q3, Q9) %>%
  inner_join(select(model_data, exposed, pid),
             by = 'pid') %>%
  mutate(period = 0) %>%
  rbind(select(model_data, -c(radio_contacts, index))) %>%
  filter(pid %in% prewave_data$pid)

names(interim_did_df)
nrow(interim_did_df)
table(interim_did_df$period)
table(interim_did_df$exposed) %>% prop.table()

# append the 495 (both pre and post) to did_df
full_did_df <- interim_did_df %>%
  mutate(matched = 0) %>%
  rbind(interim_match_prewave) %>%
  rbind(nearest_df %>%
          filter(period == 1) %>%
          select(pid:Q3, Q9, exposed, period) %>%
          mutate(matched = 1)) #%>% select(-c(S1:A9r4))


names(full_did_df)


#------------------
# Function to generate all the kpi vars
#--------------------------------------
gen_brand_kpis <- function(df, brand) {
  
  for (i in 1:length(brand)) {
    rawvars <- kpi$variable[kpi$Brand == brand[i]]
    b <- unlist(strsplit(brand[i], ' '))[1] %>% tolower()
    
    # mutate the kpi by brand and update in the same df given in the func arg
    df <- df %>% 
      mutate('never_familiar_{b}' := ifelse(.data[[ rawvars[1] ]] == 1, 1, 0),
             'heard_familiar_{b}' := ifelse(.data[[ rawvars[1] ]] == 2, 1, 0),
             'know_familiar_{b}' := ifelse(.data[[ rawvars[1] ]] == 3, 1, 0),
             'most_familiar_{b}' := ifelse(.data[[ rawvars[1] ]] == 4, 1, 0),
             'knowmost_familiar_{b}' := ifelse(.data[[ rawvars[1] ]] >= 3, 1, 0),
             'likely_{b}' := ifelse(.data[[ rawvars[2] ]] > 2, 1, 0),
             'first_choice_{b}' := case_when(b == 'waze' & .data[[ rawvars[3] ]] == 1 ~ 1,
                                             b == 'google' & .data[[ rawvars[3] ]] == 2 ~ 1,
                                             b == 'apple' & .data[[ rawvars[3] ]] == 3 ~ 1,
                                             b == 'tomtom' & .data[[ rawvars[3] ]] == 4 ~ 1,
                                             TRUE ~ 0))
    
  }
  
  return(df)
}

# example of how the function works
all_brands <- unique(kpi$Brand)
ab <- full_did_df

ef <- gen_brand_kpis(ab, all_brands)

grep('familiar|likely|first', names(ef), value = T)

# check that computation is accurate
ef %>% select(Q1r1, Q2r1, Q3, grep('waze', names(ef), value = T))

rm(ef, ab)

#--------------------------------------
# Get a did df with the all brands kpis
#--------------------------------------
brands_did_df <- gen_brand_kpis(full_did_df, all_brands)

grep('familiar|likely|first', names(brands_did_df), value = T)

head(brands_did_df)

#-------------------------
# all brand kpi df
#---------------------------
all_kpis_df <- data.frame(variable = grep('familiar|likely|first', names(brands_did_df), value = T),
                          brand = c(rep('Waze', 7), rep('Google Maps', 7), 
                                    rep('Apple Maps', 7), rep('TomTom Go', 7)),
                          stringsAsFactors = F)

all_kpis_df

#--------------------------------
# t.test and regression
#---------------------------------
t.test(likely_waze ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(likely_waze ~ exposed, data = brands_did_df %>% filter(period == 1))

likely_reg <- lm(likely_waze ~ exposed * period, 
                 data = brands_did_df) %>% summary()

likely_reg


# know familiar
t.test(know_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(know_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 1))

lm(know_familiar_waze ~ exposed * period, 
     data = brands_did_df) %>% summary()


t.test(most_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(most_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 1))

lm(most_familiar_waze ~ exposed * period, 
   data = brands_did_df) %>% summary()

# know or most familiar
t.test(knowmost_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(knowmost_familiar_waze ~ exposed, data = brands_did_df %>% filter(period == 1))

lm(knowmost_familiar_waze ~ exposed * period, 
   data = brands_did_df) %>% summary()

# first choice
t.test(first_choice_waze ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(first_choice_waze ~ exposed, data = brands_did_df %>% filter(period == 1))

lm(first_choice_waze ~ exposed * period, 
   data = brands_did_df) %>% summary()

# other brands
t.test(likely_google ~ exposed, data = brands_did_df %>% filter(period == 0))
t.test(likely_google ~ exposed, data = brands_did_df %>% filter(period == 1))

lm(likely_apple ~ exposed * period, 
   data = brands_did_df %>% filter(matched == 0)) %>% summary()


# Overview of the kpi scores by period and exposed
did_plotset <- brands_did_df %>%
  group_by(period, exposed) %>%
  dplyr::summarise(Never_Familiar = mean(never_familiar_waze),
                   Heard_Familiar = mean(heard_familiar_waze),
                   Know_Familiar = mean(know_familiar_waze),
                   Most_Familiar = mean(most_familiar_waze),
                   KnowMost_Familiar = mean(knowmost_familiar_waze),
                   Likely_Download = mean(likely_waze),
                   FirstChoice = mean(first_choice_waze),
                   nresp = n()) %>% as.data.frame()

did_plotset


# compare the prewave vs postwave using the 495
t.test(likely_waze ~ exposed, data = brands_did_df %>% filter(matched == 1, period == 0))
t.test(likely_waze ~ exposed, data = brands_did_df %>% filter(matched == 1, period == 1))


lm(likely_waze ~ exposed * period, data = brands_did_df %>% filter(matched == 0)) %>%
  summary()




#-------------------
# REWRITE FUNCTION to pull out all the result to excel file
#-------------------------------------------
get_model_reg <- function(kpi_df, did_df) {
  
  final_output <- data.frame()
  for (brand_name in unique(kpi_df$brand)) {
    #print(brand_name)
    
    vardf <- kpi_df %>% filter(brand == brand_name)
    
    nested_output <- data.frame()
    for (kpivar in vardf$variable) {
      #print(varname)
      
      # run the reg model using the varname
      indepvar <- c('period', 'exposed')
      
      f <- as.formula(paste(kpivar, paste(indepvar, collapse = " * "), sep = " ~ "))
      
      # model
      model <- lm(f, data = did_df) %>% summary()
      
      # get the coefficients
      model_df <- as.data.frame(model$coefficients) %>%
        mutate(varnames = rownames(model$coefficients),
               kpi = kpivar,
               brand = brand_name) %>%
        select(kpi, brand, varnames, Estimate:`Pr(>|t|)`)
      
      # append
      nested_output <- rbind(nested_output, model_df)
      #cat(paste('Successfully done for the Brand:', brand_name), '\n')
      #cat('\n')
      
    }
    final_output <- rbind(final_output, nested_output)
  }
  # rename the rownames
  rownames(final_output) <- 1:nrow(final_output)
  return(final_output)
}

get_model_reg(all_kpis_df, brands_did_df)

# output
model_output_matched <- get_model_reg(all_kpis_df, brands_did_df)

head(model_output_matched)

model_output_recontact <- get_model_reg(all_kpis_df, 
                                        brands_did_df %>% filter(matched == 0))

head(model_output_recontact)


#-------------------------------
# DiD output
#-------------------------------
did_output <- function(kpi_df, did_df, model_reg_output) {
  
  prewave_control_did <- filter(did_df, period == 0, exposed == 0) %>% as.data.frame()
  prewave_exposed_did <- filter(did_df, period == 0, exposed == 1) %>% as.data.frame()
  postwave_control_did <- filter(did_df, period == 1, exposed == 0) %>% as.data.frame()
  postwave_exposed_did <- filter(did_df, period == 1, exposed == 1) %>% as.data.frame()
  
  # KPI
  final_output <- data.frame()
  
  for (brand_name in unique(kpi_df$brand)) {
    #print(brand_name)
    vardf <- kpi_df %>% filter(brand == brand_name)
    # kpi individual vars
    nested_output <- data.frame()
    for (kpivar in vardf$variable) {
      #print(kpivar)
      prewave_control_kpiscore <- mean(prewave_control_did[, kpivar])
      prewave_exposed_kpiscore <- mean(prewave_exposed_did[, kpivar])
      postwave_control_kpiscore <- mean(postwave_control_did[, kpivar])
      postwave_exposed_kpiscore <- mean(postwave_exposed_did[, kpivar])
      
      
      select_model_reg <- filter(model_reg_output, kpi == kpivar, brand == brand_name)
      
      # create the dataframe
      did_output_df <- data.frame(kpi = kpivar,
                                  brand = brand_name,
                                  n_control = nrow(prewave_control_did),
                                  n_exposed = nrow(prewave_exposed_did),
                                  prewave_control = prewave_control_kpiscore,
                                  prewave_exposed = prewave_exposed_kpiscore,
                                  postwave_control = postwave_control_kpiscore,
                                  postwave_exposed = postwave_exposed_kpiscore,
                                  did_score = select_model_reg$Estimate[4],
                                  did_pvalue = select_model_reg$`Pr(>|t|)`[4])
      
      # rbind
      nested_output <- rbind(nested_output, did_output_df)
    }
    final_output <- rbind(final_output, nested_output)
  } 
  
  return(final_output)
  
}

# example of the output from using the func did_output
head(did_output(all_kpis_df, brands_did_df, model_output_matched), 7)

# output
did_output_matched <- did_output(all_kpis_df, 
                                 brands_did_df, 
                                 model_output_matched)

head(did_output_matched)

did_output_recontact <- did_output(all_kpis_df, 
                                   brands_did_df %>% filter(matched == 0), 
                                   model_output_recontact)

head(did_output_recontact)
#--------------------------------------------------------------------------------
# Look at other audience and regions
#--------------------------------------------------------------------------------

# Audience who have driving licence
brands_did_df %>% filter(matched == 0, period == 0, A7 == 1) %>%
  nrow()

table(postwave_data$A7[postwave_data$pid %in% prewave_data$pid])
table(prewave_data$A7[prewave_data$pid %in% postwave_data$pid])

lm(likely_waze ~ exposed * period, data = brands_did_df %>% filter(matched == 0)) %>%
  summary()

lm(likely_waze ~ exposed * period, 
   data = brands_did_df %>% 
     filter(matched == 0, A7 <= 2)) %>%
  summary()

model_drivers_recontact <- get_model_reg(all_kpis_df, 
                                         brands_did_df %>% 
                                           filter(matched == 0, A7 <= 2))


head(model_drivers_recontact)

did_drivers_recontact <- did_output(all_kpis_df, 
                                    brands_did_df %>% 
                                      filter(matched == 0, A7 <= 2),
                                    model_drivers_recontact)

head(did_drivers_recontact, 7)

#--------------
# region var: A6
# North West - 2
# West Midlands - 5
# East of England - 6
# London - 7
# South East - 8

table(postwave_data$A6[postwave_data$pid %in% prewave_data$pid])
table(prewave_data$A6[prewave_data$pid %in% postwave_data$pid])

target_region_cat <- c(2, 5, 6, 8)

lm(likely_waze ~ exposed * period, 
   data = brands_did_df %>% 
     filter(matched == 0, A6 %in% target_region_cat)) %>%
  summary()

#---------------------
# create a column to identify target region: incl. london (target_reg_lon)
brands_did_df <- brands_did_df %>% 
  mutate(target_reg_lon = ifelse(A6 %in% c(2, 5, 6, 7, 8), 1, 0),
         target_reg = ifelse(A6 %in% c(2, 5, 6, 8), 1, 0),
         ad_recall = ifelse(Q9 == 1, 1, 0))

t.test(likely_waze ~ target_reg_lon, 
       data = brands_did_df %>% 
         filter(period == 0, matched == 0, exposed == 0))

t.test(likely_waze ~ target_reg_lon, 
       data = brands_did_df %>% 
         filter(period == 1, matched == 0, exposed == 0))

# output
model_region_recontact <- get_model_reg(all_kpis_df, 
                                        brands_did_df %>% 
                                        filter(matched == 0, target_reg == 1))


head(model_region_recontact)

did_region_recontact <- did_output(all_kpis_df, 
                                    brands_did_df %>% 
                                    filter(matched == 0, target_reg == 1),
                                    model_region_recontact)

head(did_region_recontact, 7)

# output incl. london in target region
model_region_lon_recontact <- get_model_reg(all_kpis_df, 
                                            brands_did_df %>% 
                                            filter(matched == 0, target_reg_lon == 1))


head(model_region_lon_recontact)

did_region_lon_recontact <- did_output(all_kpis_df, 
                                       brands_did_df %>% 
                                       filter(matched == 0, target_reg_lon == 1),
                                       model_region_lon_recontact)

head(did_region_lon_recontact, 7)

#--------------------------
# write out to excel
#-------------------------------------
# write to xlsx file
# -----------------------------------
result_file <- '2. Analysis/Waze_AllBrands_DiD_Result.xlsx'
if (file.exists(result_file)) {
  # copy file to archive folder
  file.copy(from = result_file,
            to   = "2. Analysis/archive/")
}
# delete file from its original folder
file.remove(result_file)

# Now write out a new file [the old is in archive]
# Regression output
# write.xlsx2(model_output_matched, result_file,
#             sheetName = "Model_Regression_Recontact",
#             col.names = T, row.names = F, append = T)

# Overall Model Regression Output (800 recontact sample)
write.xlsx2(model_output_recontact, result_file,
            sheetName = "Overall_Model_Reg",
            col.names = T, row.names = F, append = T)

# Audience with driving license
write.xlsx2(model_drivers_recontact, result_file,
            sheetName = "Drivers_Model_Reg",
            col.names = T, row.names = F, append = T)

# Target Region (excl. London)
write.xlsx2(model_region_recontact, result_file,
            sheetName = "TargetRegion_Model_Reg",
            col.names = T, row.names = F, append = T)

# Target Region (excl. London)
write.xlsx2(model_region_lon_recontact, result_file,
            sheetName = "TargetRegion_Lon_Model_Reg",
            col.names = T, row.names = F, append = T)


#------- DiD table------------------------------

# Overall DiD output
write.xlsx2(did_output_recontact, result_file,
            sheetName = "Overall_DiD",
            col.names = T, row.names = F, append = T)

# Audience with driving license
write.xlsx2(did_drivers_recontact, result_file,
            sheetName = "Drivers_DiD",
            col.names = T, row.names = F, append = T)

# Target Region (excl. London)
write.xlsx2(did_region_recontact, result_file,
            sheetName = "TargetRegion_DiD",
            col.names = T, row.names = F, append = T)

# Target Region (excl. London)
write.xlsx2(did_region_lon_recontact, result_file,
            sheetName = "TargetRegion_Lon_DiD",
            col.names = T, row.names = F, append = T)


# check that new file exist
file.exists(result_file)



#------------------------------






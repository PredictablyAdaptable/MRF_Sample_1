setwd('SET WD')

source('SOURCE TO UDF')

library(haven)
library(dplyr)
library(DT)

df <- read_sav('sav file')

# Extract the columns names and description for easy navigation 
questbook <- CreateQuestBook(df)

# View the questbook
datatable(questbook)

head(df)

table(df$A6)

# 1 North East
# 2 North West
# 3 Yorkshire and The Humber
# 4 East Midlands
# 5 West Midlands
# 6 East of England
# 7 London
# 8 South East
# 9 South West
# 10 Wales
# 11 Scotland
# 12 Northern Ireland

table(df$A6, df$Q8br46)
select(df, Q8br39)
#Q8r23 to Q8r39
#Q8br23 to Q8br39
#Q8br87 Q8cr87 Q8dr87 (Other)
#Q8cr23 to Q8cr39
# Q8dr23 to Q8dr39

table(df$lrnMediaPlayerAudior1)

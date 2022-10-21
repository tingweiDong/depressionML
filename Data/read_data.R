
library(RNHANES)
library(foreign)
library(dplyr)

# 2005 - 2014 datasets
data2005 <- nhanes_load_data("DPQ_D", "2005-2006")
data2007 <- nhanes_load_data("DPQ_E", "2007-2008")
data2009 <- nhanes_load_data("DPQ_F", "2009-2010")
data2011 <- nhanes_load_data("DPQ_G", "2011-2012")
data2013 <- nhanes_load_data("DPQ_H", "2013-2014")

# 2015 - 2016 datasets
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DPQ_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
data2015 <- foreign::read.xport(temp)
#adding in columns that were missing from 2015 and 2017 data
data2015$file_name <- c("DPQ_I")
data2015$cycle <- c("2015-2016")
data2015$begin_year <- c(2015)
data2015$begin_year <- as.double(data2015$begin_year)
data2015$end_year <- c(2016)
data2015$end_year <- as.double(data2015$end_year)

# March 2017 - 2020 datasets
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_DPQ.xpt", 
              tf <- tempfile(), 
              mode="wb")
data2017 <- foreign::read.xport(tf)
data2017$file_name <- c("P_DPQ")
data2017$cycle <- c("2017-March 2020")
data2017$begin_year <- c(2017)
data2017$begin_year <- as.double(data2017$begin_year)
data2017$end_year <- c(2020)
data2017$end_year <- as.double(data2017$end_year)

# Combining all the datasets from 2009 to 2018 into one
dataFrom20052008 <- rbind(data2005, data2007)
dataFrom20092011 <- rbind(data2009,data2011)
dataFrom20052011 <- rbind(dataFrom20052008,dataFrom20092011)
dataFrom20112014 <- rbind(dataFrom20052011, data2013)
dataFrom20152017 <- rbind(data2015, data2017)
fulldataset <- rbind(dataFrom20112014,dataFrom20152017 )

fulldataset[fulldataset == 7] <- NA
fulldataset[fulldataset == 9] <- NA

# Omitting all NA values: 43,928 to 26,473. 
fulldataset <- na.omit(fulldataset)

# Adding in a score column by calculating all the values in each column for a individual to get them a final score 
fulldataset$score <- fulldataset$DPQ010 + fulldataset$DPQ020 + fulldataset$DPQ030 + fulldataset$DPQ040 + fulldataset$DPQ050 + fulldataset$DPQ060 + fulldataset$DPQ060 + fulldataset$DPQ070 + fulldataset$DPQ080 + fulldataset$DPQ090 + fulldataset$DPQ100

#If a score is greater or equal to 10, "1" means the individual has depression. "0" means the individual doesn't have depression
fulldataset$finalScore <- ifelse(fulldataset$score <= 10, "0", "1")

## Demographics Data
DEMdata2005 <- nhanes_load_data("DEMO_D", "2005-2006")
DEMdata2007 <- nhanes_load_data("DEMO_E", "2007-2008")
DEMdata2009 <- nhanes_load_data("DEMO_F", "2009-2010")
DEMdata2011 <- nhanes_load_data("DEMO_G", "2011-2012")
DEMdata2013 <- nhanes_load_data("DEMO_H", "2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
DEMdata2015 <- foreign::read.xport(temp)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_DEMO.xpt", 
              tf <- tempfile(), 
              mode="wb")
DEMdata2017 <- foreign::read.xport(tf)

demo_col <- c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDMARTL', 'INDFMPIR', 'DMDEDUC2', 'FIALANG')
reduce_DEMdata2005 <- DEMdata2005[,demo_col]
reduce_DEMdata2007 <- DEMdata2007[,demo_col]
reduce_DEMdata2009 <- DEMdata2009[,demo_col]
reduce_DEMdata2011 <- DEMdata2011[,demo_col]
reduce_DEMdata2013 <- DEMdata2013[,demo_col]
reduce_DEMdata2015 <- DEMdata2015[,demo_col]
demo2005_2015 <- do.call(rbind, list(reduce_DEMdata2005, reduce_DEMdata2007,
                                     reduce_DEMdata2009, reduce_DEMdata2011,
                                     reduce_DEMdata2013, reduce_DEMdata2015))
names(demo2005_2015) <- c('id', 'age','gender','race', 'marital_status', 'family_PIR',
                          'education_level_adults', 'language')

demo_col <- c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH1', 'DMDMARTZ', 'INDFMPIR', 'DMDEDUC2', 'FIALANG')
reduce_DEMdata2017 <- DEMdata2017[,demo_col]
names(reduce_DEMdata2017) <- c('id', 'age','gender','race', 'marital_status', 
                               'family_PIR','education_level_adults', 'language')

demo <- do.call(rbind, list(demo2005_2015, reduce_DEMdata2017))
demo <- demo %>% mutate(marital_status = na_if(marital_status, 77))
demo <- demo %>% mutate(marital_status = na_if(marital_status, 99))
demo <- demo %>% mutate(education_level_adults = na_if(education_level_adults, 77))
demo <- demo %>% mutate(education_level_adults = na_if(education_level_adults, 77))

## Sleep Disorders
SLPdata2005 <- nhanes_load_data("SLQ_D", "2005-2006")
SLPdata2007 <- nhanes_load_data("SLQ_E", "2007-2008")
SLPdata2009 <- nhanes_load_data("SLQ_F", "2009-2010")
SLPdata2011 <- nhanes_load_data("SLQ_G", "2011-2012")
SLPdata2013 <- nhanes_load_data("SLQ_H", "2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SLQ_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
SLPdata2015 <- foreign::read.xport(temp)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_SLQ.xpt", 
              tf <- tempfile(), 
              mode="wb")
SLPdata2017 <- foreign::read.xport(tf)

sleep_col <- c('SEQN', 'SLD010H', 'SLQ050')
reduce_SLPdata2005 <- SLPdata2005[,sleep_col]
reduce_SLPdata2007 <- SLPdata2007[,sleep_col]
reduce_SLPdata2009 <- SLPdata2009[,sleep_col]
reduce_SLPdata2011 <- SLPdata2011[,sleep_col]
reduce_SLPdata2013 <- SLPdata2013[,sleep_col]
sleep2005_2013 <- do.call(rbind, list(reduce_SLPdata2005, reduce_SLPdata2007,
                                      reduce_SLPdata2009, reduce_SLPdata2011,
                                      reduce_SLPdata2013))
names(sleep2005_2013) <- c('id', 'sleep_hours', 'trouble_sleeping_history')
sleep2005_2013 <- sleep2005_2013 %>% mutate(sleep_hours = na_if(sleep_hours, 77))
sleep2005_2013 <- sleep2005_2013 %>% mutate(sleep_hours = na_if(sleep_hours, 99))

sleep_col <- c('SEQN', 'SLD012', 'SLQ050')
reduce_SLPdata2015 <- SLPdata2015[,sleep_col]
names(reduce_SLPdata2015) <- c('id', 'sleep_hours', 'trouble_sleeping_history')
reduce_SLPdata2017 <- SLPdata2017[,sleep_col]
names(reduce_SLPdata2017) <- c('id', 'sleep_hours', 'trouble_sleeping_history')

sleep <- do.call(rbind, list(sleep2005_2013, reduce_SLPdata2015, reduce_SLPdata2017))

## Alcohol Use
ALCdata2005 <- nhanes_load_data("ALQ_D", "2005-2006")
ALCdata2007 <- nhanes_load_data("ALQ_E", "2007-2008")
ALCdata2009 <- nhanes_load_data("ALQ_F", "2009-2010")
ALCdata2011 <- nhanes_load_data("ALQ_G", "2011-2012")
ALCdata2013 <- nhanes_load_data("ALQ_H", "2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/ALQ_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
ALCdata2015 <- foreign::read.xport(temp)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_ALQ.xpt", 
              tf <- tempfile(), 
              mode="wb")
ALCdata2017 <- foreign::read.xport(tf)

alcohol_col <- c('SEQN', 'ALQ130')
reduce_ALCdata2005 <- ALCdata2005[,alcohol_col]
reduce_ALCdata2007 <- ALCdata2007[,alcohol_col]
reduce_ALCdata2009 <- ALCdata2009[,alcohol_col]
reduce_ALCdata2011 <- ALCdata2011[,alcohol_col]
reduce_ALCdata2013 <- ALCdata2013[,alcohol_col]
reduce_ALCdata2015 <- ALCdata2015[,alcohol_col]
reduce_ALCdata2017 <- ALCdata2017[,alcohol_col]

alcohol <- do.call(rbind, list(reduce_ALCdata2005, reduce_ALCdata2007,
                               reduce_ALCdata2009, reduce_ALCdata2011,
                               reduce_ALCdata2013, reduce_ALCdata2015,
                               reduce_ALCdata2017))
names(alcohol) <- c('id', 'drinks_per_occasion')
alcohol <- alcohol %>% mutate(drinks_per_occasion = na_if(drinks_per_occasion, 777))
alcohol <- alcohol %>% mutate(drinks_per_occasion = na_if(drinks_per_occasion, 999))

## Smoking - Cigarette Use
SMKdata2005 <- nhanes_load_data("SMQ_D", "2005-2006")
SMKdata2007 <- nhanes_load_data("SMQ_E", "2007-2008")
SMKdata2009 <- nhanes_load_data("SMQ_F", "2009-2010")
SMKdata2011 <- nhanes_load_data("SMQ_G", "2011-2012")
SMKdata2013 <- nhanes_load_data("SMQ_H", "2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
SMKdata2015 <- foreign::read.xport(temp)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_SMQ.xpt", 
              tf <- tempfile(), 
              mode="wb")
SMKdata2017 <- foreign::read.xport(tf)

smoking_col <- c('SEQN', 'SMQ020', 'SMD030', 'SMQ040', 'SMD641', 'SMD650', 'SMD630',
                 'SMQ670')
reduce_SMKdata2005 <- SMKdata2005[,smoking_col]
reduce_SMKdata2007 <- SMKdata2007[,smoking_col]
reduce_SMKdata2009 <- SMKdata2009[,smoking_col]
reduce_SMKdata2011 <- SMKdata2011[,smoking_col]
reduce_SMKdata2013 <- SMKdata2013[,smoking_col]
reduce_SMKdata2015 <- SMKdata2015[,smoking_col]
reduce_SMKdata2017 <- SMKdata2017[,smoking_col]

smoking <- do.call(rbind, list(reduce_SMKdata2005, reduce_SMKdata2007,
                               reduce_SMKdata2009, reduce_SMKdata2011,
                               reduce_SMKdata2013, reduce_SMKdata2015,
                               reduce_SMKdata2017))
names(smoking) <- c('id', 'SMQ020', 'SMD030', 'SMQ040', 'SMD641', 'SMD650', 'SMD630',
                    'SMQ670')
smoking <- smoking %>% mutate(across(c(SMQ020, SMQ040, SMQ670), na_if, 7))
smoking <- smoking %>% mutate(across(c(SMQ020, SMQ040, SMQ670), na_if, 9))
smoking <- smoking %>% mutate(across(c(SMD030, SMD650), na_if, 777))
smoking <- smoking %>% mutate(across(c(SMD030, SMD650), na_if, 999))
smoking <- smoking %>% mutate(across(c(SMD641, SMD630), na_if, 77))
smoking <- smoking %>% mutate(across(c(SMD641, SMD630), na_if, 99))

## Weight History
WHdata2005 <- nhanes_load_data("WHQ_D", "2005-2006")
WHdata2007 <- nhanes_load_data("WHQ_E", "2007-2008")
WHdata2009 <- nhanes_load_data("WHQ_F", "2009-2010")
WHdata2011 <- nhanes_load_data("WHQ_G", "2011-2012")
WHdata2013 <- nhanes_load_data("WHQ_H", "2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/WHQ_I.xpt", 
              temp <- tempfile(), 
              mode="wb")
WHdata2015 <- foreign::read.xport(temp)
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/P_WHQ.xpt", 
              tf <- tempfile(), 
              mode="wb")
WHdata2017 <- foreign::read.xport(tf)

weight_cols <- c("SEQN", "WHD010", "WHD020", "WHQ030", "WHQ040", "WHD050", "WHQ070", "WHD110", "WHD120", "WHD140", "WHQ150")
reduce_WHdata2005 <- WHdata2005[,weight_cols]
reduce_WHdata2007 <- WHdata2007[,weight_cols]
reduce_WHdata2009 <- WHdata2009[,weight_cols]
reduce_WHdata2011 <- WHdata2011[,weight_cols]
reduce_WHdata2013 <- WHdata2013[,weight_cols]
reduce_WHdata2015 <- WHdata2015[,weight_cols]
reduce_WHdata2017 <- WHdata2017[,weight_cols]

weight <- do.call(rbind, list(reduce_WHdata2005, reduce_WHdata2007,
                              reduce_WHdata2009, reduce_WHdata2011,
                              reduce_WHdata2013, reduce_WHdata2015,
                              reduce_WHdata2017))
names(weight) <- c('id', "WHD010", "WHD020", "WHQ030", "WHQ040", "WHD050", "WHQ070", "WHD110", "WHD120", "WHD140", "WHQ150")
weight <- weight %>% mutate(across(c(WHD010, WHD020, WHD050, WHD110, WHD120, WHD140), na_if, 7777))
weight <- weight %>% mutate(across(c(WHD010, WHD020, WHD050, WHD110, WHD120, WHD140), na_if, 9999))
weight <- weight %>% mutate(across(c(WHQ030, WHQ040, WHQ070), na_if, 7))
weight <- weight %>% mutate(across(c(WHQ030, WHQ040, WHQ070), na_if, 9))
weight <- weight %>% mutate(across(c(WHQ150), na_if, 77777))
weight <- weight %>% mutate(across(c(WHQ150), na_if, 99999))

# Complete dataset
df_list <- list(demo, sleep, alcohol, smoking, weight)      
var_df <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 

depression <- data.frame(fulldataset$SEQN, fulldataset$finalScore)
names(depression) <- c('id', 'result')
df <- merge(depression, var_df, by = c("id")) 

write.csv(df, 'Data/project_data.csv')

# numerical: age, sleep_hours, SMD641, WHD010, WHD020, WHD050, WHD110, WHD120, WHD140


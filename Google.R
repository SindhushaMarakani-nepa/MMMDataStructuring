library(stringr)
library(tidyverse)
library(lubridate)
library(ISOweek)
library(readxl)
library(tibble)
library(zoo)
library(corrplot)
library(relaimpo)
library(glmnet)
library(ggplot2)
library(rlist)
library(rJava)
library(xlsx)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(Rcpp)


country <- "Sweden"

StartWeek <- 201801
EndWeek <- 202143
year_start = 2018
year_end = 2021
week_start = 01
week_end = 52

select <- dplyr::select


input_2018 <- data.frame(Yearweek = as.character(201801:201853),
                         stringsAsFactors = F)
input_2019 <- data.frame(Yearweek = as.character(201901:201953),
                         stringsAsFactors = F)
input_2020 <- data.frame(Yearweek = as.character(202001:202053),
                         stringsAsFactors = F)
input_2021 <- data.frame(Yearweek = as.character(202101:202143),
                         stringsAsFactors = F)

input_df <- rbind(input_2018, input_2019, input_2020, input_2021) %>%
  mutate(YearWeek = as.numeric(Yearweek)) %>%
  dplyr::select(YearWeek)

# Reading spend&impressions file from Google
setwd("<<Location of raw data files recieved from Google>>")
input1 <- read.csv("XXXGoogleAds.csv", stringsAsFactors = F) %>%
  select(ReportDate, Product, Cost, Impressions)
input2 <- read.csv("XXXChannelFactoryGoogleAds.csv", stringsAsFactors = F) %>%
  select(ReportDate, Product, Cost, Impressions)

input = rbind(input1,input2)


# Reading data from Google trends and place it in the correct week
GoogleTrend <- read_csv("/Google- data used for structuring/Google trend.csv")%>%
  tbl_df() %>% 
  mutate(Date = as.Date(Week))%>%
  mutate(Week = ifelse(nchar(isoweek(Date)+1)==1,paste0("0",isoweek(Date)+1),paste0(isoweek(Date)+1)),
         Year = ifelse(as.numeric(format(Date,format="%m"))==12 & as.numeric(isoweek(Date))==1,as.numeric(format(Date,format="%Y"))+1,as.numeric(format(Date,format="%Y"))),
         YearWeek = as.numeric(paste0(Year,Week)))%>%
  rename(Google_Trend_Index = 'XXXX: (Sweden)') %>%
  select(YearWeek,
         TrendIndex = Google_Trend_Index)

GoogleTrend$YearWeek<-gsub(201753,201801, GoogleTrend$YearWeek)
GoogleTrend$YearWeek<-gsub(201853,201901, GoogleTrend$YearWeek)
GoogleTrend$YearWeek<-gsub(201953,202001, GoogleTrend$YearWeek)
GoogleTrend$YearWeek<-gsub(202154,202101, GoogleTrend$YearWeek)


## Summarize the spends on each YearWeek
GoogleTrend <- GoogleTrend %>% 
  group_by(YearWeek) %>%
  summarize(TrendIndex_final = sum(TrendIndex, na.rm = T))

## Setting up a dataframe that translates Google products to Media types 
Google_products <- data.frame(Product = c('AUCTION_SEARCH_OTHER', 
                                          'AUCTION_SEARCH_PLA',
                                          'AUCTION_SEARCH_LIA',
                                          
                                          'AUCTION_TRUEVIEW_INSTREAM',
                                          'RESERVE_VIDEO_NON_SKIPPABLE',
                                          'AUCTION_VIDEO_BUMPER',
                                          'AUCTION_VIDEO_NON_SKIPPABLE',
                                          'RESERVE_VIDEO_SKIPPABLE',
                                          'AUCTION_TRUEVIEW_DISCOVERY',
                                          'RESERVE_VIDEO_NON_SKIPPABLE',
                                          'RESERVE_MASTHEAD',
                                          
                                          'AUCTION_DISPLAY',
                                          'RESERVE_DISPLAY',
                                          'AUCTION_LIGHTBOX'),
                              
                              MediaType = c('Search',
                                            'Search',
                                            'Search',
                                            
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            'WebTV',
                                            
                                            'Display',
                                            'Display',
                                            'Display')) %>% 
  tbl_df() %>% 
  mutate(Product = as.character(Product),
         MediaType= as.character(MediaType))

# ------------------------------------------------------------------------------------------------ #
#### SEARCH: Distribute spend on Impressions & neutralize on Google search ####
# ------------------------------------------------------------------------------------------------ #

df_Search <- tbl_df(input) %>% 
  
## Creating Year, Week and YearWeek columns
  mutate(Date = as.Date(ReportDate),
         Year = year(Date),
         Week = isoweek(Date),
         Year = as.character(Year),
         Week = as.character(Week),
         Week = ifelse(nchar(Week) == 1, paste0("0", Week), Week),
         YearWeek = paste(Year, Week, sep = "")) %>% 
  
## Merging raw data with Google product table to get proper classifications
  left_join(Google_products, by = 'Product') %>% 
  
## Selecting only google search investments 
  filter(MediaType == "Search") %>% 
  
## Summarize spend and impressions per week
  group_by(YearWeek, Year) %>% 
  
  summarize(Original_Spend = sum(Cost, na.rm = T),
            Original_Impressions = sum(Impressions, na.rm = T)) %>%
  
## Summarize yearly Spend and Impressions 
  group_by(Year) %>% 
  
  mutate(Yearly_Spend = sum(Original_Spend, na.rm = T),
         Yearly_Imp = sum(Original_Impressions, na.rm = T)) %>% 
  
  ungroup() %>% 
  
## Calculate share of impressions each week
  mutate(Share_Impressions = Original_Impressions / Yearly_Imp) %>%
  
## Distribute Spend on Impressions (yearly basis)
  mutate(Distributed_Spend = Share_Impressions * Yearly_Spend) %>% 
  
## Neutralize paid search from organic search
  left_join(GoogleTrend, by = 'YearWeek') %>% 
  
## Creating the variable Index Adjusted Spend
  mutate(IndexAdjusted_spend = Distributed_Spend / TrendIndex_final) %>% 
  
## Summarises yearly Index Adjusted Spend
  group_by(Year) %>% 
  
  mutate(Yearly_IndexAdjusted_spend = sum(IndexAdjusted_spend)) %>% 
  
  ungroup() %>% 
  
## Creating the Index by which we should increase the Index Adjusted Spend
  mutate(Index = Yearly_Spend / Yearly_IndexAdjusted_spend) %>% 
  
## Creating the final Google search variable
  mutate(GoogleSearch = Index * IndexAdjusted_spend)

## Controling that spend per year correspond to original file
df_Search = df_Search %>% 
  mutate(YearWeek = as.numeric(YearWeek)) %>%
  select(YearWeek, Original_Spend_search = Original_Spend, 
         Distributed_Spend_search = Distributed_Spend, 
         GoogleSearch_with_trend = GoogleSearch)  
#  group_by(Year) %>% 
#  summarise_all(sum) 

# We do only select the final variable to be used in the data table 
#df_Search <- tbl_df(df_Search) %>% 
  #select(YearWeek, Original_Spend_search = Original_Spend,
         #Non_trend_adjusted_google_search = Distributed_Spend, Trend_adjusted_google_search = GoogleSearch)


# ------------------------------------------------------------------------------------------------ #
#### YouTube #### 
# ------------------------------------------------------------------------------------------------ #

df_Youtube <- tbl_df(input) %>% 
  
  # Introduce relevant variables for weeks
  mutate(Date = as.Date(ReportDate),
         Year = year(Date),
         Week = isoweek(Date),
         Year = as.character(Year),
         Week = as.character(Week),
         Week = ifelse(nchar(Week) == 1, paste0("0", Week), Week),
         YearWeek = paste(Year, Week, sep = "")) %>% 
  
  # Merging the columns from the Google product table to get proper classifications
  left_join(Google_products, by = 'Product') %>% 
  
  # Select only YouTube 
  filter(MediaType == "WebTV") %>% 
  
  # Summarize spend and impressions per week
  group_by(YearWeek, Year) %>% 
  
  summarize(Original_Spend = sum(Cost, na.rm = T),
            Original_Impressions = sum(Impressions, na.rm = T)) %>%
  
  # Summarises yearly Spend and Impressions 
  group_by(Year) %>% 
  
  mutate(Yearly_Spend = sum(Original_Spend, na.rm = T),
         Yearly_Imp = sum(Original_Impressions, na.rm = T)) %>% 
  
  ungroup() %>% 
  
  # Calculate share of impressions each week
  mutate(Share_Impressions = Original_Impressions / Yearly_Imp) %>%
  
  # Distribute Spend on Impressions (yearly basis)
  mutate(YouTube = Share_Impressions * Yearly_Spend)

# Controling that spend per year correspond to original file
#df_Youtube %>% 
  #select(YearWeek, Original_Spend, YouTube) %>% 
  #mutate(Diff = Original_Spend - YouTube) %>% 
  #group_by(YearWeek) %>% 
  #summarise_all(sum)

# We do only select the final variable to be used in the data table 
df_Youtube <- tbl_df(df_Youtube) %>% 
  mutate(YearWeek = as.numeric(YearWeek)) %>%
  select(YearWeek, Original_Spend_YouTube = Original_Spend,
         YouTube)

# ------------------------------------------------------------------------------------------------ #
#### Merging all media types #### 
# ------------------------------------------------------------------------------------------------ #

df <- tbl_df(input_df) %>% 
  left_join(df_Search) %>% 
  left_join(df_Youtube) %>% 
  replace(., is.na(.), 0)

setwd("<<Location to save structured data>>")
write_csv(df, "Google search and YouTube.csv")


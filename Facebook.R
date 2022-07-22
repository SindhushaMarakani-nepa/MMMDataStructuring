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


## Edit Country, Start and End Week based on modelling periods

country <- "Sweden"

StartWeek <- 202001
EndWeek <- 202245
year_start = 2020
year_end = 2022
week_start = 01
week_end = 45

select <- dplyr::select


input_2020 <- data.frame(Yearweek = as.character(202001:202053),
                         stringsAsFactors = F)
input_2021 <- data.frame(Yearweek = as.character(202101:202152),
                         stringsAsFactors = F)
input_2022 <- data.frame(Yearweek = as.character(202201:202252),
                         stringsAsFactors = F)

input_df <- rbind(input_2020, input_2021, input_2022) %>%
  mutate(YearWeek = as.numeric(Yearweek)) %>%
  dplyr::select(YearWeek)


## Reading file including all weeks for the modelling period (some data sources might not contain all weeks)
setwd("<<Location of raw data file recieved from FB>>")

## Reading each file
FB_input_2020_1 <- read.csv('Facebook 2020_1.csv')
FB_input_2020_2 <- read.csv('Facebook 2020_2.csv')
FB_input_2021<- read.csv('G:\\NEPA\\Företag (kunder, prospekts)\\Arlanda Express\\04 Projects\\MMM 2022\\04 Data\\01 Raw data\\01 Facebook\\AeX_2021.csv')
FB_input_2022<- read.csv('G:\\NEPA\\Företag (kunder, prospekts)\\Arlanda Express\\04 Projects\\MMM 2022\\04 Data\\01 Raw data\\01 Facebook\\AeX_2022.csv')
FB_rawdata_input   <- rbind(FB_input_2020_1, FB_input_2020_2, FB_input_2021,FB_input_2022)


# ------------------------------------------------------------------------------------------------ #
#### FACEBOOK: Distribute spend on Impressions & divide spends into Video and Static media  ####
# ------------------------------------------------------------------------------------------------ #


## Creating Year, Week and YearWeek columns
df_SoMe<-input_Spend %>%
  select(`Account.Name`,`Date`,`Video`,`Impressions`,`Spend`)%>%
  mutate(Year = year(Date),
         Week = isoweek(Date),
         Year = as.character(Year),
         Week = as.character(Week),
         Week = ifelse(nchar(Week) == 1, paste0("0", Week), Week),
         YearWeek = paste(Year, Week, sep = "")) %>%
  
## Aggregating multiple entries   
  group_by(YearWeek , Year, Video) %>% 
  summarise(Spend=sum(as.numeric(Spend),na.rm = T),
            Impressions=sum(as.numeric(Impressions),na.rm=T)) %>%
  
## Calculating Yearly values   
  group_by(Year)%>%
  mutate(Yearly_Spend=sum(Spend,na.rm = T),
         Yearly_impressions=sum(Impressions,na.rm=T))%>%
  ungroup() %>%
  
## Calcualting Share of Impressioons followd by Distributed Spends  
  mutate(share_imp=Impressions/Yearly_impressions) %>%
  mutate(Distributed_spend=share_imp*Yearly_Spend) %>%
  mutate(Video = ifelse(Video == "False", "Static", "Video")) %>% 
  select(YearWeek,Video, Distributed_spend) %>%
  spread(key = "Video", value = "Distributed_spend") %>%
  mutate(YearWeek = as.numeric(YearWeek)) %>%
  replace(.,is.na(.),0)


# ggplot(df_SoMe, aes(YearWeek, group = 1)) +
#   geom_line(aes(y = Static + Video, colour = "Original spend")) +
#   geom_line(aes(y = Static, colour = "Static Spend")) +
#   geom_line(aes(y = Video, colour = "Video Spend")) +
#   ggtitle(paste("Difference between original spend and distributed spend")) +
#   ylab("Distributed_spend") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


df_SoMe_Final = input_df %>%
  left_join(df_SoMe) %>%
  replace(.,is.na(.),0)


# Saving data on the server
setwd("<<Location where you would like to save the structured file>>")
write_csv(df_SoMe_Final, "Facebook.csv")



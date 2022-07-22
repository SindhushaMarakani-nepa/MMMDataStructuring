## Packages to import
library(readxl)
library(dplyr)
library(tidyverse)
library(ISOweek)
library(lubridate)
library(stringr)

### Importing Raw data ###

# For Excel files
MediaDataInput <- read_excel("Filename.xlsx", 
                           sheet = "Sheet_name",
                           range = "I3:K72",                                                # range of columns you would like to import
                           col_types = c("numeric", "text", "numeric","etc","etc")) %>%     # column tyes of the columns being imported 
  mutate(Year = as.numeric(substr(week, start = 3, stop = 6)),                              # Calculating Week, Year and YearWeek values 
         Week = substr(week, start = 1, stop = 2),
         YearWeek = as.numeric(paste0(Year,Week))) 

# For csv files
MediaDataInput <- read.csv2("Filenme.csv", sep = ",")

# When data is provided per campaign

Media_Investment <- MediaDataInput %>%
  mutate(nrdays = as.numeric((as.Date(`End date`)- as.Date(`Start date`))+1),
    id = 1:n()) 

Media_Investment <- Media_Investment[rep(seq(nrow(Media_Investment)), Media_Investment$nrdays) ,]  # Create multiple rows for each day the campaign runs

Media_Investment <- Media_Investment %>%
  mutate(Investment_perday = `Net cost` / nrdays) %>%
  group_by(id) %>%
  mutate(date_adjusted = `Insert date` + days(row_number()-1)) %>%     # Calculating new dates for the multiple rows created
  ungroup() %>%
  mutate(
    Year = year(date_adjusted),
    Week_temp = week(date_adjusted),
    YearWeek = ifelse(nchar(Week_temp) == 1, paste0(year,"0", Week_temp), paste0(year, Week_temp)),
    # Use if working with monthly data
    # Month_temp = month(date_adjusted),
    # YearMonth = ifelse(nchar(Month_temp) == 1, paste0(year,"0", Month_temp), paste0(year, Month_temp))
    ) 


Media_Investment <- Media_Investment %>%
  # Weekly investment 
  group_by(YearWeek, Channel, `Sub Channel`, Category) %>%
  mutate(Weekly_Investment = sum(Investment_perday, na.rm = TRUE)) %>% 
  ungroup() %>%
  # Monthly investment (needed)
  # group_by(YearMonth, Channel, `Sub Channel`, Category) %>%
  # mutate(Monthly_Investment = sum(Investment_perday, na.rm = TRUE)) %>% 
  # ungroup()


Media_Investment <- Media_Investment %>%
  select( #Choose all required columns
    ) %>% 
  unique()


# When data is provided per week
Media_Investment <- Media_Investment %>% 
  filter(Medietyp != "Mobil") %>%    # If you would lik to filter out the media types required
  group_by(Medietyp, Media, YearWeek) %>% 
  summarise(Spend = sum(Spend, na.rm=T)) %>% 
  ungroup()


# Distributing on TRPs / Impressions
# Either repeat the following lines of code for each media type or include Mediatype in groupby command to calculate the distributed spend for each media
Distributed_Media_Investment <- Media_Investment %>%    
  group_by(year_adjusted) %>%
  mutate(Yearly_Spend=sum(Spend,na.rm=T),Yearly_TRP =sum(as.numeric(TRP30),na.rm=T)) %>%
  ungroup() %>% 
  group_by(YearWeek)%>%
  mutate(Share_TRP30=as.numeric(TRP30)/Yearly_TRP)%>%
  mutate(Distributed_Spend= Share_TRP30*Yearly_Spend ) %>%
  summarise(Media_total = sum(Distributed_Spend)) %>%
  ungroup()

# Creating a pivot tables to get distributed spends for each mediatype  
category_pivot <- Distributed_Media_Investment %>%
  select(YearWeek, Category, `Sub Channel`, Distributed_Spend) %>% 
  spread(key=Category, value=(Distributed_Spend))%>%
  replace(.,is.na(.),0) %>%
  select(-`Sub Channel`)

# Creating Outpts
output_df_weekly <- data.frame(YearWeek = as.character(201601:201652), stringsAsFactors = F) %>%  # Create dataframe encompassing your modeling period
  bind_rows(data.frame(YearWeek = as.character(201701:201753), stringsAsFactors = F)) %>%
  bind_rows(data.frame(YearWeek = as.character(201801:201853), stringsAsFactors = F)) %>%
  bind_rows(data.frame(YearWeek = as.character(201901:201953), stringsAsFactors = F)) %>%
  bind_rows(data.frame(YearWeek = as.character(202001:202053), stringsAsFactors = F)) %>%
  bind_rows(data.frame(YearWeek = as.character(202101:202153), stringsAsFactors = F)) 

output_media_data <- left_join(output_df_weekly,category_pivot)

write.csv(output_media_data, "Media_data.csv")

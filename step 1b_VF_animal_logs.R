
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)
library(readxl)
library(anytime)


############################################################################################
############       bring in raw data                          ##############################
############################################################################################
path_step1 <- "W:/VF/Optimising_VF/Mildura/data_prep/"
raw_data <- "W:/VF/Optimising_VF/raw_data/Mildura/"
################################################################################
######            Day 1                         ################################

animal_GPS_data_1_Black <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Black sheep",
             skip = 16)

animal_GPS_data_1_Blue <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Blue sheep",
             skip = 4)

animal_GPS_data_1_Green <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Green sheep",
             skip = 5)


animal_GPS_data_1_Orange <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Orange sheep",
             skip = 5)

animal_GPS_data_1_Red <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Red sheep",
             skip = 5)

animal_GPS_data_1_Yellow <-
  read_excel(paste0(raw_data, "GPS data/VF day 1.xlsx"),
             sheet = "Yellow sheep",
             skip = 5)

animal_GPS_data_1_Black <- animal_GPS_data_1_Black %>% mutate(sheep = "Black") %>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Blue <- animal_GPS_data_1_Blue %>% mutate(sheep = "Blue")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Green <- animal_GPS_data_1_Green %>% mutate(sheep = "Green")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Orange <- animal_GPS_data_1_Orange %>% mutate(sheep = "Orange")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Red <- animal_GPS_data_1_Red %>% mutate(sheep = "Red")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Yellow <- animal_GPS_data_1_Yellow %>% mutate(sheep = "Yellow")%>% dplyr::select (ID,lat,lon,time,sheep)



GPS_day1 <- rbind(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )
rm(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )


################################################################################
######            Day 2                         ################################

animal_GPS_data_1_Black <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Black sheep ",
             skip = 16)

animal_GPS_data_1_Blue <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Blue sheep",
             skip = 5)

animal_GPS_data_1_Green <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Green sheep",
             skip = 5)


animal_GPS_data_1_Orange <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Orange sheep",
             skip = 4)

animal_GPS_data_1_Red <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Red sheep",
             skip = 4)

animal_GPS_data_1_Yellow <-
  read_excel(paste0(raw_data, "GPS data/VF day 2.xlsx"),
             sheet = "Yellow sheep",
             skip = 5)

animal_GPS_data_1_Black <- animal_GPS_data_1_Black %>% mutate(sheep = "Black") %>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Blue <- animal_GPS_data_1_Blue %>% mutate(sheep = "Blue")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Green <- animal_GPS_data_1_Green %>% mutate(sheep = "Green")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Orange <- animal_GPS_data_1_Orange %>% mutate(sheep = "Orange")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Red <- animal_GPS_data_1_Red %>% mutate(sheep = "Red")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Yellow <- animal_GPS_data_1_Yellow %>% mutate(sheep = "Yellow")%>% dplyr::select (ID,lat,lon,time,sheep)


GPS_day2 <- rbind(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )
rm(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )



################################################################################
######            Day 3                         ################################

animal_GPS_data_1_Black <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Black sheep",
             skip = 15)

animal_GPS_data_1_Blue <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Blue sheep",
             skip = 5)

animal_GPS_data_1_Green <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Green sheep",
             skip = 5)


animal_GPS_data_1_Orange <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Orange sheep",
             skip = 5)

animal_GPS_data_1_Red <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Red sheep",
             skip = 5)

animal_GPS_data_1_Yellow <-
  read_excel(paste0(raw_data, "GPS data/VF day 3.xlsx"),
             sheet = "Yellow sheep",
             skip = 5)

animal_GPS_data_1_Black <- animal_GPS_data_1_Black %>% mutate(sheep = "Black") %>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Blue <- animal_GPS_data_1_Blue %>% mutate(sheep = "Blue")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Green <- animal_GPS_data_1_Green %>% mutate(sheep = "Green")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Orange <- animal_GPS_data_1_Orange %>% mutate(sheep = "Orange")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Red <- animal_GPS_data_1_Red %>% mutate(sheep = "Red")%>% dplyr::select (ID,lat,lon,time,sheep)
animal_GPS_data_1_Yellow <- animal_GPS_data_1_Yellow %>% mutate(sheep = "Yellow")%>% dplyr::select (ID,lat,lon,time,sheep)


GPS_day3 <- rbind(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )
rm(animal_GPS_data_1_Black, animal_GPS_data_1_Blue, animal_GPS_data_1_Green, animal_GPS_data_1_Orange, animal_GPS_data_1_Red,animal_GPS_data_1_Yellow )

###############################################################################
######       Merge Day 1, 2 and 3              ################################

GPS <- rbind(GPS_day1, GPS_day2,GPS_day3 )

str(GPS)


#format time and date clm from character to time
# GPS <-  GPS %>%
#   mutate(timeOfEvent = as.POSIXct( time, tz = "GMT", format = "%d/%m/%Y %H:%M")) ## older code will work with other date time formats

GPS <-  GPS %>%
  mutate(timeOfEvent = anytime( time))



GPS <- GPS %>% 
  mutate(GMT = ymd_hms(timeOfEvent, tz = "GMT"))

GPS <- GPS %>% 
  mutate(local_time = with_tz(GMT, tz = "Australia/Melbourne"))

## Add a clm for ID_jaxs
GPS <- GPS %>% 
  dplyr::mutate( ID_jaxs = row_number())


### what are the fences called in this dataset?
unique(GPS$fencesID) # This system does not have multiple fences
GPS <- GPS %>% 
  dplyr::mutate( fencesID = "Fence1")




## reorder the clms

names(GPS)
GPS <- GPS %>% 
  dplyr::select(ID_jaxs,ID:local_time)



GPS <- GPS %>% 
  mutate(date = as.Date(local_time, tz= "Australia/Melbourne"),
         DOY = yday(date))
unique(GPS$DOY)
unique(GPS$date)

#############################################################################################
####    Assign collar to sheep names #####
unique(GPS$sheep) # was deviceName in new systems

GPS <- GPS %>% 
  mutate(Sheep_ID = case_when(
    sheep == "Black"  ~ "1",
    sheep == "Blue"  ~ "2",
    sheep == "Green"  ~ "3",
    sheep == "Orange"  ~ "4",
    sheep == "Red"  ~ "5",
    sheep == "Yellow"  ~ "6",
    TRUE                      ~ "other"
    
    
  ))
#only keep the collar that sue said:)
GPS <- GPS %>%
  filter(Sheep_ID != "other")

## ok lets just remove the Nulls
GPS <- GPS %>% 
  filter(fencesID!= "NULL")

## reorder the clms
# GPS <- GPS %>% 
#   dplyr::select(ID_jaxs,Sheep_ID, deviceUIDHex:local_time)




str(GPS)

write.csv(GPS, paste0(
          path_step1, "temp_step1.csv"))




############################################################################################
############                  Turn into spatial data          ##############################
############################################################################################
str(GPS)

#turn into spatial data
## remove null values in coodinates
GPS <- GPS %>% 
  filter(!is.na(lon))

#turn into spatial data
GPS_sf <-
  st_as_sf(GPS,
           coords = c("lon", "lat"),
           crs = 4326,
           agr = "constant")

GPS_sf_trans <-
  st_transform(GPS_sf, crs = 28354)


rm(GPS_sf)


str(GPS_sf_trans)


# GPS_sf_trans <- GPS_sf_trans %>% 
#   mutate(date = as.Date(local_time))
############################################################################################
############                  bring in boundaries             ##############################
############################################################################################

Mildura_hard_fence_bound <- st_read("W:/VF/Optimising_VF/raw_data/Mildura/Block_Bound_and_Grid/Mildura_sheep_Bound_GDA.shp")  # this is the hard fences
Mildura_hard_fence_bound <-
  st_transform(Mildura_hard_fence_bound, crs = 28354)

Mildura_hard_fence_bound_buff <- st_read("W:/VF/Optimising_VF/raw_data/Mildura/Block_Bound_and_Grid/Mildura_sheep_Bound_GDA_buff10.shp")  # this is the 
Mildura_hard_fence_bound_buff <-
  st_transform(Mildura_hard_fence_bound_buff, crs = 28354)


VF_paddock <-   st_read("W:/VF/Optimising_VF/raw_data/Mildura/Block_Bound_and_Grid/Grazing_zone.shp")
VF_paddock <-  st_transform(VF_paddock, crs = 28354)




ggplot() +
  geom_sf(data = Mildura_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


#--- UP TO HERE ----##

################################################################################
#### filtering out GPS data based on times start and end of the trial per day 2017-05-02 07:44:00 to 2017-05-02 15:06:00
names(GPS_sf_trans)
unique(GPS_sf_trans$DOY)



day1 <- GPS_sf_trans %>% 
  filter( between(local_time, ymd_hms("2017-05-02 07:44:00", tz= "Australia/Melbourne"), 
           ymd_hms("2017-05-02 15:06:00", tz= "Australia/Melbourne")))

day2 <- GPS_sf_trans %>% 
  filter( between(local_time, ymd_hms("2017-05-03 07:48:00", tz= "Australia/Melbourne"), 
                  ymd_hms("2017-05-03 15:20:00", tz= "Australia/Melbourne")))

day3 <- GPS_sf_trans %>% 
  filter( between(local_time, ymd_hms("2017-05-04 07:51:00", tz= "Australia/Melbourne"), 
                  ymd_hms("2017-05-04 15:06:00", tz= "Australia/Melbourne")))


### merge the 3 days of trimmed data

GPS_sf_trans <- rbind(day1, day2, day3)
unique(GPS_sf_trans$DOY)
unique(GPS_sf_trans$date)


ggplot() +
  geom_sf(data = Mildura_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = GPS_sf_trans ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "")


 

### define a training period with new clm No training period animals were allowed to acclimatise in neighbouring paddock

# GPS_sf_trans <- GPS_sf_trans %>% 
#   mutate(training_period = case_when(
#     local_time <= ymd_hms("2022-10-17 13:10:00", tz= "Australia/Sydney")~ "training",
#     TRUE                      ~ "non_training"
#     
#   ))






rm(day1, day2, day3, GPS, GPS_day1, GPS_day2, GPS_day3)

########################################################################################



########################################################################################

### remove the water and other animals logs


animals_GPS_trim_time <- GPS_sf_trans

# -------------------------------------------------------------------------------------------------- ###


#I think this should be the end of step 1.


#Next steps boundaries trim location and time
## merge in other animal data
## look for weather data ? anything else??




########################################################################################################



output_path <- "W:/VF/Optimising_VF/Mildura/data_prep"  #animals_GPS_trim_time


############################################################################################################################
### format the aniaml log data so I output the clm with local time and keep time difference cals and have clm for x and y

## convert the geom clm into x and y clms
str(animals_GPS_trim_time)

coordinates <-as.data.frame( st_coordinates(animals_GPS_trim_time))
GPS_trim_time_df <- as.data.frame(animals_GPS_trim_time)

GPS_trim_time_df <- GPS_trim_time_df %>% 
  dplyr::select(-"geometry")


GPS_trim_time <-   cbind(GPS_trim_time_df,coordinates )
## ensure the date and time clms are outputting and outputting in the correct format.


GPS_trim_time$local_time <-   format(GPS_trim_time$local_time, usetz=TRUE)
GPS_trim_time$GMT        <-   format(GPS_trim_time$GMT, usetz=TRUE)
GPS_trim_time$start_fence <-  format(GPS_trim_time$start_fence, usetz=TRUE)
GPS_trim_time$end_fence    <- format(GPS_trim_time$end_fence, usetz=TRUE)
GPS_trim_time$start_trial    <- format(GPS_trim_time$start_trial, usetz=TRUE)

write.csv(GPS_trim_time, 
          paste0(output_path,"/Step1b_animals_GPS_trim_time.csv"), 
          row.names=FALSE)
#############################################################




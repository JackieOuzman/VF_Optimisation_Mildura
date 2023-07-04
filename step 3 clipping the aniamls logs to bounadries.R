## step 3 clipping the data - This has not been run yet

library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(DT)
library(sp)
#install.packages("sf")
library(sf)


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




############################################################################################



################################################################
### Clip to the VF hard fences  with 10 meter buffer   #########
################################################################



step1_2 <- read_csv("W:/VF/Optimising_VF/Mildura/data_prep/Step1b_animals_GPS_trim_time.csv")

#turn into spatial data
step1_2_sf <-   st_as_sf(step1_2,
                       coords = c("X", "Y"),
                       crs = 28354, 
                       agr = "constant")







#To the large block boundary with buffer
step1_2_sf_clip <-
  st_intersection(step1_2_sf, st_difference(Mildura_hard_fence_bound)) #this 'st_difference' function is supposed to remove the duplication


##--- up to here---###

### check

ggplot() +
  geom_sf(data = Mildura_hard_fence_bound, color = "black", fill = NA) +
  geom_sf(data = Mildura_hard_fence_bound_buff, color = "black", fill = NA) +
  geom_sf(data = VF_paddock, color = "black", fill = NA) +
  geom_sf(data = step1_2_sf_clip ,alpha = 0.03) +
  theme_bw()+
  facet_wrap(.~ date)+
  theme(legend.position = "none",
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
  labs(title = "clipped")

## convert the geom clm into x and y clms


coordinates <-as.data.frame( st_coordinates(step1_2_sf_clip))
step1_2_sf_clip_df <- as.data.frame(step1_2_sf_clip)

step1_2_sf_clip_df <- step1_2_sf_clip_df %>% 
  dplyr::select(-"geometry")


step1_2_sf_clip_df <-   cbind(step1_2_sf_clip_df,coordinates )



#W:/VF/Optimising_VF/Mildura/data_prep/


path_output_files <- "W:/VF/Optimising_VF/Mildura/data_prep/" 
path_output_files
write.csv(step1_2_sf_clip_df, 
          paste0(path_output_files,"/step3_clip.csv"), 
          row.names=FALSE)

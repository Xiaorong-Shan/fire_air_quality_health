## ====================================================================== ##
### This script will evaluate temporal and spatial correlation among CMAQ
### hyads and epa monitors for 2018 Campfire event in New York
### Period: Nov. 23rd 2018 to Nov. 28th 2018
### Noted: you should get the CMAQ result in grid value from 05 script first
## ====================================================================== ##
library( data.table)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)

## ====================================================================== ##
###  EPA monitor data with PM2.5 concentration
## ====================================================================== ##
p4s <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

epa_aqs.df <- read.csv("C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/epa_aqs/daily_88101_2018.csv")

#etract the dates from data frame
start_date <- as.Date("2018-11-23")
end_date <- as.Date("2018-11-28")
epa_aqs_2018.df <- epa_aqs.df[epa_aqs.df$Date.Local >= start_date & epa_aqs.df$Date.Local<= end_date, ]

#eliminate to only New York
epa_aqs_2018_ny.df <- epa_aqs_2018.df[epa_aqs_2018.df$State.Name == "New York", ] 

#convert it to sf
epa_aqs_2018_ny.sf <- st_as_sf(epa_aqs_2018_ny.df, coords = c("Longitude", "Latitude"), crs = "WGS84")

states <- USAboundaries::us_states()

ny_state <- states[states$name == "New York", ]

epa_aqs_2018_ny.sf <- st_transform(epa_aqs_2018_ny.sf, st_crs(ny_state))

ggplot() +
  # Base layer: New York state
  geom_sf(data = ny_state,
          aes( geometry = geometry),
          color = 'grey50',
          inherit.aes = FALSE)+
  # Overlay the epa_aqs_2018_ny.sf data
  geom_sf(data = epa_aqs_2018_ny.sf, aes(color =Arithmetic.Mean )) +
  
  # Use a color scale from the viridis package for PM2.5 concentration
  scale_color_viridis(limits = c( 0, 10), 
                      breaks = c( 0, 5, 10),
                      labels = c( '0', '5', '10'), 
                      oob = scales::squish) +
  facet_wrap( . ~ Date.Local, ncol = 3) +
  
  theme_minimal() +
  labs(title = "PM2.5 Concentration in New York (2018)",
       color = expression("PM2.5 ["*mu*g/m^3*"]"),
       x = NULL,
       y = NULL) +
  theme(axis.title = element_text( size = 8),
        axis.text = element_blank(),
        strip.text = element_text( size = 8),
        legend.position = "bottom")


## ====================================================================== ##
### CMAQ data with PM2.5 concentration
## ====================================================================== ##
cmaq.df <- read.csv("C:/Users/xshan2/OneDrive - George Mason University - O365 Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/cmaq_processed/cmaq_county_ny.csv")

cmaq_2018.df <- cmaq.df[cmaq.df$date >= start_date & cmaq.df$date <= end_date, ]






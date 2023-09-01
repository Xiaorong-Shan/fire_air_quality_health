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
### Purple Air data with PM2.5 concentration
## ====================================================================== ##

samplefile1<-read.csv("/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/01_2019fall_Campfire/Daniel_single_point/EPA_disperseR_covariance/ad_viz_plotval_data.csv")

purple_air_data <- data.table(matrix(nrow = nrow(samplefile1), ncol = 5))
purple_air_data <- setnames(purple_air_data,c("Longitude","Latitude","uID","date","pm25"))
purple_air_data$Longitude <- samplefile1$SITE_LONGITUDE
purple_air_data$Latitude <- samplefile1$SITE_LATITUDE
purple_air_data$uID <- as.character(samplefile1$Site.ID)
purple_air_data$date<-as.Date(samplefile1$Date,format = "%Y-%m-%d")
purple_air_data$pm25<-samplefile1$Daily.Mean.PM2.5.Concentration

# select purple_air data within the campfire range.
date_range <- seq(from=as.Date("2018-11-23"),to=as.Date("2018-11-28"),by="day") #set up the list
purple_air_data <- purple_air_data[purple_air_data$date %in% date_range,] #subset purple_air data within campfire period
purple_air_data.sf <- st_as_sf(purple_air_data, 
                               coords = c( 'Longitude', 'Latitude'),
                               crs = 'WGS84')

## ====================================================================== ##
### CMAQ data Calculate Error Metrics between withfire and nofire
## ====================================================================== ##

cmaq_pm25_total <- st_read("/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/CMAQ/shp_cmaq/cmaq_pm25_total.shp")

cmaq_pm25_total.sf <- st_as_sf(cmaq_pm25_total)
cmaq_pm25_total.sf <- st_transform(cmaq_pm25_total.sf, st_crs(ny_state))



## ====================================================================== ##
### Error Metrics between purple air and CMAQ
## ====================================================================== ##
joined_purple_air_hyads <- st_join(purple_air_data.sf, cmaq_pm25_total.sf, left = TRUE)%>%
  subset(date.x == date.y)

#Renaming the date column if needed
names(joined_purple_air_hyads)[names(joined_purple_air_hyads) == "date.x"] <- "date"

joined_purple_air_hyads$date.y <- NULL

# Calculate errors for each point (replace with your actual column names)
joined_purple_air_hyads <- joined_purple_air_hyads %>%
  mutate(error_withfire = pm25 - fire_pm25)

# Calculate error metrics with fire
MAE <- mean(abs(joined_purple_air_hyads$error_withfire), na.rm = TRUE)
MSE <- mean(joined_purple_air_hyads$error_withfire^2, na.rm = TRUE)
RMSE <- sqrt(MSE)
MBD <- mean(joined_purple_air_hyads$error_withfire, na.rm = TRUE)

# Print the metrics
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("Mean Bias Deviation (MBD):", MBD, "\n")

# Calculate error metrics withoutfire
joined_purple_air_hyads <- joined_purple_air_hyads %>%
  mutate(error_nofire = pm25 - nofire_pm25)

MAE <- mean(abs(joined_purple_air_hyads$error_nofire), na.rm = TRUE)
MSE <- mean(joined_purple_air_hyads$error_nofire^2, na.rm = TRUE)
RMSE <- sqrt(MSE)
MBD <- mean(joined_purple_air_hyads$error_nofire, na.rm = TRUE)

# Print the metrics
cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("Mean Bias Deviation (MBD):", MBD, "\n")


## ====================================================================== ##
### This script reads CMAQ reslut in grid (12km)
### fire, nofire, difference
### Period: Nov. 23rd 2018 to Nov. 28th 2018
## ====================================================================== ##

library(ncdf4)
library(data.table)
library(dplyr)
library(raster)
library(USAboundaries)
library(sf)
library(ggplot2)
library(viridis)

cmaq_loc <- '/home/xshan2/HAQ_LAB/xshan2/R_Code/Campfire/Wilkins_CMAQ_output/CMAQ/2018'


# list all the CMAQ files
files_all <- list.files( cmaq_loc,
                         # pattern = 'daily8hrmax.*.ncf',
                         full.names = TRUE)

# create crs from file information
f_in <- nc_open(files_all[1])
xorig <- ncatt_get(f_in, varid = 0, 'XORIG')$value
yorig <- ncatt_get(f_in, varid = 0, 'YORIG')$value
xcent <- ncatt_get(f_in, varid = 0, 'XCENT')$value
ycent <- ncatt_get(f_in, varid = 0, 'YCENT')$value
ncols <- ncatt_get(f_in, varid = 0, 'NCOLS')$value
nrows <- ncatt_get(f_in, varid = 0, 'NROWS')$value
xcell <- ncatt_get(f_in, varid = 0, 'XCELL')$value
ycell <- ncatt_get(f_in, varid = 0, 'XCELL')$value
p_alp <- ncatt_get(f_in, varid = 0, 'P_ALP')$value
p_bet <- ncatt_get(f_in, varid = 0, 'P_BET')$value

# create p4s
#https://forum.cmascenter.org/t/equates-grid-coordinates/3018/3
# p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"
p4s <- paste( '+proj=lcc',
              paste0( 'lat_1=', p_alp),
              paste0( 'lat_2=', p_bet),
              paste0( 'lat_0=', ycent),
              paste0( 'lon_0=', xcent),
              'a=6370000 +b=6370000',
              sep = ' +')

# Describe grids from grid description
#define lat & lon in meters
lon <- seq( from = xorig, by = xcell, length.out = ncols)
lat <- seq( from = yorig, by = ycell, length.out = nrows)

#create an empty raster for grids with crs descriotion
latlon_raster.r <- expand.grid( lon = lon,
                                lat = lat,
                                values = NA) %>%
  as.data.table %>%
  rasterFromXYZ( 
    crs = p4s) %>% brick( nl = 365)

## ====================================================================== ##
### apply the reader function
## ====================================================================== ##
read_fn <- 
  function( f, 
            raster_fill = latlon_raster.r){
    print( f)
    
    
    # isolate the name
    year_in <- gsub( '.*daily8hrmax.|.*dailyavgs.|.12US2.baseline.*', '', f) 
    name_in <- gsub( paste0( cmaq_loc, '/|.20.*'), '', f) 
    fire_in <- gsub( '.*baseline|.ncf', 'fire', f) 
    message( name_in, year_in, fire_in)
    
    # read in the raster
    raster_in <- stack( f) #%>% rotate()
    values( raster_fill) <- values( raster_in)
    
   
    raster_values <- getValues(raster_fill)
    
    # create data table out output
    # extract values from the raster directly
    raster_values <- as.data.frame(raster_fill, xy=TRUE)
    
    # create data table out output
    data_out <- data.table(year = year_in,
                           name = name_in,
                           fire = fire_in,
                           raster_values) %>%
      na.omit()
    
    # return the dimentions
    return(data_out)
    
  }

cmaq_pm25_ny <- 
    lapply( files_all,
          read_fn,
          raster_fill = latlon_raster.r) %>%
  rbindlist( fill = TRUE)

## ====================================================================== ##
### make the fire raster
## ====================================================================== ##
cmaq_pm25_ny_fire <- cmaq_pm25_ny[cmaq_pm25_ny$fire == "firefire", ]
cmaq_pm25_fire.dt <- cmaq_pm25_ny_fire[, -c("year", "name", "fire")]

#resolution: 12km
cmaq_pm25_fire.r <- rasterFromXYZ(cmaq_pm25_fire.dt,
                                 crs=p4s)

# create sf polygon object
cmaq_pm25_fire.sp <- rasterToPolygons( cmaq_pm25_fire.r)
cmaq_pm25_fire.sf <- st_as_sf( cmaq_pm25_fire.sp)

# melt back to long format
cmaq_pm25_fire.m <-
  as.data.table( cmaq_pm25_fire.sf) %>%
  melt( id.vars = 'geometry',
        variable.name = 'date',
        value.name = 'fire_pm25')

# Convert date_numeric column back to date format
cmaq_pm25_fire.m[, date := as.Date( as.numeric( gsub( '^X', '', date)) - 1,    
                                 origin = as.Date( paste0( 2018, "-01-01")))]


#etract the dates (camfire pick period from data frame)
start_date <- as.Date("2018-11-23")
end_date <- as.Date("2018-11-28")
cmaq_pm25_fire_date.m <- cmaq_pm25_fire.m[cmaq_pm25_fire.m$date >= start_date & cmaq_pm25_fire.m$date <= end_date, ]

## ====================================================================== ##
### make the no fire raster
## ====================================================================== ##
  cmaq_pm25_ny_nofire <- cmaq_pm25_ny[cmaq_pm25_ny$fire == "fire_0firefire", ]
  cmaq_pm25_nofire.dt <- cmaq_pm25_ny_nofire[, -c("year", "name", "fire")]
  
  #resolution: 12km
  cmaq_pm25_nofire.r <- rasterFromXYZ(cmaq_pm25_nofire.dt,
                                    crs=p4s)
  
  # create sf polygon object
  cmaq_pm25_nofire.sp <- rasterToPolygons( cmaq_pm25_nofire.r)
  cmaq_pm25_nofire.sf <- st_as_sf( cmaq_pm25_nofire.sp)
  
  # melt back to long format
  cmaq_pm25_nofire.m <-
    as.data.table( cmaq_pm25_nofire.sf) %>%
    melt( id.vars = 'geometry',
          variable.name = 'date',
          value.name = 'nofire_pm25')
  
  # Convert date_numeric column back to date format
  cmaq_pm25_nofire.m[, date := as.Date( as.numeric( gsub( '^X', '', date)) - 1,    
                                      origin = as.Date( paste0( 2018, "-01-01")))]
  
  
  #etract the dates (camfire pick period from data frame)
  start_date <- as.Date("2018-11-23")
  end_date <- as.Date("2018-11-28")
  cmaq_pm25_nofire_date.m <- cmaq_pm25_nofire.m[cmaq_pm25_nofire.m$date >= start_date & cmaq_pm25_nofire.m$date <= end_date, ]
  
#################################################
#merge the fire and no fire into one table, calculate the difference between fire and nofire
cmaq_pm25_total <- cmaq_pm25_fire_date.m
cmaq_pm25_total$nofire_pm25 <- cmaq_pm25_nofire_date.m$nofire_pm25
cmaq_pm25_total$fire_diff <- cmaq_pm25_total$fire_pm25 - cmaq_pm25_total$nofire_pm25

summary(cmaq_pm25_total)

## ====================================================================== ##
### creat the plot
## ====================================================================== ##
#################################################
# create mask over NY state
ny_states <- USAboundaries::us_states( states = 'NY') %>%  st_transform(crs = st_crs(cmaq_pm25_fire.sf))
ny_bbox <- st_bbox(ny_states)

#################################################
# cmaq with fire
cmaq_fire <- 
ggplot( ) +
  # add the cmaq withfire grid
  geom_sf( data = cmaq_pm25_total,
           aes( fill = fire_pm25, geometry = geometry),
           alpha = .75, color = NA) +
  geom_sf( data = ny_states,
           aes( geometry = geometry),
           color = 'black',
           inherit.aes = FALSE, fill=NA) +
  # change the fill & color scale
  scale_fill_gradient(name = expression("PM2.5 ["*mu*g/m^3*"]"),
                      low = "blue",
                      high = "red",
                      limits = c(0, 6),
                      breaks = c(0, 3, 6),
                      labels = c('0', '3', '6'),
                      oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  facet_wrap( . ~ date, ncol = 3) +
  # set boundaries over NY
  #coord_sf( xlim = c( -79.76212, -71.85621), ylim = c( 40.50244, 45.01468)) +
  coord_sf( xlim = c( 1404152.8, 2070532.4), ylim = c( 293577.8, 795892.4)) +
  # set thematic elements
  theme_minimal() +
  labs(title = "CMAQ PM2.5 Concentration (with fire) in New York (2018)",
       x = NULL,
       y = NULL) +
  theme(axis.title = element_text( size = 12),
        axis.text = element_blank(),
        strip.text = element_text( size = 12),
        legend.position = "bottom")


ggsave( '/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/figures/cmaq_fire.png', cmaq_fire,
        width = 14, height = 5, scale = 1.2)
#################################################
# cmaq without fire
cmaq_nofire <-
  ggplot( ) +
  # add the cmaq withfire grid
  geom_sf( data = cmaq_pm25_total,
           aes( fill = nofire_pm25, geometry = geometry),
           alpha = .75, color = NA) +
  geom_sf( data = ny_states,
           aes( geometry = geometry),
           color = 'black',
           inherit.aes = FALSE, fill=NA) +
  # change the fill & color scale
  scale_fill_gradient(name = expression("PM2.5 ["*mu*g/m^3*"]"),
                      low = "blue",
                      high = "red",
                      limits = c(0, 6),
                      breaks = c(0, 3, 6),
                      labels = c('0', '3', '6'),
                      oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  facet_wrap( . ~ date, ncol = 3) +
  # set boundaries over NY
  #coord_sf( xlim = c( -79.76212, -71.85621), ylim = c( 40.50244, 45.01468)) +
  coord_sf( xlim = c( 1404152.8, 2070532.4), ylim = c( 293577.8, 795892.4)) +
  # set thematic elements
  theme_minimal() +
  labs(title = "CMAQ PM2.5 Concentration (without fire) in New York (2018)",
       x = NULL,
       y = NULL) +
  theme(axis.title = element_text( size = 12),
        axis.text = element_blank(),
        strip.text = element_text( size = 12),
        legend.position = "bottom")

ggsave( '/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/figures/cmaq_nofire.png', cmaq_nofire,
        width = 14, height = 5, scale = 1.2)

#################################################
# cmaq difference between fire & no fire
cmaq_fire_diff <-
ggplot( ) +
  # add the cmaq nofire grid
  geom_sf( data = cmaq_pm25_total,
           aes( fill = fire_diff, geometry = geometry),
           alpha = .75, color = NA) +
  geom_sf( data = ny_states,
           aes( geometry = geometry),
           color = 'black',
           inherit.aes = FALSE, fill=NA) +
  # change the fill & color scale
  scale_fill_gradient(name = expression("PM2.5 ["*mu*g/m^3*"]"),
                      low = "blue",
                      high = "red",
                      limits = c( 0, 0.5), 
                      breaks = c( 0, 0.25, 0.5),
                      labels = c( '0', '0.25', '0.5'),
                      oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  facet_wrap( . ~ date, ncol = 3) +
  # set boundaries over NY
  #coord_sf( xlim = c( -79.76212, -71.85621), ylim = c( 40.50244, 45.01468)) +
  coord_sf( xlim = c( 1404152.8, 2070532.4), ylim = c( 293577.8, 795892.4)) +
  # set thematic elements
  theme_minimal() +
  labs(title = "CMAQ PM2.5 Concentration (difference between fire & no fire) in New York (2018)",
       x = NULL,
       y = NULL) +
  theme(axis.title = element_text( size = 12),
        axis.text = element_blank(),
        strip.text = element_text( size = 12),
        legend.position = "bottom")

ggsave( '/Users/xshan2/Library/CloudStorage/OneDrive-GeorgeMasonUniversity-O365Production/GMU_PhD/01_Research/01_2019fall_Campfire/Wilkins_CMAQ_output/figures/cmaq_fire_diff.png', cmaq_fire_diff,
        width = 14, height = 5, scale = 1.2)

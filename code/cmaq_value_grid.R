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
# create mask over NY state
ny_bounds <- USAboundaries::us_states( states = 'NY')

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
### make the fire and no fire raster
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

cmaq_pm25_fire.sf <- st_as_sf(cmaq_pm25_fire_date.m)

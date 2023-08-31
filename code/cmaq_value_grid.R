library(ncdf4)
library(data.table)

cmaq_loc <- '/home/xshan2/HAQ_LAB/xshan2/R_Code/Campfire/Wilkins_CMAQ_output/CMAQ/2018'


#create an empty raster for grids with crs descriotion
latlon_raster.r <- expand.grid( lon = lon,
                                lat = lat,
                                values = NA) %>%
  as.data.table %>%
  rasterFromXYZ( 
    crs = p4s) %>% brick( nl = 365)

ny_bounds <- USAboundaries::us_states( states = 'NY')

# Transform the NY bounds to the CRS of the raster if they are different
if(st_crs(ny_bounds) != crs(latlon_raster.r)) {
  ny_bounds <- st_transform(ny_bounds, crs(latlon_raster.r))
}

# Convert the sf object to a raster mask
ny_mask <- rasterize(ny_bounds, latlon_raster.r)

# Mask the raster to New York State boundaries
masked_raster <- mask(latlon_raster.r, ny_mask)


read_fn <- 
  function( f, 
            raster_fill = latlon_raster.r,
            ny_bounds){
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
  rbindlist( fill = TRUE) %>%
  melt(id.vars = c('year', 'name', 'fire', 'x', 'y')) %>%
  dcast.data.table(year + variable + name + x + y ~ fire, value.var = 'value')

# set names
setnames( cmaq_pm25_ny, c( 'firefire', 'fire_0firefire'), c( 'withfire', 'nofire'))

# calculate differences from fires
cmaq_pm25_ny[, fire_diff := withfire - nofire]

# create date column
cmaq_pm25_ny[, date := as.Date( as.numeric( gsub( '^X', '', variable)) - 1,    
                                 origin = as.Date( paste0( year, "-01-01")))]

#etract the dates (camfire pick period from data frame)
start_date <- as.Date("2018-11-23")
end_date <- as.Date("2018-11-28")
epa_aqs_2018.df <- epa_aqs.df[epa_aqs.df$Date.Local >= start_date & epa_aqs.df$Date.Local<= end_date, ]

cmaq_pm25_campfire <- cmaq_pm25_ny[cmaq_pm25_ny$date >= start_date & cmaq_pm25_ny$date <= end_date, ]
cmaq_campfire <- cmaq_pm25_campfire[, .(x, y, nofire, withfire, fire_diff, date)]

cmaq_campfire[, date_numeric := as.integer(as.Date(date))]

cmaq_campfire.n <- cmaq_campfire[, date := NULL]

cmaq_campfire.r <- rasterFromXYZ(cmaq_campfire.n,
                               crs=p4s)

cmaq_campfire.r[is.na( cmaq_campfire.r)] <- 0

# create sf polygon object
cmaq_campfire.sp <- rasterToPolygons( cmaq_campfire.r)
cmaq_campfire.sf <- st_as_sf( cmaq_campfire.sp)

# Convert date_numeric column back to date format
cmaq_campfire.sf$date <- as.Date(cmaq_campfire.sf$date_numeric, origin = "1970-01-01")

cmaq_campfire.sf$date_numeric <- NULL

####################################
##try another way
####################################
cmaq_pm25_fire <- cmaq_pm25_test[cmaq_pm25_test$fire == "firefire", ]
cmaq_pm25_fire.n <- cmaq_pm25_fire[, -c("year", "name", "fire")]

#resolution: 12km
cmaq_pm25_fire.r <- rasterFromXYZ(cmaq_pm25_fire.n,
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

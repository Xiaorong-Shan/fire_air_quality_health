library( data.table)
library( raster)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)
library( ncdf4)


## ====================================================================== ##
### function - cmaq data and average over given areas
# aggregate_to can be polygons to area weight over or points
## ====================================================================== ##
read_fn <- 
  function( f, 
            raster_fill = latlon_raster.r,
            aggregate_to = st_centroid( ny_counties)){
    print( f)
    
    
    # isolate the name
    year_in <- gsub( '.*daily8hrmax.|.*dailyavgs.|.12US2.baseline.*', '', f) 
    name_in <- gsub( paste0( cmaq_loc, '/|.20.*'), '', f) 
    fire_in <- gsub( '.*baseline|.ncf', 'fire', f) 
    message( name_in, year_in, fire_in)
    
    # read in the raster
    raster_in <- stack( f) #%>% rotate()
    values( raster_fill) <- values( raster_in)
    
    # extract values from the raster
    aggregate_to <- st_transform( aggregate_to, p4s)
    raster_extract <- 
      raster::extract(
        raster_fill, 
        aggregate_to,
        fun = mean,
        na.rm= TRUE,
        weights = TRUE,
        exact = FALSE
      )
    
    # create data table out output
    data_out <- 
      data.table( year = year_in,
                  name = name_in,
                  fire = fire_in,
                  data.table( aggregate_to),
                  raster_extract) %>%
      na.omit()
    data_out[, geometry := NULL]
    
    # return the dimentions
    return( data_out)
  }

## ====================================================================== ##
## function - read daily AQS files
## ====================================================================== ##
AQS_dataset <- 
  function( pollutant.codes = c( 88101,  44201),
            species.data_dir = 'data/epa_aqs',
            states = state.name){
    aqs.files <- 
      list.files( species.data_dir,
                             pattern = paste0( 'daily_', 
                                               pollutant.codes, collapse = '|'),
                                              full.names = TRUE)
    aqs.in <- rbindlist( lapply( aqs.files, fread))
    
    # subset only to states called
    aqs.p <- aqs.in[ `State Name` %in% states]
    
    # read in monitor information
    # aqs.mons <- fread( '~/Dropbox/Harvard/Manuscripts/MobileBiasMS/data/aqs_observations/aqs_sites.csv')
    
    # merge site info and obs
    # merge.names <- names( aqs.in)[which( names( aqs.in) %in% names( aqs.mons))]
    # aqs.in.info <- merge( aqs.in, aqs.mons, by = merge.names, all.x = TRUE)
    
    # subset to input parameter.codes
    # aqs.p <- aqs.in.info[grepl( paste0( pollutant.codes, collapse = '|'), `Parameter Code`),]
    
    # convert to spatial object
    crs.wgs84 <-  sf::st_crs( "+proj=longlat +datum=WGS84 +no_defs")
    crs.nad83 <-  sf::st_crs( "+proj=longlat +datum=NAD83 +no_defs")
    crs.use   <-  sf::st_crs( "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m")
    aqs.wgs84.sf <- sf::st_as_sf( aqs.p[ Datum == 'WGS84'], coords = c( 'Longitude', 'Latitude'), crs = crs.wgs84)
    aqs.nad83.sf <- sf::st_as_sf( aqs.p[ Datum == 'NAD83'], coords = c( 'Longitude', 'Latitude'), crs = crs.nad83)
    aqs.wgs84.sf <- sf::st_transform( aqs.wgs84.sf, crs.use)
    aqs.nad83.sf <- sf::st_transform( aqs.nad83.sf, crs.use)
    aqs.sf <- data.table( rbind( aqs.wgs84.sf, aqs.nad83.sf))
    
    # unique measurement ID
    aqs.sf$mID <- paste( aqs.sf$`State Code`, aqs.sf$`County Code`, 
                         aqs.sf$`Site Num`, aqs.sf$`Parameter Code`, sep = '.')
    aqs.sf$ID <- paste0( as.integer( aqs.sf$`State Code`), 
                         as.integer( aqs.sf$`County Code`), 
                         as.integer( aqs.sf$`Site Num`), 
                         as.integer( aqs.sf$`Parameter Code`))
    
    # AQS monitor location setting
    # aqs.sf[ is.na( `Location Setting`) | `Location Setting` == '', `Location Setting` := 'MISSING']
    
    out <- aqs.sf
    # out <- sf::st_as_sf( aqs.sf)
    return( out)
  }


## ====================================================================== ##
## get the PM data
# AQS daily data downloaded 6/4/2023: https://aqs.epa.gov/aqsweb/airdata/download_files.html
## ====================================================================== ##
# read all data over NY
AQS_PM_O3 <- AQS_dataset( states = 'New York')

# limit to just unique sites for extraction from CMAQ
AQS_locs <- unique( AQS_PM_O3, by = c( 'mID', 'ID', 'POC')) %>%
  st_as_sf()


## ====================================================================== ##
### import information about cmaq files
## ====================================================================== ##
# cmaq data location
cmaq_loc <- 'data/cmaq/CMAQ/CMAQ'

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
# https://github.com/cestastanford/historical-us-city-populations
ny_bounds <- USAboundaries::us_states( states = 'NY')
ny_cities <- USAboundaries::us_cities( states = 'NY')
ny_counties <- USAboundaries::us_counties( states = 'NY')
nyc_bounds <- ny_cities[ny_cities$city == 'New York City',]

# read in the files and aggregate to state
# missing ozone 2015 baseline
cmaq_state_ny <- 
  lapply( files_all,
          read_fn,
          raster_fill = latlon_raster.r,
          aggregate_to = ny_bounds[, c( 'geoid', 'state_name')])  %>%
  rbindlist( fill = TRUE) %>%
  melt( id.vars = c( 'year', 'name', 'fire', 'geoid', 'state_name')) %>%
  dcast.data.table( year + variable + name + geoid + state_name ~ fire,
                    value.var = 'value') 

# read in the files and aggregate to county
cmaq_county_ny <- 
  lapply( files_all,
          read_fn,
          raster_fill = latlon_raster.r,
          aggregate_to = ny_counties[, c( 'geoid', 'state_name')])  %>%
  rbindlist( fill = TRUE) %>%
  melt( id.vars = c( 'year', 'name', 'fire', 'geoid', 'state_name')) %>%
  dcast.data.table( year + variable + name + geoid + state_name ~ fire,
                    value.var = 'value') 

# read in the files and aggregate to monitor locations
cmaq_monitors_ny <- 
  lapply( files_all,
          read_fn,
          raster_fill = latlon_raster.r,
          aggregate_to = AQS_locs[,c( 'mID', 'ID', 'POC')])  %>%
  rbindlist( fill = TRUE) %>%
  melt( id.vars = c( 'year', 'name', 'fire', 'mID', 'ID', 'POC')) %>%
  dcast.data.table( year + variable + name + mID + ID + POC ~ fire,
                    value.var = 'value') 

# calculate differences from fires
cmaq_state_ny[, fire_diff := firefire - fire_0firefire]
cmaq_county_ny[, fire_diff := firefire - fire_0firefire]
cmaq_monitors_ny[, fire_diff := firefire - fire_0firefire]

# save the output
fwrite( cmaq_state_ny, 'data/cmaq_processed/cmaq_state_ny.csv')
fwrite( cmaq_county_ny, 'data/cmaq_processed/cmaq_county_ny.csv')
fwrite( cmaq_monitors_ny, 'data/cmaq_processed/cmaq_monitors_ny.csv')



library( data.table)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)

## ====================================================================== ##
### read in county data 
## ====================================================================== ##
cmaq_county_ny <- fread( 'data/cmaq_processed/cmaq_county_ny.csv')

## ====================================================================== ##
### plot - daily average differences across counties
## ====================================================================== ##
cmaq_county_ny_avgs <- 
  cmaq_county_ny[, .( nofire = mean( nofire),
                      withfire = mean( withfire),
                      fire_diff = mean( fire_diff)),
                 by = .( name, date)] %>%
  melt( id.vars = c( 'name', 'date'))

# make the plot
ggplot( cmaq_county_ny_avgs,
        aes( x = date, y = value, color = variable, group = variable)) + 
  geom_line() + 
  facet_wrap( name ~ ., ncol = 1, scales = 'free_y')

## ====================================================================== ##
### plot - histogram of daily average differences across counties
## ====================================================================== ##
# how big are the daily differences in NY?
ggplot( cmaq_county_ny,
        aes( x = fire_diff)) + 
  geom_histogram() + 
  facet_wrap( . ~ name, scales = 'free_x')

## ====================================================================== ##
### plot - spatial plot of single day (Nov 2018)
## ====================================================================== ##
ny_counties <- USAboundaries::us_counties( states = 'NY')
ny_counties$state_name <- NULL
ny_counties$name <- NULL
cmaq_county_ny[, geoid := as.character( geoid)]

cmaq_county_ny.sf <- 
  merge( cmaq_county_ny,
         ny_counties[, c( 'geoid', 'namelsad')], 
         by = c( 'geoid')) %>%
  melt( id.vars = c( 'geoid', 'namelsad', 'year', 'variable', 'name',
                     'state_name', 'date', 'geometry'),
        variable.name = 'fire')

ggplot( cmaq_county_ny.sf[date %in% as.Date( c( '2018-11-22',
                                       '2018-11-23',
                                       '2018-11-24',
                                       '2018-11-25',
                                       '2018-11-26',
                                       '2018-11-27')) & 
                                         fire == 'fire_diff'],
        aes( fill = value, geometry = geometry)) + 
  geom_sf() + 
  facet_grid( name ~ date) + 
  theme_minimal() +
  theme( axis.text = element_blank())
















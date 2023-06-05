library( data.table)
library( sf)
library( magrittr)
library( USAboundaries)
library( data.table)
library( ggplot2)
library( viridis)

## =========================================================== ##
## Function - calculate comparison the metrics
## =========================================================== ##
eval.fn <- function( Yhat, Yact, mod.name){
  num.diff <- sum( Yhat - Yact, na.rm = T)
  abs.diff <- sum( abs( Yhat - Yact), na.rm = T)
  denom <- sum( Yact, na.rm = T)
  metrics <- data.table( year = mod.name,
                         N = length( Yhat),
                         NMB = num.diff / denom,
                         NME = abs.diff / denom,
                         MB   = num.diff / length( Yhat),
                         RMSE = sqrt( sum( ( Yhat - Yact) ^ 2, na.rm = T) / length( Yhat)),
                         R.p = cor( Yhat, Yact, use = 'complete.obs') ^ 2,
                         R.s = cor( Yhat, Yact, use = 'complete.obs', method = 'spearman') ^ 2)
  return( metrics)
}

## ====================================================================== ##
### read in monitor data 
## ====================================================================== ##
cmaq_monitors_ny <- fread( 'data/cmaq_processed/cmaq_monitors_ny.csv')



# how big are the daily differences in NY?
hist( cmaq_county_ny[name == 'daily8hrmax', firefire - fire_0firefire])
hist( cmaq_county_ny[name == 'dailyavgs', firefire - fire_0firefire])

# melt and plot
cmaq_state_ny.m <- 
  melt( cmaq_state_ny, id.vars = c( 'year', 'variable', 'name', 'geoid', 'state_name'),
        variable.name = 'fire')
cmaq_county_ny.m <- 
  melt( cmaq_county_ny, id.vars = c( 'year', 'variable', 'name', 'geoid', 'state_name'),
        variable.name = 'fire')

cmaq_state_ny.m[, date := paste( year, variable)]

ggplot( cmaq_state_ny.m,
        aes( x = date, y = value, color = fire, group = fire)) + 
  geom_line() + 
  facet_wrap( . ~ name, ncol = 1)

# combine all into brick: 11 layers in total
rasterCMAQ_brick <- 
  brick( file_dims)

plot( rasterCMAQ_brick)

plot( calc(rasterCMAQ_brick, sd))
summary( mean( rasterCMAQ_brick))

# convert rasterCMAQ_brick to sf object
rasterCMAQ_brick.sf <-
  rasterCMAQ_brick %>%
  rasterToPolygons( ) %>%
  st_as_sf %>%
  as.data.table

# download some US data
states <- us_states()

rasterCMAQ_brick.sf$geometry <-
  st_transform( rasterCMAQ_brick.sf$geometry,
                crs = st_crs( states))

rasterCMAQ_brick.sf.m <-
  as.data.table( rasterCMAQ_brick.sf) %>%
  melt( id.vars = 'geometry',
        variable.name = 'names',
        value.name = 'PM2.5')

#plot together
cmaq <- 
  ggplot( ) +
  # add the disperser grid
  geom_sf( data = rasterCMAQ_brick.sf.m,
           aes( fill = PM2.5, 
                color = PM2.5,
                geometry = geometry)) +
  # add state boundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'black', size = 1,
           fill = NA,
           inherit.aes = FALSE) +
  # change the fill & color scale
  scale_fill_viridis(  limits = c( 25, 50), oob = scales::squish) +
  scale_color_viridis( limits = c( 25, 50), oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  # facet_wrap( . ~ month, ncol = 3) +
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) + 
  # set boundaries over NY
  # coord_sf( xlim = c( -83, -69), ylim = c( 40, 45)) +
  # set thematic elements
  facet_wrap( . ~ names) +
  theme_bw()+
  labs( fill = expression( PM['2.5']*', µg'~m^{'-3'}),
        color= expression( PM['2.5']*', µg'~m^{'-3'}))+
  guides( fill = 'none') +
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20)) 

ggsave('CMAQ.png', cmaq, width = 14, height=5, scale = 1.2)

# take mean and standard deviation, convert to sf
Mean_LH.sf <- 
  calc( raster1940_brick, mean) %>%
  rasterToPolygons( ) %>%
  st_as_sf %>%
  as.data.table
SD_LH.sf <- 
  calc( raster1940_brick, sd) %>%
  rasterToPolygons( ) %>%
  st_as_sf %>%
  as.data.table

Mean_LH.sf[, metric := 'Mean across 11 models']
SD_LH.sf[, metric := 'Std. Dev. across 11 models']

raster_plot.dt <- 
  rbind( Mean_LH.sf, SD_LH.sf)


raster_plot.dt$geometry <-
  st_transform( raster_plot.dt$geometry,
                crs = st_crs( states))

# create the plot.
# Mean <-
ggplot( ) +
  # add the disperser grid
  geom_sf( data = raster_plot.dt,
           aes( fill = layer, 
                color = layer,
                geometry = geometry)) +
  # add state boundaries
  geom_sf( data = states,
           aes( geometry = geometry),
           color = 'black', size = 1,
           fill = NA,
           inherit.aes = FALSE) +
  # change the fill & color scale
  scale_fill_viridis(  limits = c( 0, 10), oob = scales::squish) +
  scale_color_viridis( limits = c( 0, 10), oob = scales::squish) +
  # be sure to show 0 in the color scales
  expand_limits( fill = 0, color = 0) +
  # create panels for each day
  # facet_wrap( . ~ month, ncol = 3) +
  coord_sf( xlim = c( -125, -67), ylim = c( 25, 50)) + 
  # set boundaries over NY
  # coord_sf( xlim = c( -83, -69), ylim = c( 40, 45)) +
  # set thematic elements
  facet_wrap( . ~ metric) +
  theme_bw()+
  labs( fill = expression( PM['2.5']*', µg'~m^{'-3'}),
        color= expression( PM['2.5']*', µg'~m^{'-3'}))+
  guides( fill = 'none') +
  # ggtitle("Mean & SD of 11 models")  + 
  theme(plot.title = element_text(size = 20, face = "bold")) + 
  theme(rect = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        strip.text = element_text( size = 20)) 
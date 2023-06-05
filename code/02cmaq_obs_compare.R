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
eval.fn <- function( Yhat, Yact){
  num.diff <- sum( Yhat - Yact, na.rm = T)
  abs.diff <- sum( abs( Yhat - Yact), na.rm = T)
  denom <- sum( Yact, na.rm = T)
  metrics <- data.table( N = length( Yhat),
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
# load monitor cmaq data
cmaq_monitors_ny <- fread( 'data/cmaq_processed/cmaq_monitors_ny.csv', drop = 'geometry')

# aqs ozone data is in ppb, convert to ppm
cmaq_monitors_ny[ `Units of Measure` == 'Parts per million', 
                  `Arithmetic Mean` := `Arithmetic Mean` * 1000]

# load the spatial data
load( file = 'data/epa_aqs/aqs_sites_ny.RData')

# merge monitor data with spatial data
cmaq_monitors_ny[, ID := as.character( ID)]
cmaq_monitors_ny.sf <- 
  merge (cmaq_monitors_ny,
         AQS_locs[, c( 'mID', 'ID')],
         by = c( 'mID', 'ID'),
         allow.cartesian = TRUE)

## ====================================================================== ##
### do the evaluation
## ====================================================================== ##
cmaq_monitors_eval_fire <- 
  cmaq_monitors_ny[, eval.fn( withfire, `Arithmetic Mean`),
                   by = .( name, year, `Parameter Code`)][, fire := 'withfire']
cmaq_monitors_eval_nofire <- 
  cmaq_monitors_ny[, eval.fn( nofire, `Arithmetic Mean`),
                   by = .( name, year, `Parameter Code`)][, fire := 'nofire']

# rbind together, melt for plotting
cmaq_monitors_eval <- rbind( cmaq_monitors_eval_fire, cmaq_monitors_eval_nofire)
cmaq_monitors_eval.m <- 
  melt( cmaq_monitors_eval,
        id.vars = c( 'name', 'year', 'fire', 'Parameter Code'))
## ====================================================================== ##
### plot the evaluation
## ====================================================================== ##

ggplot( cmaq_monitors_eval.m[variable != 'N'],
        aes( y = value, x = year, fill = fire)) + 
  geom_col( position = position_dodge()) + 
  scale_fill_brewer( palette = 'Set2') +
  facet_grid( variable ~ name, scales = 'free_y') + 
  theme_minimal() + 
  theme( panel.grid.minor = element_blank())







library( data.table)
library( sf)
library( lubridate)
library( ggplot2)

#http://files.airnowtech.org/?prefix=airnow/2023/20230607/
# download_read
downloader_fn <- 
  function( dates_download = as.Date( '2023-06-06'),
            save_dir = file.path( 'data', 'epa_aqs_api')){
    
    # create a list of urls 
    url_base <- 'https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow'
    year_download <- year( dates_download)
    date_download <- format.Date(dates_download, format = '%Y%m%d')
    urls <- paste0( url_base, '/', year_download, '/', 
                    date_download, '/HourlyData_',
                    date_download,
                            formatC( 0:23, width = 2, flag = '0'), '.dat')
    
    # create list of file destinations
    file.in.names <- 
      file.path( save_dir, 
                 gsub( paste0( '.*', date_download, '/'), '', urls))
    
    # download the files
    download.file( urls, file.in.names)

    # read the files
    file.in.read <- 
      lapply( file.in.names, fread) %>% rbindlist()
    
    # add date column
    file.in.read[, date.time.char := paste( V1, V2)]
    file.in.read[, date.time := as.POSIXct( date.time.char, 
                                        format = '%m/%d/%y %H:%M')]
    
    # return it
    return( file.in.read)
    
  }

# apply the function
alldata_in <- 
  lapply( seq.Date( as.Date( '2023-06-03'), as.Date( '2023-06-06'), by = 'day'),
          downloader_fn) %>% rbindlist()

# load the spatial data
load( file = 'data/epa_aqs/aqs_sites_ny.RData')
AQS_locs.dt <- 
  as.data.table(
    AQS_locs[,c( 'State Code', 'County Code', 'Site Num', 
                 'County Name', 'Local Site Name')]
  )[,geometry := NULL]  %>%
  unique()

# 2 digit state, 3 digit county, 4 digit site
AQS_locs.dt[, V3 := paste0( formatC( `State Code`, width = 2, flag = '0'),
                            formatC( `County Code`, width = 3, flag = '0'),
                            formatC( `Site Num`, width = 4, flag = '0'))]

# merge with station information
ny_pm_o3 <- 
  merge( alldata_in[ V6 %in% c( 'PM2.5', 'OZONE')],
         AQS_locs.dt,
         by = 'V3')
va_pm_o3 <- 
  alldata_in[ grepl( 'Virginia Dept', V9) & V6 %in% c( 'PM2.5', 'OZONE')]

# calculate mean and quantiles
ny_pm_o3_quant <- 
  ny_pm_o3[, .( min = min( V8),
                quant10 = quantile( V8, .1),
                quant50 = quantile( V8, .5),
                quant90 = quantile( V8, .9),
                mean = mean( V8),
                max = max( V8)),
           by = .( V6, V7, date.time, `State Code`)]

# plot station data
ggplot( ny_pm_o3,
        aes( x = date.time, y = V8, group = V3)) + 
  geom_line( alpha = .5) + 
  facet_wrap( . ~ V6 + V7, scales = 'free_y') + 
  theme_minimal()
ggplot( va_pm_o3,
        aes( x = date.time, y = V8, group = V3)) + 
  geom_line( alpha = .5) + 
  facet_wrap( . ~ V6 + V7, scales = 'free_y') + 
  theme_minimal()

# plot state summary
ggplot( ny_pm_o3_quant,
        aes( x = date.time, y = mean, ymin = min, ymax = max)) + 
  geom_ribbon( fill = 'grey50') +
  geom_line( alpha = .5) + 
  facet_wrap( . ~ V6 + V7, scales = 'free_y') + 
  theme_minimal()


# average ozone & PM across the state




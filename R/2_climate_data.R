
rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
#library(ncdf4)
library(maps)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SURVEY LOCATIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
geo_data = read.csv("Data/Species_LatLong_plusHabitat.csv")  # ./data/climate_data/Species_LatLong_plusHabitat.csv
# Remove missing lat and long
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SETUP API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

### SET KEYRING USER DETAILS TO CONNECT TO CLIMATE DATA STORE ACCOUNT
### Note: this is my personal CDS account so if you have your own
### its possible to change the details, but mine should work for you too.
wf_set_key(user="89307", key="e661d59a-c4f0-4e9e-9da4-476fa0a9536e", service='cds')
#wf_user_info("89307")

# This page had a really helpful instructions. In particular, you can copy and
# paste the API code from the CDS website, and then use an RStudio plugin to 
# convert the text to an R-relevant command!
#   https://cran.r-project.org/web/packages/ecmwfr/vignettes/cds_vignette.html


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# HISTORIC CLIMATIC DATA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Cycle through each surveyed point in the table and extract a record monthly mean
# temperature (2m and skin for land, SST for marine) between 1950 and 2022.
# The outputs are saved as NetCDFs, but can also be read by the terra package (see below)
# and easily converted to a vector.
# Two points to note:
#         > LAND: because I have hesitated on which temperature to use, I have 
#           downloaded both air and ground temperature. Hence when this is spat
#           out as a vector its double the length of the time series, and you 
#           can see a clear jump in values where it switches to skin temperature.
#         > MARINE: extraction of SST at the precise location of the record often
#           throws errors because the dataset essentially considers that 0.1 x 0.1
#           cell to be land. As a result I have expanded the extent of the 
#           request for marine species so that its much more likely to include 
#           some cells with values, and then we can take a median of those at 
#           each time.


for(i in which(geo_data$source=="w") ){  #nrow(geo_data)){
  
  filelist = unlist(lapply(list.files("./Data/ERA5_LAND/",pattern="point"),FUN=function(x) strsplit(x,"point")[[1]][2]))
  filelist = as.numeric(gsub("\\.nc","",filelist))
  
  if( !i %in% filelist ){
    
    Habitat= geo_data$habitat[i] %in% c("t","f")
    
    if(Habitat){
      # If the species is Terrestrial or Freshwater we refer our request to the ERA5-LAND dataset
      
      # Maintain a tightly focused spatial extent
      Xlim   = round(round(geo_data$long[i],2) + c(-0.01,0.01),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.01,0.01),2)
      
      #if(i==253){ Ylim=c(21.44,21.46) }
      if(i %in% c(9,110,150,175,228,253,262)){ 
        Xlim   = round(round(geo_data$long[i],2) + c(-0.1,0.1),2)
        Ylim   = round(round(geo_data$lat[i],2) + c(-0.1,0.1),2)
      }
      
      # If we want every hour of every day
      # "ERA5-Land hourly data from 1950 to present"
      # >> https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
      #
      #request <- list(
      #  variable = "2m_temperature",
      #  year = as.character(seq(1950,2022,1)),
      #  month = c("01","02","03","04","05","06","07","08","09","10","11","12"),
      #  day=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"),
      #  time=c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00"),
      #  format="netcdf",
      #  area = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
      #  dataset_short_name = "reanalysis-era5-land",
      #  target = paste0("era5land.hourly.point",i,".nc")
      #)
      
      # If monthly averages are OK
      # "ERA5-Land monthly averaged data from 1950 to present"
      # >> https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form
      request <- list(
        product_type = "monthly_averaged_reanalysis",
        variable = c("2m_temperature", "skin_temperature"),
        year     = as.character(seq(1950,2022,1)),
        month    = c("01","02","03","04","05","06","07","08","09","10","11","12"),
        time     = "00:00",
        format   = "netcdf",
        area = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        dataset_short_name = "reanalysis-era5-land-monthly-means",
        target   = paste0("era5land.monthlymean.point",i,".nc")
      )
      
      
    } else {
      # If the species is MARINE we extract from
      # "ERA5 monthly averaged data on single levels from 1940 to present"
      # >> https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
      
      # NOTE
      # Here we include a broader spatial extent (because many coastal margins don't have
      # an SST value). Suggest we later take the median of available values.
      Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
      
      request <- list(
        variable = "sea_surface_temperature",
        product_type = "monthly_averaged_reanalysis",
        year     = as.character(seq(1950,2022,1)),
        month    = c("01","02","03","04","05","06","07","08","09","10","11","12"),
        time     = "00:00",
        format   = "netcdf",
        area = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
        target = paste0("era5.sst.point",i,".nc")
      )
      
    }
    
    # Start downloading the data, the path of the file will be returned as a variable (ncfile)
    ncfile = wf_request(
      user     = "89307",
      request  = request,   
      transfer = TRUE,  
      path     = "./Data/ERA5_LAND",
      verbose  = FALSE
    )
    
    
    # Check (every 5 minutes) if the data has been downloaded before submitting another request
    repeat{
      if(file.exists(ncfile)){ 
        # Plot the data
        r =  try(terra::rast(ncfile))
        plot(as.vector(r), type="o", main=paste("Point ",i))
        break() 
      } else { sleep(300) }
    }
    
  }
} # Loop over sampled points


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# FUTURE CLIMATIC PROJECTIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# This is much more complicated than the choices above. There are so many 
# options to choose from.
# This data portal is useful because all the same parameters (2m temp, skin temp
# and SST) are all provided and at a 0.125 resolution.
#  > CMIP5 monthly data on single levels
#    https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip5-monthly-single-levels?tab=overview

# For future climate scenarios its generally worth considering:
#   multiple RCPs - 4.5 and 8.5
#   multiple GCMs - e.g. CSIRO-Mk3, HadGEM, CCSM, GFDL-CM3, IPSL-CM5A

#   THE PROBLEM IS THE FUTURE DATES AVAILABLE ARE DIFFERENT FOR EACH COMBINATION
#   OF RCP & GCM SO I'M FINDING IT HARD TO IDENTIFY WHICH MODELS CAN BE COMPARED


rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
library(maps)
library(ncdf4)
library(raster)

geo_data = read.csv("Data/Species_LatLong_plusHabitat.csv")  # ./data/climate_data/Species_LatLong_plusHabitat.csv
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
# SETUP API
wf_set_key(user="89307", key="e661d59a-c4f0-4e9e-9da4-476fa0a9536e", service='cds')


### TERRESTRIAL/ FRESHWATER
request <- list(
  ensemble_member = "r1i1p1",
  format = "zip",
  experiment = "rcp_4_5",
  variable = c("2m_temperature"),  #, "skin_temperature"
  model = "gfdl_esm2m",
  period = "208101-208512",
  dataset_short_name = "projections-cmip5-monthly-single-levels",
  target = paste0("gfdl2080.airtemp.zip") 
)

# Start downloading the data, the path of the file will be returned as a variable (ncfile)
ncfile = wf_request(
  user     = "89307",
  request  = request,   
  transfer = TRUE,  
  path     = "./Data/Projections",
  verbose  = FALSE
)

#--------------------------------------------------#

request <- list(
  ensemble_member = "r1i1p1",
  format = "zip",
  experiment = "rcp_4_5",
  variable = c("skin_temperature"),  #, 
  model = "gfdl_esm2m",
  period = "208101-208512",
  dataset_short_name = "projections-cmip5-monthly-single-levels",
  target = paste0("gfdl2080.skintemp.zip") 
)

# Start downloading the data, the path of the file will be returned as a variable (ncfile)
ncfile = wf_request(
  user     = "89307",
  request  = request,   
  transfer = TRUE,  
  path     = "./Data/Projections",
  verbose  = FALSE
)

#--------------------------------------------------#
  
### MARINE  
request <- list(
    ensemble_member = "r1i1p1",
    format = "zip",
    experiment = "rcp_4_5",
    variable = c("sea_surface_temperature"),
    model = "gfdl_esm2m",
    period = "208101-208512",
    dataset_short_name = "projections-cmip5-monthly-single-levels",
    target = paste0("gfdl2080.SST.zip") 
)

# Start downloading the data, the path of the file will be returned as a variable (ncfile)
ncfile = wf_request(
  user     = "89307",
  request  = request,   
  transfer = TRUE,  
  path     = "./Data/Projections",
  verbose  = FALSE
)



#--------------------------------------------------#
#--------------------------------------------------#

for(i in which(geo_data$source=="w") ){  #nrow(geo_data)){
  
  Habitat= geo_data$habitat[i] %in% c("t","f")
  
  ### TERRESTRIAL/ FRESHWATER
  if(Habitat){
    
    tempdat = data.frame("Year"        = rep(seq(2081,2085,1),e=12),
                         "Month"       = rep(seq(2081,2085,1),e=12),
                         "Temp"     = NA)
    
    #ncfname   = paste0("./Data/Projections/gfdl2080.proj",i,"/tos_Omon_GFDL-ESM2M_rcp45_r1i1p1_208101-208512.nc")
    ncfname   = paste0("./Data/Projections/gfdl2080.airtemp.zip")
    Proj      = nc_open(unzip(ncfname))   # 'tas' is "Near-Surface Air Temperature"
    dname     = "tas"   
    lon       = ncvar_get(Proj, "lon") 
    nlon      = length(lon)                  # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)                  # number of rows
    t         = ncvar_get(Proj, "time")      # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                       # monthly averages: seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months"
    
    #map = ncvar_get(Proj, 'tas',start= c(1,1,1),count= c(nlon,nlat,1) )
    #image(map)
    
    M = raster(map)
    ###  plot(M) # wrong way around
    ###  Y = cellFromCol(M, length(lat))  # add columns working from right to left
    ###  summary(M[Y])
    ###  Y = cellFromCol(M, 100) # example of middle column
    ###  summary(M[Y])
    
    Y = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2 = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    #plot(raster(M2))
    # Which grid cells contain the search limits
    Xlim   = round(round(geo_data$long[i],2) + c(-0.1,0.1),2)
    Ylim   = round(round(geo_data$lat[i],2) + c(-0.1,0.1),2)
    # Convert coordinates betrween 80 and 180 to ncdf grid
    XXlim = Xlim
    XXlim[XXlim<0] = 180+abs(XXlim[XXlim<0])
    lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    cellFromRowCol(raster(M2), row=lat.row, col=lon.col)
    xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col))
    
    #points(xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col)))
    
    # Get the values for the right cells for all months
    tas = ncvar_get(Proj, 'tas', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$Temp = apply(tas, length(dim(tas)), median, na.rm=TRUE) #ncvar_get(Proj, dname, start= c(1,1,1), count= c(nlon,nlat,nt) )
    #plot(tempdat$Temp, type="o")
    
    save(tempdat, file=paste0("./Data/Projections/gfdl2080_airtemp_",i,".RData"))
    
    ##############################################################################
    
    tempdat = data.frame("Year"        = rep(seq(2081,2085,1),e=12),
                         "Month"       = rep(seq(2081,2085,1),e=12),
                         "Temp"     = NA)
    
    #ncfname   = paste0("./Data/Projections/gfdl2080.proj",i,"/tos_Omon_GFDL-ESM2M_rcp45_r1i1p1_208101-208512.nc")
    ncfname   = paste0("./Data/Projections/gfdl2080.skintemp.zip")
    Proj      = nc_open(unzip(ncfname))    # "ts" is Surface Temperature"
    dname     = "ts"   
    lon       = ncvar_get(Proj, "lon") 
    nlon      = length(lon)                  # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)                  # number of rows
    t         = ncvar_get(Proj, "time")      # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                       # monthly averages: seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months"
    
    #map = ncvar_get(Proj, 'ts',start= c(1,1,1),count= c(nlon,nlat,1) )
    #image(map)
    
    M = raster(map)
    Y = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2 = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    #plot(raster(M2))
    # Which grid cells contain the search limits
    Xlim   = round(round(geo_data$long[i],2) + c(-0.1,0.1),2)
    Ylim   = round(round(geo_data$lat[i],2) + c(-0.1,0.1),2)
    # Convert coordinates betrween 80 and 180 to ncdf grid
    XXlim = Xlim
    XXlim[XXlim<0] = 180+abs(XXlim[XXlim<0])
    lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    cellFromRowCol(raster(M2), row=lat.row, col=lon.col)
    xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col))
    
    #points(xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col)))
    
    # Get the values for the right cells for all months
    ts = ncvar_get(Proj, 'ts', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$Temp = apply(ts, length(dim(tas)), median, na.rm=TRUE)
    #lines(tempdat$Temp ~ c(1:nt), type="o", col="red")
    
    save(tempdat, file=paste0("./Data/Projections/gfdl2080_skintemp_",i,".RData"))
    
    #############################################################################
    
    ### MARINE
  } else {
    tempdat = data.frame("Year"    = rep(seq(2081,2085,1),e=12),
                         "Month"   = rep(seq(2081,2085,1),e=12),
                         "SST"     = NA)
    
    #ncfname   = paste0("./Data/Projections/gfdl2080.proj",i,"/tos_Omon_GFDL-ESM2M_rcp45_r1i1p1_208101-208512.nc")
    ncfname   = paste0("./Data/Projections/gfdl2080.SST.zip")
    Proj      = nc_open(unzip(ncfname))
    dname     = "sst"  #Sea Surface Temperature"
    lon       = ncvar_get(Proj, "rlon") 
    nlon      = length(lon) # number of columns
    lat       = ncvar_get(Proj, "rlat") 
    nlat      = length(lat) # number of rows
    t         = ncvar_get(Proj, "time")              # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                                  # monthly averages: seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months")
    
    ## Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
    #t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00") 
    #
    
    # The SST layers are on a grid rotated to suit NOAA. So they go from -270 to +80
    map = ncvar_get(Proj, 'tos', 
                    start= c(1,1,1),       # 4 dimensions: longitude, latitude, 'expver' (variable), time
                    count= c(nlon,nlat,1) )
    image(map)
    
    M = raster(map)
    ###  plot(M) # wrong way around
    ###  Y = cellFromCol(M, length(lat))  # add columns working from right to left
    ###  summary(M[Y])
    ###  Y = cellFromCol(M, 100) # example of middle column
    ###  summary(M[Y])
    
    Y = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2 = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    plot(raster(M2))
    # Which grid cells contain the search limits
    Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
    Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
    # Convert coordinates betrween 80 and 180 to ncdf grid
    XXlim = Xlim
    XXlim[XXlim>80] = -180-(180-XXlim[XXlim>80])
    lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    cellFromRowCol(raster(M2), row=lat.row, col=lon.col)
    xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col))
    
    points(xyFromCell(raster(M2),cellFromRowCol(raster(M2), row=lat.row, col=lon.col)))
    
    # Get the values for the right cells for all months
    sst = ncvar_get(Proj, 'tos', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$SST = apply(sst, 3, median, na.rm=TRUE)
    # plot(tempdat[,3], type="o")
    
    save(tempdat, file=paste0("./Data/Projections/gfdl2080_sst_",i,".RData"))
    
  }
  
}




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# REFORMAT 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
library(ncdf4)
library(tidyr)
library(terra)

# Survey Locations
geo_data = read.csv("Data/Species_LatLong_plusHabitat.csv") 
# Remove missing lat and long
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
# Add columns for temp data
time.steps = seq(as.Date("1950/01/01"), as.Date("2022/12/01"), "months")
nT         = length(time.steps)

# Climate data matrix
clim_data = as.matrix(geo_data[,c(4,5)])
clim_data = cbind(c(1:nrow(clim_data)),clim_data, matrix(NA,nrow=nrow(geo_data),ncol=nT))
clim_data2=clim_data
for(i in which(geo_data$source=="w") ){
  Habitat= geo_data$habitat[i] %in% c("t","f")
  if(Habitat){
    # Extract NCDF as vector
    if(dim(terra::rast(paste0("./Data/ERA5_LAND/era5land.monthlymean.point",i,".nc")))[1]==1){
      r =  as.vector(terra::rast(paste0("./Data/ERA5_LAND/era5land.monthlymean.point",i,".nc")))
    } else {
      r = terra::rast(paste0("./Data/ERA5_LAND/era5land.monthlymean.point",i,".nc"))
      r = apply(as.array(r),c(3), median, na.rm=T)
    }
    # Add to matrix
    clim_data[i,-c(1:3)] = r[1:nT]     # air temperature
    clim_data2[i,-c(1:3)]= r[-c(1:nT)] # skin temperature
  } else {
    # Marine
    # Extract NCDF as array
    r =  as.array(terra::rast(paste0("./Data/ERA5_LAND/era5.sst.point",i,".nc")))
    # Median of each time point
    r = apply(r, 3, median, na.rm=TRUE)
    # Add to matrix
    clim_data[i,-c(1:3)] = r
    clim_data2[i,-c(1:3)] = r
  }
  if(exists('r')){rm(r)}
}
clim_data          = as.data.frame(clim_data)
clim_data[,1]      = geo_data$species_full
names(clim_data)   = c('Species','Longitude','Latitude',time.steps)
# Now we can do some simple summaries, like CV
cv                 = function(x){ sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) }
clim_data$cv       = apply(clim_data[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data$sd       = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data$mean     = apply(clim_data[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data$acf_lag1 = apply(clim_data[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data$acf_all  = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

which( is.na(clim_data[,5]) & (geo_data$source=="w")) 

write.csv(clim_data, "./output/climate_data/ERA5_air_and_SST_timeseries.csv")

clim_data2          = as.data.frame(clim_data2)
clim_data2[,1]      = geo_data$species_full
names(clim_data2)   = c('Species','Longitude','Latitude',paste0('Y',time.steps))
clim_data2$cv       = apply(clim_data2[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data2$sd       = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data2$mean     = apply(clim_data2[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data2$acf_lag1 = apply(clim_data2[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data2$acf_all  = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data2, "./output/climate_data/ERA5_landsurface_timeseries.csv")

table(complete.cases(clim_data))
table(complete.cases(clim_data2))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# REFORMAT PROJECTIONS 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
library(ncdf4)
library(tidyr)
library(terra)

# Survey Locations
geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv") 
# Remove missing lat and long
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
# Add columns for temp data
time.steps = seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months")
nT         = length(time.steps)

# Climate data matrix
clim_data = as.matrix(geo_data[,c(4,5)])
clim_data = cbind(c(1:nrow(clim_data)),clim_data, matrix(NA,nrow=nrow(geo_data),ncol=nT))
clim_data2=clim_data

for(i in which(geo_data$source=="w") ){
  Habitat= geo_data$habitat[i] %in% c("t","f")
  if(Habitat){
    # Extract NCDF as vector
    r =  load(paste0("./Data/Projections/gfdl2080_airtemp_",i,".RData"))
    # Add to matrix
    clim_data[i,-c(1:3)] = tempdat$Temp    # air temperature
    r =  load(paste0("./Data/Projections/gfdl2080_skintemp_",i,".RData"))
    # Add to matrix
    clim_data2[i,-c(1:3)] = tempdat$Temp    # skin temperature
  } else {
    # Extract NCDF as array
    load(paste0("./Data/Projections/gfdl2080_sst_",i,".RData"))
    # Add to matrix
    clim_data[i,-c(1:3)] = tempdat$SST
    clim_data2[i,-c(1:3)] = tempdat$SST
  }
  if(exists('r')){rm(r)}
}
clim_data          = as.data.frame(clim_data)
clim_data[,1]      = geo_data$species_full
names(clim_data)   = c('Species','Longitude','Latitude',time.steps)
# Now we can do some simple summaries, like CV
cv                 = function(x){ sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) }
clim_data$cv       = apply(clim_data[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data$sd       = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data$mean     = apply(clim_data[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data$acf_lag1 = apply(clim_data[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data$acf_all  = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data, "./output/climate_data/gfdl2081-2085_air_and_SST_timeseries.csv")

clim_data2          = as.data.frame(clim_data2)
clim_data2[,1]      = geo_data$species_full
names(clim_data2)   = c('Species','Longitude','Latitude',paste0('Y',time.steps))
clim_data2$cv       = apply(clim_data2[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data2$sd       = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data2$mean     = apply(clim_data2[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data2$acf_lag1 = apply(clim_data2[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data2$acf_all  = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data2, "./output/climate_data/Projections/gfdl2081-2085_landsurface_and_SST_timeseries.csv")


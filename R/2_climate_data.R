
rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
library(maps)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SURVEY LOCATIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv") 
# Remove missing lat and long
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
# Remove records for captive individuals
geo_data = geo_data[geo_data$source=="w",]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SETUP API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

### Set keyring and user details to connect to the Climate Data Store account.
### Note: the details below are illustrative and will not work. You need to 
### register for CDS account and enter your own details.
wf_set_key(user    = "89303", 
           key     = "e661d57a-c4f0-4e9e-9da4-477fa0a9537e", 
           service = 'cds')


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# HISTORIC CLIMATIC DATA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

for(i in 1:nrow(geo_data) ){
  
  filelist = unlist(lapply(list.files("./data/climate_data/ERA5_LAND/",pattern="point"),FUN=function(x) strsplit(x,"point")[[1]][2]))
  filelist = as.numeric(gsub("\\.nc","",filelist))
  
  if( !i %in% filelist ){
    
    Habitat= geo_data$habitat[i] %in% c("t","f")
    
    if(Habitat){
      # If the species is Terrestrial or Freshwater we refer our request to the ERA5-LAND dataset
      
      # Maintain a tightly focused spatial extent
      Xlim   = round(round(geo_data$long[i],2) + c(-0.01,0.01),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.01,0.01),2)
      
      # ERA5-Land monthly averaged data from 1950 to present
      # >> https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form
      request <- list(
        product_type      = "monthly_averaged_reanalysis",
        variable          = c("2m_temperature", "skin_temperature"),
        year              = as.character(seq(1950,2022,1)),
        month             = c("01","02","03","04","05","06","07","08","09","10","11","12"),
        time              = "00:00",
        format            = "netcdf",
        area              = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        dataset_short_name= "reanalysis-era5-land-monthly-means",
        target            = paste0("era5land.monthlymean.point",i,".nc")
      )
      
      
    } else {
      # ERA5 monthly averaged data on single levels from 1940 to present
      # >> https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form
      
      # Note we include a broader spatial extent because many coastal margins 
      # don't have an SST value).
      Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
      
      request <- list(
        variable          = "sea_surface_temperature",
        product_type      = "monthly_averaged_reanalysis",
        year              = as.character(seq(1950,2022,1)),
        month             = c("01","02","03","04","05","06","07","08","09","10","11","12"),
        time              = "00:00",
        format            = "netcdf",
        area              = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        dataset_short_name= "reanalysis-era5-single-levels-monthly-means",
        target            = paste0("era5.sst.point",i,".nc")
      )
      
    }
    
    # Start downloading the data, the path of the file will be returned as a variable (ncfile)
    ncfile = wf_request(
      user     = "89303",
      request  = request,   
      transfer = TRUE,  
      path     = "./data/climate_data/ERA5_LAND",
      verbose  = FALSE
    )
    
  }
} # Loop over sampled points


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# FUTURE CLIMATIC PROJECTIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# The CDS data portal provides all the same parameters (2m temp, skin temp
# and SST) for future climate projections at a 0.125 resolution.
#  > CMIP5 monthly data on single levels
#    https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip5-monthly-single-levels?tab=overview

rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
library(maps)
library(ncdf4)
library(raster)

geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv")  # ./data/climate_data/climate_data/Species_LatLong_plusHabitat.csv
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
geo_data = geo_data[geo_data$source=="w",]

# SETUP API
wf_set_key(user    = "89303", 
           key     = "e661d57a-c4f0-4e9e-9da4-477fa0a9537e", 
           service = 'cds')

# The code below select the GFDL global climate model, the RCP4.5 emission 
# scenario, and projections for 2081-2085. Results of these requests return 
# values for all months across the entire globe.
#--------------------------------------------------#
### TERRESTRIAL/ FRESHWATER
#--------------------------------------------------#
# Air temperature 
request <- list(
  ensemble_member    = "r1i1p1",
  format             = "zip",
  experiment         = "rcp_4_5",
  variable           = c("2m_temperature"),
  model              = "gfdl_esm2m",
  period             = "208101-208512",
  dataset_short_name = "projections-cmip5-monthly-single-levels",
  target             = paste0("gfdl2080.airtemp.zip") 
)

# Download the data
ncfile = wf_request(user = "89303", request = request, transfer = TRUE, 
                    path = "./Data/Projections", verbose = FALSE)

# Surface Temperature
request <- list(
  ensemble_member    = "r1i1p1",
  format             = "zip",
  experiment         = "rcp_4_5",
  variable           = c("skin_temperature"),  
  model              = "gfdl_esm2m",
  period             = "208101-208512",
  dataset_short_name = "projections-cmip5-monthly-single-levels",
  target             = paste0("gfdl2080.skintemp.zip") 
)

# Download the data
ncfile = wf_request(user = "89303", request = request, transfer = TRUE, 
                    path = "./Data/Projections", verbose = FALSE)

#--------------------------------------------------#
### MARINE  
#--------------------------------------------------#
# Sea Surface Temperature
request <- list(
    ensemble_member    = "r1i1p1",
    format             = "zip",
    experiment         = "rcp_4_5",
    variable           = c("sea_surface_temperature"),
    model              = "gfdl_esm2m",
    period             = "208101-208512",
    dataset_short_name = "projections-cmip5-monthly-single-levels",
    target             = paste0("gfdl2080.SST.zip") 
)

# Download the data
ncfile = wf_request(user = "89303", request = request, transfer = TRUE, 
                    path = "./Data/Projections", verbose = FALSE)

#--------------------------------------------------#
#  LOOP ACROSS SPECIES
#--------------------------------------------------#

for(i in 1:nrow(geo_data) ){ 
  
  Habitat= geo_data$habitat[i] %in% c("t","f")
  
  ### TERRESTRIAL/ FRESHWATER
  if(Habitat){
    
    tempdat = data.frame("Year"  = rep(seq(2081,2085,1),e=12),
                         "Month" = rep(seq(2081,2085,1),e=12),
                         "Temp"  = NA)
    
    ncfname   = paste0("./Data/Projections/gfdl2080.airtemp.zip")
    Proj      = nc_open(unzip(ncfname))   # 'tas' is "Near-Surface Air Temperature"
    dname     = "tas"   
    lon       = ncvar_get(Proj, "lon")    # Longitude has been rotated by 80'
    nlon      = length(lon)               # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)               # number of rows
    t         = ncvar_get(Proj, "time")   # time
    nt        = dim(t)                    # number of months
    
    M       = raster(map)
    Y       = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2      = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    Xlim    = round(round(geo_data$long[i],2) + c(-0.1,0.1),2)
    Ylim    = round(round(geo_data$lat[i],2) + c(-0.1,0.1),2)
    # Convert coordinates between 0 and -180 to ncdf grid
    XXlim   = Xlim
    XXlim[XXlim<0] = 180+abs(XXlim[XXlim<0])
    lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    # Get the values for the right cells for all months
    tas = ncvar_get(Proj, 'tas', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$Temp = apply(tas, length(dim(tas)), median, na.rm=TRUE)
    
    save(tempdat, file=paste0("./Data/Projections/gfdl2080_airtemp_",i,".RData"))
    
    ##############################################################################
    
    tempdat = data.frame("Year"        = rep(seq(2081,2085,1),e=12),
                         "Month"       = rep(seq(2081,2085,1),e=12),
                         "Temp"     = NA)
    
    ncfname   = paste0("./Data/Projections/gfdl2080.skintemp.zip")
    Proj      = nc_open(unzip(ncfname))    # "ts" is Surface Temperature"
    dname     = "ts"   
    lon       = ncvar_get(Proj, "lon") 
    nlon      = length(lon)                # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)                # number of rows
    t         = ncvar_get(Proj, "time")    # time
    nt        = dim(t)                     # number of months
    
    M         = raster(map)
    Y         = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2        = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    Xlim      = round(round(geo_data$long[i],2) + c(-0.1,0.1),2)
    Ylim      = round(round(geo_data$lat[i],2) + c(-0.1,0.1),2)
    # Convert coordinates between 0 and -180 to ncdf grid
    XXlim     = Xlim
    XXlim[XXlim<0] = 180+abs(XXlim[XXlim<0])
    lon.col   = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row   = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    # Get the values for the right cells for all months
    ts = ncvar_get(Proj, 'ts', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$Temp = apply(ts, length(dim(tas)), median, na.rm=TRUE)
    
    save(tempdat, file=paste0("./Data/Projections/gfdl2080_skintemp_",i,".RData"))
    
    #############################################################################
    
    ### MARINE
  } else {
    tempdat = data.frame("Year"    = rep(seq(2081,2085,1),e=12),
                         "Month"   = rep(seq(2081,2085,1),e=12),
                         "SST"     = NA)
    
    ncfname   = paste0("./Data/Projections/gfdl2080.SST.zip")
    Proj      = nc_open(unzip(ncfname))
    dname     = "sst"                    # Sea Surface Temperature"
    lon       = ncvar_get(Proj, "rlon") 
    nlon      = length(lon)              # number of columns
    lat       = ncvar_get(Proj, "rlat") 
    nlat      = length(lat)              # number of rows
    t         = ncvar_get(Proj, "time")  # time
    nt        = dim(t)                   # months
    
    # The SST layers are on a grid rotated to suit NOAA. So they go from -280 to +80
    map = ncvar_get(Proj, 'tos', 
                    start= c(1,1,1),       
                    count= c(nlon,nlat,1) )
    M       = raster(map)
    Y       = unlist(lapply(c(nlat:1), FUN=function(x){ cellFromCol(M,x)} ))
    M2      = matrix(M[Y], ncol=nlon, nrow=nlat, byrow = TRUE)
    # Which grid cells contain the search limits
    Xlim    = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
    Ylim    = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
    # Convert coordinates betrween 80 and 180 to ncdf grid
    XXlim   = Xlim
    XXlim[XXlim>80] = -180-(180-XXlim[XXlim>80])
    lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    # Get the values for the right cells for all months
    sst = ncvar_get(Proj, 'tos', 
                    start= c(min(lon.col),min(lat.row),1),       
                    count= c(length(lon.col),length(lat.row),nt) )
    # Take the median within each time step
    tempdat$SST = apply(sst, 3, median, na.rm=TRUE)
    
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
geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv") 
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
geo_data = geo_data[geo_data$source=="w",]

# Add columns for temp data
time.steps = seq(as.Date("1950/01/01"), as.Date("2022/12/01"), "months")
nT         = length(time.steps)

# Climate data matrix
clim_data = as.matrix(geo_data[,c(4,5)])
clim_data = cbind(c(1:nrow(clim_data)),clim_data, matrix(NA,nrow=nrow(geo_data),ncol=nT))
clim_data2=clim_data
for(i in 1:nrow(geo_data) ){
  Habitat= geo_data$habitat[i] %in% c("t","f")
  if(Habitat){
    # Extract NCDF as vector
    if(dim(terra::rast(paste0("./data/climate_data/ERA5_LAND/era5land.monthlymean.point",i,".nc")))[1]==1){
      r =  as.vector(terra::rast(paste0("./data/climate_data/ERA5_LAND/era5land.monthlymean.point",i,".nc")))
    } else {
      r = terra::rast(paste0("./data/climate_data/ERA5_LAND/era5land.monthlymean.point",i,".nc"))
      r = apply(as.array(r),c(3), median, na.rm=T)
    }
    # Add to matrix
    clim_data[i,-c(1:3)] = r[1:nT]     # air temperature
    clim_data2[i,-c(1:3)]= r[-c(1:nT)] # skin temperature
  } else {
    # Marine
    # Extract NCDF as array
    r =  as.array(terra::rast(paste0("./data/climate_data/ERA5_LAND/era5.sst.point",i,".nc")))
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
names(clim_data)   = c('Species','Latitude','Longitude',as.character(time.steps))
# Now we can do some simple summaries, like CV
cv                 = function(x){ sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) }
clim_data$cv       = apply(clim_data[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data$sd       = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data$mean     = apply(clim_data[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data$acf_lag1 = apply(clim_data[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data$acf_all  = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data, "./output/climate_data/ERA5_air_and_SST_timeseries.csv")

clim_data2          = as.data.frame(clim_data2)
clim_data2[,1]      = geo_data$species_full
names(clim_data2)   = c('Species','Latitude','Longitude',as.character(time.steps))
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
geo_data = read.csv("Data/Species_LatLong_plusHabitat.csv") 
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
geo_data = geo_data[geo_data$source=="w",]

# Add columns for temp data
time.steps = seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months")
nT         = length(time.steps)

# Climate data matrix
clim_data = as.matrix(geo_data[,c(4,5)])
clim_data = cbind(c(1:nrow(clim_data)),clim_data, matrix(NA,nrow=nrow(geo_data),ncol=nT))
clim_data2=clim_data

for(i in 1:nrow(geo_data) ){
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
names(clim_data)   = c('Species','Latitude','Longitude',as.character(time.steps))
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
names(clim_data2)   = c('Species','Latitude','Longitude',as.character(time.steps))
clim_data2$cv       = apply(clim_data2[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data2$sd       = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data2$mean     = apply(clim_data2[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data2$acf_lag1 = apply(clim_data2[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data2$acf_all  = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data2, "./output/climate_data/gfdl2081-2085_landsurface_and_SST_timeseries.csv")

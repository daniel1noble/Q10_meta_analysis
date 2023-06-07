
rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
library(ncdf4)
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
  
  filelist = unlist(lapply(list.files("./Data/ERA5_LAND/",pattern="point"),FUN=function(x) strsplit(x,"point")[[1]][2]))
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
      path     = "./Data/ERA5_LAND",
      verbose  = FALSE
    )
    
  }
} # Loop over sampled points


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# FUTURE CLIMATIC PROJECTIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
library(maps)
library(ncdf4)
library(raster)

geo_data = read.csv("Data/Species_LatLong_plusHabitat.csv")  # ./data/climate_data/Species_LatLong_plusHabitat.csv
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))

#  > CMIP5 monthly data on single levels
#    https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip5-monthly-single-levels?tab=overview
#
# Although API is available, I downloaded this manually.
# Experiment:RCP 8.5
# Variable:2m temperature, Sea surface temperature, Skin temperature
# Model:CanESM2 (CCCMA, Canada)Ensemble member:r1i1p1
# Period:200601-210012
# Format:Zip file (.zip)

#--------------------------------------------------#
#--------------------------------------------------#

for(i in nrow(geo_data)){
  
  Habitat = geo_data$habitat[i] %in% c("t","f")
  
  ### TERRESTRIAL/ FRESHWATER
  if(Habitat){
    
    ncfname   = "./Data/CanESM2_projection/tas_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc"
    Proj      = nc_open(ncfname)   # 'tas' is "Near-Surface Air Temperature"
    dname     = "tas"   
    lon       = ncvar_get(Proj, "lon") 
    nlon      = length(lon)                  # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)                  # number of rows
    t         = ncvar_get(Proj, "time")      # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                       # monthly averages: seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months"
    tdate     = seq(as.Date("1850/01/01"), as.Date("2100/12/01"), "days")
    tdate     = tdate[t]
    
    tempdat = data.frame("Year"    = unlist(lapply(as.character(tdate), FUN=function(x) strsplit(x,"-")[[1]][1] )),
                         "Month"   = unlist(lapply(as.character(tdate), FUN=function(x) strsplit(x,"-")[[1]][2] )),
                         "Temp"     = NA)
    
    ## World mp
    #map = ncvar_get(Proj, 'tos', 
    #                start= c(1,1,1),
    #                count= c(nlon,nlat,1) )
    #image(map)
    
    # Which grid cells contain the search limits
    Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
    Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
    # Convert coordinates between 0 and -180 to ncdf grid
    XXlim = Xlim
    XXlim[XXlim<0] = 180+abs(c(-180)-XXlim[XXlim<0])
    # Which rows and columns of the matrix do we need?
    lon.col = as.numeric(cut(XXlim, breaks=c(lon,360), labels=c(1:(nlon))))
    lon.col[lon.col>nlon] = nlon
    lon.col = unique(lon.col)
    if(length(lon.col)==1){ lon.count=1 } else {
      if(lon.col[1]>lon.col[2]){
        lon.count = abs(diff(c((max(lon.col)-nlon),min(lon.col))))
      } else {
        lon.count = diff(lon.col)
      }
    }
    lat.row = as.numeric(cut(Ylim, breaks=c(-90,lat,90), labels=c(1:(nlat+1))))
    lat.row[lat.row>nlat] = nlat
    lat.row = unique(lat.row)
    lat.count = ifelse(length(lat.row)==1,1,diff(lat.row))
    
    #lon.col = c(max(which(lon <= XXlim[1])):min(which(lon >= XXlim[2])))
    #lat.row = c(min(which(rev(lat) <= Ylim[1] )):max(which(rev(lat) >= Ylim[2] )))
    
    ## First time slice
    #ncvar_get(Proj, 'tos', 
    #          start= c(min(lon.col),   min(lat.row),   1), 
    #          count= c(lon.count,lat.count,1) )
    
    
    # Use first time slice as check the extent contains data. If not expand 
    # window by half degree to include more adjacent cells.
    repeat{
      if( any(!is.na(ncvar_get(Proj, 'tas', 
                               start= c(min(lon.col),min(lat.row),   1), 
                               count= c(lon.count,lat.count,1) )))){ break() }
      #Xlim   = Xlim + c(-0.5,0.5) ; Xlim[Xlim>180] = 180 ; Xlim[Xlim< -180] = -180
      Xlim[1] = ifelse(Xlim[1]-0.5 < -180,-180,Xlim[1]-0.5)
      Xlim[2] = ifelse(Xlim[2]+0.5 > 180, 180, Xlim[2]+0.5)
      #Ylim   = Ylim + c(-0.5,0.5) ; Ylim[Ylim>90] = 90 ; Ylim[Ylim< -90] = -90
      Ylim[1] = ifelse(Ylim[1]-0.5 < -90,-90,Ylim[1]-0.5)
      Ylim[2] = ifelse(Ylim[2]+0.5 > 90, 90, Ylim[2]+0.5)
      XXlim = Xlim
      XXlim[XXlim<0] = 180+abs(c(-180)-XXlim[XXlim<0])
      lon.col = as.numeric(cut(XXlim, breaks=c(0,lon,360), labels=c(1:(nlon+1))))
      lon.col[lon.col>nlon] = nlon
      lon.col = unique(lon.col)
      if(length(lon.col)==1){ lon.count=1 } else {
        if(lon.col[1]>lon.col[2]){
          lon.count = abs(diff(c((max(lon.col)-nlon),min(lon.col))))
        } else {
          lon.count = diff(lon.col)
        }
      }
      lat.row = as.numeric(cut(Ylim, breaks=c(-90,lat,90), labels=c(1:(nlat+1))))
      lat.row[lat.row>nlat] = nlat
      lat.row = unique(lat.row)
      lat.count = ifelse(length(lat.row)==1,1,diff(lat.row))
    }
    
    x = ncvar_get(Proj, 'tas',
                  start= c(min(lon.col),   min(lat.row),   1), 
                  count= c(lon.count,lat.count,nt) )
    if( (lon.count>1 & lat.count==1) | (lon.count==1 & lat.count>1) ){ x = apply(x, 2, median, na.rm=T) }
    if(lon.count>1 & lat.count>1){ x = apply(x, 3, median, na.rm=T) }
    plot(x-273 ~ tdate, type="l",ylab="Temp", main=paste0("Species",i)) ; abline(h=0)
    
    # Take the median within each time step
    tempdat$Temp = x
    save(tempdat, file=paste0("./Data/CanESM2_projection/CanESM2_airtemp_",i,".RData"))
    
    ##############################################################################
    
    ncfname   = "./Data/CanESM2_projection/ts_Amon_CanESM2_rcp85_r1i1p1_200601-210012.nc"
    Proj      = nc_open(ncfname)    # "ts" is Surface Temperature"
    dname     = "ts"   
    lon       = ncvar_get(Proj, "lon") 
    nlon      = length(lon)                  # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat)                  # number of rows
    t         = ncvar_get(Proj, "time")      # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                       # monthly averages: seq(as.Date("2081/01/01"), as.Date("2085/12/01"), "months"
    
    x = ncvar_get(Proj, 'ts',
                  start= c(min(lon.col),   min(lat.row),   1), 
                  count= c(lon.count,lat.count,nt) )
    # Take the median within each time step
    if( (lon.count>1 & lat.count==1) | (lon.count==1 & lat.count>1) ){ x = apply(x, 2, median, na.rm=T) }
    if(lon.count>1 & lat.count>1){ x = apply(x, 3, median, na.rm=T) }
    lines(x-273 ~ tdate, col="red")
    
    tempdat$Temp = x
    
    save(tempdat, file=paste0("./Data/CanESM2_projection/CanESM2_skintemp_",i,".RData"))
    
    #############################################################################
    
    ### MARINE
  } else {
    
    ncfname   = "./Data/CanESM2_projection/tos_Omon_CanESM2_rcp85_r1i1p1_200601-210012.nc"
    Proj      = nc_open(ncfname)
    dname     = "tos"  # Sea Surface Temperature"
    lon       = ncvar_get(Proj, "lon")          # degrees east 0-365
    nlon      = length(lon) # number of columns
    lat       = ncvar_get(Proj, "lat") 
    nlat      = length(lat) # number of rows
    t         = ncvar_get(Proj, "time")     # units: days since 1850-1-1
    nt        = dim(t)                      # monthly averages: seq(as.Date("1850/01/01"), as.Date("2100/12/01"), "days")
    tdate     = seq(as.Date("1850/01/01"), as.Date("2100/12/01"), "days")
    tdate     = tdate[t]
    
    tempdat = data.frame("Year"    = unlist(lapply(as.character(tdate), FUN=function(x) strsplit(x,"-")[[1]][1] )),
                         "Month"   = unlist(lapply(as.character(tdate), FUN=function(x) strsplit(x,"-")[[1]][2] )),
                         "SST"     = NA)
    
    ## World mp
    #map = ncvar_get(Proj, 'tos', 
    #                start= c(1,1,1),       # 4 dimensions: longitude, latitude, 'expver' (variable), time
    #                count= c(nlon,nlat,1) )
    #image(map)
    
    # Which grid cells contain the search limits
    Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
    Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
    # Convert coordinates between 0 and -180 to ncdf grid
    XXlim = Xlim
    XXlim[XXlim<0] = 180+abs(c(-180)-XXlim[XXlim<0])
    # Which rows and columns of the matrix do we need?
    lon.col = as.numeric(cut(XXlim, breaks=c(0,lon,360), labels=c(1:(nlon+1))))
    lon.col[lon.col>nlon] = nlon
    lon.col = unique(lon.col)
    if(length(lon.col)==1){ lon.count=1 } else {
      if(lon.col[1]>lon.col[2]){
        lon.count = abs(diff(c((max(lon.col)-nlon),min(lon.col))))
      } else {
        lon.count = diff(lon.col)
      }
    }
    lat.row = as.numeric(cut(Ylim, breaks=c(-90,lat,90), labels=c(1:(nlat+1))))
    lat.row[lat.row>nlat] = nlat
    lat.row = unique(lat.row)
    lat.count = ifelse(length(lat.row)==1,1,diff(lat.row))
    
    # Use first time slice as check the extent contains data
    repeat{
      if( any(!is.na(ncvar_get(Proj, 'tos', 
                            start= c(min(lon.col),   min(lat.row),   1), 
                            count= c(lon.count, lat.count,1)) )) ){ break() }
      #Xlim   = Xlim + c(-0.5,0.5) ; Xlim[Xlim>180] = 180 ; Xlim[Xlim< -180] = -180
      Xlim[1] = ifelse(Xlim[1]-0.5 < -180,-180,Xlim[1]-0.5)
      Xlim[2] = ifelse(Xlim[2]+0.5 > 180, 180, Xlim[2]+0.5)
      #Ylim   = Ylim + c(-0.5,0.5) ; Ylim[Ylim>90] = 90 ; Ylim[Ylim< -90] = -90
      Ylim[1] = ifelse(Ylim[1]-0.5 < -90,-90,Ylim[1]-0.5)
      Ylim[2] = ifelse(Ylim[2]+0.5 > 90, 90, Ylim[2]+0.5)
      
      XXlim = Xlim
      XXlim[XXlim<0] = 180+abs(c(-180)-XXlim[XXlim<0])
      lon.col = as.numeric(cut(XXlim, breaks=c(0,lon,360), labels=c(1:(nlon+1))))
      lon.col[lon.col>nlon] = nlon
      lon.col = unique(lon.col)
      if(length(lon.col)==1){ lon.count=1 } else {
        if(lon.col[1]>lon.col[2]){
          lon.count = abs(diff(c((max(lon.col)-nlon),min(lon.col))))
        } else {
          lon.count = diff(lon.col)
        }
      }
      lat.row = as.numeric(cut(Ylim, breaks=c(-90,lat,90), labels=c(1:(nlat+1))))
      lat.row[lat.row>nlat] = nlat
      lat.row = unique(lat.row)
      lat.count = ifelse(length(lat.row)==1,1,diff(lat.row))
    }
    
    x = ncvar_get(Proj, 'tos',
                  start= c(min(lon.col),   min(lat.row),   1), 
                  count= c(lon.count, lat.count, nt) )
    
    if( (lon.count>1 & lat.count==1) | (lon.count==1 & lat.count>1) ){ x = apply(x, 2, median, na.rm=T) }
    if(lon.count>1 & lat.count>1){ x = apply(x, 3, median, na.rm=T) }
    plot(x-273 ~ tdate, type="l",ylab="SST", main=paste0("Species",i)) ; abline(h=0)
    
    # Take the median within each time step
    tempdat$SST = x
    
    save(tempdat, file=paste0("./Data/CanESM2_projection/CanESM2_sst_",i,".RData"))
    
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
names(clim_data)   = c('Species','Latitude','Longitude',as.character(time.steps))
# Now we can do some simple summaries, like CV
cv                 = function(x){ sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) }
clim_data$cv       = apply(clim_data[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data$sd       = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data$mean     = apply(clim_data[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data$acf_lag1 = apply(clim_data[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data$acf_all  = apply(clim_data[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data, "./Data/ERA5_LAND/ERA5_air_and_SST_timeseries.csv")

clim_data2          = as.data.frame(clim_data2)
clim_data2[,1]      = geo_data$species_full
names(clim_data2)   = c('Species','Latitude','Longitude',as.character(time.steps))
clim_data2$cv       = apply(clim_data2[,c(1:nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data2$sd       = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data2$mean     = apply(clim_data2[,c(1:nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data2$acf_lag1 = apply(clim_data2[,c(1:nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data2$acf_all  = apply(clim_data2[,c(1:nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data2, "./Data/ERA5_LAND/ERA5_landsurface_timeseries.csv")

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
# Remove missing lat and long
geo_data = geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))
# Add columns for temp data
time.steps = seq(as.Date("2005/12/01"), as.Date("2100/11/01"), "months")
nT         = length(time.steps)

# Climate data matrix
clim_data = as.matrix(geo_data[,c(4,5)])
clim_data = cbind(c(1:nrow(clim_data)),clim_data, matrix(NA,nrow=nrow(geo_data),ncol=nT))
clim_data2=clim_data

for(i in which(geo_data$source=="w") ){
  Habitat= geo_data$habitat[i] %in% c("t","f")
  if(Habitat){
    # Extract NCDF as vector
    r =  load(paste0("./Data/CanESM2_projection/CanESM2_airtemp_",i,".RData"))
    # Add to matrix
    clim_data[i,-c(1:3)] = tempdat$Temp    # air temperature
    r =  load(paste0("./Data/CanESM2_projection/CanESM2_skintemp_",i,".RData"))
    # Add to matrix
    clim_data2[i,-c(1:3)] = tempdat$Temp    # skin temperature
  } else {
    # Extract NCDF as array
    load(paste0("./Data/CanESM2_projection/CanESM2_sst_",i,".RData"))
    # Add to matrix
    clim_data[i,-c(1:3)] = tempdat$SST
    clim_data2[i,-c(1:3)] = tempdat$SST
  }
  if(exists('r')){rm(r)}
}
clim_data          = as.data.frame(clim_data)
clim_data[,1]      = geo_data$species_full
names(clim_data)   = c('Species','Latitude','Longitude',as.character(time.steps))

# Now we can do some simple summaries, like CV for the last decade (specified using the '(nT-120)')
cv                 = function(x){ sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) }
clim_data$cv       = apply(clim_data[,c((nT-120):nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data$sd       = apply(clim_data[,c((nT-120):nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data$mean     = apply(clim_data[,c((nT-120):nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data$acf_lag1 = apply(clim_data[,c((nT-120):nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data$acf_all  = apply(clim_data[,c((nT-120):nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data, "./Data/CanESM2_projection/CanESM2_air_and_SST_timeseries.csv")

clim_data2          = as.data.frame(clim_data2)
clim_data2[,1]      = geo_data$species_full
names(clim_data2)   = c('Species','Latitude','Longitude',as.character(time.steps))
clim_data2$cv       = apply(clim_data2[,c((nT-120):nT)+3], 1, function(x) cv(x)) # Coefficient of variation
clim_data2$sd       = apply(clim_data2[,c((nT-120):nT)+3], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data2$mean     = apply(clim_data2[,c((nT-120):nT)+3], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data2$acf_lag1 = apply(clim_data2[,c((nT-120):nT)+3], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data2$acf_all  = apply(clim_data2[,c((nT-120):nT)+3], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data2, "./Data/CanESM2_projection/CanESM2_landsurface_and_SST_timeseries.csv")

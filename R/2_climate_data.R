rm(list=ls())
library(tidyr)
library(ecmwfr)
library(terra)
#library(ncdf4)
library(maps)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# SURVEY LOCATIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv")

# First remove missing lat and long
geo_data <- geo_data %>% dplyr::filter(!is.na(lat) & !is.na(long))

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
      
      if(i==253){ Ylim=c(21.44,21.46) }
      
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
} 

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



for(i in 1:nrow(geo_data)){
  
  filelist = unlist(lapply(dir("./Data/ERA5_LAND/",pattern="proj"),FUN=function(x) strsplit(x,"proj")[[1]][2]))
  filelist = as.numeric(gsub("\\.zip","",filelist))
  
  if( !i %in% filelist ){
    
    Habitat= geo_data$habitat[i] %in% c("t","f")
    
    if(Habitat){
      # If the species is Terrestrial or Freshwater we refer our request to the ERA5-LAND dataset
      
      # Maintain a tightly focused spatial extent
      Xlim   = round(round(geo_data$long[i],2) + c(-0.01,0.01),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.01,0.01),2)

      request <- list(
        ensemble_member = "r1i1p1",
        format = "zip",
        area = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        experiment = "rcp_4_5",
        variable = c("2m_temperature", "skin_temperature"),
        model = "gfdl_esm2m",
        period = "208101-208512",
        dataset_short_name = "projections-cmip5-monthly-single-levels",
        target = paste0("gfdl2080.proj",i,".zip") 
      )
      
      
    } else {
      # Still broader spatial extent
      Xlim   = round(round(geo_data$long[i],2) + c(-0.5,0.5),2)
      Ylim   = round(round(geo_data$lat[i],2) + c(-0.5,0.5),2)
      
      request <- list(
        ensemble_member = "r1i1p1",
        format = "zip",
        area = c(Ylim[1], Xlim[1], Ylim[2], Xlim[2]),
        experiment = "rcp_4_5",
        variable = c("sea_surface_temperature"),
        model = "gfdl_esm2m",
        period = "208101-208512",
        dataset_short_name = "projections-cmip5-monthly-single-levels",
        target = paste0("gfdl2080.proj",i,".zip") 
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
    #repeat{
    #  if(file.exists(ncfile)){ 
    #    # Plot the data
    #    r =  try(terra::rast(ncfile))
    #    plot(as.vector(r), type="o", main=paste("Projection ",i))
    #    break() 
    #  } else { sleep(300) }
    #}
    
  }
} # Loop over sampled points

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# REFORMAT 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Open NetCDF

# Take Median of SST values


# UNZIP PROJECTION FILES

# Read NetCDF

########################### **ALEX. BELOW IS THE CODE WE USED TO EXTRACT THE CLIMATE DATA AND CALCULATE the 1) variance and autocorrelation from past to current and then 2) get the future climate projections. ** I guess you just need to modify below. The path name to the data on GitHub is: "data/climate_data/ERA5_LAND/" which is the folder where all teh files you shared live. 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Previous Projections
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Loop over sampled points

# Identify which coarse ERA5 cell these lat/longs fall within:
geo_data$ERA5col     = geo_data$long
xid                  = which((!is.na(geo_data$ERA5col)) & (geo_data$ERA5col<0) )
geo_data$ERA5col[xid]= (180-abs(geo_data$ERA5col[xid]))+180   # Longitude is stored only as positive degrees east, no negatives for west.
geo_data$ERA5col     = cut(geo_data$ERA5col, seq(0,360,0.25), labels=c(1:1440))

geo_data$ERA5row     = geo_data$lat                                          # Curiously latitude does have negatives
geo_data$ERA5row     = as.numeric(cut(geo_data$ERA5row, seq(-90,90,0.25), labels=c(1:720)))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Open NETCDF file
ncfname   = paste0("./data/climate_data/ERA5/ERA5_LST_and_SST_1979-present.nc")
ERA5      = nc_open(ncfname)
dname     = "t2m"
lon       = ncvar_get(ERA5, "longitude") # ncvar_get(ERA5.air, "lon")
nlon      = length(lon) # number of columns
lat       = ncvar_get(ERA5, "latitude") # ncvar_get(ERA5.air, "lat", verbose = F)
nlat      = length(lat) # number of rows
t         = ncvar_get(ERA5, "time")                 # units: hours since 1900-01-01 00:00:00.0
nt        = dim(t)                                  # monthly averages

# Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00") 

# Full layer, at first time step
map = ncvar_get(ERA5, dname, 
                start= c(1,1,1,1),       # 4 dimensions: longitude, latitude, 'expver' (variable), time
                count= c(nlon,nlat,1,1) )
image(map)                               # Its both, upside down, back to front, and centered on 180' west...

# Convert to raster for somewhat easier plotting
library(raster)
map2 = t(map)
# Suffle the "WEST" to the left
map2 = cbind(map2[,721:1440],map2[,1:720])
map.raster = raster(map2, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=4326)
plot(map.raster)

# Note in the SST version the land is masked out, but air temperature is global.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

geo_data <- na.omit(geo_data)

# Create unique col that will take rowname because of repeat species
geo_data$row <- paste0(geo_data$species_full, "_",geo_data$lat, ",", geo_data$long)
clim_data_matrix <- data.frame(matrix(NA, nrow = dim(geo_data)[1], ncol = 522))
names <- gsub("* [10][10].+", "", t.Date)
colnames(clim_data_matrix) <- c("species", "lat", "long", names)

for(i in 1:nrow(geo_data)){
  
  # Annual time series in one cell:
  point.timeseries = ncvar_get(ERA5, dname, 
                               start= c(geo_data$ERA5col[i],geo_data$ERA5row[i],1,1),
                               count= c(1,1,1,nt) )
  clim_data_matrix[i,1] <- geo_data$species_full[i]
  clim_data_matrix[i,2] <- geo_data$lat[i]
  clim_data_matrix[i,3] <- geo_data$long[i]
  clim_data_matrix[i,c(4:522)] <- point.timeseries
  plot(point.timeseries ~ t.Date, type="l")   # point.timeseries ~ c(1:nt)
}

# Now we can do some simple summaries, like CV
cv <- function(x){
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}
clim_data_matrix$cv <- apply(clim_data_matrix[,4:522], 1, function(x) cv(x)) # Coefficient of variation
clim_data_matrix$sd <- apply(clim_data_matrix[,4:522], 1, function(x) sd(x, na.rm = TRUE)) # sd
clim_data_matrix$mean <- apply(clim_data_matrix[,4:522], 1, function(x) mean(x, na.rm = TRUE)) # mean
clim_data_matrix$acf_lag1 <- apply(clim_data_matrix[,4:522], 1, function(x) acf(as.numeric(x), lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2,1,1]) # autocorrelation for lag w
clim_data_matrix$acf_all <- apply(clim_data_matrix[,4:522], 1, function(x) sd(acf(as.numeric(x), plot = FALSE, na.action = na.pass)$acf)) # deviation in autocorrelation values...this is probably not that useful

write.csv(clim_data_matrix, "./output/climate_data/temp_timeseries.csv")

r1 <- acf(as.numeric(clim_data_matrix[1,4:522]), lag.max = 522 - 4, plot = FALSE, na.action = na.pass)$acf[-1]
lag <- c(1:518)
plot(r1 ~ lag)

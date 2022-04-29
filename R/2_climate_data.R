###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                       ERA5 Reanalysis 1 CLIMATE DATA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

# This version doesn't actively download point estimates of temperature. 
# Instead a NetCDF for ERA5 was downloaded for 1979-present.

# See Word doc: Data/Downloading ERA5 historic climate data.docx

# Note that the data is stored in Kelvin and transformed.
# 2m surface temperature
#     scale_factor: 0.00176319797961702
#     add_offset: 257.703670097788
# Sea Surface Temperature
#     scale_factor: 0.00061697729459776
#     add_offset: 289.46490147229

# Time is recorded in hours since 1900-01-01 00:00:00.0

rm(list=ls())
library(ncdf4)

geo_data = read.csv("./data/climate_data/Species_LatLong_plusHabitat.csv")

spp  = unique(geo_data$species_full)

for(i in 1:nrow(geo_data)){
  
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
  nt        = dim(t)                  # monthly averages
  # Create a copy of the data as an array in R (which unlike the netcdf, makes
  # it possible to index multiple locations at once)
  X         = ncvar_get(ERA5, dname, start= c(1,1,1,1), count= c(nlon,nlat,1,nt) )
  nc_close(ERA5)
  
  # Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
  t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00") 
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  monthly.temp.data = matrix(NA, nrow=nt, ncol=nrow(geo_data),
                             dimnames=list(c(1:nt)))
  # This is now an array that is formatted as: X[longitude, latitude, temp in Kelvin]
  t <- with(na.omit(geo_data), X[long, lat, ])
  
  
  
  for(j in 1:nt){ monthly.temp.data[j,] = X[,,j][spXY] }
  
  # Annual mean and SD
  annual.temp.means = aggregate(monthly.temp.data, by=list(format(t.Date,"%Y")),mean,na.rm=T)
  annual.temp.sd    = aggregate(monthly.temp.data, by=list(format(t.Date,"%Y")),sd,na.rm=T)
  
  
  # AT THIS POINT YOU CAN SUMMARISE ACROSS ALL COLUMNS TO GET VALUES FOR 
  # ALL LOCATIONS AND HENCE THE SPECIES AS A WHOLE
  
  
  ## Save file for each species
  # write.csv(Tdat, paste0("Output/ERA5/",sp,"_temperature_history.csv"))
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
  rm(X, monthly.temp.data, annual.temp.means, annual.temp.sd, sdat, SspXY, 
     t.Date)
  
} #  End of species loop

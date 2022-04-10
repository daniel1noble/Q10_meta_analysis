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
#library(sf)
#library(raster)

if(!file.exists("./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat_and_ERA5_rowcol.csv")){
  gbif     = read.csv("./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat.csv")
  gbif$Fid = 1:nrow(gbif)
  #gbif_shp = st_as_sf(gbif, coords = c("longitude", "latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  gbif$ERA5col = gbif$longitude
  gbif$ERA5col[gbif$ERA5col<0] = (180-abs(gbif$ERA5col[gbif$ERA5col<0]))+180
  gbif$ERA5col = cut(gbif$longitude, seq(0,360,0.25), labels=c(1:1440))
  
  gbif$ERA5col = gbif$longitude
  gbif$ERA5col[gbif$ERA5col<0] = (180-abs(gbif$ERA5col[gbif$ERA5col<0]))+180
  gbif$ERA5col = as.numeric(cut(gbif$longitude, seq(0,360,0.25), labels=c(1:1440)))
  
  ncfname   = "./data/climate_data/ERA5/ERA5_LST_and_SST_1979-present.nc"
  ERA5.air  = nc_open(ncfname)
  print(ERA5.air)
  dname     = "t2m"
  lon       = ncvar_get(ERA5.air, "longitude")
  head(lon)                                     # ERA5 data is at 0.25' resolution
  nlon      = length(lon) # number of columns
  lat       = ncvar_get(ERA5.air, "latitude", verbose = F)
  head(lat)
  nlat      = length(lat) # number of rows
  t         = ncvar_get(ERA5.air, "time")
  head(t)
  nt        = dim(t) # Monthly 
  # Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
  t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00")
  summary(t.Date)
  
  for(i in 1:nrow(gbif)){ 
    gbif$ERA5row[i] = max(which(gbif$lat[i]<lat))          
    gbif$ERA5col[i] = max(which((gbif$long[i]+180)>lon))   # Add 180 to the longitude values because file measures location in eastings up to 360.
    
  }
  head(gbif)
  
  # Full layer, at first time step
  map = ncvar_get(ERA5.air, dname, 
                  start= c(1,1,1,1),       # 4 dimensions: longitude, latitude, 'expver' (variable), time
                  count= c(nlon,nlat,1,1) )
  image(map)
  
  # Annual time series in one cell:
  point.timeseries = ncvar_get(ERA5.air, dname, 
                               start= c(1,1,1,1),
                               count= c(1,1,1,nt) )
  plot(point.timeseries ~c(1:nt), type="l")
  
  rm(map, point.timeseries)
  
  write.csv(gbif, "./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat_and_ERA5_rowcol.csv")
  
} else {
  
  gbif = read.csv("./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat_and_ERA5_rowcol.csv")
  
  spp  = unique(gbif$Species)
  
  for(sp in spp){
    
    # Take subset of GBIF data for a specific species
    sdat = gbif[gbif$Species == sp,]
    spXY = as.matrix(sdat[,c("ERA5col","ERA5row")])
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    # Open NETCDF file
    ncfname   = paste0("./data/climate_data/ERA5/ERA5_LST_and_SST_1979-present.nc")
    ERA5.air  = nc_open(ncfname)
    dname     = "t2m"
    lon       = ncvar_get(ERA5.air, "longitude") # ncvar_get(ERA5.air, "lon")
    nlon      = length(lon) # number of columns
    lat       = ncvar_get(ERA5.air, "latitude") # ncvar_get(ERA5.air, "lat", verbose = F)
    nlat      = length(lat) # number of rows
    t         = ncvar_get(ERA5.air, "time")                 # units: hours since 1900-01-01 00:00:00.0
    nt        = dim(t)                  # monthly averages
    # Create a copy of the data as an array in R (which unlike the netcdf, makes
    # it possible to index multiple locations at once)
    X         = ncvar_get(ERA5.air, dname, start= c(1,1,1,1), count= c(nlon,nlat,1,nt) )
    nc_close(ERA5.air)
    
    # Multiply 't' by 3600 so it becomes seconds since 1900 instead of hours.
    t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1900-01-01 00:00:00") 
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    monthly.temp.data = matrix(NA, nrow=nt, ncol=nrow(spXY),
                               dimnames=list(c(1:nt), sdat$Fid))
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
  
  
}




















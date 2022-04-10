###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                       NCEP Reanalysis 1 CLIMATE DATA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

# This version doesn't actively download point estimates of temperature. 
# Instead NCDFs for each year 1960-2000 were pre-downloaded and I extract what's available.

rm(list=ls())
library(ncdf4)
#library(sf)
#library(raster)

if(!file.exists("./output/data/climate_data/GBIF/Terrestrial_GBIF_occurrences_plus_habitat_and_NCEP_rowcol.csv")){
  gbif     = read.csv("./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat.csv")
  gbif$Fid = 1:nrow(gbif)
  #gbif_shp = st_as_sf(gbif, coords = c("longitude", "latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  gbif$NCEPcol = gbif$longitude
  gbif$NCEPcol[gbif$NCEPcol<0] = (180-abs(gbif$NCEPcol[gbif$NCEPcol<0]))+180
  gbif$NCEPcol = cut(gbif$longitude, seq(0,360,2.5), labels=c(1:144))
  
  gbif$NCEPcol = gbif$longitude
  gbif$NCEPcol[gbif$NCEPcol<0] = (180-abs(gbif$NCEPcol[gbif$NCEPcol<0]))+180
  gbif$NCEPcol = as.numeric(cut(gbif$longitude, seq(0,360,2.5), labels=c(1:144)))
  
  ncfname   = "./data/climate_data/NCEP_Reanalysis 1/air.sig995.1960.nc"
  NCEP.air  = nc_open(ncfname)
  print(NCEP.air)
  dname     = "air"
  lon       = ncvar_get(NCEP.air, "lon")
  head(lon)                                     # NCEP data is at 2.5' resolution
  nlon      = length(lon) # number of columns
  lat       = ncvar_get(NCEP.air, "lat", verbose = F)
  head(lat)
  nlat      = length(lat) # number of rows
  t         = ncvar_get(NCEP.air, "time")
  head(t)
  nt        = dim(t) # 4 times for every day of the year
  
  for(i in 1:nrow(gbif)){ 
    gbif$NCEProw[i] = max(which(gbif$lat[i]<lat))          
    gbif$NCEPcol[i] = max(which((gbif$long[i]+180)>lon))   # Add 180 to the longitude values because file measures location in eastings up to 360.
    
  }
  head(gbif)
  
  # Full layer, at first time step
  map = ncvar_get(NCEP.air, dname, 
                  start= c(1,1,1),
                  count= c(nlon,nlat,1) )
  image(map)
  
  # Annual time series in one cell:
  point.timeseries = ncvar_get(NCEP.air, dname, 
                               start= c(1,1,1),
                               count= c(1,1,nt) )
  plot(point.timeseries ~c(1:nt), type="l")
  
  write.csv(gbif, "data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat_and_NCEP_rowcol.csv")
  
} else {
  
  gbif = read.csv("./data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat_and_ERA5_rowcol.csv")
  
  spp  = unique(gbif$Species)
  
  for(sp in spp){
    
    # Take subset of GBIF data for a specific species
    sdat = gbif[gbif$Species == sp,]
    spXY = as.matrix(sdat[,c("NCEPcol","NCEProw")])
    
    
    for(j in 1960:2000){
      
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
      # Open NETCDF file
      ncfname   = paste0("./data/climate_data/NCEP_Reanalysis_1/air.sig995.",j,".nc")
      NCEP.air  = nc_open(ncfname)
      dname     = "air"
      lon       = ncvar_get(NCEP.air, "lon")
      nlon      = length(lon) # number of columns
      lat       = ncvar_get(NCEP.air, "lat", verbose = F)
      nlat      = length(lat) # number of rows
      t         = ncvar_get(NCEP.air, "time")                 # units: hours since 1800-01-01 00:00:0.0
      nt        = dim(t) # 4 times for every day of the year
      X         = ncvar_get(NCEP.air, dname, start= c(1,1,1), count= c(nlon,nlat,nt) )
      nc_close(NCEP.air)
      
      # Multiply 't' by 3600 so it becomes seconds since 1800 instead of hours.
      t.Date    = as.POSIXct(as.numeric(t)*3600, origin="1800-01-01 00:00:00") 
      
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
      
      # Create grid "map" of annual means
      temp.data = apply(X[,,],c(1,2), mean,na.rm=T)
      # Extract values for each species location
      temp.data = temp.data[spXY]
      # Repeat for all months
      for(q in unique(format(t.Date, "%m"))){
        temp.data = cbind(temp.data, apply(X[,,format(t.Date,"%m")==q],c(1,2),mean,na.rm=T)[spXY] ) 
      }
      # And add SD
      temp.data = cbind(temp.data, apply(X[,,],c(1,2), sd,na.rm=T)[spXY] )
      for(q in unique(format(t.Date, "%m"))){
        temp.data = cbind(temp.data, apply(X[,,format(t.Date,"%m")==q],c(1,2),sd,na.rm=T)[spXY] )
      }
      colnames(temp.data) = c(paste0("Temp.mean_",j),
                              paste0("Temp.mean_",j,"-",unique(format(t.Date, "%m"))),
                              paste0("Temp.SD_",j),
                              paste0("Temp.SD_",j,"-",unique(format(t.Date, "%m"))) )
      
      # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
      
      if(exists("Tdat")){ Tdat = cbind(Tdat, temp.data) } else { Tdat = temp.data }
      
      rm(temp.data, X, ncfname, q, t.Date)
      print(j)
      
    } # End of year loop
    
    # Save file of temperatures for each species
    rownames(Tdat) = sdat$Fid
    write.csv(Tdat, paste0("./output/climate_data/ERA5/",sp,"_temperature_history.csv"))
    
  } #  End of species loop
  
  
}




















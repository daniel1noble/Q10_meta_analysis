###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                   NCEP REANLYSIS2 CLIMATE DATA DOWNLOAD
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

# NCEP Reanlysis
# https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html


rm(list=ls())

library(raster)
library(RNCEP)

gbif = read.csv("data/climate_data/Terrestrial_GBIF_occurrences_plus_habitat.csv")

# Work through species
for(spp in unique(gbif$Species)){
  
  if(file.exists(paste0("output/climate_data/RNCEP/",spp,"_clim_stats.RData"))){
    load(paste0("output/climate_data/RNCEP/land/",spp,"_clim_stats.RData")) 
    sites               = gbif_microclimate[[2]]
    annual.thermal.data = gbif_microclimate[[3]]
    s2                  = gbif_microclimate[[4]]
    rm(gbif_microclimate)
    
  } else {
    # Create matrix to hold thermal data for this species
    annual.thermal.data = matrix(NA, ncol=30)
    colnames(annual.thermal.data) = c("Fid","Latitude","Longitude","Year",
                                      "Annual.mean",sprintf("Mean%02d",c(1:12)),
                                      "Annual.sd",sprintf("SD%02d",c(1:12)) )
    
    # Choose  site at random, and then each next site as the furthest from those selected
    sites.xy = gbif[which(gbif$Species==spp),c("longitude","latitude")]
    plot(sites.xy, col="white", las=1)
    sites    = which(gbif$Species==spp)
    s1       = sample(sites, 1)
    sites    = sites[!sites %in% s1]
    for(i in 1:length(sites)){
      if( length(s1)>250 ){    x1 = sample(s1,250) } else {   x1=s1 }
      if( length(sites)>250 ){ x2 = sample(sites,250)} else { x2=sites }
      x3    = pointDistance(sites.xy[s1,], sites.xy[x2,], lonlat = TRUE, allpairs=TRUE)/1000
      if(length(x1)>1 & length(x2)>1){ if(dim(x3)[1]>1){ x3 = apply(x3,2,min) } }
      s1    = c(s1, x2[which.max(x3)]) 
      sites = sites[!sites %in% s1]
      text(sites.xy[s1[length(s1)],], labels=i)
    }
    sites = s1
    rm(s1,x1,x2,x3)
    
    gbif_microclimate = list("Species"     = spp,
                             "XY"          = sites.xy, 
                             "Site_order"  = sites,
                             "Microclimate"= annual.thermal.data,
                             "Time_taken"  = NA)
    
  }
  
  # Now cycle through sites 'S'
  for(S in sites){
    
    s1 = Sys.time()
    
    for(Y in c(1981:2020)){
      
      temps = NCEP.interp(variable= 'air.sig995', 
                         level   = 'surface',
                         lat     = sites.xy[S,2], 
                         lon     = sites.xy[S,1], 
                         dt=seq(as.POSIXlt(paste0(Y,"-01-01 12:00:00")),as.POSIXlt(paste0(Y,"-12-31 12:00:00")),"1 day"),
                         reanalysis2 = TRUE, 
                         keep.unpacking.info=TRUE) # Not sure what it's for but manual says it runs faster when true
      
      s1 = Sys.time()
      temp = NCEP.gather(variable = 'air.sig995', 
                  level           = 'surface', 
                  months.minmax   = c(1,12), 
                  years.minmax    = c(1948,1949),
                  lat.southnorth  = c( floor(min(sites.xy[,2])), ceiling(max(sites.xy[,2])) ), 
                  lon.westeast    = c( floor(min(sites.xy[,1])), ceiling(max(sites.xy[,1])) ),  
                  reanalysis2     = FALSE,
                  return.units    = TRUE, 
                  status.bar      =TRUE)
      s2 = s1 - Sys.time()
      s2
      
      dimnames(temp)
      
      # Annual and monthly means of DEM 'r'
      r.means = mean(apply(temps$temps[,,], 3, mean,na.rm=T))  # Start with annual
      for(j in unique(months(temps$tme)) ){ r.means = c(r.means, mean(apply(temps$temps[,,months(temps$tme)==j],3,mean,na.rm=T)) )  }
      
      # Annual and monthly SDs of mean temp of DEM 'r'
      r.sd = sd(apply(temps$temps[,,], 3, mean,na.rm=T))       # Note the SD is after we have averaged across the 5x5 cells.
      for(j in unique(months(temps$tme)) ){ r.sd = c(r.sd, sd(apply(temps$temps[,,months(temps$tme)==j],3,mean,na.rm=T)) )  }
      
      # Add to matrix
      annual.thermal.data = rbind(annual.thermal.data, 
                                  c(gbif$Fid[S],gbif$latitude[S], gbif$longitude[S], 
                                    Y, r.means, r.sd) )
      
      rm(temps, r.means, r.sd)
      dev.off()
      print(Y)
    }
    # Add time taken to list
    if(exists("s2")){ s2 = c(s2,s1 - Sys.time()) } else { s2 = s1 - Sys.time() }
    
    # Save
    gbif_microclimate = list("Species"=spp,"XY"=sites.xy, "Site_order"=sites,"Microclimate"=annual.thermal.data,"Time_taken"=s2)
    save(gbif_microclimate, file=paste0("output/climate_data/RNCEP/land/",spp,"_microclimate_stats.RData"))
    
  } # Site loop
  
} # Species loop
















###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#               MICROCLIMATE OF TERRESTRIAL AND FRESHWATER TAXA
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################


rm(list=ls())
library(raster)

spp.list = read.csv("q10_meta-master/data/species_list_Q10.csv")
spp      = spp.list$species_full
spp[71]  = "Enithares"                 # Switch 'Enithares sp' to whole genus
spp[73]  = "Euchemotrema leaii"        # Synonym: Euchemotrema leai
spp[133] = "Merlangius merlangus"      # Synonym: Merlanguis merlangus
spp[175] = "Pinctada mazatlanica"      # Synonym: Pinctada mazatlantica
spp[202] = "Hoplobatrachus tigerinus"  # Synonym: Rana tigrina
spp[203] = "Lithobates virgatipes"     # Synonym: Rana vergatipes
spp[239] = "Zoarces viviparus"         # Synonym: Zoarces viviparous

widedat  = read.csv("q10_meta-master/data/CombinedData_wide.csv")
widedat$spp_names = paste(widedat$genus,widedat$species)
habitat  = widedat$habitat[match(spp.list$species_full, gsub("_"," ",widedat$spp_names))]

# Change habitat of Shortnose Sturgeon (Acipenser brevirostrum) to freshwater
habitat[4] = "f"
widedat$habitat[match("Acipenser brevirostrum", widedat$spp_names)] = "f"


#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
#     Coordinates of Q10 sites by habitat
#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #

# Try to fill some of the location rows missing data
widedat[widedat$geo_location %in% "Coruna, Spain",         c("lat","long")] = c(43.425230, -8.414665)   # Marine sp. so placed point to north
widedat[widedat$geo_location %in% "Newhay, Yorkshire UK",  c("lat","long")] = c(53.769377, -0.995573)
widedat[widedat$geo_location %in% "Ontario,CA, USA",       c("lat","long")] = c(34.035178, -117.606330)
widedat[widedat$geo_location %in% "Woods Hole, MA USA",    c("lat","long")] = c(41.507052, -70.633576)  # Marine sp. so placed point to east.
widedat[widedat$geo_location %in% "Vancouver",             c("lat","long")] = c(49.361242, -123.032042)
widedat[widedat$geo_location %in% "Osaka, Japan",          c("lat","long")] = c(34.614853, 135.664364)

# Remove rows with no lat/long
widedat = widedat[!(is.na(widedat$lat) & is.na(widedat$long)),]

# Subset by habitat
q10_terr = widedat[widedat$habitat=="t",c("spp_names","lat","long","habitat","geo_location")]
q10_fresh = widedat[widedat$habitat=="f",c("spp_names","lat","long","habitat","geo_location")]
q10_mar = widedat[widedat$habitat=="m",c("spp_names","lat","long","habitat","geo_location")]

# Quite a lot of redundant rows so I will shorten these
table(duplicated(q10_terr[,c("lat","long")]))
table(duplicated(q10_fresh[,c("lat","long")]))
table(duplicated(q10_mar[,c("lat","long")]))

q10_terr  = q10_terr[!duplicated(q10_terr[,c("lat","long")]),]
q10_fresh = q10_fresh[!duplicated(q10_fresh[,c("lat","long")]),]
q10_mar   = q10_mar[!duplicated(q10_mar[,c("lat","long")]),]

names(q10_terr)[1:3]  = c("Species","latitude","longitude")
names(q10_fresh)[1:3] = c("Species","latitude","longitude")
names(q10_mar)[1:3]   = c("Species","latitude","longitude")

# Save
#write.csv(q10_terr,  "Data/Land/Terrestrial_Q10_sites.csv", row.names = F)
#write.csv(q10_fresh, "Data/Land/Freshwater_Q10_sites.csv", row.names = F)
#write.csv(q10_mar,   "Data/SST/Marine_Q10_sites.csv", row.names = F)


#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #
#     Coordinates of GBIF sites by habitat
#  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  #

gbif             = read.csv(paste0("Data/GBIF/All_GBIF_occurrences.csv"))
names(gbif)[2:3] = c("longitude","latitude")
gbif$habitat     = habitat[match(gbif$Species, gsub(" ","_",spp))]

# Subsets
gbif_terr        = gbif[gbif$habitat=="t",1:3] # Take terrestrial subset for MODIS LCM extraction
gbif_fresh       = gbif[gbif$habitat=="f",1:3] # Take freshwater subset (also for MODIS LCM extraction)
gbif_mar         = gbif[gbif$habitat=="m",1:3] # Take marine subset to extract NOAA SST data

gbif_terr$Fid    = c(1:nrow(gbif_terr))
gbif_fresh$Fid   = c(1:nrow(gbif_fresh))
gbif_mar$Fid     = c(1:nrow(gbif_mar))


#write.csv(gbif_terr,  "Data/GBIF/Terrestrial_GBIF_occurrences.csv", row.names = F)

#write.csv(gbif_fresh, "Data/GBIF/Freshwater_GBIF_occurrences.csv", row.names = F)
#write.csv(gbif_fresh[1:160000,], "Data/GBIF/Freshwater_GBIF_occurrences_1.csv", row.names = F)
#write.csv(gbif_fresh[160001:320000,], "Data/GBIF/Freshwater_GBIF_occurrences_2.csv", row.names = F)
#write.csv(gbif_fresh[320001:480000,], "Data/GBIF/Freshwater_GBIF_occurrences_3.csv", row.names = F)
#write.csv(gbif_fresh[480001:nrow(gbif_fresh),], "Data/GBIF/Freshwater_GBIF_occurrences_4.csv", row.names = F)

#write.csv(gbif_mar,                         "Data/GBIF/Marine_GBIF_occurrences.csv",   row.names = F)
#write.csv(gbif_mar[1:50000,],               "Data/GBIF/Marine_GBIF_occurrences_1.csv", row.names = F)
#write.csv(gbif_mar[50001:100000,],          "Data/GBIF/Marine_GBIF_occurrences_2.csv", row.names = F)
#write.csv(gbif_mar[100001:150000,],         "Data/GBIF/Marine_GBIF_occurrences_3.csv", row.names = F)
#write.csv(gbif_mar[150001:200000,],         "Data/GBIF/Marine_GBIF_occurrences_4.csv", row.names = F)
#write.csv(gbif_mar[200001:250000,],         "Data/GBIF/Marine_GBIF_occurrences_5.csv", row.names = F)
#write.csv(gbif_mar[250001:300000,],         "Data/GBIF/Marine_GBIF_occurrences_6.csv", row.names = F)
#write.csv(gbif_mar[300001:350000,],         "Data/GBIF/Marine_GBIF_occurrences_7.csv", row.names = F)
#write.csv(gbif_mar[350001:400000,],         "Data/GBIF/Marine_GBIF_occurrences_8.csv", row.names = F)
#write.csv(gbif_mar[400001:450000,],         "Data/GBIF/Marine_GBIF_occurrences_9.csv", row.names = F)
#write.csv(gbif_mar[450001:nrow(gbif_mar),], "Data/GBIF/Marine_GBIF_occurrences_10.csv",row.names = F) 


# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #


## LAND COVER DATA:

##    The *microclima* package automatically calculates LAI based on **habitat** type. Choices include:
##      No.       Descriptor
##    1         Evergreen needleleaf forest
##    2         Evergreen Broadleaf forest
##    3         Deciduous needleleaf forest
##    4         Deciduous broadleaf forest
##    5         Mixed forest
##    6         Closed shrublands
##    7         Open shrublands
##    8         Woody savannas
##    9         Savannas
##    10        Short grasslands      # MODIS only defines #10 as 'Grasslands' so there is no distinction between #10 or #11
##    11        Tall grasslands       
##    12        Permanent wetlands                  # MODIS LC#11
##    13        Croplands                           # MODIS LC#12
##    14        Urban and built-up                  # MODIS LC#13
##    15        Cropland/Natural vegetation mosaic  # MODIS LC#14
##    16        Barren or sparsely vegetated        
##    17        Open water

##    These habitat types are effectively drawn from the MODIS Land Cover Global 500 m product **MCD12Q1v006**.
##    https://modis.gsfc.nasa.gov/data/dataprod/mod12.php
##    However, **note** that there are some differences in the class definitions.

# Using the MCD12Q1v006 layer we extracted values for all terrestrial species
modis_lcm = read.csv("Data/GEE/GBIF_terrestrial_records_MODIS12Q1V6_LCM.csv")
# Add the 'LC_Type1' data column to the gbif dataframe
gbif_terr$Habitat = modis_lcm$LC_Type1[match(gbif_terr$Fid,modis_lcm$Fid)]

# Modify the values around #11-#14
gbif_terr$Habitat[gbif_terr$Habitat==14] = 15
gbif_terr$Habitat[gbif_terr$Habitat==13] = 14
gbif_terr$Habitat[gbif_terr$Habitat==12] = 13
gbif_terr$Habitat[gbif_terr$Habitat==11] = 12

# some did not have values.Not sure why but as we have many records these were dropped.
gbif_terr[is.na(gbif_terr$Habitat),]
gbif_terr = gbif_terr[!is.na(gbif_terr$Habitat),]

# How many different land cover types does each species have?
spp.lcm = as.matrix(table(gbif_terr$Species,gbif_terr$Habitat))

# Note that none of the 51 species were allocated to a single Land Cover type.
apply(spp.lcm>0, 1, sum)

write.csv(gbif_terr,  "Data/GBIF/Terrestrial_GBIF_occurrences_plus_habitat.csv", row.names = F)

###################### START CODE HERE FOR MICROCLIMATE PROJECTIONS###########
###################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                         MICROCLIMATE CALCULATION
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###################################################################################

rm(list=ls())

#library("devtools")
library(raster)
library(microclima) #devtools::install_github("ilyamaclean/microclima")
library(NicheMapR) #devtools::install_github("mrke/NicheMapR")

gbif = read.csv("Data/GBIF/Terrestrial_GBIF_occurrences_plus_habitat.csv")

# Work through species
for(spp in unique(gbif$Species)){
  
  if(file.exists(paste0("Data/Land/",spp,"_microclimate_stats.RData"))){
    load(paste0("Data/Land/",spp,"_microclimate_stats.RData"))
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
  for(S in sites[1:5]){
    
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    #         Microclimate
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
    
    # Microclimate paper:
    # Kearney et al. 2019 "A method for computing hourly, historical, terrain-corrected microclimate anywhere on earth"
    # https://doi.org/10.1111/2041-210X.13330
    
    # N.B. The function automatically assumes the raster should be 200 cells by 200 cells,
    #      so for a 30m resolution grid this is 6km across.
    #      I've reduced this to 50 cells across (1.5km x 1.5km) to make it faster
    r = microclima::get_dem(lat  = gbif$latitude[S], long = gbif$longitude[S], resolution = 30, xdims = 50, ydims = 50)
    # plot(r)
    
    s1 = Sys.time()
    
    # And... we'll cycle through microclimate generation one year at a time between 1981 and 2020
    for(Y in c(1981:2020)){
      
      temps = microclima::runauto(r, dstart = paste0("01/01/",Y,")"),  # start date
                                  dfinish   = paste0("31/12/",Y,")"),  # end date
                                  hgt = 0.1, l = NA, x = NA,
                                  habitat = as.character(habitats$descriptor[gbif$Habitat[S]]),
                                  plot.progress = FALSE)
      
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
      
    } # Year loop
    
    if(exists("s2")){ s2 = c(s2,s1 - Sys.time()) } else { s2 = s1 - Sys.time() }
    
    # Save
    gbif_microclimate = list("Species"=spp,"XY"=sites.xy, "Site_order"=sites,"Microclimate"=annual.thermal.data,"Time_taken"=s2)
    save(gbif_microclimate, file=paste0("Data/Land/",spp,"_microclimate_stats.RData"))
    
    
  } # Site loop
  
} # Species loop


# Remove blank first row
#annual.thermal.data = annual.thermal.data[complete.cases(annual.thermal.data),]




# The earliest you can go with the latest reanalysis dataset is 1979. But it 
# does seem like you could go further.
# Anyway for now I select 1990 - 2020

# Most weather stations are 2m of the ground, but given your variety of ectotherms, maybe the
# setting of 10cm above the ground is a reasonable compromise?

# A bigger issue is the choice of habitat. 
# 'l' and 'x' manually control the variation in Leaf Area Index (LAI) and their orientation 
# over space and time. Globally you will have evergreen and deciduous systems etc.
# Instead you can select form 17 habitat types:
habitats
# Unless you have a very good understanding of the habitat's a species uses, you might need to 
# run this process more than once to get upper and lower bounds...


# SAMPLE POINT #1
coords[1,]
# This point is just off the southern coast of Montpellier in France. 
# Given this first point was marine, I thought I'd try the category for 'Open water'.
# However the function throws an error when it internally tries to identify what the LAI for
# this location would have been for that habitat type. Specifying 0 values for 'l' and 'x' 
# didn't help either.

# This is why I switched to SAMPLE POINT 2!
# Now the habitat type is 'Evergreen needleleaf forest'

# AND, iyt turns out the function only works for one year at a time!

s1 = Sys.time()
temps = microclima::runauto(local.dem, "01/01/2010", "31/12/2010", hgt = 0.1,
                            l = 0, x = 0, 
                            habitat = "Evergreen needleleaf forest",
                            plot.progress = FALSE)
s2 = s1 - Sys.time()

# 1 year takes
print(s2)























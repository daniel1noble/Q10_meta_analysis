setwd("~/Dropbox/q10_meta/")

rm(list = ls())

library(googlesheets)
library(tidyverse)
library(magrittr)

source("R/functions.R")

gs <- gs_title("q10_newdata")
data <- as_tibble(gs_read(ss = gs))

head(data)


#Creating ES

data$obs <- as.factor(1:dim(data)[1])

# getting effect sizes
data %<>% Q10S10(temp_1, temp_2, r1.1, r1.2, r2.1, r2.2, r1.1_se, r1.2_se, r2.1_se, 
                 r2.2_se, r1.1_N, r1.2_N, r2.1_N, r2.2_N)

# adding species names
data %<>% mutate(spp_names = str_c(genus, species, sep = "_"))

data

#Other raw data
names(data)
data[,1:45]

#ES
es <- c("RR", "SQ10", "CV")

grep(es[1],colnames(data), perl = T)

grep(es[2],colnames(data))

grep(es[3],colnames(data))

#treats

treats <- c("ac", "aw", "pa")

grep(treats[1],colnames(data))

grep(treats[2],colnames(data))

grep(treats[3],colnames(data))

#Q10RR and Change Q10RR colnames
Q10RRdat <- select(data, obs, grep(es[1],colnames(data), perl = T))

Q10RRdat_ac <- select(Q10RRdat, obs, grep(treats[1], colnames(Q10RRdat)))
colnames(Q10RRdat_ac) <- c("obs", "ES", "V_ES")
Q10RRdat_ac$treat <- rep("ac", nrow(Q10RRdat_ac))


Q10RRdat_aw <- select(Q10RRdat,  obs, grep(treats[2], colnames(Q10RRdat)))
colnames(Q10RRdat_aw) <- c("obs", "ES", "V_ES")
Q10RRdat_aw$treat <- rep("aw", nrow(Q10RRdat_aw))

Q10RRdat_pa <- select(Q10RRdat,  obs, grep(treats[3], colnames(Q10RRdat)))
colnames(Q10RRdat_pa) <- c("obs", "ES", "V_ES")
Q10RRdat_pa$treat <- rep("pa", nrow(Q10RRdat_pa))


Q10RRdat <- rbind(Q10RRdat_ac,
                  Q10RRdat_aw,
                  Q10RRdat_pa)

Q10RRdat$Type <- rep(paste0("Q10",es[1]), nrow(Q10RRdat))

#SQ10 and Change SQ10 colnames
SQ10dat <- select(data, obs, grep(es[2],colnames(data), perl = T))

SQ10dat_ac <- select(SQ10dat, obs, grep(treats[1], colnames(SQ10dat)))
colnames(SQ10dat_ac) <- c("obs", "ES", "V_ES")
SQ10dat_ac$treat <- rep("ac", nrow(SQ10dat_ac))

SQ10dat_aw <- select(SQ10dat,obs, grep(treats[2], colnames(SQ10dat)))
colnames(SQ10dat_aw) <- c("obs", "ES", "V_ES")
SQ10dat_aw$treat <- rep("aw", nrow(SQ10dat_aw))

SQ10dat_pa <- select(SQ10dat, obs,grep(treats[3], colnames(SQ10dat)))
colnames(SQ10dat_pa) <- c("obs", "ES", "V_ES")
SQ10dat_pa$treat <- rep("pa", nrow(SQ10dat_pa))

SQ10dat <- rbind(SQ10dat_ac,
                 SQ10dat_aw,
                 SQ10dat_pa)

SQ10dat$Type <- rep(es[2], nrow(SQ10dat))

#CVQ10 and Change CVQ10 colnames

CV10dat <- select(data, obs, grep(es[3],colnames(data), perl = T))

CV10dat_ac <- select(CV10dat, obs,grep(treats[1], colnames(CV10dat)))
colnames(CV10dat_ac) <- c("obs", "ES", "V_ES")
CV10dat_ac$treat <- rep("ac", nrow(CV10dat_ac))

CV10dat_aw <- select(CV10dat, obs, grep(treats[2], colnames(CV10dat)))
colnames(CV10dat_aw) <- c("obs", "ES", "V_ES")
CV10dat_aw$treat <- rep("aw", nrow(CV10dat_aw))

CV10dat_pa <- select(CV10dat, obs, grep(treats[3], colnames(CV10dat)))
colnames(CV10dat_pa) <- c("obs", "ES", "V_ES")
CV10dat_pa$treat <- rep("pa", nrow(CV10dat_pa))

CV10dat <- rbind(CV10dat_ac,
                 CV10dat_aw,
                 CV10dat_pa)

CV10dat$Type <- rep(es[3], nrow(CV10dat))

#Combine all three

long_dat <- rbind(Q10RRdat,
                    SQ10dat,
                    CV10dat) %>% as.data.frame()

#Delete NAs
str(long_dat) ; nrow(long_dat)
sum(is.na(long_dat$ES) | is.na(long_dat$V_ES))

long_dat_na.omit <- filter(long_dat, !is.na(ES) & !is.na(V_ES))
str(long_dat_na.omit) ; nrow(long_dat_na.omit)

obs.need <- unique(long_dat_na.omit$obs)

#Joining back to raw data
str(data)

test <- left_join(long_dat_na.omit, data[,c(1:45,64)])
str(test)

long_dat_na.inf.omit <- filter(test, !is.infinite(ES) & !is.infinite(V_ES))
str(long_dat_na.inf.omit); nrow(long_dat_na.inf.omit)

write.csv(long_dat_na.inf.omit, row.names = F, "output/long_dat_na.inf.omit.csv")

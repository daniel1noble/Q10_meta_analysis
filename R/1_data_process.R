##################################
# Load Data & Manipulate
##################################

data_wide <- read.csv("./data/CombinedData_wide.csv", stringsAsFactors = FALSE)
data_wide$lat <- as.numeric(data_wide$lat)
new_papers <- data_wide[-grep("_o", data_wide$record_num),]
old_papers <- data_wide[grep("_o", data_wide$record_num),]

# Add full species to data
data_wide <- data_wide %>%
  mutate(species_full = paste0(genus, " ",species))

# habitat
spp_habitat <- data_wide %>%
  group_by(habitat) %>%
  summarise(spp_n = length(unique(species_full)))

# Note that some species need to be replaced
data_wide <- data_wide %>%
  mutate(species_full = ifelse(species_full == "Chinemys reevesii", 
                               "Mauremys reevesii", species_full)) %>%
  mutate(species_full = ifelse(species_full == "Enithares sp", 
                               "Enithares ciliata", species_full))  %>%
  mutate(habitat = ifelse(species_full == "Pungitius pungitius", "m", habitat))  %>% # Fix a few errors in habitat classificaton
  mutate(habitat = ifelse(species_full == "Danio rerio", "f", habitat)) 

# Convert se to SD so it can be used for effect sizes. Drop out the SE columns as these are no longer needed
data_wide <- data_wide %>%
  mutate(r1.1_sd = se_to_sd(r1.1_se, r1.1_N),
         r1.2_sd = se_to_sd(r1.2_se, r1.2_N),
         r2.1_sd = se_to_sd(r2.1_se, r2.1_N),
         r2.2_sd = se_to_sd(r2.2_se, r2.2_N)) %>%
  dplyr::select(-c(r1.1_se, r1.2_se, r2.1_se, r2.2_se,
                   r1.1_CV, r1.2_CV, r2.1_CV, r2.2_CV, 
                   r1.1_logCV, r1.2_logCV, r2.1_logCV, r2.2_logCV))
##################################
## Calculate effect sizes
##################################
# Calculate Q10 effect sizes and sampling variances; filter out non-finate values and clean up dataframe a bit

data_wide <- data_wide %>%
  mutate(lnRR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r1.2, sd1=r1.1_sd, sd2=r1.2_sd, n1=r1.1_N, n2=r1.2_N, 
                  name = "acute_t1"),
         lnRR_Q10(t2=temp_2, t1=temp_1, r1=r2.1, r2=r2.2, sd1=r2.1_sd, sd2=r2.2_sd, n1=r2.1_N, n2=r2.2_N, 
                  name = "acute_t2"),
         lnRR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r2.2, sd1=r1.1_sd, sd2=r2.2_sd, n1=r1.1_N, n2=r2.2_N, 
                  name = "acclim"),
         lnVR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r1.2, sd1=r1.1_sd, sd2=r1.2_sd, n1=r1.1_N, n2=r1.2_N, 
                  name = "acute_t1"),
         lnVR_Q10(t2=temp_2, t1=temp_1, r1=r2.1, r2=r2.2, sd1=r2.1_sd, sd2=r2.2_sd, n1=r2.1_N, n2=r2.2_N, 
                  name = "acute_t2"),
         lnVR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r2.2, sd1=r1.1_sd, sd2=r2.2_sd, n1=r1.1_N, n2=r2.2_N, 
                  name = "acclim"),
         lnCVR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r1.2, sd1=r1.1_sd, sd2=r1.2_sd, n1=r1.1_N, n2=r1.2_N, 
                   name = "acute_t1"),
         lnCVR_Q10(t2=temp_2, t1=temp_1, r1=r2.1, r2=r2.2, sd1=r2.1_sd, sd2=r2.2_sd, n1=r2.1_N, n2=r2.2_N, 
                   name = "acute_t2"),
         lnCVR_Q10(t2=temp_2, t1=temp_1, r1=r1.1, r2=r2.2, sd1=r1.1_sd, sd2=r2.2_sd, n1=r1.1_N, n2=r2.2_N, 
                   name = "acclim"),
         obs = 1:n()) %>%
  dplyr::select(-c(data_source, source_page,phylum, class, order, family, genus, species, geo_location, details, notes)) %>%
  filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

# #******** Problem with effect size calculation. V_lnVR and V_lnCVR are negative for a couple studies, should not be! Hence warning messages NaN in plotting of precision; explored this probalem more. It's actually incorrect data in the old dataset from Frank. Fonti explored the extraction for p088_o and error bars not found, sample sizes not correct. For second study, the N value was incorrect, should have been N = 5 not 0.11. Will exclude p_088_o from the data. Fonti did a check and tried to re-extract but in the end missing error so excluded

data_wide <- data_wide %>%
  filter(!record_num == "p088_o")

write.csv(data_wide, file = "./output/data/data_final_wide.csv", row.names = FALSE)
##################################
## Re-organise the data frame
##################################

# We want to be able to reorganise the data longitudianlly based on the two acute and acclimation data. Extract the data in chunks

################ THIS SHOULD FIX THE DATA_LONG problems
data_long <- data_wide %>%
  pivot_longer(cols = c(27, 29, 31, 33, 35, 37, 39, 41, 43), values_to = "effect_size", values_drop_na = FALSE) %>% 
  mutate(type = ifelse(grepl("acute", name), "acute", "acclim")) %>% data.frame()

# Checks:
dim(data_long)
with(data_long, table(type)) # Acute should be 2x larger
with(data_long, table(type, trait_category))
write.csv(data_long, file = "../output/data/data_final_long.csv", row.names = FALSE)
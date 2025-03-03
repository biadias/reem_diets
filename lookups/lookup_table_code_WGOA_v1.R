# --- ---- --- ---- --- --- --- --- --- --- 
# Lookup table edits.
# Author: Bia Dias
# Contact: bia.dias@noaa.gov
# --- ---- --- ---- --- --- --- --- --- --- 

library(tidyverse)
library(here)
source("R/REEM_fooddata_functions.R")

race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")

unique(race_lookup_base$final_goa)

race_lookup_base_v2 <- race_lookup_base %>%
  mutate(final_goa = case_when(final_goa =="MISC_NA"         ~ "MISC_NA",   
                           final_goa =="Shelf_demersals" ~ "Shelf demersal fish", 
                           final_goa =="ZERO"            ~ "ZERO", 
                           final_goa =="Sharks_demersal" ~ "Pacific sleeper shark", #FLAG ####
                           final_goa =="Salmon_shark"    ~ "Salmon shark", 
                           final_goa =="Pacific_dogfish" ~ "Pacific dogfish", 
                           final_goa =="Other_skates"    ~ "Other skates", 
                           final_goa =="Big_skate"       ~ "Big skate", 
                           final_goa =="Longnose_skate"  ~ "Longnose skate", 
                           final_goa =="Spotted_ratfish" ~ "Shelf demersal fish",
                           final_goa =="Shallow_water_flatfish" ~ "Shallow-water flatfish", 
                           final_goa =="Arrowtooth_flounder"    ~"Arrowtooth flounder adult", 
                           final_goa =="Pacific_halibut"        ~ "Pacific halibut adult", 
                           final_goa =="Flathead_sole"          ~ "Flathead sole adult"  , 
                           final_goa =="Deep_water_flatfish"    ~ "Deep-water flatfish" , 
                           final_goa =="Rex_sole"               ~"Rex sole adult", 
                           final_goa == "NA"                    ~ "NA", 
                           final_goa =="Miscellaneous_deep_sea_fish"~"Miscellaneous deep-sea fish", 
                           final_goa =="Pacific_sandlance"     ~ "Pacific sandlance", 
                           final_goa =="Sablefish"             ~ "Sablefish adult", 
                           final_goa =="Shelf_forage_fish"     ~ "Shelf forage fish", 
                           final_goa =="Slope_forage_fish"     ~ "Slope forage fish", 
                           final_goa =="Pacific_pomfret"       ~ "MISC_NA", #FLAG ####
                           final_goa =="Pacific_herring"       ~ "Pacific herring adult", 
                           final_goa =="Slope_demersals"       ~"Slope demersal fish", 
                           final_goa =="Large_sculpins"        ~ "Large sculpins", 
                           final_goa =="Pacific_cod"           ~"Pacific cod adult", 
                           final_goa =="Walleye_pollock"       ~ "Walleye pollock adult", 
                           final_goa =="Lingcod"               ~"Lingcod", 
                           final_goa =="Atka_mackerel"         ~ "Atka mackerel", 
                           final_goa =="Pacific_hake"          ~"Pacific hake", 
                           final_goa =="Eulachon"              ~ "Shelf forage fish", 
                           final_goa =="Pacific_capelin"       ~"Pacific capelin", 
                           final_goa =="Chinook_salmon"  ~ "Salmon returning", #FLAG ####
                           final_goa =="Coho_salmon"     ~ "Salmon returning", #FLAG ####
                           final_goa =="Pink_salmon"     ~ "Salmon returning", #FLAG ####
                           final_goa =="Chum_salmon"     ~ "Salmon returning", #FLAG ####
                           final_goa =="Sockeye_salmon"  ~ "Salmon returning", #FLAG ####
                           final_goa =="Pacific_saury"   ~ "MISC_NA", #FLAG ####
                           final_goa =="Thornyheads"     ~ "Thornyheads", 
                           final_goa =="Slope_rockfish"  ~ "Slope rockfish", 
                           final_goa =="POP"             ~ "Pacific ocean perch adult", 
                           final_goa =="Demersal_shelf_rockfish" ~ "Demersal shelf rockfish", 
                           final_goa =="Pelagic_shelf_rockfish"  ~ "Pelagic shelf rockfish", 
                           final_goa =="Unlisted_rockfish"       ~ "Demersal shelf rockfish", 
                           final_goa =="Black_rockfish"          ~ "MISC_NA", 
                           final_goa =="Sessile_epifauna"        ~ "Sessile epifauna", 
                           final_goa =="Gelatinous_carnivores"   ~ "Gelatinous carnivores", 
                           final_goa =="Infauna"                 ~ "Infauna", 
                           final_goa =="Motile_epifauna"         ~ "Motile epifauna", 
                           final_goa =="Euphausiids"             ~ "Euphausiids", 
                           final_goa =="Mysids"                  ~ "Mysids", 
                           final_goa =="Pandalid_shrimp"         ~ "Pandalid shrimp", 
                           final_goa =="Tanner_crab"             ~ "Tanner crab", 
                           final_goa =="King_crab"               ~ "King crab", 
                           final_goa =="Squid"                   ~ "Squid", 
                           final_goa =="Octopus"                 ~ "Octopus", 
                           final_goa =="Gelatinous_filter_feeders" ~ "Other gelatinous zooplankton", 
                           final_goa =="MISC_SHELLS"         ~ "MISC_SHELLS"))
         

race_lookup_base_v2 <- race_lookup_base_v2 %>% fill(final_goa)

write.csv(race_lookup_base_v2,"lookups/race_lookup_base_v2.csv",row.names=F)
         
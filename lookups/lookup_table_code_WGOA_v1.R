# --- ---- --- ---- --- --- --- --- --- ---
# Lookup table edits fo WGOA and EGOA
# Author: Bia Dias
# Contact: bia.dias@noaa.gov
# --- ---- --- ---- --- --- --- --- --- ---

library(tidyverse)
library(here)
source("R/REEM_fooddata_functions.R")

race_lookup_base <- read.clean.csv("lookups/race_lookup_combined.csv")

unique(race_lookup_base$final_goa)

race_lookup_base_v2 <- race_lookup_base %>%
  rename_with( ~ "final_wgoa", matches("final_goa")) %>%
  mutate(final_egoa = final_wgoa) %>%
  mutate(
    final_wgoa = case_when(
      final_wgoa == "MISC_NA"         ~ "MISC_NA",
      final_wgoa == "Shelf_demersals" ~ "Shelf demersal fish",
      final_wgoa == "ZERO"            ~ "ZERO",
      final_wgoa == "Sharks_demersal" ~ "Pacific sleeper shark",
      #FLAG ####
      final_wgoa == "Salmon_shark"    ~ "Salmon shark",
      final_wgoa == "Pacific_dogfish" ~ "Pacific dogfish",
      final_wgoa == "Other_skates"    ~ "Other skates",
      final_wgoa == "Big_skate"       ~ "Big skate",
      final_wgoa == "Longnose_skate"  ~ "Longnose skate",
      final_wgoa == "Spotted_ratfish" ~ "Shelf demersal fish",
      final_wgoa == "Shallow_water_flatfish" ~ "Shallow-water flatfish",
      final_wgoa == "Arrowtooth_flounder"    ~ "Arrowtooth flounder adult",
      final_wgoa == "Pacific_halibut"        ~ "Pacific halibut adult",
      final_wgoa == "Flathead_sole"          ~ "Flathead sole adult"  ,
      final_wgoa == "Deep_water_flatfish"    ~ "Deep-water flatfish" ,
      final_wgoa == "Rex_sole"               ~ "Rex sole adult",
      final_wgoa == "NA"                    ~ "NA",
      final_wgoa == "Miscellaneous_deep_sea_fish" ~
        "Miscellaneous deep-sea fish",
      final_wgoa == "Pacific_sandlance"     ~ "Pacific sandlance",
      final_wgoa == "Sablefish"             ~ "Sablefish adult",
      final_wgoa == "Shelf_forage_fish"     ~ "Shelf forage fish",
      final_wgoa == "Slope_forage_fish"     ~ "Slope forage fish",
      final_wgoa == "Pacific_pomfret"       ~ "MISC_NA",
      #FLAG ####
      final_wgoa == "Pacific_herring"       ~ "Pacific herring adult",
      final_wgoa == "Slope_demersals"       ~ "Slope demersal fish",
      final_wgoa == "Large_sculpins"        ~ "Large sculpins",
      final_wgoa == "Pacific_cod"           ~ "Pacific cod adult",
      final_wgoa == "Walleye_pollock"       ~ "Walleye pollock adult",
      final_wgoa == "Lingcod"               ~ "Lingcod",
      final_wgoa == "Atka_mackerel"         ~ "Atka mackerel",
      final_wgoa == "Pacific_hake"          ~ "Pacific hake",
      final_wgoa == "Eulachon"              ~ "Shelf forage fish",
      final_wgoa == "Pacific_capelin"       ~ "Pacific capelin",
      final_wgoa == "Chinook_salmon"  ~ "Salmon returning",
      #FLAG ####
      final_wgoa == "Coho_salmon"     ~ "Salmon returning",
      #FLAG ####
      final_wgoa == "Pink_salmon"     ~ "Salmon returning",
      #FLAG ####
      final_wgoa == "Chum_salmon"     ~ "Salmon returning",
      #FLAG ####
      final_wgoa == "Sockeye_salmon"  ~ "Salmon returning",
      #FLAG ####
      final_wgoa == "Pacific_saury"   ~ "MISC_NA",
      #FLAG ####
      final_wgoa == "Thornyheads"     ~ "Thornyheads",
      final_wgoa == "Slope_rockfish"  ~ "Slope rockfish",
      final_wgoa == "POP"             ~ "Pacific ocean perch adult",
      final_wgoa == "Demersal_shelf_rockfish" ~ "Demersal shelf rockfish",
      final_wgoa == "Pelagic_shelf_rockfish"  ~ "Pelagic shelf rockfish",
      final_wgoa == "Unlisted_rockfish"       ~ "Demersal shelf rockfish",
      final_wgoa == "Black_rockfish"          ~ "MISC_NA",
      final_wgoa == "Sessile_epifauna"        ~ "Sessile epifauna",
      final_wgoa == "Gelatinous_carnivores"   ~ "Gelatinous carnivores",
      final_wgoa == "Infauna"                 ~ "Infauna",
      final_wgoa == "Motile_epifauna"         ~ "Motile epifauna",
      final_wgoa == "Euphausiids"             ~ "Euphausiids",
      final_wgoa == "Mysids"                  ~ "Mysids",
      final_wgoa == "Pandalid_shrimp"         ~ "Pandalid shrimp",
      final_wgoa == "Tanner_crab"             ~ "Tanner crab",
      final_wgoa == "King_crab"               ~ "King crab",
      final_wgoa == "Squid"                   ~ "Squid",
      final_wgoa == "Octopus"                 ~ "Octopus",
      final_wgoa == "Gelatinous_filter_feeders" ~ "Other gelatinous zooplankton",
      final_wgoa == "MISC_SHELLS"         ~ "MISC_SHELLS"
    )
  ) %>%
  mutate(
    final_egoa = case_when(
      final_egoa == "MISC_NA"         ~ "MISC_NA",
      final_egoa == "Shelf_demersals" ~ "Shelf demersal fish",
      final_egoa == "ZERO"            ~ "ZERO",
      final_egoa == "Sharks_demersal" ~ "Pacific sleeper shark",
      #FLAG ####
      final_egoa == "Salmon_shark"    ~ "Salmon shark",
      final_egoa == "Pacific_dogfish" ~ "Pacific dogfish",
      final_egoa == "Other_skates"    ~ "Other skates",
      final_egoa == "Big_skate"       ~ "Big skate",
      final_egoa == "Longnose_skate"  ~ "Longnose skate",
      final_egoa == "Spotted_ratfish" ~ "Shelf demersal fish",
      final_egoa == "Shallow_water_flatfish" ~ "Shallow-water flatfish",
      final_egoa == "Arrowtooth_flounder"    ~
        "Arrowtooth flounder adult",
      final_egoa == "Pacific_halibut"        ~ "Pacific halibut adult",
      final_egoa == "Flathead_sole"          ~ "Flathead sole adult"  ,
      final_egoa == "Deep_water_flatfish"    ~ "Deep-water flatfish" ,
      final_egoa == "Rex_sole"               ~
        "Rex sole adult",
      final_egoa == "NA"                    ~ "NA",
      final_egoa == "Miscellaneous_deep_sea_fish" ~
        "Miscellaneous deep-sea fish",
      final_egoa == "Pacific_sandlance"     ~ "Pacific sandlance",
      final_egoa == "Sablefish"             ~ "Sablefish adult",
      final_egoa == "Shelf_forage_fish"     ~ "Shelf forage fish",
      final_egoa == "Slope_forage_fish"     ~ "Slope forage fish",
      final_egoa == "Pacific_pomfret"       ~ "MISC_NA",
      #FLAG ####
      final_egoa == "Pacific_herring"       ~ "Pacific herring adult",
      final_egoa == "Slope_demersals"       ~
        "Slope demersal fish",
      final_egoa == "Large_sculpins"        ~ "Large sculpins",
      final_egoa == "Pacific_cod"           ~
        "Pacific cod adult",
      final_egoa == "Walleye_pollock"       ~ "Walleye pollock adult",
      final_egoa == "Lingcod"               ~
        "Lingcod",
      final_egoa == "Atka_mackerel"         ~ "ZERO",
      final_egoa == "Pacific_hake"          ~
        "Pacific hake",
      final_egoa == "Eulachon"              ~ "Shelf forage fish",
      final_egoa == "Pacific_capelin"       ~
        "Pacific capelin",
      final_egoa == "Chinook_salmon"  ~ "Chinook (king) salmon",
      #FLAG ####
      final_egoa == "Coho_salmon"     ~ "Coho (silver) salmon",
      #FLAG ####
      final_egoa == "Pink_salmon"     ~ "Pink (humpback) salmon",
      #FLAG ####
      final_egoa == "Chum_salmon"     ~ "Chum (dog) salmon",
      #FLAG ####
      final_egoa == "Sockeye_salmon"  ~ "Sockeye (red) salmon",
      #FLAG ####
      final_egoa == "Pacific_saury"   ~ "MISC_NA",
      #FLAG ####
      final_egoa == "Thornyheads"     ~ "Thornyheads",
      final_egoa == "Slope_rockfish"  ~ "Slope rockfish",
      final_egoa == "POP"             ~ "Pacific ocean perch adult",
      final_egoa == "Demersal_shelf_rockfish" ~ "Demersal shelf rockfish",
      final_egoa == "Pelagic_shelf_rockfish"  ~ "Pelagic shelf rockfish",
      final_egoa == "Unlisted_rockfish"       ~ "Demersal shelf rockfish",
      final_egoa == "Black_rockfish"          ~ "MISC_NA",
      final_egoa == "Sessile_epifauna"        ~ "Sessile epifauna",
      final_egoa == "Gelatinous_carnivores"   ~ "Gelatinous carnivores",
      final_egoa == "Infauna"                 ~ "Infauna",
      final_egoa == "Motile_epifauna"         ~ "Motile epifauna",
      final_egoa == "Euphausiids"             ~ "Euphausiids",
      final_egoa == "Mysids"                  ~ "Mysids",
      final_egoa == "Pandalid_shrimp"         ~ "Pandalid shrimp",
      final_egoa == "Tanner_crab"             ~ "Tanner crab",
      final_egoa == "King_crab"               ~ "King crab",
      final_egoa == "Squid"                   ~ "Squid",
      final_egoa == "Octopus"                 ~ "Octopus",
      final_egoa == "Gelatinous_filter_feeders" ~ "Other gelatinous zooplankton",
      final_egoa == "MISC_SHELLS"         ~ "MISC_SHELLS"
    )
  ) %>% 
  mutate(
    final_wgoa = case_when(
      goa_guild == "Shrimp" &
        final_wgoa == "Motile epifauna" ~ "Nonpandalid shrimp", TRUE ~ final_wgoa))


race_lookup_base_v2 <- race_lookup_base_v2 %>% fill(final_wgoa) %>% fill(final_egoa)

write.csv(race_lookup_base_v2,
          "lookups/race_lookup_base_v2.csv",
          row.names = F)

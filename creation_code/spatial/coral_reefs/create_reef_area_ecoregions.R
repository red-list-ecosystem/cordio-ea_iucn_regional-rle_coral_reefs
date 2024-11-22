##
##  Name:       create_reef_area_ecoregions.R
##
##  Objective:  Extract coral reefs from wio ecoregions for
##              regional analysis
##
##  Approach:   Point to wio ecoregions and global reef
##              spatial layers, filter for relevant ecoregions,
##              extract and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith
##              CORDIO East Africa
##
##  Date:       2024-02-26
##

##
## 1. Set up
##
 ## -- call to custom wio ecoregions -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # point to data file
    data_file <- "regional_coral_reefs.rda"

  # call to wio ecoregions
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
 ## -- get reef areas for ecoregions -- ##
  # get areas
    reef_area_ecoregions <-
      regional_coral_reefs %>%
        # rename(Ecoregion = ECOREGION) %>%
        st_transform(32737) %>%
        group_by(Ecoregion) %>%
        reframe(geometry = geometry %>% st_union()) %>%
      st_as_sf() %>%
        group_by(Ecoregion) %>%
        reframe(reef_area = geometry %>% st_area())
# # A tibble: 10 × 2
   # Ecoregion                          reef_area
   # <chr>                                  [m^2]
 # 1 Bight of Sofala/Swamp Coast       158602022.
 # 2 Cargados Carajos/Tromelin Island  347580403.
 # 3 Central Somali Coast               11096295.
 # 4 Delagoa                            35501209.
 # 5 East African Coral Coast         3554247450.
 # 6 Mascarene Islands                 505286608.
 # 7 Northern Monsoon Current Coast    380162123.
 # 8 Seychelles                       1586108194.
 # 9 Southeast Madagascar               56015266.
# 10 Western and Northern Madagascar  3177320079.

  # compare to original values
    reef_area_ecoregions %<>%
      mutate(reef_area = (reef_area %>% as.numeric()) / 1e6)
# # A tibble: 10 × 2
   # Ecoregion                        reef_area
   # <chr>                                <dbl>
 # 1 Bight of Sofala/Swamp Coast          159.
 # 2 Cargados Carajos/Tromelin Island     348.
 # 3 Central Somali Coast                  11.1
 # 4 Delagoa                               35.5
 # 5 East African Coral Coast            3554.
 # 6 Mascarene Islands                    505.
 # 7 Northern Monsoon Current Coast       380.
 # 8 Seychelles                          1586.
 # 9 Southeast Madagascar                  56.0
# 10 Western and Northern Madagascar     3177.

 ## -- set proportional area -- ##
  # calculate proportion
    reef_area_ecoregions %<>%
      mutate(prop_area = (reef_area / sum(reef_area)) %>% round(4))
# # A tibble: 10 × 3
   # Ecoregion                        reef_area prop_area
   # <chr>                                <dbl>     <dbl>
 # 1 Bight of Sofala/Swamp Coast          159.     0.0162
 # 2 Cargados Carajos/Tromelin Island     348.     0.0354
 # 3 Central Somali Coast                  11.1    0.0011
 # 4 Delagoa                               35.5    0.0036
 # 5 East African Coral Coast            3554.     0.362
 # 6 Mascarene Islands                    505.     0.0515
 # 7 Northern Monsoon Current Coast       380.     0.0387
 # 8 Seychelles                          1586.     0.162
 # 9 Southeast Madagascar                  56.0    0.0057
# 10 Western and Northern Madagascar     3177.     0.324


  # get ecoregion list
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Bight of Sofala/Swamp Coast"
 # [2] "Cargados Carajos/Tromelin Island"
 # [3] "Central Somali Coast"
 # [4] "Delagoa"
 # [5] "East African Coral Coast"
 # [6] "Mascarene Islands"
 # [7] "Northern Monsoon Current Coast"
 # [8] "Seychelles"
 # [9] "Southeast Madagascar"
# [10] "Western and Northern Madagascar"


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/spatial/coral_reefs/"

  # save reef area to file
    save(reef_area_ecoregions,
      file = paste0(save_locale, "reef_area_ecoregions.rda"))


##
## 4. Clean up workspace
##
  # clean up paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs)

  # remove core objects
    rm(reef_area_ecoregions)


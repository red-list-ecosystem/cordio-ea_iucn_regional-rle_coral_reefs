##
##  Name:       create_criteria_b1_extent_of_occurrence.R
##
##  Objective:  Standardise & format data for analysing criterion B1:
##                Extent of occurrence
##
##  Approach:   Call to coral reefs and ecoregions for wio,
##              calculate extent of occurrence and
##              classify to threat categories:
##              - Critically Endangered (CR) if number of EOO units <= 2
##              - Endangered (EN) if number of EOO units == 20
##              - Vulnerable (VU) if number of EOO units <= 50
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. For this analysis, Criterion B1 will be assessed
##                 based on the *extent of occurrence* of coral reef
##                 ecosystem of the WIO assessed within 12 defined
##                 ecoregions.
##              2. This is a two step process that entails generating
##                 the smallest convex polygon encompassing an ecosystem
##                 (coral reef) and calculation of area (Km sq) of
##                 the developed EOO geometry.
##              3. As the coral reef layer has already been extracted
##                 from the ecoregions, this simplifies the calculation
##                 of area.
##              4. Changing projection to epsg:32737 instead of wio_crs, but
##                 need to confirm with james                 [ fs: 2024-03-13 ]
##

##
## 1. Set up
##
  # call to additional functionality
    # library(redlistr)

 ## -- import coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # set data file name
    data_file <- "regional_coral_reefs.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    regional_coral_reefs
# Simple feature collection with 1349 features and 4 fields
# Geometry type: GEOMETRY
# Dimension:     XY
# Bounding box:  xmin: 32.86711 ymin: -27.01737 xmax: 63.51225 ymax: 2.755531
# Geodetic CRS:  WGS 84
# # A tibble: 1,349 × 5
   # Ecoregion    Eco_code Province Prov_code                  geometry
   # <chr>           <dbl> <chr>        <dbl>            <GEOMETRY [°]>
 # 1 Cargados Ca…    20097 Western…        20 POLYGON ((56.59693 -10.3…
 # 2 Cargados Ca…    20097 Western…        20 POLYGON ((56.67842 -10.4…
 # 3 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.51909…
 # 4 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.50409…
 # 5 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.50409…
 # 6 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((54.51485…
 # 7 Cargados Ca…    20097 Western…        20 POLYGON ((54.51764 -15.9…
 # 8 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((59.49749…
 # 9 Cargados Ca…    20097 Western…        20 POLYGON ((59.57061 -16.7…
# 10 Cargados Ca…    20097 Western…        20 MULTIPOLYGON (((59.48067…
# # ℹ 1,339 more rows
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Calculate extent of occupancy & threat categories
##
  # get list of ecoregions
    ecoregion_list <-
      regional_coral_reefs %>% pull(Ecoregion) %>% unique()
 # [1] "Cargados Carajos/Tromelin Island"
 # [2] "Central Somali Coast"            
 # [3] "Delagoa"                         
 # [4] "Mascarene Islands"               
 # [5] "Seychelles"                      
 # [6] "Southeast Madagascar"            
 # [7] "East African Coral Coast"        
 # [8] "Northern Monsoon Current Coast"  
 # [9] "Bight of Sofala/Swamp Coast"     
# [10] "Western and Northern Madagascar" 

  # create empty object to hold results
    criterion_b1_extent_of_occurrence <- tibble()

  # loop ecoregions
    for(i in 1:length(ecoregion_list)){

      # create eeo object
        eco_eoo <-
          regional_coral_reefs %>%
          filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32737) %>%
          as_Spatial() %>%
          redlistr::makeEOO()

      # calculate area
        eco_eoo_area <-
           eco_eoo %>%
             redlistr::getAreaEOO()

      # set name
        dat <-
          tribble(       ~Ecoregion,  ~`EOO Area`,
                  ecoregion_list[i], eco_eoo_area)

      # harvest results
        criterion_b1_extent_of_occurrence %<>%
          bind_rows(dat)


    }

 ## -- set threat categories -- ##
  # classify
    criterion_b1_extent_of_occurrence %<>%
      mutate(Status = ifelse(`EOO Area` <= 2e3,                  "CR",     NA),
             Status = ifelse(`EOO Area` %>% between(2e3, 20e3),  "EN", Status),
             Status = ifelse(`EOO Area` %>% between(20e3, 50e3), "VU", Status),
             Status = ifelse(`EOO Area` > 50e3,               "NT/LC", Status))
# # A tibble: 10 × 3
   # Ecoregion                        `EOO Area` Status
   # <chr>                                 <dbl> <chr> 
 # 1 Cargados Carajos/Tromelin Island    199262. NT/LC 
 # 2 Central Somali Coast                   249. CR    
 # 3 Delagoa                              63276. NT/LC 
 # 4 Mascarene Islands                    66324. NT/LC 
 # 5 Seychelles                          386289. NT/LC 
 # 6 Southeast Madagascar                 22081. VU    
 # 7 East African Coral Coast            253070. NT/LC 
 # 8 Northern Monsoon Current Coast       23347. VU    
 # 9 Bight of Sofala/Swamp Coast          17769. EN    
# 10 Western and Northern Madagascar    1154479. NT/LC 


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_b1_extent_of_occurrence,
      file = paste0(save_locale, "criterion_b1_extent_of_occurrence.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs,
       ecoregion_list)

  # remove core data objects
    rm(criterion_b1_extent_of_occurrence)


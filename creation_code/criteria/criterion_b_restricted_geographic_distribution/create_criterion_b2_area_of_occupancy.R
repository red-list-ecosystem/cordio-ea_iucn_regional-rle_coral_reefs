##
##  Name:       create_criteria_b2_area_of_occupancy.R
##
##  Objective:  Standardise & format data for analysing criterion B1:
##                Extent of occurrence
##
##  Approach:   Call to coral reefs and ecoregions for wio,
##              calculate extent of occurrence and
##              classify to threat categories:
##              - Critically Endangered (CR) if number of AOO units <= 2
##              - Endangered (EN) if number of AOO units == 20
##              - Vulnerable (VU) if number of AOO units <= 50
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Criterion B2 measures the distribution of coral reef
##                 ecosystems based on a standard 10x10km grid. AOO is
##                 determined by counting the number of 10x10 km cells
##                 occupied by >1 km2 of the reef ecosystem within 12
##                 defined ecoregions.
##              2. This process basically entails creation of 10x10km grid,
##                 calculating grid uncertainty and getting the number of
##                 grid cells that are occupied by more than 1% of the
##                 reef ecosystem..
##              3. As the coral reef layer has already been extracted
##                 from the ecoregions, this simplifies the calculation
##                 of area.
##              4. Number of Improvements for gridUncertainty() sets
##                 number of iterations
##

##
## 1. Set up
##
  # call to additional functionality
    # library(redlistr)

 ## -- point to regional coral reefs coral reefs -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # set data file name
    data_file <- "regional_coral_reefs.rda"

  # call to coral reefs
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
## 3. Calculate area of occupancy & threat categories
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

  # set grid size [ 10 km ]
    grid_size <- 10e3

  # set number of improvements
    n_improvements <- 5

  # set percent threshold
    p_thresh <- 1

  # create empty object to hold results
    criterion_b2_area_of_occupancy <- tibble()

  # loop ecoregions    # i=1  ## -- for testing -- ##
    for(i in 1:length(ecoregion_list)){

      # print progress to screen
        cat(paste0("...processing:  ", ecoregion_list[i],
                   " [ ", i, " of ", length(ecoregion_list), " ]\n"))

      # create area of occurrence grid
        aoo_grid <-
          regional_coral_reefs %>%
          filter(Ecoregion %in% ecoregion_list[i]) %>%
          st_transform(32737) %>%
          as_Spatial() %>%
          redlistr::makeAOOGrid(grid.size = grid_size)

      # get grid uncertainty
        grid_uncertainty <-
          aoo_grid %>%
            redlistr::gridUncertainty(grid.size         = grid_size,
                                       n.AOO.improvement = n_improvements)

     # get min grid uncertainty
       min_aoo_number <-
         grid_uncertainty$min.AOO.grid$AOO.number

     # calculate uncertainty and apply 1% rule
       aoo <-
         regional_coral_reefs %>%
           filter(Ecoregion %in% ecoregion_list[i]) %>%
           st_transform(32737) %>%
           as_Spatial() %>%
         redlistr::getAOO(grid.size        = grid_size,
                          min.percent.rule = TRUE,
                          percent          = p_thresh)

      # set name
        dat <-
          tribble(       ~Ecoregion,  ~`Grid Uncertainty`, ~ `AOO`,
                  ecoregion_list[i],       min_aoo_number,     aoo)

      # harvest results
        criterion_b2_area_of_occupancy %<>%
          bind_rows(dat)


    }

 ## -- set threat categories -- ##
  # set threat categories
    threat_categories <-
      tribble(   ~Status,                      ~Description,
                    "CR",  "AOO is less or equal to 2 (CR)",
                    "EN", "AOO is less or equal to 20 (EN)",
                    "VU", "AOO is less or equal to 50 (VU)",
                 "NT/LC",  "AOO is greater than 50 (NT/LC)")

  # classify
    criterion_b2_area_of_occupancy %<>%
      mutate(Status = ifelse(AOO <= 2, "CR", NA),
             Status = ifelse(AOO %>% between(2, 20), "EN", Status),
             Status = ifelse(AOO %>% between(20, 50), "VU", Status),
             Status = ifelse(AOO > 50, "NT/LC", Status))
# # A tibble: 10 × 4
   # Ecoregion                        `Grid Uncertainty`   AOO Status
   # <chr>                                         <int> <int> <chr> 
 # 1 Cargados Carajos/Tromelin Island                 22    19 EN    
 # 2 Central Somali Coast                              5     1 CR    
 # 3 Delagoa                                          22    12 EN    
 # 4 Mascarene Islands                                40    36 VU    
 # 5 Seychelles                                      118   103 NT/LC 
 # 6 Southeast Madagascar                              6     4 EN    
 # 7 East African Coral Coast                        356   305 NT/LC 
 # 8 Northern Monsoon Current Coast                   55    40 VU    
 # 9 Bight of Sofala/Swamp Coast                      24    20 VU    
# 10 Western and Northern Madagascar                 404   323 NT/LC 

  # join descriptions
    criterion_b2_area_of_occupancy %<>%
      left_join(threat_categories)


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_b2_area_of_occupancy,
      file = paste0(save_locale, "criterion_b2_area_of_occupancy.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(regional_coral_reefs,
       ecoregion_list,
       threat_categories)

  # remove criterion parameters
    rm(grid_size,
       n_improvements,
       p_thresh)

  # remove core data objects
    rm(criterion_b2_area_of_occupancy)


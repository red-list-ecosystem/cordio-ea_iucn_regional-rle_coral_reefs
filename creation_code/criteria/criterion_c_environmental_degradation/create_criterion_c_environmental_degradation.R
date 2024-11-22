##
##  Name:       create_criterion_c_environmental_degradation.R
##
##  Objective:  Standardise & format data for analysing criterion C:
##                Extent of occurrence
##
##  Approach:   Import degree heating weeks data and groom.
##
##              As the DHW values is an average across the 
##              entire ecoregion, the relative severity was
##              calculated over an extent of 100% (>80%).
##
##              For each eco-region (time period C2a)
##              - extent >= 80% & relative_severity >= 80% -> (CR)
##              - extent >= 80% & relative_severity >= 50% -> (EN) 
##              - extent >= 80% & relative_severity >= 30% -> (VU)
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Criterion C will be assessed using exceedance 
##                   of DHW levels that cause severe coral bleaching
##                   and mortality over a 50-year period into the future.
##              2. As the DHW values is an average across the entire 
##                   ecoregion, relative severity was calculated over
##                   an extent of 100% (>80%).
##                 For each eco-region (time period C2a):
##                   extent >= 80% & relative_severity >= 80% -> (CR) 
##                   extent >= 80% & relative_severity >= 50% -> (EN) 
##                   extent >= 80% & relative_severity >= 30% -> (VU)

##
## 1. Set up
##
 ## -- call to relative reef areas -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # set data file name
    data_file <- "reef_area_ecoregions.rda"

  # call to data
    load(paste0(data_locale, data_file))

 ## -- call to representative concentration pathways dhws -- ##
  # point to data locale
    data_locale <- 
      "data_intermediate/geophysical/representative_concentration_pathways/"

  # point to data file
    data_file <- "representative_concentration_pathways.rda"

  # call to data
    load(paste0(data_locale, data_file))


##
## 2. Groom data
##
  # have a look
    representative_concentration_pathways
# # A tibble: 4,136 × 4
    # Year file_name Ecoregion             DHW
   # <dbl> <chr>     <chr>               <dbl>
 # 1  2006 RCP2.6    N Tanzania - Kenya 0.0037
 # 2  2007 RCP2.6    N Tanzania - Kenya 0.0071
 # 3  2008 RCP2.6    N Tanzania - Kenya 0.0108
 # 4  2009 RCP2.6    N Tanzania - Kenya 0     
 # 5  2010 RCP2.6    N Tanzania - Kenya 0.0069
 # 6  2011 RCP2.6    N Tanzania - Kenya 0.0015
 # 7  2012 RCP2.6    N Tanzania - Kenya 0     
 # 8  2013 RCP2.6    N Tanzania - Kenya 0.0342
 # 9  2014 RCP2.6    N Tanzania - Kenya 0.190 
# 10  2015 RCP2.6    N Tanzania - Kenya 0.423 
# # ℹ 4,126 more rows
# # ℹ Use `print(n = ...)` to see more rows

  # get value ranges
    representative_concentration_pathways %>% 
      pull(DHW) %>% range(na.rm = TRUE)
# [1]  0.0000 38.2028

  # round values
    representative_concentration_pathways %<>%
      mutate(DHW = DHW %>% round(0))

 ## -- calculate number of exceedences of bleaching threshold -- ##
  # set thresholds
    low_thresh  <-  8
    high_thresh <- 12

 ## -- loop creates two new columns dhw_bl_low and dhw_bl_high          ##
 ##    for each of the bleaching thresholds. The loop assigns a 1       ## 
 ##    if the respective thresholds are met or exceeded and 0 if not -- ##

  # set thresholds
    rcp_thresholds <-
      representative_concentration_pathways %>%
        mutate(dhw_bl_low  = ifelse(DHW <  low_thresh, 0, NA),
               dhw_bl_high = ifelse(DHW > high_thresh, 1, NA))

 ## -- sum exceedences per decade -- ##
  # set decade 5 years either side of 2020
    rcp_thresholds %<>%
      mutate(decade = Year - (Year + 5) %% 10)

  # summarise number of exceedences
    rcp_thresholds_per_decade <-
      rcp_thresholds %>%
        group_by(Ecoregion,
                 file_name,
                 decade) %>%
        summarise(dhw_exc_low  = dhw_bl_low %>% sum(na.rm = TRUE),
                  dhw_exc_high = dhw_bl_high %>% sum(na.rm = TRUE))

 ## -- calculate relative severity -- ##
  # set collapse threshold
    collapse_thresh <- 2

  # calculate
    rel_severity <-
      rcp_thresholds_per_decade %>%
      group_by(Ecoregion,
               file_name) %>%
      summarise(rel_sev_low  = ((dhw_exc_low[decade == 2015] - dhw_exc_low[decade == 2065]) /
                                (dhw_exc_low[decade == 2015] - collapse_thresh)) * 100,
                rel_sev_high = ((dhw_exc_high[decade == 2015] - dhw_exc_high[decade == 2065]) /
                                (dhw_exc_high[decade == 2015] - collapse_thresh)) * 100)
# `summarise()` has grouped output by 'Ecoregion'. You can override using
# the `.groups` argument.
# # A tibble: 44 × 4
# # Groups:   Ecoregion [11]
   # Ecoregion       file_name rel_sev_low rel_sev_high
   # <chr>           <chr>           <dbl>        <dbl>
 # 1 Comoros         RCP2.6              0            0
 # 2 Comoros         RCP4.5              0           50
 # 3 Comoros         RCP6.0              0            0
 # 4 Comoros         RCP8.5              0          500
 # 5 Delagoa         RCP2.6              0            0
 # 6 Delagoa         RCP4.5              0            0
 # 7 Delagoa         RCP6.0              0            0
 # 8 Delagoa         RCP8.5              0          500
 # 9 East Madagascar RCP2.6              0            0
# 10 East Madagascar RCP4.5              0           50
# # ℹ 34 more rows
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Evaluate criterion
##
 ## -- create columns for extent and status -- ##
  # classify for low relative severity
    rel_severity %<>%
      # mutate(extent      = 80) %>%
      mutate(status_low = ifelse(rel_sev_low >= 100,                    "CO",         NA),
             status_low = ifelse(rel_sev_low < 100 & rel_sev_low >= 50, "CR", status_low),
             status_low = ifelse(rel_sev_low <  80 & rel_sev_low >= 50, "EN", status_low),
             status_low = ifelse(rel_sev_low <  50 & rel_sev_low >= 30, "VU", status_low),
             status_low = ifelse(rel_sev_low < 30,                   "NT/LT", status_low))

  # classify for high bleaching threshold
    rel_severity %<>%
      # mutate(extent      = 80) %>%
      mutate(status_high = ifelse(rel_sev_high >= 100,                     "CO",          NA),
             status_high = ifelse(rel_sev_high < 100 & rel_sev_high >= 50, "CR", status_high),
             status_high = ifelse(rel_sev_high <  80 & rel_sev_high >= 50, "EN", status_high),
             status_high = ifelse(rel_sev_high <  50 & rel_sev_high >= 30, "VU", status_high),
             status_high = ifelse(rel_sev_high < 30,                    "NT/LT", status_high))

 ## -- export options -- ##
# #re-format the data to wide
# results_DHW8 <- rel_severity[,c(1,2,6)] %>% spread(RCP, status_low)
# results_DHW12 <- rel_severity[,c(1,2,7)] %>% spread(RCP, status_high)

# results_DHW8$threshold<-8
# results_DHW12$threshold<-12

# k<-rbind(results_DHW8,results_DHW12)
# k1<-k[order(k$eco_rgn),]

# #output various results
# write.csv(results_DHW8,"Crit_C_results_DHW8_threshold.csv",row.names = F)
# write.csv(results_DHW12,"Crit_C_results_DHW12_threshold.csv",row.names = F)
# write.csv(k1,"Crit_C_results.csv",row.names = F)


 ## -- calculate relative severity -- ##
  # inspect ecoregions
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                 "Delagoa"                
 # [3] "East Madagascar"         "Mascarene Isl."         
 # [5] "N Mozambique-S Tanzania" "N Tanzania-Kenya"       
 # [7] "North Madagascar"        "Seychelles Outer"       
 # [9] "Seychelles north"        "South Madagascar"       
# [11] "West Madagascar" 

   # check from relative severity object
     rel_severity %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                   "Delagoa"                  
 # [3] "East Madagascar"           "Mascarene Isl"            
 # [5] "N Mozambique - S Tanzania" "N Tanzania - Kenya"       
 # [7] "North Madagascar"          "Seychelles North"         
 # [9] "Seychelles Outer"          "South Madagascar"         
# [11] "West Madagascar"

   # standardise for ecoregions reef area 
     reef_area_ecoregions %<>%
       mutate(Ecoregion = ifelse(Ecoregion == "Mascarene Isl.", 
                                              "Mascarene Isl", Ecoregion),
              Ecoregion = ifelse(Ecoregion == "N Mozambique-S Tanzania",
                                              "N Mozambique - S Tanzania", Ecoregion),
              Ecoregion = ifelse(Ecoregion == "N Tanzania-Kenya",
                                              "N Tanzania - Kenya", Ecoregion),
              Ecoregion = ifelse(Ecoregion == "Seychelles north",
                                              "Seychelles North", Ecoregion))

    # join objects
      rel_severity %<>%
        left_join(reef_area_ecoregions %>%
                    dplyr::select(Ecoregion,
                                  prop_area))

    # calculate extent
 ## -- 1. Proportion of eco-regions which have relative    ## 
 ##         severity above 50 and 80 respectively          ##
 ##    2. Proportion of coral reef area with relative      ##
 ##         severity above 50 and 80 respectively       -- ##

  # calculate relative extent
    rel_severity_regional <-
      rel_severity %>%
        group_by(file_name) %>%
          summarise(rel_s_50      = (length(rel_sev_high[rel_sev_high >= 50 & rel_sev_high < 80]) / 
                                       length(rel_sev_high) * 100) %>% round(1),
                    rel_s_80      = (length(rel_sev_high[rel_sev_high >= 80]) / 
                                       length(rel_sev_high) * 100) %>% round(1),
                    rel_s_50_area = (prop_area[rel_sev_high >= 50 & 
                                               rel_sev_high < 80] %>% sum(na.rm = TRUE)) * 100 %>% round(1),
                    rel_s_80_area = (prop_area[rel_sev_high >= 80] %>% sum(na.rm = TRUE)) * 100 %>% round(1))
# # A tibble: 4 × 5
  # file_name rel_s_50 rel_s_80 rel_s_50_area rel_s_80_area
  # <chr>        <dbl>    <dbl>         <dbl>         <dbl>
# 1 RCP2.6         0        0            0             0   
# 2 RCP4.5        18.2      9.1         11.6           0.26
# 3 RCP6.0         9.1     18.2          5.05          4.49
# 4 RCP8.5         0      100            0           100. 


  # assign threat status based on extent & relative severity
    rel_severity_regional %<>%
      mutate(status_high = ifelse(rel_s_50 >= 50 & rel_s_50 <= 80 | 
                                  rel_s_50_area >= 50 & rel_s_50_area <= 80, "VU", NA),
             status_high = ifelse(rel_s_50 >= 80 | rel_s_50_area >= 80,      "EN", status_high),
             status_high = ifelse(rel_s_80 >= 50 & rel_s_80 <=80 | 
                                  rel_s_80_area >= 50 & rel_s_80_area <= 80, "EN", status_high),
             status_high = ifelse(rel_s_80 >= 30 & rel_s_80 <= 50| 
                                  rel_s_80_area >= 30 & rel_s_80_area <= 50, "VU", status_high),
             status_high = ifelse(rel_s_80 >= 80 | rel_s_80_area >= 80,      "CR", status_high))
# # A tibble: 4 × 6
  # file_name rel_s_50 rel_s_80 rel_s_50_area rel_s_80_area status_high
  # <chr>        <dbl>    <dbl>         <dbl>         <dbl> <chr>      
# 1 RCP2.6         0        0            0             0    <NA>       
# 2 RCP4.5        18.2      9.1         11.6           0.26 <NA>       
# 3 RCP6.0         9.1     18.2          5.05          4.49 <NA>       
# 4 RCP8.5         0      100            0           100.   CR  

  # set fix for nas
    rel_severity_regional %<>%
      mutate(status_high = ifelse(status_high %>% is.na(), "NT/LT", status_high))


  # create criterion objects
    criterion_c_environmental_degradation_ecoregion <- rel_severity
    criterion_c_environmental_degradation_regional  <- rel_severity_regional


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save ecoregion results to file
    save(criterion_c_environmental_degradation_ecoregion,
      file = paste0(save_locale, "criterion_c_environmental_degradation_ecoregion.rda"))

  # save regional results to file
    save(criterion_c_environmental_degradation_regional,
      file = paste0(save_locale, "criterion_c_environmental_degradation_regional.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove limit objects
    rm(low_thresh,
       high_thresh,       
       collapse_thresh)

  # remove intermediate objects
    rm(representative_concentration_pathways,
       rcp_thresholds,
       rcp_thresholds_per_decade,
       rel_severity,
       reef_area_ecoregions,
       rel_severity_regional)

  # remove core objects
    rm(criterion_c_environmental_degradation_ecoregion,
       criterion_c_environmental_degradation_regional)


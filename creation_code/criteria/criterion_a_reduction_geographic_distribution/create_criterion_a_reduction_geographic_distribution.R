##
##  Name:       create_criteria_a_reduction_geographic_distribution.R
##
##  Objective:  Standardise & format data for analysing criterion A:
##                Reduction in geographic distribution
##
##  Approach:   Call to clean data compilation, filter by time
##                period, geographic region, summarise and save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-15
##

##  Notes:      1. Analysis assumes *current* state represents a 50-year
##                   period and that all sites were above threshold
##                   50 years ago.
##                 This is sub-criterion A1 (i.e. past 50 years)
##              2. A cut-off year for most recent year (e.g. 2010), sets
##                   all sites at same time period
##              3. *Current* hard coral cover is the average of all time-points
##                   from cut-off year (e.g. 2013)
##              4. Need to parse out geographic coordinates for spatial
##                   visualisation
##              5. Need to adjust creation code to match original
##                   criterion a table

##
## 1. Set up
##
 # ## -- call to trend data -- ##
  # # point to data locale
    # data_locale <- "data_intermediate/criteria/"

  # # point to data file name
    # data_file <- "hard_coral_percent_cover_trend.rda"

  # # load data
    # load(paste0(data_locale, data_file))

 ## -- call to relative reef areas -- ##
  # point to data locale
    data_locale <- "data_intermediate/spatial/coral_reefs/"

  # set data file name
    data_file <- "reef_area_ecoregions.rda"

  # call to data
    load(paste0(data_locale, data_file))


 # ## -- call to percent cover data -- ##
  # # point to data locale
    # data_locale <- "data_intermediate/criteria/"

  # # set data file name
    # data_file <- "criterion_a_data_table.rda"

  # # call to data
    # load(paste0(data_locale, data_file))

 ## -- call to original data table -- ##
  # point to data locale
    data_locale <- "data_raw/criteria/"

  # point to data file
    data_file <- "criterion_a_data_table.xlsx"

  # import data table
    criterion_a_data_table <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


##
## 2. Groom data
##
  # have a look
    criterion_a_data_table
# # A tibble: 441 × 15
# # Groups:   site_id, Country [441]
   # site_id    Country ecoregion_country first_year recent_year   lat  long
   # <chr>      <chr>   <chr>                  <dbl>       <dbl> <dbl> <dbl>
 # 1 Anjouan_A… Comoros Comoros_Comoros         2003        2017 -12.2  44.2
 # 2 Anjouan_A… Comoros Comoros_Comoros         2015        2015 -12.2  44.2
 # 3 Anjouan_B… Comoros Comoros_Comoros         2009        2017 -12.2  44.5
 # 4 Anjouan_B… Comoros Comoros_Comoros         2016        2016 -12.2  44.2
 # 5 Anjouan_V… Comoros Comoros_Comoros         2016        2016 -12.2  44.3
 # 6 Anjouan_W… Comoros Comoros_Comoros         2003        2017 -12.1  44.3
 # 7 Grand_Com… Comoros Comoros_Comoros         2018        2018 -11.7  43.2
 # 8 Grande_Co… Comoros Comoros_Comoros         1999        2017 -11.7  43.3
 # 9 Grande_Co… Comoros Comoros_Comoros         1999        2017 -11.4  43.3
# 10 Grande_Co… Comoros Comoros_Comoros         2003        2017 -11.4  43.2
# # ℹ 431 more rows
# # ℹ 8 more variables: no_years <int>, year_gap <dbl>,
# #   recent_coral_cover <dbl>, original_coral_cover <dbl>,
# #   mean_coral_cover_thresh <dbl>, n <int>, n_diff <int>,
# #   current_coral_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # separate ecoregion country
    # criterion_a_data_table %<>%
      # separate(ecoregion_country,
               # into  = c("Ecoregion"),
               # sep   = "_",
               # extra = "drop")


 ## -- match ecoregion names -- ##
  # from criterion table
    criterion_a_data_table %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                 "Delagoa"
 # [3] "N Mozambique-S Tanzania" "Seychelles north"
 # [5] "N Tanzania-Kenya"        "East Madagascar"
 # [7] "North Madagascar"        "South Madagascar"
 # [9] "West Madagascar"         "Seychelles Outer"

  # check from reef area table
    reef_area_ecoregions %>% pull(Ecoregion) %>% unique()
 # [1] "Comoros"                   "Delagoa"
 # [3] "East Madagascar"           "Mascarene Isl"
 # [5] "N Mozambique - S Tanzania" "N Tanzania - Kenya"
 # [7] "North Madagascar"          "Seychelles Outer"
 # [9] "Seychelles North"          "South Madagascar"
# [11] "West Madagascar"

  # match ecoregion names
    criterion_a_data_table %<>%
      mutate(Ecoregion = ifelse(Ecoregion == "Mascarene Isl.",
                                             "Mascarene Isl", Ecoregion),
             Ecoregion = ifelse(Ecoregion == "N Mozambique-S Tanzania",
                                             "N Mozambique - S Tanzania", Ecoregion),
             Ecoregion = ifelse(Ecoregion == "N Tanzania-Kenya",
                                             "N Tanzania - Kenya", Ecoregion),
             Ecoregion = ifelse(Ecoregion == "Seychelles north",
                                             "Seychelles North", Ecoregion))


##
## 3. Evaluate criterion
##
  # set start threshold percent cover
    s_cover <- 1

  # set final threshold cover
    f_cover <- 10

  # set test interval
    t_interval <- 1

  # create empty object to hold results
    criterion_a_reduction_geographic_distribution <- tibble()

  # loop to calculate   # p=10  ## -- for testing -- ##
    for (p in seq(from = s_cover,
                  to   = f_cover,
                  by   = t_interval)){

      # set threshold
        dat <-
          criterion_a_data_table %>%
            mutate(threshold = p)

      # create ratio
        dat %<>%
          mutate(ratio = current_coral_cover / threshold)

     ## -- set to assign a 1 if it has collapsed -- ##
      # set colapse
        dat %<>%
          mutate(collapse = ifelse(ratio <= 1, 1, 0))

      # for each eco-region caculate proportion of sites which collapsed
        dat_collapse_raw <-
          dat %>%
            group_by(Ecoregion) %>%
            summarise(prop_collapse = (sum(collapse) / length(collapse)) %>% round(2))

     ## -- calculate weighted -- ##
      # link data
        dat_collapse_weighted <-
          dat_collapse_raw %>%
            left_join(reef_area_ecoregions %>%
                        dplyr::select(Ecoregion,
                                      prop_area))

      # calculate weighting
        dat_collapse_weighted %<>%
          mutate(weighted_prop = (prop_collapse * prop_area) %>% round(5))

     ## -- calculate regional statistics -- ##
      # calculate regional collapsed
        dat_collapse_regional <-
          dat_collapse_weighted %>%
            summarise(weighted_prop = weighted_prop %>% mean(na.rm = TRUE) %>% round(2),
                      prop_collapse = prop_collapse %>% mean(na.rm = TRUE) %>% round(2))

      # add weighted ecoregion identifier
        dat_collapse_regional %<>%
          mutate(Ecoregion = "Region")

     ## -- combine objects -- ##
      # join weighted & unweighted objects
        dat_collapse <-
          dat_collapse_weighted %>%
            dplyr::select(Ecoregion,
                          prop_collapse,
                          weighted_prop)

      # add regional statistics
        dat_collapse %<>%
          bind_rows(dat_collapse_regional)


     ## -- set threat categories -- ##
      # set threshold cover
        dat_collapse %<>%
          mutate(threshold = p)

      # set to long format
        dat_collapse %<>%
          rename(Unweighted = prop_collapse,
                 Weighted   = weighted_prop) %>%
          gather(Variable, prop_collapse,
                 -Ecoregion,
                 -threshold)

      # classify
        dat_collapse %<>%
          mutate(status = ifelse(prop_collapse >= 0.80,                        "CR",     NA),
                 status = ifelse(prop_collapse >= 0.50 & prop_collapse < 0.80, "EN", status),
                 status = ifelse(prop_collapse >= 0.30 & prop_collapse < 0.50, "VU", status),
                 status = ifelse(prop_collapse >= 0.27 & prop_collapse < 0.30, "NT", status),
                 status = ifelse(prop_collapse < 0.27,                         "LC", status))


      # harvest results
        criterion_a_reduction_geographic_distribution %<>%
          bind_rows(dat_collapse)


       }


##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_a_reduction_geographic_distribution,
      file = paste0(save_locale, "criterion_a_reduction_geographic_distribution.rda"))


##
## 5. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove parameter objects
    rm(s_cover,
       f_cover,
       t_interval)

  # remove intermediate objects
    rm(criterion_a_data_table)

  # remove core data objects
    rm(criterion_a_reduction_geographic_distribution)


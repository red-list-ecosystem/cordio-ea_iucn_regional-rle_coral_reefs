##
##  Name:       create_criterion_d_biotic_disruption_key_fish_consumers.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                key fish consumers
##
##  Approach:   Import fish data for Western Indian Ocean monitoring,
##                groom and evaluate criterion with the following
##                steps:
##               1. Randomly baseline value of coral cover for
##                    each Ecoregion from range
##               2. Calculate relative severity for each Ecoregion
##               3. For each station: calculate the relative severity:
##                    relative severity = 100 * (baseline - current) /
##                                         (baseline - threshold)
##               4. Determine the extent for each Ecoregion and
##                    calculate the proportion of sites with relative
##                    severity above 30, 50 , 80
##               5. Assign max threat category from each iteration
##                    for each Ecoregion for the following:
##                  extent >= 80% & relative_severity >= 80% -> (CR)
##                  extent >= 80% & relative_severity >= 50% -> (EN)
##                  extent >= 80% & relative_severity >= 30% -> (VU)
##                  extent >= 50% & relative_severity >= 80% -> (EN)
##                  extent >= 50% & relative_severity >= 50% -> (VU)
##                  extent >= 30% & relative_severity >= 80% -> (VU)
##
##                For each iteration, the max threat status for each Ecoregion
##                  stored.
##                Runs for different number of iterations are used to
##                  determine at what number of iterations the results stabilise
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-03-29
##

##  Notes:      1. Only evaluating two of the threshold models:
##                   ref max and model max
##              2. Need to validate the threshold_ser_model_max &
##                   threshold_ser_ref_max values    [ fs: 2024-04-29 ]

##
## 1. Set up
##
 ## -- call to fish baseline reference min & max -- ##
  # point to data locale
    data_locale <- "data_raw/biological/fishes/"

  # set data file name
    data_file <- "Criterion_D_fish_baseline_ref.xlsx"

  # call to data
    fish_baseline_reference <-
      paste0(data_locale, data_file) %>%
        read_xlsx()


 ## -- call to fish data table -- ##
  # point to data locale
    data_locale <- "data_raw/biological/fishes/"

  # set data file name
    data_file <- "Crit_D_fish_data_table.xlsx"

  # call to data
    fish_data_table <-
      paste0(data_locale, data_file) %>%
        read_xlsx()

##
## 2. Groom data
##
  # review reference data
    fish_baseline_reference
# # A tibble: 13 × 4
   # eco_rgn                   fish    baseline_mean baseline_sd
   # <chr>                     <chr>           <dbl>       <dbl>
 # 1 N Tanzania-Kenya          grouper         179.         441.
 # 2 Mascarene Isl.            grouper          77.2        269.
 # 3 Comoros                   grouper         379.         649.
 # 4 Seychelles north          grouper         168.         377.
 # 5 West Madagascar           grouper         235.         605.
 # 6 Delagoa                   grouper        1390.        3067.
 # 7 N Mozambique - S Tanzania grouper         253.         580.
 # 8 N Tanzania-Kenya          parrot          629.         740.
 # 9 Comoros                   parrot          493.         300.
# 10 Seychelles north          parrot          915.         729.
# 11 West Madagascar           parrot          310.         322.
# 12 Delagoa                   parrot          502.         304.
# 13 N Mozambique - S Tanzania parrot          867.         736.

  # review data table
    fish_data_table
# # A tibble: 368 × 9
   # eco_rgn site_id     first_year recent_year current_den recent_den
   # <chr>   <chr>            <dbl>       <dbl>       <dbl>      <dbl>
 # 1 Comoros Mayotte;Do…       2018        2018         180        180
 # 2 Comoros Mayotte;Do…       2018        2018         200        200
 # 3 Comoros Mayotte;Do…       2018        2018          60         60
 # 4 Comoros Mayotte;Do…       2018        2018          40         40
 # 5 Comoros Mayotte;Gr…       2018        2018         120        120
 # 6 Comoros Mayotte;Lo…       2018        2018          20         20
 # 7 Comoros Mayotte;Pa…       2018        2018        2840       2840
 # 8 Comoros Mayotte;Pa…       2013        2013         880        880
 # 9 Comoros Mayotte;Pa…       2013        2018        5490       9400
# 10 Comoros Mayotte;Pr…       2018        2018         140        140
# # ℹ 358 more rows
# # ℹ 3 more variables: threshold_ref_max <dbl>,
# #   threshold_ref_min <dbl>, fish <chr>
# # ℹ Use `print(n = ...)` to see more rows

##
## 3. Evaluate criterion
##
  # set iteration levels
    i_min <- 1
    i_max <- 750

  # set iteration interval
    i_interval <- 1

 ## -- Loop repeats four times to allow for the         ##
 ##      calculations of relative severity using        ##
 ##      the four different threshold values. Each      ##
 ##      time it calls the appropriate baseline file    ##
 ##      based on the threshold column used.            ##
 ##    The fish data table contains both the parrot     ##
 ##      and grouper data, and fish is used as a        ##
 ##      grouping variable to ensure that they can      ##
 ##      be analysed independently.                  -- ##

  # create empty object to hold results
    criterion_d_biotic_disruption_key_fish_consumers <- tibble()

  # set seed for reproducibility
    set.seed(66)

  # loop through iterations # i=1  ## -- for testing -- ##
    for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){

     ## -- first with baseline model -- ##
      # randomly assign baseline values
        baseline_bio <-
          fish_baseline_reference %>%
            dplyr::filter(!baseline_mean %>% is.na()) %>%
           group_by(eco_rgn) %>%
           mutate(baseline_random = rnorm(1, mean = baseline_mean,
                                               sd = baseline_sd))

      # set negative values to zero
        baseline_bio %<>%
          mutate(baseline_random = ifelse(baseline_random < 0, 0, baseline_random))

      # set iteration & method
        baseline_bio %<>%
          mutate(Iteration = i)


     ## -- harvest results -- ##
      # bind
        baseline_bio %<>%
          left_join(fish_data_table)

      # calculate rel severity max
        baseline_bio_max <-
          baseline_bio %>%
            mutate(relative_severity = 100 * (baseline_random - current_den) /
                                       (baseline_random - threshold_ref_max)) %>%
            mutate(Method = "ref_max")

      # calculate rel severity min
        baseline_bio_min <-
          baseline_bio %>%
            mutate(relative_severity = 100 * (baseline_random - current_den) /
                                   (baseline_random - threshold_ref_min)) %>%
            mutate(Method = "ref_min")


     ## -- combine objects -- ##
      # link
        baseline_bio %<>%
          bind_rows(baseline_bio_max,
                    baseline_bio_min)

      # bound by 0 and 100
        baseline_bio %<>%
          mutate(relative_severity = relative_severity %>% scales::rescale(to = c(0, 100)))

     ## -- determine severity status -- ##
      # set proportion of stations in each category of relative severity
        baseline_bio %<>%
          rename(Ecoregion = eco_rgn) %>%
          group_by(Ecoregion,
                   fish,
                   Method,
                   Iteration) %>%
          summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 &
                                          relative_severity <= 50) /  length(relative_severity),
                    rel_sev_50 = 100 * sum(relative_severity >= 50 &
                                          relative_severity <= 80) /  length(relative_severity),
                    rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                          relative_severity <= 100) / length(relative_severity))

     ## -- assign threat status -- ##
      # set status
        baseline_bio %<>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80),
                 status_80 = ifelse(rel_sev_80 >= 27 & rel_sev_80 < 30,   1, status_80),
                 status_50 = ifelse(rel_sev_50 >= 45 & rel_sev_50 < 50,   1, status_50),
                 status_30 = ifelse(rel_sev_30 >= 72 & rel_sev_30 < 80,   1, status_30))

      # set nas to zero
        baseline_bio %<>%
          mutate(status_30 = ifelse(is.na(status_30), 0, status_30),
                 status_50 = ifelse(is.na(status_50), 0, status_50),
                 status_80 = ifelse(is.na(status_80), 0, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        baseline_bio %<>%
          mutate(max_threat = pmax(status_30, status_50, status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              0,    "LC",
                              1,    "NT",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        baseline_bio %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))

     ## -- harvest results -- ##
      # combine
        criterion_d_biotic_disruption_key_fish_consumers %<>%
          bind_rows(baseline_bio)


   }


##
## 4. Review results
##
  # summarise
    criterion_d_biotic_disruption_key_fish_consumers %>%
      group_by(Ecoregion,
               fish,
               Method,
               status) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
# `summarise()` has grouped output by 'Ecoregion', 'fish',
# 'Method'. You can override using the `.groups` argument.
# # A tibble: 122 × 6
# # Groups:   Ecoregion, fish, Method [39]
   # Ecoregion fish    Method  status n_categories percent
   # <chr>     <chr>   <chr>   <chr>         <int>   <dbl>
 # 1 Comoros   grouper ref_max CR              525  70
 # 2 Comoros   grouper ref_max EN                7   0.933
 # 3 Comoros   grouper ref_max LC              211  28.1
 # 4 Comoros   grouper ref_max VU                7   0.933
 # 5 Comoros   grouper ref_min CR              523  69.7
 # 6 Comoros   grouper ref_min EN                7   0.933
 # 7 Comoros   grouper ref_min LC              211  28.1
 # 8 Comoros   grouper ref_min VU                9   1.2
 # 9 Comoros   grouper <NA>    LC              750 100
# 10 Comoros   parrot  ref_max CR              523  69.7
# # ℹ 112 more rows
# # ℹ Use `print(n = ...)` to see more rows


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_d_biotic_disruption_key_fish_consumers,
      file = paste0(save_locale, "criterion_d_biotic_disruption_key_fish_consumers.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects
    rm(i_min,
       i_max,
       i_interval,
       threat_conversions)

  # remove core objects
    rm(criterion_d_biotic_disruption_key_fish_consumers)


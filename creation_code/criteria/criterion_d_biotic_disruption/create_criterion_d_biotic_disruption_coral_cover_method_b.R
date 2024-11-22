##
##  Name:       create_criterion_d_biotic_disruption_coral-cover-method-b.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using coral cover method b
##
##  Approach:   Method a uses average hard coral cover for each 
##                Ecoregion to calculate relative severity.
##
##              Import raw data from gcrmn regional surveys from 
##                the Western Indian Ocean, groom and
##                apply criteria from collapse value:
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & James Mbugua
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##  Notes:      1. Suspect there is an error with the baseline sd
##                   for coral cover.  Should test with recalculation. [ fs: 2024-04-30 ]
##              2. Need to evaluate relative severity corrections and
##                   threat assignments with mishal's update    [fs: 2024-05-01 ]


##
## 1. Set up
##
 ## -- call to criterion d data table -- ##
  # point to data locale
    data_locale <- "data_raw/biological/percent_cover/"

  # point to data file
    data_file <- "Crit_D_HC_data_table.xlsx"

  # call to data
    criterion_d_data_table <-
      paste0(data_locale, data_file) %>%
      read_xlsx()

 ## -- call to hard coral baseline data -- ##
  # point to data locale
    data_locale <- "data_raw/biological/percent_cover/"

  # point to data file
    data_file <- "Criterion_D_hard_coral_baseline.xlsx"

  # call to data
    hard_coral_baseline <-
      paste0(data_locale, data_file) %>%
      read_xlsx()


##
## 2. Groom data
## 
  # review criterion d data table
    criterion_d_data_table
# # A tibble: 574 × 10
   # eco_rgn site_id    recent_year first_year no_years recent_coral
   # <chr>   <chr>            <dbl>      <dbl>    <dbl>        <dbl>
 # 1 Comoros Anjouan;A…        2017       2017        1         54.2
 # 2 Comoros Anjouan;A…        2015       2015        1         65  
 # 3 Comoros Anjouan;B…        2017       2017        1         23.8
 # 4 Comoros Anjouan;B…        2016       2016        1         34  
 # 5 Comoros Anjouan;V…        2016       2016        1         65  
 # 6 Comoros Anjouan;W…        2017       2015        2         61  
 # 7 Comoros Grand_Com…        2018       2018        1          3.3
 # 8 Comoros Grand_Com…        2018       2018        1          4.2
 # 9 Comoros Grand_Com…        2018       2018        1         17.5
# 10 Comoros Grande_Co…        2017       2017        1         57.2
# # ℹ 564 more rows
# # ℹ 4 more variables: current_cover <dbl>, lat <dbl>, long <dbl>,
# #   threshold_cover <dbl>
# # ℹ Use `print(n = ...)` to see more rows

  # review of hard coral baseline data
    hard_coral_baseline
# # A tibble: 11 × 4
   # eco_rgn                 Zone  baseline_mean baseline_sd
   # <chr>                   <lgl>         <dbl>       <dbl>
 # 1 Comoros                 NA             56.5        13.5
 # 2 N Tanzania-Kenya        NA             37.2        13.5
 # 3 West Madagascar         NA             50.9        13.5
 # 4 East Madagascar         NA             47.1        13.5
 # 5 Seychelles Outer        NA             47          13.5
 # 6 Mascarene Isl.          NA             43.2        13.5
 # 7 N Mozambique-S Tanzania NA             44.2        13.5
 # 8 Delagoa                 NA             39.6        13.5
 # 9 Seychelles north        NA             30.3        13.5
# 10 North Madagascar        NA             50.9        13.5
# 11 South Madagascar        NA             54.7        13.5

##
## 3. Evaluate criterion
##
 # set iteration levels
    i_min <- 10
    i_max <- 1e3

  # set iteration interval
    i_interval <- 10

  # create empty object to hold results
    criterion_d_biotic_disruption_coral_cover_method_b <- tibble()

  # loop through iterations # i=10  ## -- for testing -- ##
    for(i in seq(from = i_min,
                 to   = i_max,
                 by   = i_interval)){
  
     ## -- calculate severity -- ##
      # set seed for reproducibility
        set.seed(i + 81)

      # randomly assign baseline values
        dat <-
          criterion_d_data_table %>%
              rename(Ecoregion = eco_rgn) %>%
            left_join(hard_coral_baseline %>%
                        dplyr::select(Ecoregion = eco_rgn,
                                      baseline_mean,
                                      baseline_sd)) %>%
           group_by(Ecoregion) %>%
           mutate(baseline_cover = rnorm(1, mean = baseline_mean,
                                               sd = baseline_sd))

      # calculate relative severity
        dat %<>%
          mutate(relative_severity = 100 * (baseline_cover - current_cover) /
                                           (baseline_cover - threshold_cover))

      # bound by 0 and 100
        dat %<>%
          mutate(relative_severity = relative_severity %>% scales::rescale(to = c(0, 100)))

     ## -- determine extent -- ##
      # get proportion of stations for relative severity classes
        dat %<>%
          group_by(Ecoregion) %>%
            summarise(rel_sev_30 = 100 * sum(relative_severity >= 30 & 
                                             relative_severity < 50) /  
                                               length(relative_severity),
                      rel_sev_50 = 100 * sum(relative_severity >= 50 &  
                                             relative_severity < 80) / 
                                               length(relative_severity),
                      rel_sev_80 = 100 * sum(relative_severity >= 80 &
                                             relative_severity <= 100) / 
                                               length(relative_severity))
# # A tibble: 10 × 4
   # Ecoregion               rel_sev_30 rel_sev_50 rel_sev_80
   # <chr>                        <dbl>      <dbl>      <dbl>
 # 1 Comoros                      18.8        41.7      14.6 
 # 2 Delagoa                      18.8        35.4      37.5 
 # 3 East Madagascar              35.7        14.3      28.6 
 # 4 Mascarene Isl.               43.5        13.0       4.35
 # 5 N Mozambique-S Tanzania      21.4        50        17.3 
 # 6 N Tanzania-Kenya             15.9        47.8      30.1 
 # 7 North Madagascar             15.4        23.1      15.4 
 # 8 Seychelles Outer             15.2        39.4      30.3 
 # 9 Seychelles north              6.57       45.3      46.7 
# 10 West Madagascar              14.9        40.4      31.9 


 ## -- correction from mishal 2024-04-04 -- ##
  # need to re-evaluate from updated script
# # correct rel severity levels
# t_coral2$rel_30 <- rowSums(t_coral2[, c("rel_sev_30", "rel_sev_50", "rel_sev_80")])
# t_coral2$rel_50 <- rowSums(t_coral2[, c("rel_sev_50", "rel_sev_80")])

      # correct rel severity levels
        dat %>%
          mutate(rel_30 = (rel_sev_30 + rel_sev_50 + rel_sev_80),
                 rel_50 = (rel_sev_50 + rel_sev_80))
# # A tibble: 10 × 6
   # Ecoregion        rel_sev_30 rel_sev_50 rel_sev_80 rel_30 rel_50
   # <chr>                 <dbl>      <dbl>      <dbl>  <dbl>  <dbl>
 # 1 Comoros               18.8        41.7      14.6    75     56.2
 # 2 Delagoa               18.8        35.4      37.5    91.7   72.9
 # 3 East Madagascar       35.7        14.3      28.6    78.6   42.9
 # 4 Mascarene Isl.        43.5        13.0       4.35   60.9   17.4
 # 5 N Mozambique-S …      21.4        50        17.3    88.8   67.3
 # 6 N Tanzania-Kenya      15.9        47.8      30.1    93.8   77.9
 # 7 North Madagascar      15.4        23.1      15.4    53.8   38.5
 # 8 Seychelles Outer      15.2        39.4      30.3    84.8   69.7
 # 9 Seychelles north       6.57       45.3      46.7    98.5   92.0
# 10 West Madagascar       14.9        40.4      31.9    87.2   72.3


     ## -- assign threat status -- ##
      # set status
        dat %<>%
          mutate(status_30 = ifelse(rel_sev_30 >= 80 & rel_sev_30 <= 100, 2,        NA),
                 status_50 = ifelse(rel_sev_50 >= 80 & rel_sev_50 <= 100, 3,        NA),
                 status_50 = ifelse(rel_sev_50 >= 50 & rel_sev_50 < 80,   2, status_50),
                 status_80 = ifelse(rel_sev_80 >= 50 & rel_sev_80 < 80,   3,        NA),
                 status_80 = ifelse(rel_sev_80 >= 80 & rel_sev_80 <= 100, 4, status_80),  
                 status_80 = ifelse(rel_sev_80 >= 30 & rel_sev_80 < 50,   2, status_80))

      # set nas to 1
        dat %<>%
          mutate(status_30 = ifelse(is.na(status_30), 1, status_30),
                 status_50 = ifelse(is.na(status_50), 1, status_50),
                 status_80 = ifelse(is.na(status_80), 1, status_80))

     ## -- pick most severe categories -- ##
      # set max from status categories
        dat %<>%
         mutate(max_threat = pmax(status_30, 
                                  status_50, 
                                  status_80))

      # create conversion object for threat values
        threat_conversions <-
          tribble(~threat_value, ~status,
                              # 0,    "LC",
                              # 1,    "NT",
                              1, "NT/LC",
                              2,    "VU",
                              3,    "EN",
                              4,    "CR",
                              5,    "CO")

      # convert threat values
        dat %<>%
          left_join(threat_conversions %>%
                      rename(max_threat = threat_value))


      # set iteration
        dat %<>%
          mutate(Iteration = i)

      # harvest results
        criterion_d_biotic_disruption_coral_cover_method_b %<>%
          bind_rows(dat)


      }


##
## 4. Review results
##
  # summarise
    criterion_d_biotic_disruption_coral_cover_method_b %>%
      group_by(Ecoregion,
               status) %>%
      summarise(n_categories = n()) %>%
      mutate(percent = 100 * n_categories / sum(n_categories))
# `summarise()` has grouped output by 'Ecoregion'. You can override
# using the `.groups` argument.
# # A tibble: 14 × 4
# # Groups:   Ecoregion [10]
   # Ecoregion               status n_categories percent
   # <chr>                   <chr>         <int>   <dbl>
 # 1 Comoros                 NT/LC           100     100
 # 2 Delagoa                 NT/LC             1       1
 # 3 Delagoa                 VU               99      99
 # 4 East Madagascar         NT/LC           100     100
 # 5 Mascarene Isl.          NT/LC            99      99
 # 6 Mascarene Isl.          VU                1       1
 # 7 N Mozambique-S Tanzania VU              100     100
 # 8 N Tanzania-Kenya        VU              100     100
 # 9 North Madagascar        NT/LC           100     100
# 10 Seychelles Outer        NT/LC             1       1
# 11 Seychelles Outer        VU               99      99
# 12 Seychelles north        NT/LC             3       3
# 13 Seychelles north        VU               97      97
# 14 West Madagascar         VU              100     100

##
## 4. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/criteria/"

  # save to file
    save(criterion_d_biotic_disruption_coral_cover_method_b,
      file = paste0(save_locale, 
                    "criterion_d_biotic_disruption_coral_cover_method_b.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove intermediate objects


  # remove core objects
    rm(criterion_d_biotic_disruption_coral_cover_method_b)


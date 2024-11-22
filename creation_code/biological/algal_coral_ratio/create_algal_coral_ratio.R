##
##  Name:       create_algal_coral_ratio.R
##
##  Objective:  Standardise & format data for analysing criterion D:
##                using algal-coral ratio method
##
##  Approach:   Import degree heating weeks data and groom.
##
##              For each eco-region as the DHW values is an average
##              across the entire eco_region, can assume that the
##              relative severity was calculated over an extent
##              of 100% (>80%).
##
##              For each eco-region (time period C2a)
##              - extent >= 80% & relative_severity >= 80% -> (CR)
##              - extent >= 80% & relative_severity >= 50% -> (EN)
##              - extent >= 80% & relative_severity >= 30% -> (VU)
##
##              Output saved as *.rda
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-29
##

##
## 1. Set up
##
 ## -- call to percent cover data -- ##
  # point to data locale
    data_locale <- "data_intermediate/biological/percent_cover/"

  # set data file name
    data_file <- "gcrmn_percent_cover.rda"

  # call to data
    load(paste0(data_locale, data_file))

##
## 2. Groom data
##
  # have a look
    gcrmn_percent_cover
# # A tibble: 18,276 × 19
   # Country  Year Sector  Site  Station Reef.zone Depth level1_code mean_cover
   # <chr>   <dbl> <chr>   <chr> <chr>   <chr>     <chr> <chr>            <dbl>
 # 1 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    HC                 3.3
 # 2 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    SC                 0
 # 3 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    INV                0
 # 4 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    AMAC               1
 # 5 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    AHAL               0
 # 6 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    ACOR               7.3
 # 7 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    ATRF              56
 # 8 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    BS                 0
 # 9 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    RUB               25.7
# 10 Comoros  2018 Grand_… NA    GC_A_P… Fore_reef NA    SND                6.7
# # ℹ 18,266 more rows
# # ℹ 10 more variables: sd <chr>, number_replicates <dbl>, Latitude <dbl>,
# #   Longitude <dbl>, Organization <chr>, Period <chr>, Source <chr>,
# #   eco_rgn <chr>, site_id <chr>, ecoregion_country <chr>
# # ℹ Use `print(n = ...)` to see more rows

  # rename ecoregion
    gcrmn_percent_cover %<>%
      rename(Ecoregion = eco_rgn)


 ## -- filter sites without macroalgae data -- ##
  # set level 1 coral codes
    level1_corals <-
      c("HC")

  # set level 1 algal codes
    level1_algae <-
      c("AHAL",
        "AMAC",
        "ALG",
        "ATRF")

  # filter & summarise
    algal_coral_cover <-
      gcrmn_percent_cover %>%
        dplyr::filter(level1_code %in% c(level1_corals, level1_algae)) %>%
        mutate(level1_code = ifelse(level1_code %in% level1_algae, "FA", level1_code)) %>%
        group_by(Ecoregion,
                 Country,
                 site_id,
                 Year,
                 level1_code) %>%
        summarise(percent_cover_sum = mean_cover %>% sum(na.rm = TRUE))
# # A tibble: 4,981 × 6
# # Groups:   Ecoregion, Country, site_id, Year [2,693]
   # Ecoregion Country site_id            Year level1_code percent_cover_sum
   # <chr>     <chr>   <chr>             <dbl> <chr>                   <dbl>
 # 1 Comoros   Comoros Anjouan_Alamage_…  2003 HC                       25
 # 2 Comoros   Comoros Anjouan_Alamage_…  2004 HC                       18
 # 3 Comoros   Comoros Anjouan_Alamage_…  2005 HC                       19
 # 4 Comoros   Comoros Anjouan_Alamage_…  2007 HC                       24
 # 5 Comoros   Comoros Anjouan_Alamage_…  2011 HC                       24
 # 6 Comoros   Comoros Anjouan_Alamage_…  2017 HC                       54.2
 # 7 Comoros   Comoros Anjouan_Alamage_…  2015 HC                       65
 # 8 Comoros   Comoros Anjouan_Bambao_m…  2009 HC                       14
 # 9 Comoros   Comoros Anjouan_Bambao_m…  2011 HC                       25
# 10 Comoros   Comoros Anjouan_Bambao_m…  2017 HC                       23.8
# # ℹ 4,971 more rows
# # ℹ Use `print(n = ...)` to see more rows

 ## -- explore temporal resolution of data -- ##


 ## -- select time period -- ##

 ## -- Since we cannot extrapolate into future we will           ##
 ##      use values from 2013-2019.                              ##
 ##    Time period options for 50 year periods:                  ##
 ##      ~1969-2019: assumption here is that only minor          ##
 ##      declines between 1969-1997, so pre-1998 baseline        ##
 ##      values from literature can be assumed to be             ##
 ##      equivalent to 1969 (or slightly lower) -                ##
 ##      D1 past 50 years                                        ##
 ##    The advantage of this is that we don’t need to            ##
 ##      extrapolate into the future, which comes with a         ##
 ##      number of assumptions, particularly picking the         ##
 ##      pattern (non-linear change due to bleaching events). -- ##


 ## -- calculate current algal-coral ratio -- ##
  # calculate acr
    algal_coral_ratio <-
      algal_coral_cover %>%
        dplyr::filter(!percent_cover_sum %>% is.na()) %>%
        spread(level1_code, percent_cover_sum) %>%
        dplyr::filter(!FA %>% is.na(),
                      !HC %>% is.na()) %>%
        mutate(algal_coral_ratio = FA / (FA + HC))
# # A tibble: 2,288 × 7
# # Groups:   Ecoregion, Country, site_id, Year [2,288]
   # Ecoregion Country site_id            Year    FA    HC algal_coral_ratio
   # <chr>     <chr>   <chr>             <dbl> <dbl> <dbl>             <dbl>
 # 1 Comoros   Comoros Grand_Comore_NA_…  2018  57     3.3            0.945
 # 2 Comoros   Comoros Grand_Comore_NA_…  2018  77.8   4.2            0.949
 # 3 Comoros   Comoros Grand_Comore_NA_…  2018  63.9  17.5            0.785
 # 4 Comoros   Comoros Moheli_Ferenga_M…  2018  10.2  40.4            0.202
 # 5 Comoros   Comoros Moheli_Ferenga_M…  2018  16.9  60.1            0.219
 # 6 Comoros   Comoros Moheli_Itsamia_W…  2018  33.6  39.9            0.457
 # 7 Comoros   Comoros Moheli_Itsamia_W…  2018  28.2  39.9            0.414
 # 8 Comoros   Comoros MohÈli_Mea_Mea_F…  2017   1    62.9            0.0156
 # 9 Comoros   Comoros Moheli_Mirereni_…  2018  14.3  60.9            0.190
# 10 Comoros   Comoros Moheli_Mirereni_…  2018   6.4  78.5            0.0754
# # ℹ 2,278 more rows
# # ℹ Use `print(n = ...)` to see more rows

  # set cut off and average for all valuees in period
    algal_coral_ratio_summary <-
      algal_coral_ratio %>%
        group_by(Ecoregion,
                 Country,
                 site_id) %>%
        summarise(recent_year = Year %>% max(na.rm = TRUE),
                  first_year  = Year %>% min(na.rm = TRUE),
                  no_years    = Year %>% unique() %>% length(),
                  acr_recent  = algal_coral_ratio %>% head(1),
                  acr_mean    = algal_coral_ratio %>% mean(na.rm = TRUE))


##
## 5. Generate outputs
##
  # point to save locale
    save_locale <- "data_intermediate/biological/algal_coral_ratio/"

  # save to file
    save(algal_coral_ratio,
      file = paste0(save_locale, "algal_coral_ratio.rda"))

  # save summary to file
    save(algal_coral_ratio_summary,
      file = paste0(save_locale, "algal_coral_ratio_summary.rda"))


##
## 6. Clean up workspace
##
  # remove paths
    rm(data_locale,
       data_file,
       save_locale)

  # remove variables
    rm(level1_corals,
       level1_algae)

  # remove intermediate objects
    rm(gcrmn_percent_cover,
       algal_coral_cover)

  # remove core objects
    rm(algal_coral_ratio,
       algal_coral_ratio_summary)


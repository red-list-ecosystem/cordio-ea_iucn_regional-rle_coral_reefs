##
##  Name:       create_representative_concentration_pathways.R
##
##  Objective:  Combine raw data for degree heating weeks from
##                representative_concentration_pathways
##
##  Approach:   Import dhw projection data, combine and
##              save.
##
##              Output saved as *.rda
##
##
##  Authors:    Franz Smith & Mishal Gudka
##              CORDIO East Africa
##
##  Date:       2024-02-23
##

##  Notes:      1. Criterion C will be assessed using exceedance 
##                 of DHW levels that cause severe coral bleaching
##                 and mortality over a 50-year period into the future.
##              2. Degree-Heating-Weeks (DHW) projections are from 
##                 model outputs which were adjusted to the mean and
##                 annual cycle of observations of SST based on the
##                 OISST V2 1982-2005 climatology (van Hooidonk & Huber, 2012; 
##                 van Hooidonk et al., 2013).
##              3. Degree heating months were calculated by summing 
##                 the positive anomalies above the warmest monthly 
##                 temperature from the OISST V2 1982-2005 climatology 
##                 (Reynolds et al., 2002) for each 3-month period. 
##                 Degree heating months are then converted into DHWs by 
##                 multiplying by 4.35 (see also Donner et al., 2005; 
##                 van Hooidonk et al., 2013). DHWs were regridded to 
##                 720x360 using cdo remapbil.
##              4. Should point to original source data for dhw


##
## 1. Set up
##
  # set data locale
    data_locale <-
      "data_raw/geophysical/representative_concentration_pathways/"

  # get data files
    data_files <-
      paste0(# project_locale, 
             data_locale) %>%
      list.files(pattern    = "RCP",
                 full.names = FALSE)

 ## -- import data -- ##
  # create empty object to hold results
    representative_concentration_pathways <- tibble()

  # loop
    for(i in 1:length(data_files)){

      # import data
        dat <-
          paste0(# project_locale, 
                 data_locale,
                 data_files[i]) %>%
          read_csv()

      # add origin file
        dat %<>%
          mutate(file_name = data_files[i] %>% str_remove(".csv"))

      # set to long
        dat %<>%
          gather(Ecoregion, DHW,
                 -file_name,
                 -Year)

      # harvest results
        representative_concentration_pathways %<>%
          bind_rows(dat)

    }


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


##
## 3. Generate outputs
##
  # point to save locale
    save_locale <- 
      "data_intermediate/geophysical/representative_concentration_pathways/"

  # save to file
    save(representative_concentration_pathways,
      file = paste0(save_locale, "representative_concentration_pathways.rda"))

##
## 4. Clean up workspace
##
  # remove paths
    rm(# project_locale,
       data_locale,
       data_files,
       save_locale)

  # remove intermediate objects

  # remove core objects
    rm(representative_concentration_pathways)


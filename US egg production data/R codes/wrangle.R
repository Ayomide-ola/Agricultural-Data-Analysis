# This script requires data frames created in the companion script `parse.R`
# Please run the shell script `make.R` to load all dependencies.

# This script is annotated carefully to provide details about variable units.

#   ____________________________________________________________________________
#   cage-free eggs report                                                   ####

# Data frame constructed in `parse.R` script, containing cage-free variables
tbl_cagefree %<>%

  # convert to numbers
  dplyr::mutate(
    # remove commas and non-numeric characters
    n_hens = as.numeric(stringr::str_replace_all(n_hens, ",", "")),
    n_eggs_weekly = as.numeric(
      stringr::str_replace_all(n_eggs_weekly, ",", "")
    ),
    lay_rate = as.numeric(stringr::str_replace(lay_rate, "%", ""))
  ) %>%

  # clean up observed_month by assigning observed month and year
  dplyr::mutate(
    # assign year based on original report title and report publication
    # for all months except observed December (comes from January publication)
    year = stringr::str_sub(source, 3, 6),
    observed_month = dplyr::case_when(
      observed_month == "" ~ "August 2016",
      observed_month == "December" ~ paste(
        observed_month, as.character(as.integer(year) - 1)
      ),
      TRUE ~ paste(observed_month, year)
    ),
    observed_month = lubridate::parse_date_time(observed_month, "mY"),
    observed_month = lubridate::as_date(observed_month)
  ) %>%

  # remove year variable
  dplyr::select(-year) %>%

  # compute montly production of cage-free eggs
  # (from weekly 30-dozen cases of eggs to monthly number of eggs)
  dplyr::mutate(
    n_eggs = n_eggs_weekly * 30 * 12 *
      (lubridate::days_in_month(observed_month) / 7)
  ) %>%

  # select and order variables and omit lay rates
  # (can be constructed by end user as n_eggs/n_hens)
  dplyr::select(
    observed_month, prod_type, prod_process, n_hens, n_eggs, source
  ) %>%

  # remove aggregated cage-free eggs/hens (can be calculated by end user)
  dplyr::filter(prod_process != "cage-free")


#   ____________________________________________________________________________
#   chicken and eggs report                                                 ####

tbl_chickegg %<>%

  # convert to numbers
  dplyr::mutate(
    n_hens = as.integer(stringr::str_replace_all(n_hens, ",", "")),
    n_eggs = as.numeric(stringr::str_replace_all(n_eggs, ",", "")),
  ) %>%

  # convert units (from monthly 1000s of hens to monthly number of hens
  # and monthly millions of eggs to monthly number of eggs)
  dplyr::mutate(
    # number of hens
    n_hens = n_hens * 1000,
    # monthly production of eggs
    n_eggs = n_eggs * 1000000
  ) %>%

  # clean up observed_month and handful of missing prod_process values
  dplyr::mutate(
    year = dplyr::case_when(
      nchar(source) == 23 ~ stringr::str_sub(source, 16, 19),
      nchar(source) == 12 ~ paste0("20", stringr::str_sub(source, 7, 8))
    ),
    observed_month = dplyr::case_when(
      observed_month == "November" &
        source == "ChicEggs-12-21-2018.pdf" ~ paste(
          observed_month, "2018"
        ),
      observed_month == "December" | observed_month == "November" ~ paste(
        observed_month, as.character(as.integer(year) - 1)
      ),
      TRUE ~ paste(observed_month, year)
    ),
    observed_month = lubridate::parse_date_time(observed_month, "mY"),
    observed_month = lubridate::as_date(observed_month)
  ) %>%

  # remove year variable
  dplyr::select(-year)


#   ____________________________________________________________________________
#   combine prod data                                                       ####

tbl_allegg_prod <- dplyr::bind_rows(
  tbl_cagefree,
  tbl_chickegg
) %>%

  # create factor variables
  dplyr::mutate(
    prod_type = as.factor(prod_type),
    prod_process = as.factor(prod_process),
    source = as.factor(source)
  ) %>%

  # sort data by production type, then process, then month
  dplyr::arrange(prod_type, prod_process, observed_month) %>%
  
  # use last day of the month
  dplyr::mutate(
    observed_month = lubridate::ceiling_date(observed_month, "month") - lubridate::days(1)
  )


##  ____________________________________________________________________________
##  write data to file                                                      ####

# write to csv file in the "final" subdirectory
readr::write_csv(tbl_allegg_prod, paste0(path_final, "egg-production.csv"))

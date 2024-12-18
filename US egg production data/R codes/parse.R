# This script requires data frames created in the companion script `download.R`
# Please run the shell script `make.R` to load all dependencies.

#   ____________________________________________________________________________
#   functions                                                               ####

extract_items <- function(pdf_data, vct_identifier, no_items) {
  text <- pdf_data$text
  index <- which(
    zoo::rollapply(text, length(vct_identifier), identical, vct_identifier)
  )
  if (length(index) > 0) {
    index <- index[[1]]
    text[(index + length(vct_identifier)):
    (index + length(vct_identifier) - 1 + no_items)]
  } else {
    ""
  }
}

extract_prod_est <- function(vct) {
  tibble::tribble(
    ~prod_type, ~prod_process, ~n_hens, ~n_eggs_weekly, ~lay_rate,
    "table eggs", "cage-free", vct[[38]], vct[[46]], vct[[42]],
    "table eggs", "cage-free (non-organic)", vct[[10]], vct[[28]], vct[[18]],
    "table eggs", "cage-free (organic)", vct[[5]], vct[[22]], vct[[14]]
  )
}


#   ____________________________________________________________________________
#   cage-free eggs report                                                   ####

pdf_paths_cf <- list.files(path_cagefree, full.names = TRUE)

lst_pdf_data_cf <- pdf_paths_cf %>%
  purrr::map(~pdftools::pdf_data(.x)[[1]]) %>%
  stats::setNames(basename(pdf_paths_cf))

##  ............................................................................
##  extract observed months                                                 ####

ident_month_cf <- c(
  "For",
  "the",
  "Month",
  "of"
)

lst_month_cf <- lst_pdf_data_cf %>%
  purrr::map(~extract_items(.x, ident_month_cf, 1))


##  ............................................................................
##  extract production data                                                 ####

ident_prod <- c(
  "Certified",
  "Organic",
  "Cage-Free",
  "Layers",
  "Non-Organic",
  "Cage-Free",
  "Layers"
)

lst_prod <- lst_pdf_data_cf %>%
  purrr::map(~extract_items(.x, ident_prod, 48)) %>%
  purrr::map(~extract_prod_est(.x))


#   ............................................................................
#   combine data                                                            ####

tbl_cagefree <- list(
  a = lst_month_cf,
  b = lst_prod,
  c = basename(pdf_paths_cf)
) %>%
  purrr::pmap(function(a, b, c) cbind(observed_month = a, b, source = c)) %>%
  purrr::map(~dplyr::mutate_all(.x, as.character)) %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble()


#   ____________________________________________________________________________
#   chicken and eggs report                                                 ####

pdf_paths_ce <- list.files(path_chickegg, full.names = TRUE)

lst_pdf_data_ce <- pdf_paths_ce %>%
  # use page 6 for the more accurate measure of the previous month
  purrr::map(~pdftools::pdf_data(.x)[[6]]) %>%
  stats::setNames(basename(pdf_paths_ce))


##  ............................................................................
##  extract observed months                                                 ####

ident_month_ce <- c(
  "Eggs",
  "produced",
  "during"
)

lst_month_ce <- lst_pdf_data_ce %>%
  purrr::map(~extract_items(.x, ident_month_ce, 1))


##  ............................................................................
##  extract hens                                                            ####

ident_hens_ce <- c(
  "1,000",
  "Egg-type",
  "hatching"
)

lst_hens_ce <- lst_pdf_data_ce %>%
  purrr::map(~extract_items(.x, ident_hens_ce, 10))

lst_hens_all <- lst_hens_ce %>%
  purrr::map(~tibble::tibble(
    prod_type = "table eggs",
    prod_process = "all",
    n_hens = .x[[9]]
  ))

lst_hens_hatch <- lst_hens_ce %>%
  purrr::map(~tibble::tibble(
    prod_type = "hatching eggs",
    prod_process = "all",
    n_hens = .x[[10]]
  ))


##  ............................................................................
##  extract eggs                                                            ####

ident_eggs_ce <- c(
  "million",
  "Egg-type",
  "hatching"
)

lst_eggs_ce <- lst_pdf_data_ce %>%
  purrr::map(~extract_items(.x, ident_eggs_ce, 10))

lst_eggs_all <- lst_eggs_ce %>%
  purrr::map(~tibble::tibble(
    n_eggs = .x[[9]]
  ))

lst_eggs_hatch <- lst_eggs_ce %>%
  purrr::map(~tibble::tibble(
    n_eggs = .x[[10]]
  ))


##  ............................................................................
##  government shutdown                                                     ####

# note:
# no report was released in January 2019 due to the shutdown
# extract data for December 2018 from the February report

tbl_pdf_data_shutdown <- pdftools::pdf_data(
  paste0(path_chickegg, "ChicEggs-12-21-2018.pdf")
)[[7]]

lst_hens_shutdown <- tbl_pdf_data_shutdown %>%
  extract_items(ident_hens_ce, 10)

lst_eggs_shutdown <- tbl_pdf_data_shutdown %>%
  extract_items(ident_eggs_ce, 10)

tbl_chickegg_shutdown <- tibble::tribble(
  ~prod_type, ~prod_process, ~n_hens, ~n_eggs,
  "table eggs", "all", lst_hens_shutdown[[9]], lst_eggs_shutdown[[9]],
  "hatching eggs", "all", lst_hens_shutdown[[10]], lst_eggs_shutdown[[10]]
) %>%
  dplyr::mutate(
    observed_month = "November",
    source = "ChicEggs-12-21-2018.pdf"
  ) %>%
  dplyr::select(
    observed_month, dplyr::everything()
  )


##  ............................................................................
##  current month                                                           ####

# from the latest PDF, extract also the (possibly less accurate) data for the
# current month

pdf_chickegg_latest <- tibble::tibble(
  pdf = basename(pdf_paths_ce),
  date = lubridate::as_date(
    stringr::str_sub(pdf, 10, 19),
    format = "%m-%d-%Y", tz = "UTC"
  )
) %>%
  dplyr::top_n(1, date) %>%
  .$pdf

tbl_pdf_data_latest <- pdftools::pdf_data(
  paste0(path_chickegg, pdf_chickegg_latest)
)[[7]]

lst_month_latest <- tbl_pdf_data_latest %>%
  extract_items(ident_month_ce, 1)

lst_hens_latest <- tbl_pdf_data_latest %>%
  extract_items(ident_hens_ce, 10)

lst_eggs_latest <- tbl_pdf_data_latest %>%
  extract_items(ident_eggs_ce, 10)

tbl_chickegg_latest <- tibble::tribble(
  ~prod_type, ~prod_process, ~n_hens, ~n_eggs,
  "table eggs", "all", lst_hens_latest[[9]], lst_eggs_latest[[9]],
  "hatching eggs", "all", lst_hens_latest[[10]], lst_eggs_latest[[10]]
) %>%
  dplyr::mutate(
    observed_month = lst_month_latest[[1]],
    source = pdf_chickegg_latest
  ) %>%
  dplyr::select(
    observed_month, dplyr::everything()
  )


##  ............................................................................
##  combine                                                                 ####

tbl_chickegg_all <- list(
  a = lst_month_ce,
  b = lst_hens_all,
  c = lst_eggs_all,
  d = basename(pdf_paths_ce)
) %>%
  purrr::pmap(
    function(a, b, c, d) cbind(observed_month = a, b, c, source = d)
  ) %>%
  purrr::map(~dplyr::mutate_all(.x, as.character)) %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble()

tbl_chickegg_hatch <- list(
  a = lst_month_ce,
  b = lst_hens_hatch,
  c = lst_eggs_hatch,
  d = basename(pdf_paths_ce)
) %>%
  purrr::pmap(
    function(a, b, c, d) cbind(observed_month = a, b, c, source = d)
  ) %>%
  purrr::map(~dplyr::mutate_all(.x, as.character)) %>%
  dplyr::bind_rows() %>%
  tibble::as_tibble()

tbl_chickegg <- dplyr::bind_rows(
  tbl_chickegg_all,
  tbl_chickegg_hatch,
  tbl_chickegg_shutdown,
  tbl_chickegg_latest
)

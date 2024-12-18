#   ____________________________________________________________________________
#   functions                                                               ####

# Compares list of pdfs with pdfs already saved at given path.
# If not all pdfs are present, downloads the remaining pdfs from AMS website.
website_download_missing <- function(vct_pdfs, path) {
  to_download <- !basename(vct_pdfs) %in% list.files(path)

  vct_pdfs[to_download] %>%
    purrr::map(
      ~download.file(.x, paste0(path, basename(.x)), mode = "wb")
    )
}

# Returns list of pdfs for a given API search query.
api_get_pdf_list <- function(url, headers) {
  httr::GET(url, httr::add_headers(headers)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    tibble::as_tibble()
}

# Compares list of pdfs with pdfs already saved at given path.
# If not all pdfs are present, downloads the remaining pdfs using Cornell API.
api_download_missing <- function(tbl_pdfs, path) {
  to_download <- tbl_pdfs %>%
    dplyr::filter(!files_to %in% list.files(path))

  purrr::map2(
    to_download$files,
    to_download$files_to,
    ~download.file(.x, paste0(path, .y), mode = "wb")
  )
}


#   ____________________________________________________________________________
#   download "Cage-free Eggs" reports                                       ####

##  ............................................................................
##  from AMS website (until end of 2018)                                    ####

# The Cornell website only hosts this report from January 2019 onwards. The code
# below downloads all pre-2019 reports from the AMS website.
vct_cagefree <- c(
  "https://search.ams.usda.gov/mndms/2016/09/PY20160919MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2016/10/PY20161004MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2016/11/PY20161102MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2016/12/PY20161205MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/01/PY20170109MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/02/PY20170206MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/03/PY20170306MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/04/PY20170403MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/05/PY20170503MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/06/PY20170605MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/06/PY20170630MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/08/PY20170808MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/09/PY20170905MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/10/PY20171002MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/11/PY20171107MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2017/12/PY20171204MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/01/PY20180103MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/02/PY20180206MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/03/PY20180305MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/04/PY20180402MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/05/PY20180507MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/06/PY20180604MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/06/PY20180629MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/07/PY20180731MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/09/PY20180904MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/10/PY20181001MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/11/PY20181102MCAGEFREE.PDF",
  "https://search.ams.usda.gov/mndms/2018/12/PY20181203MCAGEFREE.PDF"
)

website_download_missing(vct_cagefree, path_cagefree)


##  ............................................................................
##  using Cornell API (from 2019 on)                                        ####

api_base <- "https://usda.library.cornell.edu/api/v1/"
api_endpoint <- "release/findByPubId/"
api_key <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOjM3OTJ9.wdmXHBeR9ctJncXEr5p_HCTDysUlhOPvI9Ap5P2ovvA"
api_headers <- c(
  accept = "application/json",
  Authorization = paste("Bearer", api_key)
)

api_cagefree <- paste0(
  api_base, api_endpoint, "rj4304553",
  "?lastest=false&start_date=", date_start, "&end_date=", date_end
)

tbl_pdf_cagefree_api <- api_get_pdf_list(api_cagefree, api_headers) %>%
  dplyr::mutate(
    files_to = paste0(
      "PY", stringr::str_replace_all(release_datetime, "-", ""), "MCAGEFREE.PDF"
    )
  )

api_download_missing(tbl_pdf_cagefree_api, path_cagefree)


#   ____________________________________________________________________________
#   download "Chicken and Eggs" reports (using Cornell API)                 ####

api_chickegg <- paste0(
  api_base, api_endpoint, "fb494842n",
  "?lastest=false&start_date=", date_start, "&end_date=", date_end
)

tbl_pdf_chickegg_api <- api_get_pdf_list(api_chickegg, api_headers)
tbl_pdf_chickegg_api <- tibble::tibble(
  date = tbl_pdf_chickegg_api$release_datetime,
  files = tbl_pdf_chickegg_api$files %>%
    purrr::map(
      ~stringr::str_subset(.x, "(ChicEggs|ckeg).*\\.pdf")
    ),
  match = tbl_pdf_chickegg_api$files %>%
    purrr::map(
      ~stringr::str_subset(.x, "(ChicEggs|ckeg).*\\.pdf")
    ) %>%
    purrr::map(
      ~length(.x) == 1
    ) %>%
    purrr::as_vector()
) %>%
  dplyr::filter(match) %>%
  dplyr::mutate(
    files_to = paste0(
      "ChicEggs-",
      stringr::str_sub(date, 6, 7), "-",
      stringr::str_sub(date, 9, 10), "-",
      stringr::str_sub(date, 1, 4),
      ".pdf"
    )
  )

api_download_missing(tbl_pdf_chickegg_api, path_chickegg)

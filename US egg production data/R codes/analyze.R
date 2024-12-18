# This script requires data frames created in the companion script `parse.R`
# Please run the shell script `make.R` to load all dependencies.

# This script is annotated carefully to provide details about the data analysis.

##  ____________________________________________________________________________
##  construct cage-free percentages                                         ####

# data frame of historic annual cage-free percentages
annual_percent_hens <- tibble::tribble(
  ~observed_month, ~percent_hens, ~source,
  "2007-12-31", 3.2, "Egg-Markets-Overview-2019-10-19.pdf",
  "2008-12-31", 3.5, "Egg-Markets-Overview-2019-10-19.pdf",
  "2009-12-31", 3.6, "Egg-Markets-Overview-2019-10-19.pdf",
  "2010-12-31", 4.4, "Egg-Markets-Overview-2019-10-19.pdf",
  "2011-12-31", 5.4, "Egg-Markets-Overview-2019-10-19.pdf",
  "2012-12-31", 6.0, "Egg-Markets-Overview-2019-10-19.pdf",
  "2013-12-31", 5.9, "Egg-Markets-Overview-2019-10-19.pdf",
  "2014-12-31", 5.7, "Egg-Markets-Overview-2019-10-19.pdf",
  "2015-12-31", 8.6, "Egg-Markets-Overview-2019-10-19.pdf",
  "2016-04-30", 9.9, "Egg-Markets-Overview-2016-12-02.pdf",
  "2016-08-31", 12.0, "Egg-Markets-Overview-2016-12-02.pdf",
  "2016-12-31", 12.3, "Egg-Markets-Overview-2019-10-19.pdf",
  "2017-02-10", 12.7, "Egg-Markets-Overview-2017-02-10.pdf",
  "2017-03-31", 12.9, "Egg-Markets-Overview-2017-04-28.pdf",
  "2017-05-01", 14.1, "Egg-Markets-Overview-2017-06-02.pdf",
  "2017-07-01", 14.2, "Egg-Markets-Overview-2017-08-11.pdf", #truncated report
  "2017-09-01", 14.6, "Egg-Markets-Overview-2017-09-29.pdf",
  "2017-11-01", 16.6, "Egg-Markets-Overview-2017-11-24.pdf",
  "2017-12-31", 16.6, "Egg-Markets-Overview-2019-10-19.pdf",
  "2018-03-09", 16.9, "Egg-Markets-Overview-2018-03-09.pdf",
  "2018-10-31", 18.4, "Egg-Markets-Overview-2019-10-19.pdf",
  "2019-03-15", 18.4, "Egg-Markets-Overview-2019-03-15.pdf", #from graph 
  "2019-05-03", 19.2, "Egg-Markets-Overview-2019-05-03.pdf", #from graph 
  "2019-08-01", 21.8, "Egg-Markets-Overview-2019-08-09.pdf",
  "2019-09-01", 23.2, "Egg-Markets-Overview-2019-09-06.pdf", #from graph 
  "2019-10-25", 24.1, "Egg-Markets-Overview-2019-10-25.pdf",
  "2019-11-25", 23.9, "Egg-Markets-Overview-2019-11-29.pdf",
  "2019-12-27", 23.4, "Egg-Markets-Overview-2019-12-27.pdf",
  "2020-01-24", 23.3, "Egg-Markets-Overview-2020-01-24.pdf",
  "2020-02-28", 23.6, "Egg-Markets-Overview-2020-03-06.pdf",
  "2020-03-24", 26.2, "Egg-Markets-Overview-2020-03-27.pdf",
  "2020-04-21", 26.2, "Egg-Markets-Overview-2020-04-24.pdf",
  "2020-05-29", 26.6, "Egg-Markets-Overview-2020-05-29.pdf",  #from graph
  "2020-06-26", 27.4, "Egg-Markets-Overview-2020-06-26.pdf",
  "2020-07-27", 27.6, "Egg-Markets-Overview-2020-07-27.pdf",
  "2020-08-28", 28.0, "Egg-Markets-Overview-2020-08-28.pdf",  #from graph
  "2020-09-25", 28.5, "Egg-Markets-Overview-2020-09-25.pdf",
  "2020-10-30", 28.5, "Egg-Markets-Overview-2020-10-30.pdf",
  "2020-11-27", 28.3, "Egg-Markets-Overview-2020-11-27.pdf",
  "2020-12-23", 28.2, "Egg-Markets-Overview-2020-12-25.pdf",
  "2021-01-25", 28.5, "Egg-Markets-Overview-2021-01-25.pdf",
  "2021-02-28", 29.2, "Egg-Markets-Overview-2021-03-05.pdf",
  "2021-03-26", 29.2, "Egg-Markets-Overview-2021-03-26.pdf",
  "2021-04-23", 29.7, "Egg-Markets-Overview-2021-04-23.pdf",
  "2021-05-28", 30.3, "Egg-Markets-Overview-2021-05-28.pdf",
  "2021-06-25", 30.6, "Egg-Markets-Overview-2021-06-25.pdf",
  "2021-07-23", 30.5, "Egg-Markets-Overview-2021-07-23.pdf",
  "2021-08-27", 31.8, "Egg-Markets-Overview-2021-08-27.pdf",
  "2021-09-24", 32.3, "Egg-Markets-Overview-2021-09-24.pdf",
  "2021-10-29", 32.2, "Egg-Markets-Overview-2021-10-29.pdf",
  "2021-11-26", 31.9, "Egg-Markets-Overview-2021-11-26.pdf",
  "2021-12-24", 33.9, "Egg-Markets-Overview-2021-12-24.pdf",
  "2022-01-21", 34.1, "Egg-Markets-Overview-2022-01-21.pdf",  
  "2022-02-28", 34.4, "Egg-Markets-Overview-2022-03-04.pdf",
  "2022-03-31", 33.7, "Egg-Markets-Overview-2022-04-04.pdf",
  "2022-04-29", 36.0, "Egg-Markets-Overview-2022-04-29.pdf",
  "2022-05-27", 35.5, "Egg-Markets-Overview-2022-05-27.pdf",
  "2022-06-24", 34.8, "Egg-Markets-Overview-2022-06-24.pdf",
  "2022-07-29", 35.1, "Egg-Markets-Overview-2022-07-29.pdf",
  "2022-08-26", 34.7, "Egg-Markets-Overview-2022-08-26.pdf",
  "2022-09-30", 34.4, "Egg-Markets-Overview-2022-09-30.pdf",
  "2022-10-28", 34.8, "Egg-Markets-Overview-2022-10-28.pdf",
  "2022-11-25", 34.6, "Egg-Markets-Overview-2022-11-25.pdf",
  "2022-12-23", 34.4, "Egg-Markets-Overview-2022-12-23.pdf",
  "2023-01-27", 36.3, "Egg-Markets-Overview-2022-01-27.pdf",
  "2023-03-03", 37.1, "Egg-Markets-Overview-2022-03-03.pdf",
  "2023-03-31", 38.3, "Egg-Markets-Overview-2022-03-31.pdf",

) %>%
  dplyr::mutate(
    observed_month = as.Date(observed_month)
  )

# compute cage-free percentages
cf_percentages <- tbl_allegg_prod %>%
  dplyr::filter(prod_type == "table eggs") %>%
  dplyr::group_by(observed_month) %>%
  dplyr::summarize(
    percent_hens = (
      n_hens[prod_process == "cage-free (organic)"][1] +
        n_hens[prod_process == "cage-free (non-organic)"][1]
    )*100 /
      n_hens[prod_process == "all"][1],
    percent_eggs = (
      n_eggs[prod_process == "cage-free (organic)"][1] +
        n_eggs[prod_process == "cage-free (non-organic)"][1]
    )*100 /
      n_eggs[prod_process == "all"][1],
  ) %>%
  dplyr::mutate(source = "computed") %>%

  # join with historic annual data
  dplyr::full_join(
    annual_percent_hens,
    by = c("observed_month", "source", "percent_hens")
  ) %>%
  dplyr::filter(!is.na(percent_hens)) %>%
  dplyr::arrange(observed_month)

# write data frame to csv file in the "final" subdirectory
readr::write_csv(cf_percentages, paste0(path_final, "cage-free-percentages.csv"))

##  ____________________________________________________________________________
##  graph cage-free hen percentages from Overview report                    ####

plot_fig_percent_overview <- function(color, name = NULL) {

  # dynamically set upper y-axis limit
  y_limit <- round(max(cf_percentages$percent_hens), 2) + 1

  p <- subset(cf_percentages, source != "computed") %>%
    ggplot2::ggplot(ggplot2::aes(x = observed_month, y = percent_hens)) +
    ggplot2::geom_point(color = color, size = 1) +
    ggplot2::geom_line(color = color) +
    ggplot2::scale_x_date(
      breaks = scales::date_breaks("1 year"),
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      labels = function(x) paste0(x, '%'),
      # Sets y axis at zero
      expand = c(0, 0),
      limits = c(0, y_limit)
    ) +
    ggplot2::labs(
      x = "Date (year)",
      y = "Percentage of US hens in cage-free housing"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        color = "#1D252C", face = "bold"
      ),
      panel.background = ggplot2::element_rect(
        fill = "white", colour = "white", size = 0.5, linetype = "solid"
      ),
      panel.grid.major = ggplot2::element_line(
        size = 0.5, linetype = "solid", colour = "grey"
      ),
      panel.grid.minor = ggplot2::element_line(
        size = 0.21, linetype = "solid", colour = "grey"
      )
    )

   if (!is.null(name)){
     ggplot2::ggsave(
       paste0("report/images/", name, ".png"),
       p,
       width = 20, height = 10, units = "cm"
     )
   } else {
     p
   }
}

##  ____________________________________________________________________________
##  graph cage-free hen percentages from Cage-Free Egg report               ####

plot_fig_percent_monthly <- function(color, name = NULL) {
  
  # dynamically set upper y-axis limit
  y_limit <- round(max(cf_percentages$percent_hens), 2) + 1
  
  p <- subset(cf_percentages, source == "computed") %>%
    ggplot2::ggplot(ggplot2::aes(x = observed_month, y = percent_hens)) +
    ggplot2::geom_point(color = color, size = 1) +
    ggplot2::geom_line(color = color) +
    ggplot2::scale_x_date(
      breaks = scales::date_breaks("1 year"),
      date_labels = "%Y"
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n = 10),
      labels = function(x) paste0(x, '%'),
      # Sets y axis at zero
      expand = c(0, 0),
      limits = c(0, y_limit)
    ) +
    ggplot2::labs(
      x = "Date (year)",
      y = "Percentage of US hens in cage-free housing"
    ) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        color = "#1D252C", face = "bold"
      ),
      panel.background = ggplot2::element_rect(
        fill = "white", colour = "white", size = 0.5, linetype = "solid"
      ),
      panel.grid.major = ggplot2::element_line(
        size = 0.5, linetype = "solid", colour = "grey"
      ),
      panel.grid.minor = ggplot2::element_line(
        size = 0.21, linetype = "solid", colour = "grey"
      )
    )
  
  if (!is.null(name)){
    ggplot2::ggsave(
      paste0("report/images/", name, ".png"),
      p,
      width = 20, height = 10, units = "cm"
    )
  } else {
    p
  }
}

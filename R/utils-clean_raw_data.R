#' clean_raw_data
#'
#' Takes historical data and compresses it into values for the most recent day and calculates aggregate/daily values metrics - totals, means, new
#' @param data historical data table
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @return data table with most recent data for each jurisdiction with historical data nested
#' @import dplyr
#' @export
#'
#' @examples
#'
#'
clean_raw_data <- function(data,
                           date_column = Date,
                           state_abbr  = NULL,
                           lag         = 0,
                           start_date  = NULL){


  data %>%
        rename(date  := {{date_column}},
               juris := {{state_abbr}}) %>%
        mutate(juris := case_when(
            juris == "MP" ~ "CNMI",
            juris == "VI" ~ "USVI",
            juris == "MH" ~ "RMI",
            TRUE ~ juris)) %>%
        dplyr::filter(date  <= lubridate::today()-lag)%>%
        dplyr::filter(if (is.null(start_date)) date else date >=lubridate::ymd(start_date))

}


clean_raw_hosp_nat_data <- function(data,
                           date_column = Date,
                           lag         = 0,
                           start_date  = NULL){


    data %>%
        rename(date  := {{date_column}}) %>%
        filter(date  <= lubridate::today()-lag)%>%
        filter(if (is.null(start_date)) date else date >=lubridate::ymd(start_date))

}

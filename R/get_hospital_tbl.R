#' Load, clean, and analyze state hospital data
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @export
#' @import dplyr
#' @return returns aggregate hospital table with metrics
#' @export
#'
#' @examples
get_hosp_juris_tbl <- function(username,
                                date_column = "collection_date",
                                state_abbr  = 'state',
                                lag         = 2,
                                start_date  = "2020-01-01"){

    pop <- readr::read_rds(here::here("data/pop.rds"))

    read_csv(paste0("C:/Users/",
                    username,
                    "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/HOSPITAL DATA/hospitals_state_daily_incident_management.csv"),
             guess_max = 3000,
             show_col_types = FALSE)  |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date) |>
        select(juris,
               date,
               adm_all_covid_confirmed,
               avg_adm_all_covid_confirmed
        ) |>
        mutate(hosp_adm    = adm_all_covid_confirmed,
               hosp_cum    = cumsum(hosp_adm),
               hosp_avg    = avg_adm_all_covid_confirmed |> round(),
               hosp_new    = adm_all_covid_confirmed) |>
        group_by(date,juris) |>
        summarise(
            hosp_cum = hosp_cum %>% format(justify  = "right",
                                           big.mark = ","),
            hosp_new = hosp_new,
            hosp_avg = hosp_avg
        ) |>
        ungroup() |>
        left_join(pop) |>
        group_by(juris) |>
        mutate(
            hosp_change = get_per_change(hosp_avg, date),
            hosp_7cum   = get_juris_7day_cum_per_100k(hosp_new, pop = pop)
        ) |>
        ungroup() |>
        rename(hosp_date = date)



}

#' Load, clean, and analyze regional hospital data
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @export
#' @import dplyr
#' @return returns aggregate hospital table with metrics
#' @export
#'
#' @examples
get_hosp_reg_tbl <- function(username,
                               date_column = "collection_date",
                               state_abbr  = 'state',
                               lag         = 2,
                               start_date  = "2020-01-01"){

    pop <- readr::read_rds(here::here("data/pop.rds"))

    read_csv(paste0("C:/Users/",
                    username,
                    "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/HOSPITAL DATA/hospitals_state_daily_incident_management.csv"),
             guess_max = 3000,
             show_col_types = FALSE)  |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date) |>
        select(juris,
               date,
               adm_all_covid_confirmed,
               avg_adm_all_covid_confirmed
        ) |>
        mutate(hosp_adm    = adm_all_covid_confirmed,
               hosp_cum    = cumsum(hosp_adm),
               hosp_avg    = avg_adm_all_covid_confirmed |> round(),
               hosp_new    = adm_all_covid_confirmed) |>
        group_by(date,juris) |>
        summarise(
            hosp_cum = hosp_cum %>% format(justify  = "right",
                                           big.mark = ","),
            hosp_new = hosp_new,
            hosp_avg = hosp_avg
        ) |>
        ungroup() |>
        left_join(pop) |>
        group_by(juris) |>
        mutate(
            hosp_change = get_per_change(hosp_avg, date),
            hosp_7cum   = get_juris_7day_cum_per_100k(hosp_new, pop = pop)
        ) |>
        ungroup() |>
        rename(hosp_date = date)



}

#' get_hosp_nat_tbl
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @export
#' @import dplyr
#' @return returns aggregate hospital table with metrics
#' @export
#'
#' @examples
get_hosp_nat_tbl <- function(username,
                               date_column = "collection_date",
                               lag         = 2,
                               start_date  = "2020-01-01"){



    read_csv(paste0("C:/Users/",
                    username,
                    "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/HOSPITAL DATA/hospitals_country_daily_incident_management.csv"),
             guess_max      = 3000,
             show_col_types = FALSE)  |>
        clean_raw_hosp_nat_data(date_column = date_column,
                                lag         = lag,
                                start_date  = start_date) |>
        select(date,
               adm_all_covid_confirmed,
               avg_adm_all_covid_confirmed
               ) |>
        mutate(hosp_adm    = adm_all_covid_confirmed,
               hosp_cum    = cumsum(hosp_adm),
               hosp_avg    = avg_adm_all_covid_confirmed |> round(),
               hosp_new    = adm_all_covid_confirmed) |>
        group_by(date) |>
        summarise(
            hosp_cum = hosp_cum %>% format(justify  = "right",
                                              big.mark = ","),
            hosp_new = hosp_new,
            hosp_avg = hosp_avg
            ) |>
        ungroup() |>
        mutate(
            hosp_change = get_per_change(hosp_avg, date),
            hosp_7cum   = get_7day_cum_per_100k(hosp_new),
            hosp_date   = date
            ) |>
        select(-date)
}

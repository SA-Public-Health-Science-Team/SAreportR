#' Load, clean, and analyze case and death tab
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @return returns aggregate case and death table with metrics
#' @export
#' @import dplyr
#'
#' @examples
get_case_death_juris_tbl <- function(username,
                                 date_column = "submission_date",
                                 state_abbr  = "state_abbr",
                                 lag         = 0,
                                 start_date  = "2020-01-01"){

   pop <-  readr::read_rds(here::here("data/pop.rds"))
    # case and death data
readr::read_csv(paste0(
        "C:/Users/",
        username,
        "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/DOMESTIC CASE COUNTS/US CASE COUNTS SPREADSHEET/CDC Final US Case Counts/final_counts_check.csv"),
        guess_max = 3000,
        show_col_types = FALSE)  |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date)|>
        select(state_name,
               juris,
               date,
               tot_cases,
               New_case,
               tot_death,
               new_death) |>
        mutate(juris = if_else(juris == "NY", "NYX", juris)) |>

        group_by(date, juris, state_name) |>
        summarise(cases_cum  = sum(tot_cases),
                  cases_new  = sum(New_case),
                  deaths_cum = sum(tot_death),
                  deaths_new = sum(new_death)
        ) |>
        ungroup() |>
        left_join(pop) |>
        group_by(juris,state_name) |>
        mutate(
            cases_avg           = get_mean(cases_new) |> round(),
            deaths_avg          = get_mean(deaths_new)|> round(),
            cases_7cum          = get_juris_7day_cum_per_100k(cases_new, pop = pop),
            deaths_7cum         = get_juris_7day_cum_per_100k(deaths_new,pop = pop),
            cases_change        = get_per_change(cases_avg, date),
            deaths_change       = get_per_change(deaths_avg, date),
            cases_avg_per_100k  = (cases_avg/pop) *100000,
            deaths_avg_per_100k = (deaths_avg/pop)*100000
        )|>
        ungroup() |>
    # mutate(across(!ends_with("change"), ~ format(.x,
    #                                              justify = "right",
    #                                              big.mark   = ","))) |>
        mutate(cases_date  = date,
               deaths_date = date,
               juris = juris |> stringr::str_trim()) |>
        select(-date)
}


get_case_death_nat_tbl <- function(username,
                                     date_column = "submission_date",
                                     state_abbr  = "state_abbr",
                                     lag         = 0,
                                     start_date  = "2020-01-01"){
    # case and death dat
    readr::read_csv(paste0(
        "C:/Users/",
        username,
        "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/DOMESTIC CASE COUNTS/US CASE COUNTS SPREADSHEET/CDC Final US Case Counts/final_counts_check.csv"),
        guess_max = 3000,
        show_col_types = FALSE)  |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date)|>
        select(date,
               tot_cases,
               New_case,
               tot_death,
               new_death) |>
        group_by(date) |>
        summarise(cases_cum  = sum(tot_cases),
                  cases_new  = sum(New_case),
                  deaths_cum = sum(tot_death),
                  deaths_new = sum(new_death)
        ) |>
        ungroup() |>
        mutate(
            cases_avg           = get_mean(cases_new) |> round(),
            deaths_avg          = get_mean(deaths_new)|> round(),
            cases_7cum          = get_7day_cum_per_100k(cases_new),
            deaths_7cum         = get_7day_cum_per_100k(deaths_new),
            cases_change        = get_per_change(cases_avg, date),
            deaths_change       = get_per_change(deaths_avg, date)
        )|>
        # mutate(across(!ends_with("change"), ~ format(.x,
        #                                              justify = "right",
        #                                              big.mark   = ","))) |>
        mutate(cases_date  = date,
               deaths_date = date) |>
        select(-date)
}

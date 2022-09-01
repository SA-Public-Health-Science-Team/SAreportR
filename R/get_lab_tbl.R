#' Load, clean, and analyze lab data
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @export
#' @import dplyr
#'
#' @return returns aggregate lab table with metrics
#' @export
#'
#' @examples
get_lab_nat_tbl <- function(username,
                         date_column = "date",
                         state_abbr  = "state",
                         lag         = 3,
                         start_date  = "2020-03-01"){

    readr::read_csv(paste0(
        "C:/Users/",
        username,
        "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/OD LAB REPORT ANALYSIS/Pct Positive PBI Project/unified datasets/unified_reporting_results_states_historical.csv"),
        guess_max  = 3000,
        show_col_types = FALSE) |>
        dplyr::filter(!(date >=  lubridate::ymd("2022-02-16") & state == "IA")) |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date) |>
        group_by(date) |>
        summarise(new_test_vol = sum(new_test_results_reported),
                  pos_test     = sum(new_positive_test_results_reported),
                  new_pos      = 100*sum(new_positive_test_results_reported)/sum(new_test_results_reported)
        ) |>
        ungroup() |>
        mutate(testvol_avg = get_mean(new_test_vol) |> lag(order_by = date, n = 4),
               testvol_cum = cumsum(new_test_vol),
               testpos_cum = 100 * cumsum(pos_test)/ cumsum(new_test_vol),
               testpos_avg = 100 * get_7day_cum(pos_test) / get_7day_cum(new_test_vol)
        ) |>
        mutate(testpos_cum = 100 * cumsum(pos_test)/ cumsum(new_test_vol),
               testpos_avg = 100 * get_7day_cum(pos_test) / get_7day_cum(new_test_vol),
               testvol_7cum = get_7day_cum_per_100k(new_test_vol)

        ) |>
        mutate(testvol_change =  get_per_change(testvol_avg, date),
               testpos_change =  get_per_change(testpos_avg, date)
        ) |>
        rename(testvol_date = date) |>
        mutate(testpos_date = testvol_date,
               testpos_avg  = scales::label_percent(decimal.mark = ".",accuracy = .1)(testpos_avg *.01))|>
        mutate(testvol_avg  = testvol_avg |> round(),
               testpos_cum  = testpos_cum |> round(1)
        )|>
        mutate(across(starts_with("testvol"),
                      ~ format(.x,
                               justify = "right",
                               big.mark   = ",")))|>
        mutate(testpos_cum  = scales::label_percent(decimal.mark = ".",
                                                    accuracy = .1)(testpos_cum *.01))   |>

        select(-pos_test)
}


#' get_lab_juris_tbl
#'
#' @param username cpu username of user compiling the report.
#' @param date_column name of date column to be renamed
#' @param state_abbr name of column with state and territory abbreviations to be renamed and standardize abbreviations
#' @param start_date first day date to filter
#' @param lag number of days to remove from the most recent date
#' @export
#' @import dplyr
#'
#' @return returns aggregate lab table with metrics
#' @export
#'
#' @examples
get_lab_juris_tbl <- function(username,
                            date_column = "date",
                            state_abbr  = "state",
                            lag         = 3,
                            start_date  = "2020-03-01"){

    pop <- readr::read_rds(here::here("data/pop.rds"))

    readr::read_csv(paste0(
        "C:/Users/",
        username,
        "/CDC/Situational Awareness COVID-19 Response - WORKING FOLDER/OD LAB REPORT ANALYSIS/Pct Positive PBI Project/unified datasets/unified_reporting_results_states_historical.csv"),
        guess_max  = 3000,
        show_col_types = FALSE) |>
        dplyr::filter(!(date >=  lubridate::ymd("2022-02-16") & state == "IA")) |>
        clean_raw_data(date_column = date_column,
                       state_abbr  = state_abbr,
                       lag         = lag,
                       start_date  = start_date) |>
        group_by(date, juris, state_name) |>
        summarise(new_test_vol = sum(new_test_results_reported),
                  pos_test     = sum(new_positive_test_results_reported),
                  new_pos      = 100*sum(new_positive_test_results_reported)/sum(new_test_results_reported)
        ) |>
        ungroup() |>
        left_join(pop) |>
        group_by(juris, state_name) |>
        mutate(testvol_avg = get_mean(new_test_vol) |> lag(order_by = date, n = 4),
               testvol_cum = cumsum(new_test_vol),
               testpos_cum = 100 * cumsum(pos_test)/ cumsum(new_test_vol),
               testpos_avg = 100 * get_7day_cum(pos_test) / get_7day_cum(new_test_vol)
        ) |>
        mutate(testpos_cum  = 100 * cumsum(pos_test)/ cumsum(new_test_vol),
               testpos_avg  = 100 * get_7day_cum(pos_test) / get_7day_cum(new_test_vol),
               testvol_7cum = get_juris_7day_cum_per_100k(new_test_vol, pop = pop)

        ) |>
        mutate(testvol_change =  get_per_change(testvol_avg, date),
               testpos_change =  get_per_change(testpos_avg, date)
        ) |>
        rename(testvol_date = date) |>
        mutate(testpos_date = testvol_date,
               testpos_avg  = scales::label_percent(decimal.mark = ".",accuracy = .1)(testpos_avg *.01))|>
        mutate(testvol_avg  = testvol_avg |> round(),
               testpos_cum  = testpos_cum |> round(1)
        )|>
        mutate(across(starts_with("testvol"),
                      ~ format(.x,
                               justify = "right",
                               big.mark   = ",")))|>
        mutate(testpos_cum  = scales::label_percent(decimal.mark = ".",
                                                    accuracy = .1)(testpos_cum *.01))   |>

        select(-pos_test) |>
        ungroup()
}

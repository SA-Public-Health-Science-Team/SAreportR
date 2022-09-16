#' create_summary_table
#'
#' @param ... single row data sets with most recent metrics
#'
#' @return summary table
#' @export
#'
#' @examples
create_summary_table <-  function(...){
    bind_cols(...)  |>
    pivot_longer( cols = everything(),
                  names_to = "indicator",
                  values_to = "val",
                  values_transform = list(val = as.character)) |>
    separate(indicator, c("indicator", "metric"), sep="_")  |>
    pivot_wider(names_from = metric,
                values_from = val,
                values_fill = "N/A") |>
    mutate(indicator = indicator  |>  factor(levels = c("cases",
                                                        "hosp",
                                                        "deaths",
                                                        "testvol",
                                                        "testpos"))
    )  |>
    arrange(indicator)  |>
    mutate(indicator = case_when(
        indicator == "cases"   ~ "Cases",
        indicator == "hosp"    ~ "Hospital Admissions",
        indicator == "deaths"  ~ "Deaths",
        indicator == "testvol" ~ "Test Volume",
        indicator == "testpos" ~ "Test Positivity",
        TRUE ~ "NA")
    ) %>%
    select(-date)

}

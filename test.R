region <- 1

case_death_tbl <- read_rds(here::here("data/cases_deaths_juris_tbl.rds"))



case_death_tbl |>
    mutate(date = cases_date) |>
    filter(date >= max(date)-30) |>
    arrange(date) |>
    group_by(state) |>
    summarise(cases_trend =  list(cases_new),
              deaths_trend =  list(deaths_new),
              cases_cum = cases_cum[which.max(date)],
              cases_new = cases_new[which.max(date)],
              cases_avg = cases_avg[which.max(date)],
              cases_avg_per_100k = cases_avg_per_100k[which.max(date)],
              deaths_cum = deaths_cum[which.max(date)],
              deaths_new = deaths_new[which.max(date)],
              deaths_avg = deaths_avg[which.max(date)],
              deaths_avg_per_100k = deaths_avg_per_100k[which.max(date)],
              .groups = "drop"
              ) |>
    select(-cases_trend,-deaths_trend) |>
    gt()   |>
    fmt_integer(columns = c("cases_cum",
                           "cases_new",
                           "cases_avg",
                           "deaths_cum",
                           "deaths_new",
                           "deaths_avg")) |>
    fmt_number(columns = c("cases_avg_per_100k",
                           'deaths_avg_per_100k'),
               decimals = 1) |>
    # gt_plt_sparkline(deaths_trend,
    #                  palette = c("black",
    #                              "black",
    #                              "black",
    #                              "red",
    #                              "lightgrey"),
    #                  fig_dim = c(15,25),
    #                  same_limit = TRUE,
    #                  label = FALSE) |>
    # gt_plt_dist(cases_trend) |>
    gt_color_rows(columns = c("cases_avg","deaths_avg")) |>
    gt_theme_espn()


l


gt_sparkline_tab <- mtcars %>%
    dplyr::group_by(cyl) %>%
    # must end up with list of data for each row in the input dataframe
    dplyr::summarize(mpg_data = list(mpg), .groups = "drop")

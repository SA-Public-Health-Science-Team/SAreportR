#' plot daily, weekly, and monthly epi curves
#'
#' @param data_tbl indicator table with daily values
#' @param timeframe indicate timeframe, options are "Daily", "Weekly", and "Monthly"
#' @param color color of epicurve bars
#'
#' @return
#' @export
#'
#' @examples
plot_epi_curve <- function(data_tbl, timeframe, color){


    date_col <-  data_tbl |>
        select(contains("date")) |>
        select(1) |>
        colnames()

    new_col <-  data_tbl |>
        select(contains("new")) |>
        select(1) |>
        colnames()


    data <-  data_tbl |>
        rename(date  := {{date_col}},
               new   := {{new_col}}) |>
        mutate(date       = date  |> lubridate::ymd(),
               day        = format(date, "%b %d"),
               week       = lubridate::week(date),
               month      = lubridate::month(date,
                                             label = TRUE,
                                             abbr = TRUE),
               year       = lubridate::year(date),
               day_year   = glue::glue('{day}, {year}'),
               week_year  = glue::glue('{week}-{year}'),
               month_year = glue::glue('{month}-{year}'))


    period <- if_else(timeframe == "Daily",  "day_year",
                      if_else(timeframe == "Weekly", "week_year",
                              if_else(timeframe == "Monthly", "month_year", "date")))

    avg_range <- if_else(timeframe == "Daily",  7,
                         if_else(timeframe == "Weekly", 14,
                                 if_else(timeframe == "Monthly",0,0)))
   prepped_data <-  data |>
        rename(period := {{period}}) |>
        select(date, new, period) |> arrange(date) |>
        mutate( avg_holder  = get_mean(new, days = avg_range)) |>
        group_by(period) |>
        summarise(date  = max(date),
                  new   = sum(new),
                  avg   = avg_holder [which.max(date)] ,
                  group = 'group') |> arrange(date) |>
        ungroup() |>
        mutate(period = fct_reorder(period,
                                    date))

   ylimit <- max(prepped_data$new) * 1.1

   prepped_data |>
        ggplot(aes(y = new,
                   x = period)) +
        geom_col(fill=color)  +
        geom_line(aes(y = avg,
                      group = group),
                  size=1) +

         scale_y_continuous(labels = scales::label_number(big.mark = ","),
        #                    sec.axis =  sec_axis( trans=~./7, name="14-Day Average"),
                            limits = c(0, ylimit)) +
        theme(axis.text.x = element_text(angle = 40,
                                         vjust=0.5)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
}

#' get_per_change
#'
#' @param column avg column
#' @param column_date date column
#'
#' @return formatted percent change column
#' @export
#'
#' @examples
get_per_change <- function(column, column_date){

per_change <-  100 * (column - lag(column,
                   order_by = column_date,
                   7))/lag(column,
                           order_by = column_date,
                           7)

per_change_formatted <- per_change |>
    round(1)

scales::label_percent(decimal.mark = ".",accuracy = .1)(per_change_formatted *.01)
}

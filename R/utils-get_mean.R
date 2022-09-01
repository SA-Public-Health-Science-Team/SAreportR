#' get_mean
#'
#' @param column column to calculate mean
#' @param days date range, number of days to calculate mean
#'
#' @return mean column
#'
#' @export
#'
#' @examples
#'
get_mean <- function(column,
                     days = 7){

    column %>%
        zoo::rollmean(days,
                 align = "right",
                 fill = 0)
}



#' get_mean_per_100k
#'
#' @param column column to calculate mean
#' @param days date range, number of days to calculate mean
#' @param pop population data
#'
#' @return
#' @export
#'
#' @examples
get_mean_per_100k <- function(column,
                              days  = 7,
                              pop   = pop){

    avg_7day_rate <-  get_mean(column) / pop *100000

    avg_7day_rate_formatted <- avg_7day_rate %>%
        round(1) %>%
        format(justify = "right",
               big.mark   = ",",
               nsmall = 1)
    return(avg_7day_rate_formatted)
}


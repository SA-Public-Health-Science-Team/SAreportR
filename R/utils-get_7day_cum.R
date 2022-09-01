#' Get rolling cumulative metrics
#'
#' @param data column to calculate sum
#' @param days date range, number of days to calculate sum, default is 7
#'
#' @return sum column
#'
#' @export
#'
#' @examples
get_7day_cum <- function(column,
                         days = 7){
    column %>%
        zoo::rollsum(days,
                     align = "right",
                     fill = NA)
}



#' Get rolling cumulative metrics
#'
#' @param data column to calculate sum
#' @param days date range, number of days to calculate sum, default is 14
#'
#' @return sum column
#'
#' @export
#'
#' @examples
get_14day_cum <- function(column,
                         days = 14){
    column %>%
        zoo::rollsum(days,
                     align = "right",
                     fill = NA)
}
#' get_7day_cum_per_100k
#'
#' @param data column to calculate sum
#' @param days date range, number of days to calculate sum
#'
#' @return sum column
#'
#' @export
#'
#' @examples
get_7day_cum_per_100k <- function(column,
                                  days = 7){
    us_pop <- 331996199

    cum_7day <- get_7day_cum(column)

    cum_7day_rate <- cum_7day / us_pop *100000

    cum_7day_rate_formatted <- cum_7day_rate %>%
        round(1) %>%
        format(justify = "right",
               big.mark   = ",",
               nsmall = 1)
    return(cum_7day_rate_formatted)
}



get_juris_7day_cum_per_100k <- function(column,
                                        days  = 7,
                                        pop   = pop){

    cum_7day_rate <- get_7day_cum(column) / pop *100000

    cum_7day_rate_formatted <- cum_7day_rate %>%
        round(1) %>%
        format(justify = "right",
               big.mark   = ",",
               nsmall = 1)
    return(cum_7day_rate_formatted)
}

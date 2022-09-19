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



#' get_new_per_100k
#'
#' @param column column to calculate mean
#' @param days date range, number of days to calculate mean
#' @param pop population data
#'
#' @return
#' @export
#'
#' @examples
get_new_per_100k <- function(column,
                              days  = 7,
                              pop   = pop){

    new_7day_rate <-  get_7day_cum(column) / pop *100000

    return(new_7day_rate)
}


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


#' get_7day_cum_per_100k by jurisdiction
#'
#' @param data column to calculate sum
#' @param days date range, number of days to calculate sum
#'
#' @return sum column
#'
#' @export
#'
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

